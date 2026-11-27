package providers

// The check for diag.iif_guard_ineffective (issue #372): an IIf whose
// condition tests an array or an object, and whose value argument for the
// "it is there" outcome reads from it. IIf evaluates both value arguments,
// so the read runs whatever the condition says, and the test protects
// nothing.

import (
	"fmt"
	"strconv"
	"strings"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// iifProof is what one outcome of a condition proves about a subject.
type iifProof int

const (
	// proofSized: the subject has at least one element (an `Empty` or
	// `Len` test). It protects an element read.
	proofSized iifProof = 1 << iota
	// proofNotNil: the subject is not NIL (a comparison with NIL, or an
	// `Empty` test of a name that declares an object). It protects an
	// element read and a member read.
	proofNotNil
)

// iifProofs maps a subject's key (stableExprKey) to what is proved of it.
type iifProofs map[string]iifProof

// iifRead is a read of a proved subject found in a value argument.
type iifRead struct {
	// expr is the element or member read.
	expr *parser.Expr
	// proof is what the condition proved of the subject.
	proof iifProof
	// lead is how many of an element read's leading subscripts belong to
	// the subject: 1 for `x[3, 1]` read against a test of `x[3]`.
	lead int
}

// checkIIfGuardIneffective flags the first read, in an `IIf` value
// argument, of something the condition proves is there only when that
// argument is the one selected. See the file comment.
func checkIIfGuardIneffective(tokens []lexer.Token, calls []parser.CallSite) []Diagnostic {
	var diagnostics []Diagnostic
	reported := map[int]bool{}

	for _, call := range calls {
		if call.Qualified() || !strings.EqualFold(call.Name, "IIf") || len(call.Args) != 3 {
			continue
		}
		whenTrue, whenFalse := conditionProofs(call.Args[0])
		for i, proofs := range []iifProofs{whenTrue, whenFalse} {
			if len(proofs) == 0 {
				continue
			}
			read, found := firstProtectedRead(call.Args[i+1], proofs)
			if !found || reported[read.expr.Start] {
				continue
			}
			rng, ok := exprRange(tokens, read.expr)
			if !ok {
				continue
			}
			reported[read.expr.Start] = true
			state := "empty"
			if read.proof&proofSized == 0 {
				state = "NIL"
			}
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    rng,
				Message: fmt.Sprintf("IIf evaluates both value arguments, so %s is read even when %s is %s, which raises an error. "+
					"Make the test an :IF and read the value inside it.", exprSourceText(tokens, read.expr), readSubjectText(tokens, read), state),
				Source: "ssl-lsp",
				Code:   CodeIIfGuardIneffective,
			})
		}
	}

	return diagnostics
}

// conditionProofs returns what a condition proves about its subjects when
// it is true and when it is false. Only shapes whose meaning is certain
// contribute: an `Empty(x)` call, a `Len(x)` or `ALen(x)` call compared
// with a whole-number literal, and a comparison with NIL, combined with
// `.NOT.`, `.AND.`, `.OR.` and parentheses. Anything else proves nothing.
func conditionProofs(cond *parser.Expr) (whenTrue, whenFalse iifProofs) {
	whenTrue, whenFalse = iifProofs{}, iifProofs{}
	if cond == nil {
		return
	}
	switch cond.Kind {
	case parser.ExprGroup:
		if len(cond.Children) == 1 {
			return conditionProofs(cond.Children[0])
		}
	case parser.ExprUnary:
		if len(cond.Children) == 1 && (cond.Op == "!" || strings.EqualFold(cond.Op, ".NOT.")) {
			t, f := conditionProofs(cond.Children[0])
			return f, t
		}
	case parser.ExprCall:
		// Empty(x): false proves x has content.
		if subject, ok := singleArgCall(cond, "Empty"); ok {
			if key, stable := stableExprKey(subject); stable {
				proof := proofSized
				if declaresObject(subject) {
					proof |= proofNotNil
				}
				whenFalse[key] = proof
			}
		}
	case parser.ExprBinary:
		if len(cond.Children) != 2 {
			return
		}
		left, right := cond.Children[0], cond.Children[1]
		switch op := strings.ToUpper(cond.Op); op {
		case ".AND.":
			// True: both operands are true. False: at least one is false,
			// so only what both prove when false still holds.
			lt, lf := conditionProofs(left)
			rt, rf := conditionProofs(right)
			return unionProofs(lt, rt), intersectProofs(lf, rf)
		case ".OR.":
			lt, lf := conditionProofs(left)
			rt, rf := conditionProofs(right)
			return intersectProofs(lt, rt), unionProofs(lf, rf)
		case "==", "=", "!=", "<>", "#", "<", "<=", ">", ">=":
			comparisonProofs(op, left, right, whenTrue, whenFalse)
		}
	}
	return
}

// comparisonProofs records what `left op right` proves: a subject compared
// with NIL, or a length compared with a whole-number literal.
func comparisonProofs(op string, left, right *parser.Expr, whenTrue, whenFalse iifProofs) {
	left, right = unwrapGroup(left), unwrapGroup(right)
	equality := op == "==" || op == "="
	inequality := op == "!=" || op == "<>" || op == "#"

	// x == NIL, NIL != x
	if equality || inequality {
		subject := (*parser.Expr)(nil)
		switch {
		case isNilExpr(right) && !isNilExpr(left):
			subject = left
		case isNilExpr(left) && !isNilExpr(right):
			subject = right
		}
		if subject != nil {
			if key, stable := stableExprKey(subject); stable {
				if equality {
					whenFalse[key] |= proofNotNil
				} else {
					whenTrue[key] |= proofNotNil
				}
			}
			return
		}
	}

	// Len(x) op N, N op Len(x)
	subject, okLeft := lengthCall(left)
	n, okRight := wholeNumberLiteral(right)
	if !okLeft || !okRight {
		subject, okLeft = lengthCall(right)
		n, okRight = wholeNumberLiteral(left)
		if !okLeft || !okRight {
			return
		}
		op = flipComparison(op)
	}
	key, stable := stableExprKey(subject)
	if !stable {
		return
	}
	// A length is never negative. Each case asks: does this outcome prove
	// the length is at least 1?
	switch op {
	case ">": // true: len >= n+1
		whenTrue[key] |= proofSized
	case ">=": // true: len >= n
		if n >= 1 {
			whenTrue[key] |= proofSized
		}
	case "<": // false: len >= n
		if n >= 1 {
			whenFalse[key] |= proofSized
		}
	case "<=": // false: len >= n+1
		whenFalse[key] |= proofSized
	case "==", "=": // true: len == n; false: len != n
		if n >= 1 {
			whenTrue[key] |= proofSized
		} else {
			whenFalse[key] |= proofSized
		}
	case "!=", "<>", "#": // true: len != n; false: len == n
		if n >= 1 {
			whenFalse[key] |= proofSized
		} else {
			whenTrue[key] |= proofSized
		}
	}
}

// firstProtectedRead returns the first read, in source order, of a subject
// in proofs inside a value argument: an element read (`x[...]`) of any
// proved subject, or a member read (`x:name`, `x:Name(...)`) of one proved
// not NIL. `.AND.` and `.OR.` short-circuit, so a read in a right operand
// is skipped when the left operand already tests its subject.
func firstProtectedRead(arg *parser.Expr, proofs iifProofs) (iifRead, bool) {
	if arg == nil || len(proofs) == 0 {
		return iifRead{}, false
	}
	switch arg.Kind {
	case parser.ExprBinary:
		if len(arg.Children) == 2 {
			op := strings.ToUpper(arg.Op)
			if op == ".AND." || op == ".OR." {
				if read, found := firstProtectedRead(arg.Children[0], proofs); found {
					return read, true
				}
				// The right operand runs only when the left was true
				// (.AND.) or false (.OR.): what the left proves there is
				// a real guard.
				leftTrue, leftFalse := conditionProofs(arg.Children[0])
				guarded := leftTrue
				if op == ".OR." {
					guarded = leftFalse
				}
				return firstProtectedRead(arg.Children[1], withoutProofs(proofs, guarded))
			}
		}
	case parser.ExprIndex:
		if len(arg.Children) >= 1 {
			// The receiver first: in `x[1][2]` the inner `x[1]` is the read.
			if read, found := firstProtectedRead(arg.Children[0], proofs); found {
				return read, true
			}
			// `x[3, 1]` reads an element of `x`, and also of `x[3]`: try
			// the receiver, then the receiver with each leading subscript.
			if key, stable := stableExprKey(arg.Children[0]); stable {
				for lead, sub := range arg.Children[1:] {
					if proof, ok := proofs[key]; ok {
						return iifRead{expr: arg, proof: proof, lead: lead}, true
					}
					subKey, ok := stableExprKey(sub)
					if !ok {
						break
					}
					key += "[" + subKey + "]"
				}
			}
			for _, sub := range arg.Children[1:] {
				if read, found := firstProtectedRead(sub, proofs); found {
					return read, true
				}
			}
			return iifRead{}, false
		}
	case parser.ExprMember:
		if len(arg.Children) >= 1 {
			if read, found := firstProtectedRead(arg.Children[0], proofs); found {
				return read, true
			}
			if key, stable := stableExprKey(arg.Children[0]); stable {
				if proof, ok := proofs[key]; ok && proof&proofNotNil != 0 {
					return iifRead{expr: arg, proof: proofNotNil}, true
				}
			}
			return iifRead{}, false
		}
	}
	for _, child := range arg.Children {
		if read, found := firstProtectedRead(child, proofs); found {
			return read, true
		}
	}
	return iifRead{}, false
}

// readSubjectText names the subject of a read for a message: the receiver
// as written, with the leading subscripts that belong to the subject.
func readSubjectText(tokens []lexer.Token, read iifRead) string {
	if read.expr == nil || len(read.expr.Children) == 0 {
		return "the value"
	}
	text := exprSourceText(tokens, unwrapGroup(read.expr.Children[0]))
	if read.lead > 0 && read.lead < len(read.expr.Children) {
		var subs []string
		for _, sub := range read.expr.Children[1 : 1+read.lead] {
			subs = append(subs, exprSourceText(tokens, sub))
		}
		text += "[" + strings.Join(subs, ", ") + "]"
	}
	return text
}

// stableExprKey returns a canonical text for an expression that names the
// same thing each time it is evaluated: a variable, a member of one, or an
// element of one at a fixed position. Names compare without case, and
// `x[1, 2]` equals `x[1][2]`. An expression with a call, an operator or
// anything the parser could not read has no key: two occurrences of it are
// not known to be the same value.
func stableExprKey(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	switch e.Kind {
	case parser.ExprGroup:
		if len(e.Children) == 1 {
			return stableExprKey(e.Children[0])
		}
	case parser.ExprIdentifier:
		return strings.ToLower(e.Name), true
	case parser.ExprLiteral:
		if isNilExpr(e) {
			return "", false
		}
		return "=" + e.Name, true
	case parser.ExprMember:
		if len(e.Children) == 1 {
			if recv, ok := stableExprKey(e.Children[0]); ok {
				return recv + ":" + strings.ToLower(e.Name), true
			}
		}
	case parser.ExprIndex:
		if len(e.Children) >= 2 {
			key, ok := stableExprKey(e.Children[0])
			if !ok {
				return "", false
			}
			for _, sub := range e.Children[1:] {
				subKey, ok := stableExprKey(sub)
				if !ok {
					return "", false
				}
				key += "[" + subKey + "]"
			}
			return key, true
		}
	}
	return "", false
}

// singleArgCall returns the argument of an unqualified call to name with
// exactly one argument.
func singleArgCall(e *parser.Expr, name string) (*parser.Expr, bool) {
	e = unwrapGroup(e)
	if e == nil || e.Kind != parser.ExprCall || len(e.Children) != 2 {
		return nil, false
	}
	callee := e.Children[0]
	if callee.Kind != parser.ExprIdentifier || !strings.EqualFold(callee.Name, name) {
		return nil, false
	}
	if e.Children[1].Kind == parser.ExprSkipped {
		return nil, false
	}
	return e.Children[1], true
}

// lengthCall returns the argument of `Len(x)` or `ALen(x)`.
func lengthCall(e *parser.Expr) (*parser.Expr, bool) {
	if arg, ok := singleArgCall(e, "Len"); ok {
		return arg, true
	}
	return singleArgCall(e, "ALen")
}

// wholeNumberLiteral reads a non-negative integer literal.
func wholeNumberLiteral(e *parser.Expr) (int, bool) {
	e = unwrapGroup(e)
	if e == nil || e.Kind != parser.ExprLiteral {
		return 0, false
	}
	n, err := strconv.Atoi(e.Name)
	if err != nil || n < 0 {
		return 0, false
	}
	return n, true
}

// isNilExpr reports whether e is the NIL literal.
func isNilExpr(e *parser.Expr) bool {
	return e != nil && e.Kind == parser.ExprLiteral && strings.EqualFold(e.Name, "NIL")
}

// declaresObject reports whether the subject's own name declares an
// object by its Hungarian prefix (`oOwner`, `Me:oClient`). An `Empty`
// test of such a name is a NIL test; of any other name it may be a test
// for an empty string, whose members are still there to read.
func declaresObject(e *parser.Expr) bool {
	e = unwrapGroup(e)
	if e == nil {
		return false
	}
	switch e.Kind {
	case parser.ExprIdentifier, parser.ExprMember:
		return hungarianType(e.Name) == typeObject
	}
	return false
}

// unwrapGroup strips parentheses.
func unwrapGroup(e *parser.Expr) *parser.Expr {
	for e != nil && e.Kind == parser.ExprGroup && len(e.Children) == 1 {
		e = e.Children[0]
	}
	return e
}

// flipComparison mirrors a comparison so its operands can be swapped.
func flipComparison(op string) string {
	switch op {
	case "<":
		return ">"
	case "<=":
		return ">="
	case ">":
		return "<"
	case ">=":
		return "<="
	}
	return op
}

func unionProofs(a, b iifProofs) iifProofs {
	out := make(iifProofs, len(a)+len(b))
	for key, proof := range a {
		out[key] |= proof
	}
	for key, proof := range b {
		out[key] |= proof
	}
	return out
}

func intersectProofs(a, b iifProofs) iifProofs {
	out := iifProofs{}
	for key, proof := range a {
		if both := proof & b[key]; both != 0 {
			out[key] = both
		}
	}
	return out
}

// withoutProofs returns proofs minus what guarded already establishes. A
// subject proved to have an element is not NIL either, so that proof
// clears both; one proved only not NIL may still be empty.
func withoutProofs(proofs, guarded iifProofs) iifProofs {
	if len(guarded) == 0 {
		return proofs
	}
	out := make(iifProofs, len(proofs))
	for key, proof := range proofs {
		established := guarded[key]
		if established&proofSized != 0 {
			established |= proofNotNil
		}
		if rest := proof &^ established; rest != 0 {
			out[key] = rest
		}
	}
	return out
}

// exprSourceText returns an expression as written, with each run of
// whitespace or comments reduced to one space, for use in a message.
func exprSourceText(tokens []lexer.Token, e *parser.Expr) string {
	if e == nil || e.Start < 0 || e.End >= len(tokens) || e.End < e.Start {
		return "the value"
	}
	var b strings.Builder
	gap := false
	for _, token := range tokens[e.Start : e.End+1] {
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			gap = b.Len() > 0
			continue
		}
		if gap {
			b.WriteByte(' ')
			gap = false
		}
		b.WriteString(token.Text)
	}
	text := b.String()
	if len(text) > 60 {
		return text[:57] + "..."
	}
	return text
}
