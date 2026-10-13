package providers

// Native-member misuse on SSL values (issue #267). Strings, numbers,
// logicals, dates and arrays have no SSL-defined members: a colon member
// reaches the value's underlying .NET type, and a member that type does not
// have raises at run time ("Invalid property: Count", "Invalid method:
// toupper"). Code blocks have no colon members at all. The reference
// documents these failures (guides/native-members.md, types/array.md).
//
// DECISIONS.md D10 still governs: an unmatched member on a value is
// usually a legitimate .NET passthrough, so this check never reasons from
// "not in a list of known members". It fires only on members the reference
// documents as failing, on a receiver whose type is certain.

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// nativeMemberRule describes one documented failure. call is true for a
// method call (`x:Name()`), false for a property read (`x:Name`).
type nativeMemberRule struct {
	name string
	call bool
	why  string
}

// Members the reference documents as raising on a value of each type. Names
// are matched exactly: native member names are case-sensitive.
var (
	sslStyleValueMembers = []nativeMemberRule{
		{"IsEmpty", true, "SSL values have no IsEmpty(); use Empty()"},
		{"ToJson", true, "SSL values have no ToJson(); use the ToJson() function"},
		{"clone", true, "native members are case-sensitive; there is no clone()"},
		{"value", false, "SSL values have no value property"},
	}
	arrayMembers = []nativeMemberRule{
		{"Count", false, "arrays have no Count property; use ALen() or :Length"},
		{"Count", true, "arrays have no Count method; use ALen() or :Length"},
		{"Append", true, "arrays have no Append method; use AAdd()"},
		{"length", false, "native members are case-sensitive; use :Length"},
		{"Length", true, "Length is a property; write :Length without parentheses"},
	}
	stringMembers = []nativeMemberRule{
		{"Split", true, "the native Split is not reachable; use BuildArray()"},
		{"length", false, "native members are case-sensitive; use :Length"},
		{"Length", true, "Length is a property; write :Length without parentheses"},
	}
	// Verified native members whose wrong-case spelling raises; a call
	// spelled in any other case is reported (sText:toupper()).
	stringMethods = []string{"Format", "ToString", "ToUpper", "ToLower", "Trim", "PadLeft", "Substring", "IndexOf", "LastIndexOf", "Replace", "Contains", "StartsWith", "EndsWith", "Insert", "Remove", "Equals"}
	numberMethods = []string{"ToString", "GetType"}
	dateMethods   = []string{"ToString", "ToShortDateString", "AddDays", "AddHours", "AddMonths", "AddYears", "Subtract"}
	dateProps     = []string{"Year", "Month", "Day", "Hour", "DayOfYear", "DayOfWeek", "Date", "Ticks"}
)

// nativeMemberProblem returns why member (called or read) raises on a value
// of type t, or "" when the reference documents no failure.
func nativeMemberProblem(t sslType, member string, call bool) string {
	if t == typeCodeBlock {
		return "code blocks have no colon members; run one with Eval()"
	}
	var rules []nativeMemberRule
	var methods, props []string
	switch t {
	case typeArray:
		rules = arrayMembers
	case typeString:
		rules, methods = stringMembers, stringMethods
	case typeNumber:
		methods = numberMethods
	case typeBoolean:
		methods = []string{"ToString"}
	case typeDate:
		methods, props = dateMethods, dateProps
	default:
		return ""
	}
	for _, r := range append(append([]nativeMemberRule{}, rules...), sslStyleValueMembers...) {
		if r.name == member && r.call == call {
			return r.why
		}
	}
	known := props
	if call {
		known = methods
	}
	for _, k := range known {
		if k != member && strings.EqualFold(k, member) {
			return fmt.Sprintf("native members are case-sensitive; write %s", k)
		}
	}
	return ""
}

// checkInvalidNativeMember reports a colon member on an SSL value whose
// type is certain when the reference documents that member as failing
// (diag.invalid_native_member).
func checkInvalidNativeMember(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	scope := procedureScopes(tokens)
	scopedName := func(i int, name string) string { return scopeKey(scope, i, name) }
	types := certainVariableTypes(tokens, stmts, scope)
	var diagnostics []Diagnostic

	report := func(member *parser.Expr, call bool) {
		recv := member.Children[0]
		t := typeUnknown
		switch recv.Kind {
		case parser.ExprIdentifier:
			t = types[scopedName(recv.Start, recv.Name)]
		case parser.ExprLiteral, parser.ExprArrayLiteral, parser.ExprCodeBlock:
			t = inferExprType(recv)
		}
		if t == typeUnknown || t == typeNIL || t == typeObject {
			return
		}
		why := nativeMemberProblem(t, member.Name, call)
		if why == "" {
			return
		}
		rng := tokenToRange(tokens[member.End])
		form := ":" + member.Name
		if call {
			form += "()"
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range:    rng,
			Message:  fmt.Sprintf("'%s' on %s raises at run time: %s", form, withArticle(t.String()), why),
			Source:   "ssl-lsp",
			Code:     CodeInvalidNativeMember,
		})
	}

	var walk func(e *parser.Expr, parentCallsIt bool)
	walk = func(e *parser.Expr, parentCallsIt bool) {
		if e == nil {
			return
		}
		if e.Kind == parser.ExprMember && len(e.Children) == 1 && e.End < len(tokens) {
			report(e, parentCallsIt)
		}
		// `++`/`--` on a value that is certainly a string raises an
		// invalid-operand error (operators/increment.md, decrement.md).
		if e.Kind == parser.ExprIncrement && len(e.Children) == 1 && e.Children[0].Kind == parser.ExprIdentifier {
			target := e.Children[0]
			if types[scopedName(target.Start, target.Name)] == typeString {
				if rng, ok := exprRange(tokens, e); ok {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityWarning,
						Range:    rng,
						Message:  fmt.Sprintf("'%s' on a string raises at run time: %s works on numbers and dates only", e.Op, e.Op),
						Source:   "ssl-lsp",
						Code:     CodeInvalidNativeMember,
					})
				}
			}
		}
		for i, c := range e.Children {
			walk(c, e.Kind == parser.ExprCall && i == 0)
		}
	}
	for _, s := range stmts {
		for _, e := range s.Exprs {
			walk(e, false)
		}
	}
	return diagnostics
}

// certainVariableTypes maps a scoped variable name to its type when every
// plain := assignment to it in its scope infers the same definite type from
// structure alone (literals, operators, builtin return types — never
// Hungarian prefixes). A name is disqualified by any other way it can be
// rebound: a compound assignment, a :FOR counter, a :PARAMETERS or :PUBLIC
// declaration, an assignment whose type is unknown or NIL, or being passed
// to DoProc/ExecFunction/ExecUdf (whose callee may rebind it).
func certainVariableTypes(tokens []lexer.Token, stmts []parser.StatementExprs, scope []int) map[string]sslType {
	scopedName := func(i int, name string) string { return scopeKey(scope, i, name) }
	types := map[string]sslType{}
	poisoned := map[string]bool{}
	bind := func(key string, t sslType) {
		if poisoned[key] {
			return
		}
		if t == typeUnknown || t == typeNIL {
			poisoned[key] = true
			delete(types, key)
			return
		}
		if prev, ok := types[key]; ok && prev != t {
			poisoned[key] = true
			delete(types, key)
			return
		}
		types[key] = t
	}
	poison := func(key string) {
		poisoned[key] = true
		delete(types, key)
	}

	// Declarations that bind from outside: parameters and publics.
	for i := 0; i < len(tokens); i++ {
		if tokens[i].Type != lexer.TokenKeyword {
			continue
		}
		kw := strings.ToUpper(strings.TrimPrefix(tokens[i].Text, ":"))
		if kw != "PARAMETERS" && kw != "PUBLIC" {
			continue
		}
		for j := i + 1; j < len(tokens); j++ {
			if tokens[j].Type == lexer.TokenPunctuation && tokens[j].Text == ";" {
				break
			}
			if tokens[j].Type == lexer.TokenIdentifier {
				poison(scopedName(j, tokens[j].Text))
			}
		}
	}

	var markPassed func(e *parser.Expr)
	markPassed = func(e *parser.Expr) {
		if e == nil {
			return
		}
		if e.Kind == parser.ExprIdentifier {
			poison(scopedName(e.Start, e.Name))
		}
		if e.Kind == parser.ExprArrayLiteral {
			for _, c := range e.Children {
				markPassed(c)
			}
		}
	}
	var findDispatch func(e *parser.Expr)
	findDispatch = func(e *parser.Expr) {
		if e == nil {
			return
		}
		if e.Kind == parser.ExprCall && len(e.Children) > 0 && e.Children[0].Kind == parser.ExprIdentifier {
			switch strings.ToUpper(e.Children[0].Name) {
			case "DOPROC", "EXECFUNCTION", "EXECUDF":
				for _, arg := range e.Children[1:] {
					markPassed(arg)
				}
			}
		}
		for _, c := range e.Children {
			findDispatch(c)
		}
	}

	for _, s := range stmts {
		for _, e := range s.Exprs {
			findDispatch(e)
		}
		if len(s.Exprs) == 0 || s.Exprs[0].Kind != parser.ExprIdentifier {
			continue
		}
		target := s.Exprs[0]
		key := scopedName(target.Start, target.Name)
		switch s.Kind {
		case parser.StmtAssign:
			if s.Assign != ":=" || len(s.Exprs) != 2 {
				poison(key)
				continue
			}
			bind(key, inferExprType(s.Exprs[1]))
		case parser.StmtFor, parser.StmtDefault:
			poison(key)
		}
	}
	return types
}

// scopeKey keys a variable by the procedure that encloses token index i
// (or the script level) and its case-folded name.
func scopeKey(scope []int, i int, name string) string {
	owner := -1
	if i >= 0 && i < len(scope) {
		owner = scope[i]
	}
	return fmt.Sprintf("%d/%s", owner, strings.ToLower(name))
}

// procedureScopes returns, for each token index, the index of the
// :PROCEDURE that encloses it, or -1 at script level.
func procedureScopes(tokens []lexer.Token) []int {
	scope := make([]int, len(tokens))
	current := -1
	for j, t := range tokens {
		if t.Type == lexer.TokenKeyword {
			switch strings.ToUpper(strings.TrimPrefix(t.Text, ":")) {
			case "PROCEDURE":
				current = j
			case "ENDPROC":
				scope[j] = current
				current = -1
				continue
			}
		}
		scope[j] = current
	}
	return scope
}

// withArticle prefixes a type name with "a" or "an".
func withArticle(name string) string {
	if strings.ContainsAny(name[:1], "aeiou") {
		return "an " + name
	}
	return "a " + name
}
