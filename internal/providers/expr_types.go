package providers

// Coarse static typing over the expression AST (issue #184). SSL is
// dynamically typed, so inference here is deliberately partial: every
// judgment is either a definite type or typeUnknown, and typeUnknown means
// "no claim" — never "not that type". Consumers must stay silent on
// typeUnknown rather than guess, per the constraint recorded on #184.
//
// Two evidence sources, kept separable:
//
//   - structure + the element inventory (literals, operators, builtin
//     return types) — always sound;
//   - Hungarian prefixes on identifiers and member names — a naming
//     convention, so it is evidence about author intent rather than about
//     the runtime value. Callers opt into it explicitly.

import (
	"strings"
	"unicode"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/parser"
)

// sslType is a coarse type judgment. The set matches the style guide's
// Hungarian prefix table (ssl-style-guide.schema.yaml
// lints.hungarian_notation.prefixes) plus typeNIL for the NIL literal.
type sslType int

const (
	// typeUnknown is the absence of a judgment. It is never evidence.
	typeUnknown sslType = iota
	typeString
	typeNumber
	typeBoolean
	typeDate
	typeArray
	typeObject
	typeCodeBlock
	typeNIL
)

// String names the type for diagnostic messages.
func (t sslType) String() string {
	switch t {
	case typeString:
		return "string"
	case typeNumber:
		return "number"
	case typeBoolean:
		return "boolean"
	case typeDate:
		return "date"
	case typeArray:
		return "array"
	case typeObject:
		return "object"
	case typeCodeBlock:
		return "code block"
	case typeNIL:
		return "NIL"
	default:
		return "unknown"
	}
}

// hungarianPrefixTypes maps the style guide's documented prefixes to
// types. `v` (variant) is deliberately absent: it declares the type
// unknown, which is exactly typeUnknown. Loop-counter names (i, j, k, x,
// y, z) are exceptions to the prefix requirement, not type declarations,
// so they too stay unknown.
var hungarianPrefixTypes = map[string]sslType{
	"s":  typeString,
	"n":  typeNumber,
	"b":  typeBoolean,
	"d":  typeDate,
	"a":  typeArray,
	"o":  typeObject,
	"fn": typeCodeBlock,
}

// hungarianType reads a variable or member name as a type declaration.
// It requires the full documented shape — a lowercase prefix followed by
// an uppercase body letter (`sUserName`, `fnAdd`) — so `String` (prefix
// `s`, lowercase `t`) and ALLCAPS constants never claim a type. Leading
// underscores are ignored.
func hungarianType(name string) sslType {
	trimmed := strings.TrimLeft(name, "_")
	if len(trimmed) < 3 || trimmed[0] != 'f' || trimmed[1] != 'n' {
		if len(trimmed) < 2 {
			return typeUnknown
		}
		if t, ok := hungarianPrefixTypes[trimmed[:1]]; ok && unicode.IsUpper(rune(trimmed[1])) {
			return t
		}
		return typeUnknown
	}
	if unicode.IsUpper(rune(trimmed[2])) {
		return typeCodeBlock
	}
	return typeUnknown
}

// builtinReturnTypes maps the element inventory's documented return
// wording to a type. Multi-type wordings ("string, number, or date") and
// "any"/"none" are absent on purpose — they carry no single claim.
var builtinReturnTypes = map[string]sslType{
	"string":  typeString,
	"number":  typeNumber,
	"boolean": typeBoolean,
	"date":    typeDate,
	"array":   typeArray,
	"object":  typeObject,
	"nil":     typeNIL,
}

// builtinReturnType returns the documented return type of a builtin, or
// typeUnknown when the name is not a builtin or its return wording names
// more than one type. A wording that names a built-in class
// (`SSLDataset`, `SQLConnection`) is an object.
func builtinReturnType(name string) sslType {
	meta, ok := constants.GeneratedFunctionSummaries[strings.ToLower(name)]
	if !ok {
		return typeUnknown
	}
	returns := strings.ToLower(strings.Trim(strings.TrimSpace(meta.Returns), "`"))
	if t, ok := builtinReturnTypes[returns]; ok {
		return t
	}
	if _, isClass := constants.GeneratedClassDetails[returns]; isClass {
		return typeObject
	}
	return typeUnknown
}

// inferExprType infers a type from structure and the element inventory
// alone. Identifiers and member accesses are always typeUnknown here — no
// name is read as evidence.
func inferExprType(e *parser.Expr) sslType {
	return inferType(e, false, 0)
}

// inferExprTypeNamed infers as inferExprType does, and additionally reads
// Hungarian prefixes on bare identifiers and member names as type
// evidence. Use it where the naming convention is the intended contract
// (`sFmt` promising a string); prefer inferExprType where only the runtime
// value matters.
func inferExprTypeNamed(e *parser.Expr) sslType {
	return inferType(e, true, 0)
}

// maxInferDepth bounds recursion the same way the expression parser bounds
// nesting; a tree deeper than this yields no claim.
const maxInferDepth = 100

func inferType(e *parser.Expr, useNames bool, depth int) sslType {
	if e == nil || depth > maxInferDepth {
		return typeUnknown
	}
	switch e.Kind {
	case parser.ExprLiteral:
		return literalType(e.Name)
	case parser.ExprArrayLiteral:
		return typeArray
	case parser.ExprCodeBlock:
		return typeCodeBlock
	case parser.ExprInstantiate:
		return typeObject
	case parser.ExprGroup:
		if len(e.Children) == 1 {
			return inferType(e.Children[0], useNames, depth+1)
		}
	case parser.ExprIncrement:
		// `++`/`--` are documented on numbers, and `--` additionally on
		// strings and dates; the result keeps the operand's type.
		if len(e.Children) == 1 {
			switch operand := inferType(e.Children[0], useNames, depth+1); operand {
			case typeNumber, typeString, typeDate:
				return operand
			}
			return typeUnknown
		}
		return typeNumber
	case parser.ExprUnary:
		switch strings.ToUpper(e.Op) {
		case "-":
			return typeNumber
		case "!", ".NOT.":
			return typeBoolean
		}
	case parser.ExprBinary:
		return binaryType(e, useNames, depth)
	case parser.ExprCall:
		// Only bare builtin calls have a documented return type; a member
		// call is dispatched on a receiver whose class is not resolved
		// here.
		if len(e.Children) > 0 && e.Children[0].Kind == parser.ExprIdentifier {
			return builtinReturnType(e.Children[0].Name)
		}
	case parser.ExprIdentifier:
		if useNames {
			return hungarianType(e.Name)
		}
	case parser.ExprMember:
		if useNames {
			return hungarianType(e.Name)
		}
	}
	// ExprIndex yields an element whose type the array literal does not
	// pin down; ExprUnknown and ExprSkipped make no claim by definition.
	return typeUnknown
}

// literalType classifies a literal from its source text. The lexer has
// already decided the token is a literal; the text distinguishes which.
func literalType(text string) sslType {
	if text == "" {
		return typeUnknown
	}
	switch strings.ToUpper(text) {
	case ".T.", ".F.":
		return typeBoolean
	case "NIL":
		return typeNIL
	}
	switch text[0] {
	case '"', '\'', '[':
		return typeString
	}
	if r := rune(text[0]); unicode.IsDigit(r) || r == '.' {
		return typeNumber
	}
	return typeUnknown
}

// namedTypes maps the type words the element inventory uses in its
// operator matrix and return wordings to sslType. Wildcard and error
// wordings ("any", "non-number", "incompatible", "n/a", ...) are absent on
// purpose: a row that does not name two concrete operand types and a
// concrete result makes no claim this inference can use.
var namedTypes = map[string]sslType{
	"string":     typeString,
	"number":     typeNumber,
	"boolean":    typeBoolean,
	"date":       typeDate,
	"array":      typeArray,
	"object":     typeObject,
	"code block": typeCodeBlock,
	"nil":        typeNIL,
}

// operandPair keys the operator result matrix.
type operandPair struct {
	op          string
	left, right sslType
}

// operatorResults is the documented operator type matrix from the element
// inventory (GeneratedOperatorBySymbol TypeBehavior rows), reduced to the
// rows that name two concrete operand types and a concrete result. Any
// combination absent from it — `aList + sText`, `nCount * sText` — is a
// combination the language documents no result for, so it yields no claim
// rather than a guess.
var operatorResults = buildOperatorResults()

func buildOperatorResults() map[operandPair]sslType {
	out := make(map[operandPair]sslType)
	for _, details := range constants.GeneratedOperatorBySymbol {
		op := strings.ToUpper(details.Symbol)
		for _, row := range details.TypeBehavior {
			left, okL := namedTypes[strings.ToLower(row.Left)]
			right, okR := namedTypes[strings.ToLower(row.Right)]
			result, okR2 := namedTypes[strings.ToLower(row.Result)]
			if okL && okR && okR2 {
				out[operandPair{op, left, right}] = result
			}
		}
	}
	return out
}

// isBooleanOperator reports whether an operator always yields a boolean.
// Every documented row for these operators results in either boolean or
// an error, and an erroring expression produces no value to type.
func isBooleanOperator(op string) bool {
	switch op {
	case ".AND.", ".OR.", "&&", "||",
		"=", "==", "!=", "<>", "#", "$",
		"<", ">", "<=", ">=":
		return true
	}
	return false
}

func binaryType(e *parser.Expr, useNames bool, depth int) sslType {
	op := strings.ToUpper(e.Op)
	if isBooleanOperator(op) {
		return typeBoolean
	}
	if len(e.Children) != 2 {
		return typeUnknown
	}
	// A plain assignment inside a group (`(x := f()) != NIL`) evaluates to
	// the assigned value; the compound forms go through the matrix, which
	// documents them the same way as their bare operators.
	if op == ":=" {
		return inferType(e.Children[1], useNames, depth+1)
	}
	left := inferType(e.Children[0], useNames, depth+1)
	if left == typeUnknown {
		return typeUnknown
	}
	right := inferType(e.Children[1], useNames, depth+1)
	if right == typeUnknown {
		return typeUnknown
	}
	return operatorResults[operandPair{op, left, right}]
}

func isAssignOp(op string) bool {
	switch op {
	case ":=", "+=", "-=", "*=", "/=", "^=", "%=":
		return true
	}
	return false
}
