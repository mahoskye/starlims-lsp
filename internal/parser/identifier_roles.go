package parser

// Identifier role classification (issue #184). Rename and reference search
// match identifiers by word, which cannot tell a variable named `sName`
// from the property in `oRec:sName` or from a procedure that happens to
// share the name — so renaming one rewrote the others. The expression tree
// already knows what each identifier occurrence *is*; this exposes that
// per token position.

import (
	"strings"

	"starlims-lsp/internal/lexer"
)

// IdentifierRole says what an identifier token does at its own position.
type IdentifierRole int

const (
	// RoleUnclassified is the default: the token is not an identifier, or
	// it sits in a region the expression parser could not resolve. It is
	// never evidence — consumers should fall back to their prior behavior
	// rather than treat it as a negative answer.
	RoleUnclassified IdentifierRole = iota
	// RoleVariable is a bare variable reference or assignment target.
	RoleVariable
	// RoleMember is a member name after `:` — a property or method on some
	// receiver, which is a different symbol from a like-named variable.
	RoleMember
	// RoleCall is the callee of a bare `Name(...)` call.
	RoleCall
	// RoleInstantiation is the class name of `Name{...}`.
	RoleInstantiation
	// RoleDeclaredName is a name bound by `:DECLARE` / `:PARAMETERS` /
	// `:PUBLIC`.
	RoleDeclaredName
	// RoleProcedureName is the name in a `:PROCEDURE` header.
	RoleProcedureName
)

// String names the role for tests and debugging.
func (r IdentifierRole) String() string {
	switch r {
	case RoleVariable:
		return "variable"
	case RoleMember:
		return "member"
	case RoleCall:
		return "call"
	case RoleInstantiation:
		return "instantiation"
	case RoleDeclaredName:
		return "declared"
	case RoleProcedureName:
		return "procedure"
	default:
		return "unclassified"
	}
}

// IdentifierRoles classifies every token position in the stream. The
// result is indexed by token index and is always len(tokens) long.
func IdentifierRoles(tokens []lexer.Token) []IdentifierRole {
	roles := make([]IdentifierRole, len(tokens))

	set := func(idx int, role IdentifierRole) {
		if idx >= 0 && idx < len(tokens) && tokens[idx].Type == lexer.TokenIdentifier {
			roles[idx] = role
		}
	}

	// Binding positions first — a declared name is a declaration wherever
	// the expression walk might also see it.
	for _, decl := range CollectDeclarations(tokens) {
		for _, idx := range decl.Indices {
			set(idx, RoleDeclaredName)
		}
	}
	for i := 0; i < len(tokens); i++ {
		t := tokens[i]
		if t.Type != lexer.TokenKeyword ||
			!strings.EqualFold(strings.TrimPrefix(t.Text, ":"), "PROCEDURE") {
			continue
		}
		end := statementEnd(tokens, i)
		for j := i + 1; j <= end && j < len(tokens); j++ {
			if tokens[j].Type == lexer.TokenIdentifier {
				set(j, RoleProcedureName)
				break
			}
		}
		i = end
	}

	for _, stmt := range ExtractStatementExpressions(tokens) {
		for _, e := range stmt.Exprs {
			classifyExpr(e, roles, set)
		}
	}

	return roles
}

func classifyExpr(e *Expr, roles []IdentifierRole, set func(int, IdentifierRole)) {
	if e == nil {
		return
	}
	switch e.Kind {
	case ExprIdentifier:
		if e.Start >= 0 && e.Start < len(roles) && roles[e.Start] == RoleUnclassified {
			set(e.Start, RoleVariable)
		}
	case ExprMember:
		// End is the member-name token; the receiver is a child and is
		// classified on its own terms.
		set(e.End, RoleMember)
	case ExprCall:
		if len(e.Children) > 0 && e.Children[0].Kind == ExprIdentifier {
			set(e.Children[0].Start, RoleCall)
		}
	case ExprInstantiate:
		set(e.Start, RoleInstantiation)
	}
	for i, c := range e.Children {
		// A call's callee is classified above; recursing into it as a bare
		// identifier would relabel it a variable.
		if e.Kind == ExprCall && i == 0 && c.Kind == ExprIdentifier {
			continue
		}
		classifyExpr(c, roles, set)
	}
}
