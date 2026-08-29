package parser

// Statement-based declaration resolution (issue #184). The AST-node walk
// this replaces read a declaration's names out of whatever tokens the
// structural parser had grouped into one node, which is a line-shaped
// grouping: a declaration written as a bare `:DECLARE` followed by its
// names on the next lines lost every name. Names are now read from the
// declaration *statement* — keyword through its terminating `;` — so
// layout cannot hide a declaration.

import (
	"strings"

	"starlims-lsp/internal/lexer"
)

// DeclarationStatement is one `:DECLARE` / `:PARAMETERS` / `:PUBLIC`
// statement and the names it binds.
type DeclarationStatement struct {
	// Scope is the scope the keyword establishes.
	Scope VariableScope
	// Start and End are inclusive token indices bracketing the whole
	// statement, from the keyword through its terminating `;` (or the
	// last token when unterminated).
	Start, End int
	// Names are the declared names in source order.
	Names []VariableInfo
	// Indices are the token indices of Names, positionally aligned.
	Indices []int
}

// declarationScopes maps the declaring keywords to the scope they open.
var declarationScopes = map[string]VariableScope{
	"DECLARE":    ScopeLocal,
	"PUBLIC":     ScopePublic,
	"PARAMETERS": ScopeParameter,
}

// CollectDeclarations returns every declaration statement in the token
// stream, in source order.
func CollectDeclarations(tokens []lexer.Token) []DeclarationStatement {
	var out []DeclarationStatement

	for i := 0; i < len(tokens); i++ {
		t := tokens[i]
		if t.Type != lexer.TokenKeyword {
			continue
		}
		scope, ok := declarationScopes[strings.ToUpper(strings.TrimPrefix(t.Text, ":"))]
		if !ok {
			continue
		}
		end := statementEnd(tokens, i)
		names, indices := declaredNames(tokens, i+1, end, scope)
		out = append(out, DeclarationStatement{
			Scope:   scope,
			Start:   i,
			End:     end,
			Names:   names,
			Indices: indices,
		})
		i = end
	}

	return out
}

// declaredNames reads the comma-separated name list of a declaration.
// Only the name position of each slot counts: an initializer
// (`:DECLARE nCount := nOther;` — itself flagged by
// diag.declare_initializer) contributes its target and not the
// identifiers on its right-hand side.
func declaredNames(tokens []lexer.Token, from, end int, scope VariableScope) ([]VariableInfo, []int) {
	var names []VariableInfo
	var indices []int
	expectingName := true

	for i := from; i <= end && i < len(tokens); i++ {
		t := tokens[i]
		switch t.Type {
		case lexer.TokenWhitespace, lexer.TokenComment:
			continue
		case lexer.TokenPunctuation:
			if t.Text == "," {
				expectingName = true
			}
			continue
		case lexer.TokenIdentifier:
			if expectingName {
				names = append(names, VariableInfo{
					Name:   t.Text,
					Line:   t.Line,
					Column: t.Column,
					Scope:  scope,
				})
				indices = append(indices, i)
				expectingName = false
			}
			continue
		default:
			// An operator, keyword, literal, or anything else ends the
			// current slot; the next comma opens the next one.
			expectingName = false
		}
	}

	return names, indices
}

// DeclarationSpans reports, per token index, whether that token sits
// inside a statement that binds names rather than uses them — a
// declaration, or a `:PROCEDURE` header naming the procedure. Consumers
// resolving identifier uses skip these positions.
func DeclarationSpans(tokens []lexer.Token) []bool {
	spans := make([]bool, len(tokens))

	mark := func(start, end int) {
		for i := start; i <= end && i < len(tokens); i++ {
			spans[i] = true
		}
	}
	for _, decl := range CollectDeclarations(tokens) {
		mark(decl.Start, decl.End)
	}
	for i := 0; i < len(tokens); i++ {
		t := tokens[i]
		if t.Type != lexer.TokenKeyword ||
			!strings.EqualFold(strings.TrimPrefix(t.Text, ":"), "PROCEDURE") {
			continue
		}
		end := statementEnd(tokens, i)
		mark(i, end)
		i = end
	}

	return spans
}
