// Package providers implements LSP feature providers for SSL.
package providers

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// DiagnosticSeverity represents the severity of a diagnostic.
type DiagnosticSeverity int

const (
	SeverityError   DiagnosticSeverity = 1
	SeverityWarning DiagnosticSeverity = 2
	SeverityInfo    DiagnosticSeverity = 3
	SeverityHint    DiagnosticSeverity = 4
)

// Range represents a range in a text document.
type Range struct {
	Start Position
	End   Position
}

// Position represents a position in a text document.
type Position struct {
	Line      int
	Character int
}

// Diagnostic represents a diagnostic message.
type Diagnostic struct {
	Range    Range
	Severity DiagnosticSeverity
	Message  string
	Source   string
}

// DiagnosticOptions configures diagnostic checking.
type DiagnosticOptions struct {
	CheckUnclosedBlocks  bool
	CheckUnmatchedParens bool
	CheckUndeclaredVars  bool
	CheckUnusedVars      bool
	MaxBlockDepth        int
}

// DefaultDiagnosticOptions returns default diagnostic options.
func DefaultDiagnosticOptions() DiagnosticOptions {
	return DiagnosticOptions{
		CheckUnclosedBlocks:  true,
		CheckUnmatchedParens: true,
		CheckUndeclaredVars:  false,
		CheckUnusedVars:      false,
		MaxBlockDepth:        10,
	}
}

// GetDiagnostics returns all diagnostics for a document.
func GetDiagnostics(text string, opts DiagnosticOptions) []Diagnostic {
	var diagnostics []Diagnostic

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()

	// Check for lexer-level issues
	diagnostics = append(diagnostics, checkTokenErrors(tokens)...)

	// Check for unmatched parentheses/brackets
	if opts.CheckUnmatchedParens {
		diagnostics = append(diagnostics, checkUnmatchedDelimiters(tokens)...)
	}

	// Parse and check for structural issues
	p := parser.NewParser(tokens)
	ast := p.Parse()

	// Check for unclosed blocks
	if opts.CheckUnclosedBlocks {
		diagnostics = append(diagnostics, checkUnclosedBlocks(tokens)...)
	}

	// Check block depth
	if opts.MaxBlockDepth > 0 {
		diagnostics = append(diagnostics, checkBlockDepth(ast, opts.MaxBlockDepth)...)
	}

	return diagnostics
}

// checkTokenErrors checks for token-level errors.
func checkTokenErrors(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type == lexer.TokenUnknown {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("Unknown token: '%s'", token.Text),
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkUnmatchedDelimiters checks for unmatched parentheses and brackets.
func checkUnmatchedDelimiters(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	type stackItem struct {
		char  string
		token lexer.Token
	}
	var stack []stackItem

	pairs := map[string]string{
		"(": ")",
		"[": "]",
		"{": "}",
	}

	closers := map[string]string{
		")": "(",
		"]": "[",
		"}": "{",
	}

	for _, token := range tokens {
		if token.Type == lexer.TokenPunctuation {
			if _, isOpener := pairs[token.Text]; isOpener {
				stack = append(stack, stackItem{char: token.Text, token: token})
			} else if expected, isCloser := closers[token.Text]; isCloser {
				if len(stack) == 0 {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Unmatched '%s'", token.Text),
						Source:   "ssl-lsp",
					})
				} else if stack[len(stack)-1].char != expected {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Expected '%s' but found '%s'", pairs[stack[len(stack)-1].char], token.Text),
						Source:   "ssl-lsp",
					})
					stack = stack[:len(stack)-1]
				} else {
					stack = stack[:len(stack)-1]
				}
			}
		}
	}

	// Report unclosed delimiters
	for _, item := range stack {
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(item.token),
			Message:  fmt.Sprintf("Unclosed '%s'", item.char),
			Source:   "ssl-lsp",
		})
	}

	return diagnostics
}

// checkUnclosedBlocks checks for unclosed block statements.
func checkUnclosedBlocks(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	blockPairs := map[string][]string{
		"IF":        {"ENDIF"},
		"WHILE":     {"ENDWHILE"},
		"FOR":       {"NEXT"},
		"BEGINCASE": {"ENDCASE"},
		"TRY":       {"ENDTRY"},
		"PROCEDURE": {"ENDPROC"},
		"CLASS":     {"ENDPROC"},
		"REGION":    {"ENDREGION"},
	}

	endToStart := make(map[string][]string)
	for start, ends := range blockPairs {
		for _, end := range ends {
			endToStart[end] = append(endToStart[end], start)
		}
	}

	type stackItem struct {
		keyword string
		token   lexer.Token
	}
	var stack []stackItem

	for _, token := range tokens {
		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))

			if _, isStart := blockPairs[normalized]; isStart {
				stack = append(stack, stackItem{keyword: normalized, token: token})
			} else if validStarts, isEnd := endToStart[normalized]; isEnd {
				if len(stack) == 0 {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Unexpected ':%s' without matching block start", normalized),
						Source:   "ssl-lsp",
					})
				} else {
					top := stack[len(stack)-1]
					if contains(validStarts, top.keyword) {
						stack = stack[:len(stack)-1]
					} else {
						// Try to find a matching opener further down the stack
						found := false
						for i := len(stack) - 1; i >= 0; i-- {
							if contains(validStarts, stack[i].keyword) {
								// Report missing closers for items above
								for j := len(stack) - 1; j > i; j-- {
									unclosed := stack[j]
									expectedEnd := blockPairs[unclosed.keyword][0]
									diagnostics = append(diagnostics, Diagnostic{
										Severity: SeverityError,
										Range:    tokenToRange(unclosed.token),
										Message:  fmt.Sprintf("Unclosed ':%s' - expected ':%s'", unclosed.keyword, expectedEnd),
										Source:   "ssl-lsp",
									})
								}
								stack = stack[:i]
								found = true
								break
							}
						}

						if !found {
							diagnostics = append(diagnostics, Diagnostic{
								Severity: SeverityError,
								Range:    tokenToRange(token),
								Message:  fmt.Sprintf("':%s' does not match ':%s'", normalized, top.keyword),
								Source:   "ssl-lsp",
							})
						}
					}
				}
			}
		}
	}

	// Report any remaining unclosed blocks
	for _, item := range stack {
		expectedEnd := blockPairs[item.keyword][0]
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(item.token),
			Message:  fmt.Sprintf("Unclosed ':%s' - expected ':%s'", item.keyword, expectedEnd),
			Source:   "ssl-lsp",
		})
	}

	return diagnostics
}

// checkBlockDepth checks for excessive block nesting depth.
func checkBlockDepth(ast *parser.Node, maxDepth int) []Diagnostic {
	var diagnostics []Diagnostic

	var checkNode func(node *parser.Node, depth int)
	checkNode = func(node *parser.Node, depth int) {
		if node.Type == parser.NodeBlock && depth > maxDepth {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range: Range{
					Start: Position{Line: node.StartLine - 1, Character: 0},
					End:   Position{Line: node.StartLine - 1, Character: 100},
				},
				Message: fmt.Sprintf("Block nesting depth (%d) exceeds maximum (%d)", depth, maxDepth),
				Source:  "ssl-lsp",
			})
		}

		for _, child := range node.Children {
			newDepth := depth
			if child.Type == parser.NodeBlock {
				newDepth++
			}
			checkNode(child, newDepth)
		}
	}

	checkNode(ast, 0)
	return diagnostics
}

// tokenToRange converts a token to an LSP range.
func tokenToRange(token lexer.Token) Range {
	return Range{
		Start: Position{
			Line:      token.Line - 1,
			Character: token.Column - 1,
		},
		End: Position{
			Line:      token.Line - 1,
			Character: token.Column - 1 + len(token.Text),
		},
	}
}

// contains checks if a string slice contains a value.
func contains(slice []string, val string) bool {
	for _, s := range slice {
		if s == val {
			return true
		}
	}
	return false
}

// Helper to check if a keyword is in constants
func isBlockStartKeyword(s string) bool {
	return constants.IsBlockStartKeyword(s)
}
