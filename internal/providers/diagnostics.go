// Package providers implements LSP feature providers for SSL.
package providers

import (
	"fmt"
	"strings"
	"unicode"

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
	CheckUnclosedBlocks    bool
	CheckUnmatchedParens   bool
	CheckUndeclaredVars    bool
	CheckUnusedVars        bool
	CheckHungarianNotation bool
	HungarianPrefixes      []string
	GlobalVariables        []string
	MaxBlockDepth          int
}

// DefaultDiagnosticOptions returns default diagnostic options.
func DefaultDiagnosticOptions() DiagnosticOptions {
	return DiagnosticOptions{
		CheckUnclosedBlocks:    true,
		CheckUnmatchedParens:   true,
		CheckUndeclaredVars:    false,
		CheckUnusedVars:        false,
		CheckHungarianNotation: false,
		HungarianPrefixes:      []string{"a", "b", "d", "n", "o", "s"},
		MaxBlockDepth:          10,
	}
}

// GetDiagnostics returns all diagnostics for a document.
func GetDiagnostics(text string, opts DiagnosticOptions) []Diagnostic {
	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	return collectDiagnostics(tokens, ast, p, opts)
}

// GetDiagnosticsFromTokens returns diagnostics using cached tokens/AST.
// Note: A parser instance is created even when AST is provided because
// ExtractVariables requires parser helper methods to traverse the AST.
// Parser creation is O(1) as it just stores a reference to the tokens.
func GetDiagnosticsFromTokens(tokens []lexer.Token, ast *parser.Node, opts DiagnosticOptions) []Diagnostic {
	if len(tokens) == 0 {
		return nil
	}

	p := parser.NewParser(tokens)
	if ast == nil {
		ast = p.Parse()
	}

	return collectDiagnostics(tokens, ast, p, opts)
}

func collectDiagnostics(tokens []lexer.Token, ast *parser.Node, p *parser.Parser, opts DiagnosticOptions) []Diagnostic {
	var diagnostics []Diagnostic

	// Check for lexer-level issues
	diagnostics = append(diagnostics, checkTokenErrors(tokens)...)

	// Check for unmatched parentheses/brackets
	if opts.CheckUnmatchedParens {
		diagnostics = append(diagnostics, checkUnmatchedDelimiters(tokens)...)
	}

	// Check for unclosed blocks
	if opts.CheckUnclosedBlocks {
		diagnostics = append(diagnostics, checkUnclosedBlocks(tokens)...)
	}

	// Check block depth
	if opts.MaxBlockDepth > 0 {
		diagnostics = append(diagnostics, checkBlockDepth(ast, opts.MaxBlockDepth)...)
	}

	// Check for Hungarian notation (opt-in)
	if opts.CheckHungarianNotation {
		variables := p.ExtractVariables(ast)
		diagnostics = append(diagnostics, checkHungarianNotation(variables, opts.HungarianPrefixes)...)
	}

	// SSL language rule enforcement (always enabled)
	diagnostics = append(diagnostics, checkMissingExitCase(tokens)...)
	diagnostics = append(diagnostics, checkBareLogicalOperators(tokens)...)
	diagnostics = append(diagnostics, checkDefaultOnDeclareLine(tokens)...)

	// Check for assignment to global variables
	if len(opts.GlobalVariables) > 0 {
		diagnostics = append(diagnostics, checkGlobalAssignment(tokens, opts.GlobalVariables)...)
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
			// Guard against invalid line numbers
			line := node.StartLine - 1
			if line < 0 {
				line = 0
			}
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range: Range{
					Start: Position{Line: line, Character: 0},
					End:   Position{Line: line, Character: 0},
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

func checkHungarianNotation(variables []parser.VariableInfo, prefixes []string) []Diagnostic {
	var diagnostics []Diagnostic

	if len(prefixes) == 0 {
		return diagnostics
	}

	for _, variable := range variables {
		if prefix, ok := hasHungarianPrefix(variable.Name, prefixes); ok {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range: Range{
					Start: Position{Line: variable.Line - 1, Character: variable.Column - 1},
					End:   Position{Line: variable.Line - 1, Character: variable.Column - 1 + len(variable.Name)},
				},
				Message: fmt.Sprintf("Hungarian notation prefix '%s' detected in '%s'", prefix, variable.Name),
				Source:  "ssl-lsp",
			})
		}
	}

	return diagnostics
}

func hasHungarianPrefix(name string, prefixes []string) (string, bool) {
	trimmed := strings.TrimLeft(name, "_")
	if trimmed == "" {
		return "", false
	}

	lower := strings.ToLower(trimmed)
	for _, prefix := range prefixes {
		if !strings.HasPrefix(lower, prefix) {
			continue
		}

		remainder := trimmed[len(prefix):]
		remainder = strings.TrimLeft(remainder, "_")
		if remainder == "" {
			continue
		}
		firstRune := []rune(remainder)[0]
		if unicode.IsUpper(firstRune) {
			return prefix, true
		}
	}

	return "", false
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

// checkMissingExitCase checks that every :CASE and :OTHERWISE block ends with :EXITCASE.
// This is a mandatory SSL rule per ssl_agent_instructions.md (Gotcha #7).
func checkMissingExitCase(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Track state within BEGINCASE/ENDCASE blocks
	inBeginCase := false
	var currentCaseToken *lexer.Token
	hasExitCase := false

	for i := range tokens {
		token := &tokens[i]
		if token.Type != lexer.TokenKeyword {
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))

		switch normalized {
		case "BEGINCASE":
			inBeginCase = true
			currentCaseToken = nil
			hasExitCase = false

		case "CASE", "OTHERWISE":
			if inBeginCase {
				// If we had a previous CASE/OTHERWISE without EXITCASE, report it
				if currentCaseToken != nil && !hasExitCase {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityWarning,
						Range:    tokenToRange(*currentCaseToken),
						Message:  fmt.Sprintf("':%s' block should end with ':EXITCASE;'", strings.ToUpper(strings.TrimPrefix(currentCaseToken.Text, ":"))),
						Source:   "ssl-lsp",
					})
				}
				currentCaseToken = token
				hasExitCase = false
			}

		case "EXITCASE":
			hasExitCase = true

		case "ENDCASE":
			if inBeginCase {
				// Check the last CASE/OTHERWISE block
				if currentCaseToken != nil && !hasExitCase {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityWarning,
						Range:    tokenToRange(*currentCaseToken),
						Message:  fmt.Sprintf("':%s' block should end with ':EXITCASE;'", strings.ToUpper(strings.TrimPrefix(currentCaseToken.Text, ":"))),
						Source:   "ssl-lsp",
					})
				}
			}
			inBeginCase = false
			currentCaseToken = nil
			hasExitCase = false
		}
	}

	return diagnostics
}

// checkBareLogicalOperators checks for AND, OR, NOT without enclosing periods.
// SSL requires .AND., .OR., .NOT. - bare operators are an error.
func checkBareLogicalOperators(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Bare logical operators that should be .AND., .OR., .NOT.
	bareOperators := map[string]string{
		"AND": ".AND.",
		"OR":  ".OR.",
		"NOT": ".NOT.",
	}

	for _, token := range tokens {
		// Only check identifiers - the lexer tokenizes bare AND/OR/NOT as identifiers
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		upper := strings.ToUpper(token.Text)
		if correct, isBare := bareOperators[upper]; isBare {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("Use '%s' instead of '%s' for logical operations in SSL", correct, token.Text),
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkDefaultOnDeclareLine checks for :DEFAULT appearing on the same line as :DECLARE.
// Per ssl_agent_instructions.md (Gotcha #1), these must be separate statements.
func checkDefaultOnDeclareLine(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Track lines where :DECLARE appears
	declareLines := make(map[int]lexer.Token)

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))

		if normalized == "DECLARE" {
			declareLines[token.Line] = token
		} else if normalized == "DEFAULT" {
			if declareToken, found := declareLines[token.Line]; found {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range:    tokenToRange(declareToken),
					Message:  "':DEFAULT' cannot be used with ':DECLARE' - use ':PARAMETERS' with ':DEFAULT' instead",
					Source:   "ssl-lsp",
				})
			}
		}
	}

	return diagnostics
}

// checkGlobalAssignment checks for assignment to global variables.
// Global variables are pre-declared and should not be assigned to.
func checkGlobalAssignment(tokens []lexer.Token, globals []string) []Diagnostic {
	var diagnostics []Diagnostic

	// Build a case-insensitive set of global variable names
	globalSet := make(map[string]bool)
	for _, g := range globals {
		globalSet[strings.ToUpper(g)] = true
	}

	// Look for assignment patterns: identifier := value
	for i := 0; i < len(tokens); i++ {
		token := tokens[i]

		// Skip non-identifiers
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		// Check if this identifier is a global
		if !globalSet[strings.ToUpper(token.Text)] {
			continue
		}

		// Look ahead for := assignment operator
		j := i + 1
		for j < len(tokens) && tokens[j].Type == lexer.TokenWhitespace {
			j++
		}

		if j < len(tokens) && tokens[j].Type == lexer.TokenOperator && tokens[j].Text == ":=" {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("Cannot assign to global variable '%s'", token.Text),
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}
