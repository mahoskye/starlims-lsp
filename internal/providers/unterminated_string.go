package providers

import (
	"fmt"

	"starlims-lsp/internal/lexer"
)

// checkUnterminatedStrings reports a string literal the lexer never saw
// close (catalog: diag.unterminated_string, issue #240). SSL strings may
// span lines, so one with no closer runs to end of file and swallows
// everything after its opener; the opener is the one place the fix goes,
// so that is where the diagnostic sits. The verdict is the lexer's
// Unterminated flag: a bracket string's text can end in `]` and still be
// open (`[[a]`), so the token's last character is not evidence.
func checkUnterminatedStrings(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic
	for _, tok := range tokens {
		if tok.Type != lexer.TokenString || !tok.Unterminated || tok.Text == "" {
			continue
		}
		closer := tok.Text[:1]
		if closer == "[" {
			closer = "]"
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range: Range{
				Start: Position{Line: tok.Line - 1, Character: tok.Column - 1},
				End:   Position{Line: tok.Line - 1, Character: tok.Column},
			},
			Message: fmt.Sprintf("Unterminated string literal - expected a closing '%s' before end of file", closer),
			Source:  "ssl-lsp",
			Code:    CodeUnterminatedString,
		})
	}
	return diagnostics
}
