package providers

import (
	"strings"

	"starlims-lsp/internal/lexer"
)

// Parameter-list and builder-directive rules from the reference's
// data-sources guide and :PARAMETERS page (issue #260).

// checkParameterListAndDirectives runs empty_parameters in every file and
// the directive-value checks in data sources.
func checkParameterListAndDirectives(tokens []lexer.Token, isDataSource bool) []Diagnostic {
	diagnostics := checkEmptyParameters(tokens)
	if isDataSource {
		diagnostics = append(diagnostics, checkDataSourceDirectiveValues(tokens)...)
	}
	return diagnostics
}

// checkEmptyParameters reports a `:PARAMETERS;` statement with no names
// (issue #260). The reference says a parameter list must contain at least
// one name; a script or data source that takes no parameters leaves the
// line out.
func checkEmptyParameters(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic
	for i, token := range tokens {
		if token.Type != lexer.TokenKeyword ||
			!strings.EqualFold(strings.TrimPrefix(token.Text, ":"), "PARAMETERS") {
			continue
		}
		next := nextSignificantTokenIndex(tokens, i+1)
		if next >= 0 && (tokens[next].Type != lexer.TokenPunctuation || tokens[next].Text != ";") {
			continue
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(token),
			Message:  "':PARAMETERS' must name at least one parameter; leave the line out when there are none",
			Source:   "ssl-lsp",
			Code:     CodeEmptyParameters,
		})
	}
	return diagnostics
}

// checkDataSourceDirectiveValues reports builder-directive values the data
// source builder silently ignores (issue #260, reference
// guides/data-sources.md): `:NULLASBLANK` takes only the words true or
// false (an SSL literal such as .F. is ignored, so null-to-blank stays on),
// and `:INVARIANTDATECOLUMNS` takes bare column names (a quoted name keeps
// its quotes and matches no column).
func checkDataSourceDirectiveValues(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic
	for i, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}
		directive := strings.TrimPrefix(token.Text, ":")
		if directive != "NULLASBLANK" && directive != "INVARIANTDATECOLUMNS" {
			continue
		}
		assign := nextSignificantTokenIndex(tokens, i+1)
		if assign < 0 || tokens[assign].Text != ":=" {
			continue
		}
		var values []lexer.Token
		for j := assign + 1; j < len(tokens); j++ {
			t := tokens[j]
			if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
				continue
			}
			if t.Type == lexer.TokenPunctuation && t.Text == ";" {
				break
			}
			values = append(values, t)
		}
		if len(values) == 0 {
			continue
		}
		switch directive {
		case "NULLASBLANK":
			if len(values) == 1 && (strings.EqualFold(values[0].Text, "true") || strings.EqualFold(values[0].Text, "false")) {
				continue
			}
			r := tokenToRange(values[0])
			r.End = tokenToRange(values[len(values)-1]).End
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    r,
				Message:  "':NULLASBLANK' takes the word true or false; any other value is ignored and null-to-blank stays on",
				Source:   "ssl-lsp",
				Code:     CodeNullAsBlankValue,
			})
		case "INVARIANTDATECOLUMNS":
			for _, v := range values {
				if v.Type != lexer.TokenString {
					continue
				}
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range:    tokenToRange(v),
					Message:  "':INVARIANTDATECOLUMNS' takes bare column names; the quotes are kept as part of the name, so it matches no column",
					Source:   "ssl-lsp",
					Code:     CodeInvariantDateColumnsQuoted,
				})
			}
		}
	}
	return diagnostics
}
