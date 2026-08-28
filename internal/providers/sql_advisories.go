package providers

// Info-tier SQL advisories (issue #220, formatting-review proposals
// I1–I7): stylistic and portability observations about embedded SQL that
// the formatter cannot act on. All seven are info severity — auto-gated
// by ssl.diagnostics.infoDiagnostics, promotable per rule via
// ssl.diagnostics.rules — and detect over the SQL lexer's token stream so
// SQL comments and character literals can never false-trigger them.

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
)

// sqlAdvisoryCodes lists the codes this file emits — used to short-circuit
// the whole pass when the info tier is off and none of them is promoted.
var sqlAdvisoryCodes = []string{
	CodeSQLCommaJoin, CodeSQLLegacyOuterJoin, CodeSQLInconsistentAlias,
	CodeSQLLiteralSplice, CodeSQLDialectMix, CodeSQLSelectStar,
	CodeSQLSuspectPlaceholder,
}

// oracleDialectMarkers and mssqlDialectMarkers are deliberately short,
// high-precision idiom lists (issue #220 I5): each name is exclusive to
// its dialect in practice. ODBC escapes ({fn}, {d}) are dialect-neutral
// and excluded.
var oracleDialectMarkers = map[string]bool{
	"SYSDATE": true, "NVL": true, "NVL2": true, "DECODE": true,
	"ROWNUM": true, "DUAL": true, "TO_DATE": true, "TO_CHAR": true,
	"TO_NUMBER": true, "LISTAGG": true,
}

var mssqlDialectMarkers = map[string]bool{
	"GETDATE": true, "ISNULL": true, "CHARINDEX": true, "DATEADD": true,
	"DATEDIFF": true, "NEWID": true, "NOLOCK": true,
}

// checkSQLAdvisories runs the info-tier SQL advisory pass over the first
// string argument of every recognized embedded-SQL call.
func checkSQLAdvisories(tokens []lexer.Token, opts DiagnosticOptions) []Diagnostic {
	if !opts.IncludeInfoDiagnostics {
		promoted := false
		for _, code := range sqlAdvisoryCodes {
			if _, ok := opts.RuleOverrides[code]; ok {
				promoted = true
				break
			}
		}
		if !promoted {
			return nil
		}
	}

	var diagnostics []Diagnostic
	add := func(tok lexer.Token, code, message string) {
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityInfo,
			Range:    tokenToRange(tok),
			Message:  message,
			Source:   "ssl-lsp",
			Code:     code,
		})
	}

	spliceFlaggedCall := -1
	sqlCallFirstArgStrings(tokens, constants.IsSQLFunction, func(callIdx, strIdx int) {
		tok := tokens[strIdx]
		content := unquoteSSLString(tok.Text)

		// I4 — a spliced character literal: the string ends (or begins)
		// inside an open SQL '…' literal continued across concatenation.
		// The SQL lexer cannot tokenize such a fragment coherently, so it
		// is also the only advisory that fires on it — once per call, not
		// once per concatenation piece.
		if strings.Count(content, "'")%2 == 1 {
			if callIdx == spliceFlaggedCall {
				return
			}
			spliceFlaggedCall = callIdx
			add(tok, CodeSQLLiteralSplice,
				"SQL character literal spliced across string concatenation - prefer a ?param? placeholder: splices are fragile to format, invisible to the parameter APIs, and the classic injection surface")
			return
		}

		sqlToks := significantSQLTokens(content)
		if len(sqlToks) == 0 {
			return
		}

		checkCommaJoin(tok, sqlToks, add)
		checkLegacyOuterJoin(tok, sqlToks, add)
		checkInconsistentAlias(tok, sqlToks, add)
		checkDialectMix(tok, sqlToks, add)
		checkSelectStar(tok, sqlToks, add)
		checkSuspectPlaceholder(tok, sqlToks, add)
	})

	return diagnostics
}

// significantSQLTokens lexes SQL content and drops whitespace and comments.
func significantSQLTokens(content string) []SQLToken {
	var out []SQLToken
	for _, t := range NewSQLLexer(content).Tokenize() {
		switch t.Type {
		case SQLTokenWhitespace, SQLTokenComment, SQLTokenHint:
			continue
		}
		out = append(out, t)
	}
	return out
}

// checkCommaJoin (I1): a comma inside a SELECT's FROM clause at paren
// depth zero is a pre-ANSI comma join.
func checkCommaJoin(tok lexer.Token, sqlToks []SQLToken, add func(lexer.Token, string, string)) {
	depth := 0
	inFrom := false
	isSelect := len(sqlToks) > 0 && strings.EqualFold(sqlToks[0].Text, "SELECT")
	if !isSelect {
		return
	}
	for _, t := range sqlToks {
		switch t.Text {
		case "(":
			depth++
			continue
		case ")":
			depth--
			continue
		}
		if depth != 0 {
			continue
		}
		upper := strings.ToUpper(t.Text)
		if t.Type == SQLTokenKeyword {
			switch upper {
			case "FROM":
				inFrom = true
				continue
			case "WHERE", "GROUP", "ORDER", "HAVING", "UNION", "MINUS",
				"INTERSECT", "CONNECT", "START", "JOIN", "INNER", "LEFT",
				"RIGHT", "FULL", "CROSS", "FOR":
				inFrom = false
			}
		}
		if inFrom && t.Text == "," {
			add(tok, CodeSQLCommaJoin,
				"Comma join in FROM clause - ANSI JOIN ... ON syntax keeps each join condition next to its table and the WHERE clause for real filters")
			return
		}
	}
}

// checkLegacyOuterJoin (I2): Oracle's `(+)` outer-join marker.
func checkLegacyOuterJoin(tok lexer.Token, sqlToks []SQLToken, add func(lexer.Token, string, string)) {
	for i := 0; i+2 < len(sqlToks); i++ {
		if sqlToks[i].Text == "(" && sqlToks[i+1].Text == "+" && sqlToks[i+2].Text == ")" {
			add(tok, CodeSQLLegacyOuterJoin,
				"Oracle (+) outer-join marker - ANSI LEFT/RIGHT JOIN is dialect-portable and keeps the join direction readable")
			return
		}
	}
}

// checkInconsistentAlias (I3): one SELECT list mixing explicit `AS alias`
// and bare `expr alias` forms.
func checkInconsistentAlias(tok lexer.Token, sqlToks []SQLToken, add func(lexer.Token, string, string)) {
	if len(sqlToks) == 0 || !strings.EqualFold(sqlToks[0].Text, "SELECT") {
		return
	}
	depth := 0
	var item []SQLToken
	hasAS, hasBare := false, false
	classify := func() {
		if len(item) < 2 {
			return
		}
		last := item[len(item)-1]
		prev := item[len(item)-2]
		if strings.EqualFold(prev.Text, "AS") && prev.Type == SQLTokenKeyword {
			hasAS = true
			return
		}
		// Bare alias: a trailing identifier directly after another value
		// token (identifier, number, string, placeholder, or a closing
		// paren) with no dot chaining.
		if last.Type == SQLTokenIdentifier && prev.Text != "." &&
			(prev.Type == SQLTokenIdentifier || prev.Type == SQLTokenNumber ||
				prev.Type == SQLTokenString || prev.Type == SQLTokenPlaceholder ||
				prev.Text == ")") {
			hasBare = true
		}
	}
	for i := 1; i < len(sqlToks); i++ {
		t := sqlToks[i]
		switch t.Text {
		case "(":
			depth++
		case ")":
			depth--
		}
		if depth == 0 {
			if t.Type == SQLTokenKeyword && strings.EqualFold(t.Text, "FROM") {
				classify()
				break
			}
			if t.Text == "," {
				classify()
				item = item[:0]
				continue
			}
		}
		// DISTINCT/TOP prefixes are not part of the first item.
		if len(item) == 0 && t.Type == SQLTokenKeyword &&
			(strings.EqualFold(t.Text, "DISTINCT") || strings.EqualFold(t.Text, "TOP")) {
			continue
		}
		item = append(item, t)
	}
	if hasAS && hasBare {
		add(tok, CodeSQLInconsistentAlias,
			"SELECT list mixes explicit 'AS alias' and bare 'expr alias' forms - a uniform explicit AS keeps aliases scannable")
	}
}

// checkDialectMix (I5): Oracle-only and MSSQL-only idioms in one statement.
func checkDialectMix(tok lexer.Token, sqlToks []SQLToken, add func(lexer.Token, string, string)) {
	var oracle, mssql []string
	seen := map[string]bool{}
	note := func(list *[]string, name string) {
		if !seen[name] {
			seen[name] = true
			*list = append(*list, name)
		}
	}
	for i, t := range sqlToks {
		upper := strings.ToUpper(t.Text)
		switch t.Type {
		case SQLTokenIdentifier, SQLTokenKeyword, SQLTokenFunction:
			if oracleDialectMarkers[upper] {
				note(&oracle, upper)
			}
			if mssqlDialectMarkers[upper] {
				note(&mssql, upper)
			}
		}
		if t.Text == "(" && i+2 < len(sqlToks) && sqlToks[i+1].Text == "+" && sqlToks[i+2].Text == ")" {
			note(&oracle, "(+)")
		}
	}
	if len(oracle) > 0 && len(mssql) > 0 {
		add(tok, CodeSQLDialectMix,
			fmt.Sprintf("Oracle-only (%s) and SQL Server-only (%s) idioms in one statement - STARLIMS environments run one dialect, so one side of this will fail there",
				strings.Join(oracle, ", "), strings.Join(mssql, ", ")))
	}
}

// checkSelectStar (I6): `SELECT *` / `SELECT t.*` at the head of a select
// list. COUNT(*) and other in-paren stars never match.
func checkSelectStar(tok lexer.Token, sqlToks []SQLToken, add func(lexer.Token, string, string)) {
	for i, t := range sqlToks {
		if t.Type != SQLTokenKeyword || !strings.EqualFold(t.Text, "SELECT") {
			continue
		}
		j := i + 1
		for j < len(sqlToks) && sqlToks[j].Type == SQLTokenKeyword &&
			(strings.EqualFold(sqlToks[j].Text, "DISTINCT") || strings.EqualFold(sqlToks[j].Text, "ALL")) {
			j++
		}
		if j < len(sqlToks) && sqlToks[j].Text == "*" {
			add(tok, CodeSQLSelectStar,
				"SELECT * in embedded SQL - an explicit column list survives schema changes and documents what the code consumes")
			return
		}
		if j+2 < len(sqlToks) && sqlToks[j].Type == SQLTokenIdentifier &&
			sqlToks[j+1].Text == "." && sqlToks[j+2].Text == "*" {
			add(tok, CodeSQLSelectStar,
				"SELECT alias.* in embedded SQL - an explicit column list survives schema changes and documents what the code consumes")
			return
		}
	}
}

// checkSuspectPlaceholder (I7): a `?…?` placeholder wrapping a `<<…>>`
// template marker — two substitution layers stacked (`?'<<username>>'?`,
// corpus-observed and judged suspect by the corpus owner). Plain
// quoted-literal placeholders (`?'Y'?`, `?'N/A'?`) are an established
// production idiom — 271 corpus uses across 99 files — and never flag.
func checkSuspectPlaceholder(tok lexer.Token, sqlToks []SQLToken, add func(lexer.Token, string, string)) {
	for _, t := range sqlToks {
		if t.Type == SQLTokenPlaceholder && strings.Contains(t.Text, "<<") {
			add(tok, CodeSQLSuspectPlaceholder,
				fmt.Sprintf("Placeholder %s stacks a <<...>> template substitution inside ?...? - verify both layers actually resolve; a plain ?name? placeholder avoids the ambiguity", t.Text))
			return
		}
	}
}
