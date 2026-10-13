package providers

import (
	"strings"
	"testing"
)

// Issue #255 regressions for the embedded-SQL checks: placeholder ranges
// in multi-line strings, the case-mapping slice panic, the expression-tree
// sql_injection rule, and the lexer-based SQL advisories.

// [spec diag.udobject_array_in_clause] A placeholder on a later line of a
// multi-line SQL string is ranged on that line, not on the string's first
// line past its end. The same helper serves complex_sql_placeholder and
// invalid_sql_param.
func TestPlaceholderRange_MultiLineString(t *testing.T) {
	code := "aRows := SQLExecute(\"\n    SELECT c1\n    FROM t1\n    WHERE c2 IN (?oFilter:aCodes?)\n      AND c3 = ?sA + sB?\n\");"
	opts := DefaultDiagnosticOptions()
	opts.IncludeInfoDiagnostics = true
	diags := GetDiagnostics(code, opts)

	check := func(code string, wantLine, wantChar, wantLen int) {
		t.Helper()
		got := diagsWithCode(diags, code)
		if len(got) != 1 {
			t.Fatalf("%s: want 1 diagnostic, got %d (%v)", code, len(got), diags)
		}
		r := got[0].Range
		if r.Start.Line != wantLine || r.Start.Character != wantChar ||
			r.End.Line != wantLine || r.End.Character != wantChar+wantLen {
			t.Errorf("%s: range %+v, want line %d chars %d-%d", code, r, wantLine, wantChar, wantChar+wantLen)
		}
	}
	// `    WHERE c2 IN (` is 17 bytes before the placeholder.
	check(CodeUdObjectArrayInClause, 3, 17, len("?oFilter:aCodes?"))
	// `      AND c3 = ` is 15 bytes before the placeholder.
	check(CodeComplexSqlPlaceholder, 4, 15, len("?sA + sB?"))

	// invalid_sql_param (opt-in) uses the same helper.
	opts.CheckSQLParams = true
	diags = GetDiagnostics(":DECLARE aRows;\naRows := SQLExecute(\"\n    SELECT c1 FROM t1\n    WHERE c2 = ?sMissing?\n\");", opts)
	got := diagsWithCode(diags, CodeInvalidSqlParam)
	if len(got) != 1 {
		t.Fatalf("invalid_sql_param: want 1 diagnostic, got %v", diags)
	}
	if r := got[0].Range; r.Start.Line != 3 || r.Start.Character != 15 || r.End.Character != 15+len("?sMissing?") {
		t.Errorf("invalid_sql_param: range %+v, want line 3 chars 15-25", r)
	}

	// Single-line strings keep their existing ranges.
	diags = GetDiagnostics(`SQLExecute("SELECT c1 FROM t1 WHERE c2 IN (?oX:aIds?)");`, DefaultDiagnosticOptions())
	got = diagsWithCode(diags, CodeUdObjectArrayInClause)
	if len(got) != 1 || got[0].Range.Start.Line != 0 || got[0].Range.Start.Character != 43 {
		t.Errorf("single-line range moved: %v", got)
	}
}

// [spec diag.udobject_array_in_clause] Case mapping can change a string's
// byte length (a Turkish dotless ı is two bytes and upper-cases to the
// one-byte I). Slicing the upper-cased copy by an offset into the original
// panicked, and the pipeline's recover replaced every diagnostic in the
// file with internal_error.
func TestUDObjectArrayInClause_CaseMappingDoesNotPanic(t *testing.T) {
	code := `aRows := SQLExecute("select a from t where c = '` + strings.Repeat("ı", 30) + `' and b IN (?oX:aIds?)");`
	diags := GetDiagnostics(code, DefaultDiagnosticOptions())
	if len(diagsWithCode(diags, "internal_error")) > 0 {
		t.Fatalf("checks panicked: %v", diags)
	}
	if len(diagsWithCode(diags, CodeUdObjectArrayInClause)) != 1 {
		t.Errorf("want udobject_array_in_clause once, got %v", diags)
	}
}

// [spec diag.sql_injection] The rule judges the first argument's `+` tree:
// a string-literal operand plus a runtime operand flags; constant-only
// concatenation and a lone safe builder do not; a safe builder no longer
// hides a later splice; leading calls and parentheses are seen through.
func TestSQLInjection_ExpressionTree(t *testing.T) {
	cases := []struct {
		name string
		stmt string
		want bool
	}{
		{"literal splice", `a := SQLExecute("SELECT c1 FROM t1 WHERE c2 = '" + sV + "'");`, true},
		{"identifier first", `a := LSearch(sSql + " WHERE c2 = 1");`, true},
		{"call first", `a := SQLExecute(Upper(sSql) + " WHERE c2 = 1");`, true},
		{"parenthesized", `a := SQLExecute(("SELECT c1 FROM t1 WHERE c2 = '" + sV + "'"));`, true},
		{"builder then splice", `a := SQLExecute("SELECT c1 FROM t1 WHERE c2 IN " + BuildStringForIn(aV) + " AND c3 = '" + sV + "'");`, true},
		{"inside a condition", ":IF Len(SQLExecute(\"SELECT c1 FROM t1 WHERE c2 = \" + LimsString(nV))) > 0;\n:ENDIF;", true},
		{"constants only", `a := SQLExecute("SELECT c1 FROM t1" + " WHERE c2 = 'A'");`, false},
		{"number literal", `a := SQLExecute("SELECT c1 FROM t1 WHERE c2 = " + 5);`, false},
		{"builder only", `a := SQLExecute("SELECT c1 FROM t1 WHERE c2 IN " + BuildStringForIn(aV));`, false},
		{"identifier + identifier", `a := SQLExecute(sSql + sWhere);`, false},
		{"plain variable", `a := SQLExecute(sSql);`, false},
		{"concatenation in a later argument", `a := LSearch("SELECT c1 FROM t1 WHERE c2 = ?", "", "", {sA + "x"});`, false},
	}
	for _, tc := range cases {
		got := diagsWithCode(GetDiagnostics(tc.stmt, DefaultDiagnosticOptions()), CodeSqlInjection)
		if (len(got) > 0) != tc.want {
			t.Errorf("%s: flagged=%v, want %v (%v)", tc.name, len(got) > 0, tc.want, got)
		}
		if len(got) > 1 {
			t.Errorf("%s: want at most one diagnostic per call, got %d", tc.name, len(got))
		}
	}

	// The range stays on the `+` adjoining the runtime operand.
	got := diagsWithCode(GetDiagnostics(`a := SQLExecute("SELECT c1 FROM t1 WHERE c2 = '" + sV + "'");`, DefaultDiagnosticOptions()), CodeSqlInjection)
	if len(got) != 1 || got[0].Range.Start.Character != 49 || got[0].Range.End.Character != 50 {
		t.Errorf("range not on the + before the runtime operand: %v", got)
	}
}

// [spec diag.sql_literal_splice] Quotes are counted outside SQL comments,
// and only a concatenated piece is a splice; a non-splice piece keeps the
// structural advisories.
func TestSQLLiteralSplice_CommentAndConcatenation(t *testing.T) {
	opts := infoEnabledOptions()
	comment := GetDiagnostics("a := SQLExecute(\"select c1 from t1, t2 -- it's fine\n\");", opts)
	if len(diagsWithCode(comment, CodeSQLLiteralSplice)) != 0 {
		t.Errorf("apostrophe in a SQL comment read as a splice: %v", comment)
	}
	if len(diagsWithCode(comment, CodeSQLCommaJoin)) != 1 {
		t.Errorf("comma join hidden behind the comment apostrophe: %v", comment)
	}
	lone := GetDiagnostics(`a := SQLExecute("select c1 from t1 where c2 = 'x");`, opts)
	if len(diagsWithCode(lone, CodeSQLLiteralSplice)) != 0 {
		t.Errorf("unconcatenated string reported as a splice: %v", lone)
	}
}

// [spec diag.unicode_literal_prefix] [spec diag.unjustified_collate]
// Detection runs over the SQL lexer: flag literals, placeholders, literal
// text, and comments never match; a closing splice piece is lexed in step
// with the literal it continues.
func TestUnicodeAndCollate_LexerBased(t *testing.T) {
	opts := infoEnabledOptions()
	quiet := []string{
		"a := SQLExecute(\"select c1 from t1 where c2 = 'N' and c3 = ?'N'? and c4 = 'COLLATE' -- N'x' collate\n\");",
		`a := SQLExecute("select c1 from t1 where c2 = '" + sV + "' and c3 = 'N'");`,
	}
	for _, code := range quiet {
		diags := GetDiagnostics(code, opts)
		for _, c := range []string{CodeUnicodeLiteralPrefix, CodeUnjustifiedCollate} {
			if len(diagsWithCode(diags, c)) != 0 {
				t.Errorf("%s fired on %q: %v", c, code, diags)
			}
		}
	}
	loud := GetDiagnostics(":PROCEDURE P;\n\ta := SQLExecute(\"select c1 from t1 where c2 = n'x' order by c1 collate Latin1_General_CI_AS\");\n:ENDPROC;", opts)
	for _, c := range []string{CodeUnicodeLiteralPrefix, CodeUnjustifiedCollate} {
		if len(diagsWithCode(loud, c)) != 1 {
			t.Errorf("%s did not fire on its target: %v", c, loud)
		}
	}
}
