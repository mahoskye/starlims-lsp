package providers

import (
	"strings"
	"testing"
)

// unexpectedTokens returns the unexpected_token diagnostics for a script
// under default options.
func unexpectedTokens(t *testing.T, script string) []Diagnostic {
	t.Helper()
	var out []Diagnostic
	for _, d := range GetDiagnostics(script, DefaultDiagnosticOptions()) {
		if d.Code == CodeUnexpectedToken {
			out = append(out, d)
		}
	}
	return out
}

func hasCode(t *testing.T, script, code string) bool {
	t.Helper()
	for _, d := range GetDiagnostics(script, DefaultDiagnosticOptions()) {
		if d.Code == code {
			return true
		}
	}
	return false
}

// [spec diag.unexpected_token] The range covers the offending token and the
// message names its class, its text, and what was expected.
func TestUnexpectedToken_MessagesAndAnchors(t *testing.T) {
	cases := []struct {
		name   string
		script string
		line   int // 0-based
		col    int // 0-based
		want   string
	}{
		{"prose", ":DECLARE nCount;\nnCount := 1;\nThis is a bunch of text but it does not evaluate as wrong\nnCount := 2;\n",
			2, 5, "Unexpected identifier 'is' - expected an operator or ';'"},
		{"for each", ":DECLARE aItems, nTotal;\n:FOR EACH nItem IN aItems;\n\tnTotal += nItem;\n:NEXT;\n",
			1, 10, "Unexpected identifier 'nItem' in ':FOR' header - expected ':=' (SSL has no ':FOR EACH' form)"},
		{"for missing :TO", ":DECLARE i;\n:FOR i := 1 10;\n:NEXT;\n",
			1, 12, "Unexpected number '10' in ':FOR' header - expected ':TO'"},
		{"for no target", ":FOR :TO 10;\n:NEXT;\n",
			0, 5, "Unexpected keyword ':TO' in ':FOR' header - expected a loop variable"},
		{"missing terminator", ":DECLARE nCount;\nnCount := 1\nnCount := 2;\n",
			2, 0, "Unexpected identifier 'nCount' - expected an operator or ';' (is the previous statement missing its ';'?)"},
		{"for header missing terminator", ":DECLARE i;\n:FOR i := 1 :TO 10\n:NEXT;\n",
			2, 0, "Unexpected keyword ':NEXT' in ':FOR' header - expected ':STEP' or ';' (is the previous statement missing its ';'?)"},
		{"missing operand", ":DECLARE nCount;\nnCount := ;\n",
			1, 10, "Unexpected ';' - expected an expression"},
		{"dangling operator", ":DECLARE x;\nx := 1 + ;\n",
			1, 9, "Unexpected ';' - expected an expression"},
		{"missing condition", ":IF;\n:ENDIF;\n",
			0, 3, "Unexpected ';' - expected a condition"},
		{"missing comma in list", ":DECLARE a;\nDoProc(\"X\" {a});\n",
			1, 11, "Unexpected '{' - expected ',' or ')'"},
		{"matched closer after operator", ":DECLARE x;\nx := (1 + );\n",
			1, 10, "Unexpected ')' - expected an expression"},
		{"adjacent strings", ":DECLARE sX;\nsX := \"a\" \"b\";\n",
			1, 10, "Unexpected string \"b\" - expected an operator or ';'"},
		{"default without comma", ":PARAMETERS sName;\n:DEFAULT sName \"x\";\n",
			1, 15, "Unexpected string \"x\" - expected ','"},
		{"unary operator after an operand", ":DECLARE x;\nx := 1 ! 2;\n",
			1, 7, "Unexpected operator '!' - expected a binary operator or ';'"},
		{"dangling operator at end of file", ":DECLARE x;\nx := 1 +",
			1, 8, "Unexpected end of file - expected an expression"},
		{"statement cannot start here", ":DECLARE x;\n*x;\n",
			1, 0, "Unexpected operator '*' - expected a statement"},
		{"unary plus does not exist", ":DECLARE x;\nx := +5;\n",
			1, 5, "Unexpected operator '+' - expected an expression"},
		{"malformed exponent is nobody else's", ":DECLARE nX, nVar;\nnX := 2.0E+nVar;\n",
			1, 9, "Unexpected identifier 'E' - expected an operator or ';'"},
	}
	for _, c := range cases {
		got := unexpectedTokens(t, c.script)
		if len(got) != 1 {
			t.Errorf("%s: want 1 diagnostic, got %d: %+v", c.name, len(got), got)
			continue
		}
		d := got[0]
		if d.Severity != SeverityError || d.Source != "ssl-lsp" {
			t.Errorf("%s: severity/source %v/%q", c.name, d.Severity, d.Source)
		}
		if d.Range.Start.Line != c.line || d.Range.Start.Character != c.col {
			t.Errorf("%s: anchored at %d:%d, want %d:%d", c.name, d.Range.Start.Line, d.Range.Start.Character, c.line, c.col)
		}
		if d.Message != c.want {
			t.Errorf("%s:\n  got  %s\n  want %s", c.name, d.Message, c.want)
		}
	}
}

// [spec diag.unexpected_token] One diagnostic per statement, on the first
// unexpected token; checking resumes at the next statement.
func TestUnexpectedToken_OnePerStatementAndRecovery(t *testing.T) {
	script := ":DECLARE i, nCount;\nThis is a bunch of text but it does not evaluate as wrong;\n:FOR i := 1 10;\n:NEXT;\nnCount := 1;\nfoo bar baz qux;\n"
	got := unexpectedTokens(t, script)
	if len(got) != 3 {
		t.Fatalf("want 3 diagnostics (one per bad statement), got %d: %+v", len(got), got)
	}
	wantLines := []int{1, 2, 5}
	for i, d := range got {
		if d.Range.Start.Line != wantLines[i] {
			t.Errorf("diagnostic %d on line %d, want %d", i, d.Range.Start.Line, wantLines[i])
		}
	}
}

// [spec diag.unexpected_token] Well-formed statements, prose in literal
// text, multi-line continuations, bare expression statements, optional
// operands, and undeclared names in well-formed expressions never flag.
func TestUnexpectedToken_DoesNotFlag(t *testing.T) {
	clean := map[string]string{
		"valid loops":               ":DECLARE i, aItems;\n:FOR i := 1 :TO Len(aItems);\n:NEXT;\n:FOR i := 10 :TO 1 :STEP -1;\n:NEXT;\n",
		"literal text":              ":DECLARE sText;\n/* This is a bunch of text but it does not evaluate as wrong;\nsText := \"This is a bunch of text but it does not evaluate as wrong.\";\n",
		"continuations":             ":DECLARE nTotal, nA, nB, i, nMax;\nnTotal := nA\n\t+ nB;\n:FOR i := 1\n\t:TO nMax;\n:NEXT;\n",
		"bare and optional":         ":PROCEDURE Demo;\n\tnTotal := nMissing + 1;\n\tThis;\n\t:RETURN;\n:ENDPROC;\n",
		"end of file":               ":DECLARE nCount;\nnCount := 1",
		"expression forms":          ":DECLARE oEmail, x, bOk, fn, oObj, aItems, i;\noEmail := Email{};\nx++;\n++x;\nbOk := .T. .AND. .F.;\nfn := {|n| n + 1};\nx := oObj:Method(1)[2]:Prop;\nx := -aItems[i] ^ 2;\n:WHILE (i += 1) <= 10;\n:ENDWHILE;\nx := NIL;\nx := \"s\" $ \"haystack\";\nx := 5 % 2 ** 3;\nx := [bracket string];\nDoProc(\"P\", {a,,c});\n-x;\n",
		"signed exponents":          ":DECLARE nA, nB, nD;\nnA := 9.0E+1;\nnB := 9.0E-1;\nnD := 1.5e+3 * 2;\n",
		"member chain across lines": ":DECLARE txt, sTitle, sUser;\n:RETURN txt:ToString()\n\t:Replace(\"##TITLE##\", sTitle)\n\t:Replace(\"##USER##\", sUser);\n",
		"assignment forms":          ":DECLARE a, b, c, x, n, oObj;\na := b := c;\noObj:Prop := oObj:Items[1] := x;\n:RETURN x := .T.;\n:RETURN { .T., n += 1 };\nDoProc(\"P\", {a := 1});\n",
	}
	for name, script := range clean {
		if got := unexpectedTokens(t, script); len(got) != 0 {
			t.Errorf("%s: unexpected diagnostics: %+v", name, got)
		}
	}
}

// [spec diag.unexpected_token] When a lexer- or operator-level rule already
// names the token, this rule stays silent — and that rule does fire, so the
// deferral never leaves the mistake unreported.
func TestUnexpectedToken_DefersToOwningRule(t *testing.T) {
	cases := []struct {
		name, script, owner string
	}{
		{"unknown token", ":DECLARE nResult;\nnResult := `nCount`;\n", CodeUnknownToken},
		{"dot property access", ":DECLARE nTotal, oObj;\nnTotal := oObj.Value;\n", CodeDotPropertyAccess},
		{"bare AND", ":DECLARE bA, bB, nTotal;\n:IF bA AND bB;\n\tnTotal := 1;\n:ENDIF;\n", CodeBareLogicalOperator},
		{"single ampersand", ":DECLARE bOk, bA, bB;\nbOk := bA & bB;\n", CodeInvalidOperatorSequence},
		{"glued ===", ":DECLARE nCount;\n:IF nCount === 1;\n:ENDIF;\n", CodeInvalidOperatorSequence},
		{"assignment in condition", ":DECLARE nCount;\n:IF nCount := 1;\n:ENDIF;\n", CodeAssignmentInCondition},
		{"scientific notation", ":DECLARE nTotal;\nnTotal := 7e2;\n", CodeScientificNotation},
		{"stray closer", ":DECLARE x;\nx := 1);\n", CodeUnmatchedDelimiter},
		{"unclosed opener", ":DECLARE x, y;\nx := (1 + 2;\ny := 3;\n", CodeUnclosedDelimiter},
	}
	for _, c := range cases {
		if got := unexpectedTokens(t, c.script); len(got) != 0 {
			t.Errorf("%s: should defer, got %+v", c.name, got)
		}
		if !hasCode(t, c.script, c.owner) {
			t.Errorf("%s: owning rule %s did not fire, so the deferral hides the mistake", c.name, c.owner)
		}
	}
}

// [spec diag.unexpected_token] The missing-terminator hint needs a real
// line break before the token; a multi-line string token ending just
// before it is not one.
func TestUnexpectedToken_TerminatorHintNeedsLineBreak(t *testing.T) {
	script := ":DECLARE sText;\nsText := \"never closed;\nsText := \"closed\";\n"
	got := unexpectedTokens(t, script)
	if len(got) != 1 {
		t.Fatalf("want 1 diagnostic, got %d: %+v", len(got), got)
	}
	if strings.Contains(got[0].Message, "previous statement") {
		t.Errorf("hint should not fire without a line break: %s", got[0].Message)
	}
}

// [spec diag.unexpected_token] Data-source files are outside the rule
// entirely: their SSL is directive syntax and their bodies are SQL.
func TestUnexpectedToken_DataSourceGate(t *testing.T) {
	script := ":PARAMETERS sName;\nsName := 1;\nThis is a bunch of text\n"
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true
	for _, d := range GetDiagnostics(script, opts) {
		if d.Code == CodeUnexpectedToken {
			t.Fatalf("fired on a data-source file: %+v", d)
		}
	}
	if got := unexpectedTokens(t, script); len(got) != 1 {
		t.Errorf("same content as SSL should flag once, got %d", len(got))
	}
}

// [spec diag.unexpected_token] A keyword statement that takes nothing is
// complete at its keyword: the first token after it that is not its ';'
// is unexpected.
func TestUnexpectedToken_BareKeywordTails(t *testing.T) {
	tests := []struct {
		name, script string
		line, col    int // 0-based position of the finding
		want         string
	}{
		{"operand on :NEXT", ":DECLARE i;\n:FOR i := 1 :TO 2;\n:NEXT i;\n", 2, 6,
			"Unexpected identifier 'i' - expected ';' after ':NEXT'"},
		{"statement on the :ELSE line", ":DECLARE nX;\n:IF .T.;\n:ELSE nX := 1;\n:ENDIF;\n", 2, 6,
			"Unexpected identifier 'nX' - expected ';' after ':ELSE'"},
		{"name on :ENDPROC", ":PROCEDURE Run;\n:ENDPROC Run;\n", 1, 9,
			"Unexpected identifier 'Run' - expected ';' after ':ENDPROC'"},
		{"number on :ENDIF", ":IF .T.;\n:ENDIF 1;\n", 1, 7,
			"Unexpected number '1' - expected ';' after ':ENDIF'"},
		{"two closers in one statement", ":PROCEDURE Run;\n:IF .T.;\n:ENDIF :ENDPROC;\n", 2, 7,
			"Unexpected keyword ':ENDPROC' - expected ';' after ':ENDIF'"},
		{":ENDIF missing its ';'", ":DECLARE nX;\n:IF .T.;\n:ENDIF\nnX := 1;\n", 3, 0,
			"Unexpected identifier 'nX' - expected ';' after ':ENDIF' (is ':ENDIF' missing its ';'?)"},
		{"lowercase keyword", ":if .T.;\n:endif 1;\n", 1, 7,
			"Unexpected number '1' - expected ';' after ':ENDIF'"},
		{":TRY with an operand", ":TRY 1;\n\tnX := 1;\n:CATCH;\n:ENDTRY;\n", 0, 5,
			"Unexpected number '1' - expected ';' after ':TRY'"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := unexpectedTokens(t, tc.script)
			if len(got) != 1 {
				t.Fatalf("want one finding, got %d: %+v", len(got), got)
			}
			d := got[0]
			if d.Range.Start.Line != tc.line || d.Range.Start.Character != tc.col || d.Message != tc.want {
				t.Errorf("got %d:%d %q\nwant %d:%d %q", d.Range.Start.Line, d.Range.Start.Character, d.Message,
					tc.line, tc.col, tc.want)
			}
			if d.Severity != SeverityError {
				t.Errorf("severity = %v, want error", d.Severity)
			}
		})
	}
}

// [spec diag.unexpected_token] A :PROCEDURE header is the keyword, one
// name and the ';'.
func TestUnexpectedToken_ProcedureHeader(t *testing.T) {
	tests := []struct {
		name, script string
		col          int
		want         string
	}{
		{"no name", ":PROCEDURE;\n:ENDPROC;\n", 10,
			"Unexpected ';' - expected a procedure name after ':PROCEDURE'"},
		{"number for a name", ":PROCEDURE 123;\n:ENDPROC;\n", 11,
			"Unexpected number '123' - expected a procedure name after ':PROCEDURE'"},
		{"literal word for a name", ":PROCEDURE NIL;\n:ENDPROC;\n", 11,
			"Unexpected keyword 'NIL' - expected a procedure name after ':PROCEDURE'"},
		{"string for a name", ":PROCEDURE \"Run\";\n:ENDPROC;\n", 11,
			"Unexpected string \"Run\" - expected a procedure name after ':PROCEDURE'"},
		{"two names", ":PROCEDURE Run Extra;\n:ENDPROC;\n", 15,
			"Unexpected identifier 'Extra' - expected ';' after the procedure name"},
		{"keyword in the header", ":PROCEDURE Run :IF .T.;\n:ENDIF;\n:ENDPROC;\n", 15,
			"Unexpected keyword ':IF' - expected ';' after the procedure name"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := unexpectedTokens(t, tc.script)
			if len(got) != 1 {
				t.Fatalf("want one finding, got %d: %+v", len(got), got)
			}
			if d := got[0]; d.Range.Start.Line != 0 || d.Range.Start.Character != tc.col || d.Message != tc.want {
				t.Errorf("got %d:%d %q\nwant 0:%d %q", d.Range.Start.Line, d.Range.Start.Character, d.Message, tc.col, tc.want)
			}
		})
	}

	// The header's ';' missing: the next statement's keyword is unexpected.
	got := unexpectedTokens(t, ":PROCEDURE Run\n:PARAMETERS nX;\n:ENDPROC;\n")
	if len(got) != 1 || !strings.HasSuffix(got[0].Message, "(is the ':PROCEDURE' line missing its ';'?)") {
		t.Errorf("missing header terminator: got %+v", got)
	}
}

// [spec diag.unexpected_token] What the keyword-tail check leaves alone.
func TestUnexpectedToken_BareKeywordTailsDoNotFlag(t *testing.T) {
	clean := []struct{ name, script string }{
		{"well-formed blocks", ":PROCEDURE Run;\n\t:PARAMETERS nX;\n\t:IF nX > 1;\n\t\tnX := 1;\n\t:ELSE;\n\t\tnX := 2;\n\t:ENDIF;\n\t:RETURN nX;\n:ENDPROC;\n"},
		{"comment between keyword and ';'", ":IF .T.;\n:ELSE /* nothing;\n;\n:ENDIF;\n"},
		{"end-of-line comment after the ';'", ":IF .T.;\n:ENDIF; /* done;\n"},
		{"last statement without ';'", ":PROCEDURE Run;\n:ENDPROC\n"},
		{"header at end of file", ":PROCEDURE Run"},
		{"keyword alone at end of file", ":PROCEDURE"},
		{"member named like a keyword", ":DECLARE oX, vY;\nvY := oX:Next;\nvY := oX:EndIf(1);\n"},
	}
	for _, tc := range clean {
		t.Run(tc.name, func(t *testing.T) {
			if got := unexpectedTokens(t, tc.script); len(got) != 0 {
				t.Errorf("want no unexpected_token, got %+v", got)
			}
		})
	}

	// Findings that belong to another rule stay with it, once.
	if got := unexpectedTokens(t, ":PROCEDURE Run(nX);\n:ENDPROC;\n"); len(got) != 0 {
		t.Errorf(":PROCEDURE Name( is procedure_declaration_syntax's: %+v", got)
	}
	if got := unexpectedTokens(t, ":TRY;\n\tnX := 1;\n:CATCH oErr;\n:ENDTRY;\n"); len(got) != 0 {
		t.Errorf("what follows :CATCH is catch_clause_form's: %+v", got)
	}
	// One finding per statement: a run of keyword lines with no ';' at all
	// is one statement.
	if got := unexpectedTokens(t, ":IF .T.\n:ELSE\n:ENDIF\n:RETURN 1;\n"); len(got) != 1 {
		t.Errorf("a run with no ';' is one statement: want one finding, got %+v", got)
	}
}

// [spec diag.unexpected_token] A declaration list is names separated by
// commas.
func TestUnexpectedToken_DeclarationLists(t *testing.T) {
	tests := []struct {
		name, script string
		col          int
		want         string
	}{
		{"empty :DECLARE", ":DECLARE;\n", 8, "Unexpected ';' - expected a name after ':DECLARE'"},
		{"empty :PUBLIC", ":PUBLIC;\n", 7, "Unexpected ';' - expected a name after ':PUBLIC'"},
		{"number in :DECLARE", ":DECLARE 1;\n", 9, "Unexpected number '1' - expected a name after ':DECLARE'"},
		{"string in :PARAMETERS", ":PARAMETERS \"x\";\n", 12, "Unexpected string \"x\" - expected a name after ':PARAMETERS'"},
		{"missing comma", ":DECLARE nA nB;\n", 12, "Unexpected identifier 'nB' - expected ',' or ';'"},
		{"trailing comma", ":DECLARE nA,;\n", 12, "Unexpected ';' - expected a name after ','"},
		{"number after a comma", ":DECLARE nA, 2;\n", 13, "Unexpected number '2' - expected a name after ','"},
		{"subscript on a name", ":DECLARE aX[10], nB;\n", 11, "Unexpected '[' - expected ',' or ';'"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := unexpectedTokens(t, tc.script)
			if len(got) != 1 {
				t.Fatalf("want one finding, got %d: %+v", len(got), got)
			}
			if d := got[0]; d.Range.Start.Line != 0 || d.Range.Start.Character != tc.col || d.Message != tc.want {
				t.Errorf("got %d:%d %q\nwant 0:%d %q", d.Range.Start.Line, d.Range.Start.Character, d.Message, tc.col, tc.want)
			}
		})
	}

	// The list's ';' missing: the next statement's first word is unexpected.
	got := unexpectedTokens(t, ":DECLARE nA\nnA := 1;\n")
	if len(got) != 1 || !strings.HasSuffix(got[0].Message, "(is the ':DECLARE' list missing its ';'?)") {
		t.Errorf("missing list terminator: got %+v", got)
	}

	clean := []struct{ name, script string }{
		{"lists on one line and several", ":PARAMETERS sIn, nMax;\n:DECLARE nA, nB,\n\tnC;\n:PUBLIC gRate;\n"},
		{"comment inside a list", ":DECLARE nA, /* the first;\n\tnB;\n"},
		{"list at end of file", ":DECLARE nA, nB"},
	}
	for _, tc := range clean {
		t.Run(tc.name, func(t *testing.T) {
			if got := unexpectedTokens(t, tc.script); len(got) != 0 {
				t.Errorf("want no unexpected_token, got %+v", got)
			}
		})
	}

	// What another rule names stays with it, once.
	owned := []struct{ name, script, code string }{
		{"empty :PARAMETERS", ":PARAMETERS;\n", CodeEmptyParameters},
		{"initializer in :DECLARE", ":DECLARE nA := 1;\n", CodeDeclareInitializer},
		{"inline default in a script", ":PARAMETERS sIn := \"x\";\n", CodeInlineDefaultOutsideDatasource},
	}
	for _, tc := range owned {
		t.Run(tc.name, func(t *testing.T) {
			if got := unexpectedTokens(t, tc.script); len(got) != 0 {
				t.Errorf("want no unexpected_token, got %+v", got)
			}
			if !hasCode(t, tc.script, tc.code) {
				t.Errorf("want %s to report it", tc.code)
			}
		})
	}
	if got := unexpectedTokens(t, ":PARAMETERS sIn :DEFAULT sIn, \"\";\n"); len(got) != 1 {
		t.Errorf("a keyword in a list is reported once: %+v", got)
	}
}

// [spec diag.unexpected_token] :CLASS, :REGION, :INHERIT and :INCLUDE take
// one name.
func TestUnexpectedToken_NameHeaders(t *testing.T) {
	tests := []struct {
		name, script string
		line, col    int
		want         string
	}{
		{":CLASS with two names", ":CLASS A B;\n", 0, 9, "Unexpected identifier 'B' - expected ';' after the name in ':CLASS'"},
		{":CLASS with a number", ":CLASS 123;\n", 0, 7, "Unexpected number '123' - expected a class name or ';' after ':CLASS'"},
		{":INHERIT with no name", ":CLASS A;\n:INHERIT;\n", 1, 8, "Unexpected ';' - expected a class name after ':INHERIT'"},
		{":INHERIT with two names", ":CLASS A;\n:INHERIT Rules.Base Extra;\n", 1, 20, "Unexpected identifier 'Extra' - expected ';' after the name in ':INHERIT'"},
		{":REGION with no name", ":REGION;\n:ENDREGION;\n", 0, 7, "Unexpected ';' - expected a region name after ':REGION'"},
		{":REGION with two names", ":REGION A B;\ntext\n:ENDREGION;\n", 0, 10, "Unexpected identifier 'B' - expected ';' after the name in ':REGION'"},
		{":INCLUDE with no name", ":INCLUDE;\n", 0, 8, "Unexpected ';' - expected a script name after ':INCLUDE'"},
		{":INCLUDE with two targets", ":INCLUDE Lib.A Lib.B;\n", 0, 15, "Unexpected identifier 'Lib' - expected ';' after the name in ':INCLUDE'"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := unexpectedTokens(t, tc.script)
			if len(got) != 1 {
				t.Fatalf("want one finding, got %d: %+v", len(got), got)
			}
			if d := got[0]; d.Range.Start.Line != tc.line || d.Range.Start.Character != tc.col || d.Message != tc.want {
				t.Errorf("got %d:%d %q\nwant %d:%d %q", d.Range.Start.Line, d.Range.Start.Character, d.Message, tc.line, tc.col, tc.want)
			}
		})
	}

	clean := []struct{ name, script string }{
		{":CLASS with no name", ":CLASS;\n"},
		{"plain headers", ":CLASS DueRule;\n:INHERIT Rules.BaseRule;\n"},
		{"a digit inside a dotted name", ":CLASS A;\n:INHERIT Rules.Level2Rule;\n"},
		{"an underscore inside a dotted name", ":INCLUDE Lib.Date_Rules;\n"},
		{"a class name the lexer cannot read whole", ":CLASS Units[];\n"},
		{"a region with a name", ":REGION Query;\nSELECT 1\n:ENDREGION;\n"},
		{"a quoted :INCLUDE target is include_quoted_target's", ":INCLUDE \"Lib.A\";\n"},
		{"a quoted :INHERIT parent is left alone", ":CLASS A;\n:INHERIT \"Rules.Base\";\n"},
		{"a header at end of file", ":INHERIT Rules.BaseRule"},
	}
	for _, tc := range clean {
		t.Run(tc.name, func(t *testing.T) {
			if got := unexpectedTokens(t, tc.script); len(got) != 0 {
				t.Errorf("want no unexpected_token, got %+v", got)
			}
		})
	}
}
