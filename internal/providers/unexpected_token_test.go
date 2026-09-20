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
