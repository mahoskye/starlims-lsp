package providers

import (
	"strings"
	"testing"

	"starlims-lsp/internal/constants"
)

// Hover, signature help, and completion render built-in functions from the
// reference-generated signatures (issue #252). These tests pin what a user
// reads; TestFunctionSignaturesMatchReference (constants) pins the data.

// Every built-in's hover and signature help name its parameters exactly as
// the reference does, in order.
func TestFunctionDocsUseReferenceParameterNames(t *testing.T) {
	// [spec feature.hover/A21] [spec feature.signature_help/A10]
	for _, name := range constants.SSLFunctionNames {
		sig, _ := constants.GetFunctionSignature(name)
		doc := buildFunctionDoc(sig)
		if len(doc.Parameters) != len(sig.Parameters) {
			t.Errorf("%s: doc has %d parameters, signature %d", name, len(doc.Parameters), len(sig.Parameters))
			continue
		}
		hover := getFunctionHover(name)
		if hover == nil {
			t.Errorf("%s: no hover", name)
			continue
		}
		for i, p := range sig.Parameters {
			if doc.Parameters[i].Label != p.Name {
				t.Errorf("%s: parameter %d labelled %q, reference name %q", name, i+1, doc.Parameters[i].Label, p.Name)
			}
			if !strings.Contains(hover.Contents, "`"+p.Name+"`") {
				t.Errorf("%s: hover does not list parameter %q", name, p.Name)
			}
		}
		help := functionSignatureHelp(sig, 0)
		for _, s := range help.Signatures {
			for _, p := range s.Parameters {
				if !strings.Contains(s.Label, p.Label) {
					t.Errorf("%s: signature label %q does not contain its parameter %q", name, s.Label, p.Label)
				}
			}
		}
	}
}

// SQLExecute was the issue's example: the hover described parameter 2 as a
// "friendly name" while the exceptions beneath it named sConnectionName.
// It must now read top to bottom: call form, what it does, each parameter
// with type/optional/default, what it returns, then the exceptions.
func TestGetHover_SQLExecuteReadsTopToBottom(t *testing.T) {
	// [spec feature.hover/A22]
	hover := GetHover(`aRows := SQLExecute(sSQL);`, 1, 12, nil, nil)
	if hover == nil {
		t.Fatal("expected SQLExecute hover")
	}
	c := hover.Contents
	order := []string{
		"```ssl\nSQLExecute(sCommandString, [sConnectionName], [bRollbackExistingTransaction], [bNullAsBlank], [aInvariantDateCols], [vReturnType], [sTableName], [bIncludeSchema], [bIncludeHeader])\n```",
		"Executes SQL and returns either query results or a success flag.",
		"**Parameters:**",
		"- `sConnectionName`: *(string, optional, default: current default connection)*",
		"- `vReturnType`: *(boolean or string, optional, default: \"array\")*",
		"**Returns:** `any` —",
		"**Documented exceptions:**",
		"sConnectionName is provided but is not a string.",
	}
	at := 0
	for _, want := range order {
		i := strings.Index(c[at:], want)
		if i < 0 {
			t.Fatalf("expected %q after position %d in hover:\n%s", want, at, c)
		}
		at += i + len(want)
	}
	for _, stale := range []string{"friendlyName", ": variant", "`variant`", "commandString:"} {
		if strings.Contains(c, stale) {
			t.Errorf("hover still carries the stale curated text %q:\n%s", stale, c)
		}
	}
}

// A function with alternative call forms shows each form instead of one
// flat parameter list, in hover and in signature help.
func TestCreateUdObjectShowsCallForms(t *testing.T) {
	// [spec feature.hover/A23] [spec feature.signature_help/A9]
	hover := getFunctionHover("CreateUdObject")
	if hover == nil {
		t.Fatal("expected CreateUdObject hover")
	}
	want := "```ssl\nCreateUdObject()\nCreateUdObject(aPropertyDefs)\nCreateUdObject(sClassName, [aArgs])\n```"
	if !strings.Contains(hover.Contents, want) {
		t.Errorf("expected the three call forms, got:\n%s", hover.Contents)
	}
	if strings.Contains(hover.Contents, "aPropertyDefs, sClassName") || strings.Contains(hover.Contents, "[aPropertyDefs], [sClassName]") {
		t.Errorf("hover must not present the forms as one positional list:\n%s", hover.Contents)
	}

	text := `o := CreateUdObject("Cat.Cls", `
	help := GetSignatureHelpWithProcedures(tokenizeForSignatureHelp(t, text), nil, 1, len(text)+1)
	if help == nil {
		t.Fatal("expected CreateUdObject signature help")
	}
	if len(help.Signatures) != 3 {
		t.Fatalf("expected one signature per call form, got %d", len(help.Signatures))
	}
	if !strings.HasPrefix(help.Signatures[help.ActiveSignature].Label, "CreateUdObject(sClassName, [aArgs])") {
		t.Errorf("second argument must select the class-name form, got %q", help.Signatures[help.ActiveSignature].Label)
	}
}

// Signature help labels use reference names with optional parameters
// bracketed.
func TestSignatureHelp_ReferenceLabel(t *testing.T) {
	// [spec feature.signature_help/A10]
	text := `aRows := SQLExecute(sSQL, `
	help := GetSignatureHelpWithProcedures(tokenizeForSignatureHelp(t, text), nil, 1, len(text)+1)
	if help == nil || len(help.Signatures) != 1 {
		t.Fatalf("expected one SQLExecute signature, got %+v", help)
	}
	s := help.Signatures[0]
	if !strings.HasPrefix(s.Label, "SQLExecute(sCommandString, [sConnectionName], ") || !strings.HasSuffix(s.Label, " → any") {
		t.Errorf("unexpected label %q", s.Label)
	}
	if s.Parameters[1].Label != "sConnectionName" || !strings.Contains(s.Parameters[1].Documentation, "default: current default connection") {
		t.Errorf("unexpected second parameter %+v", s.Parameters[1])
	}
}

// Keyword, operator, literal, and class-context hovers render the reference
// summary plus its caveats and don't-list — which is where the language
// gotchas the old hand-written one-liners carried now come from.
func TestReferenceHoversCarryGotchas(t *testing.T) {
	// [spec feature.hover/A24] [spec feature.hover/A25]
	cases := []struct {
		name  string
		hover *Hover
		want  []string
	}{
		{":EXITCASE", getKeywordHover(":EXITCASE"), []string{
			"Ends the current :CASE or :OTHERWISE branch", "```ssl\n:EXITCASE;\n```",
			"Without it, later :CASE expressions are still evaluated"}},
		{":FINALLY", getKeywordHover("FINALLY"), []string{":RETURN", ":EXITWHILE", ":EXITFOR", ":LOOP"}},
		{":PUBLIC", getKeywordHover("PUBLIC"), []string{"shared across the running program"}},
		{"=", getOperatorHover("="), []string{
			"prefix matching for strings",
			"| string | string | boolean | Returns .T. when the right string is empty",
			"Use = for exact string equality"}},
		{"!=", getOperatorHover("!="), []string{
			"not strictly equal",
			"| string | string | boolean | Returns .T. when the strings are not exactly equal. |",
			"!= negates strict equality, not ="}},
		{"==", getOperatorHover("=="), []string{"strictly equal"}},
		{"NIL", getLiteralHover("nil"), []string{"explicit absent value", "start as empty string, not NIL"}},
		{"Me", getClassContextHover("Me"), []string{"current class instance", "Me:MethodName(args)", "compile-time error"}},
		{"Base", getClassContextHover("base"), []string{"immediate parent type", "Base:MethodName(args)"}},
		{"Constructor", getClassContextHover("Constructor"), []string{"one-time class initialization", ":RETURN value;"}},
	}
	for _, c := range cases {
		if c.hover == nil {
			t.Errorf("%s: no hover", c.name)
			continue
		}
		for _, w := range c.want {
			if !strings.Contains(c.hover.Contents, w) {
				t.Errorf("%s: hover missing %q:\n%s", c.name, w, c.hover.Contents)
			}
		}
	}
	// The string gotcha must never be inverted: != is not "not =".
	if h := getOperatorHover("!="); strings.Contains(h.Contents, "under SSL's prefix matching") {
		t.Errorf("!= hover describes prefix matching:\n%s", h.Contents)
	}
}

// A documented exception whose published message carries stray backticks
// from the extraction renders as plain text, not as a broken code span.
func TestExceptionMessageWithStrayBackticks(t *testing.T) {
	if got := exceptionMessage("Plain message."); got != "`Plain message.`" {
		t.Errorf("plain message should be a code span, got %q", got)
	}
	if got := exceptionMessage("Value cannot be null., then Parameter name: x` on the second line"); strings.Contains(got, "`") {
		t.Errorf("stray backticks must be removed, got %q", got)
	}
}
