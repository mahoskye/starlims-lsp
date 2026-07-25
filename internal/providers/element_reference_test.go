package providers

import (
	"strings"
	"testing"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
)

func TestClassHoverShowsConstructorsAndMethods(t *testing.T) {
	hover := getClassHover("Email")
	if hover == nil {
		t.Fatal("expected class hover for Email, got nil")
	}
	for _, snippet := range []string{"Constructors:", "Methods:", "Email"} {
		if !strings.Contains(hover.Contents, snippet) {
			t.Errorf("expected Email hover to contain %q, got:\n%s", snippet, hover.Contents)
		}
	}
}

func TestOperatorHoverIncludesTypeBehavior(t *testing.T) {
	hover := getOperatorHover("+=")
	if hover == nil {
		t.Fatal("expected hover for +=, got nil")
	}
	if !strings.Contains(hover.Contents, "Type behavior:") {
		t.Errorf("expected += hover to include type behavior table, got:\n%s", hover.Contents)
	}
	if !strings.Contains(hover.Contents, "number") {
		t.Errorf("expected += hover to mention number rows, got:\n%s", hover.Contents)
	}
}

func TestTypeHoverNew(t *testing.T) {
	hover := getTypeHover("array")
	if hover == nil {
		t.Fatal("expected hover for array type, got nil")
	}
	if !strings.Contains(hover.Contents, "runtime type") {
		t.Errorf("expected array type hover to mention runtime type, got:\n%s", hover.Contents)
	}
	if !strings.Contains(hover.Contents, "Append") {
		t.Errorf("expected array type hover to list members like Append, got:\n%s", hover.Contents)
	}
}

func TestSpecialFormHover(t *testing.T) {
	hover := getSpecialFormHover("base")
	if hover == nil {
		t.Fatal("expected hover for special form base, got nil")
	}
	if !strings.Contains(hover.Contents, "Syntax:") {
		t.Errorf("expected base special-form hover to include syntax block, got:\n%s", hover.Contents)
	}
}

func TestClassMemberCompletions(t *testing.T) {
	items := GetClassMemberCompletions("Email")
	if len(items) == 0 {
		t.Fatal("expected completion items for Email members, got 0")
	}

	hasMethod := false
	for _, it := range items {
		if it.Kind == CompletionKindMethod {
			hasMethod = true
			if it.Detail == "" {
				t.Errorf("expected method detail for %s, got empty", it.Label)
			}
			break
		}
	}
	if !hasMethod {
		t.Error("expected at least one method completion for Email")
	}
}

func TestClassConstructorCompletionsAreSnippets(t *testing.T) {
	items := GetClassConstructorCompletions("AzureStorage")
	if len(items) == 0 {
		t.Fatal("expected constructor completions for AzureStorage, got 0")
	}
	for _, it := range items {
		if it.Kind != CompletionKindConstructor {
			t.Errorf("expected constructor kind, got %d", it.Kind)
		}
		if it.InsertTextFormat != InsertTextFormatSnippet {
			t.Errorf("expected snippet format for %s, got %d", it.Label, it.InsertTextFormat)
		}
	}
}

func TestConstructorSignatureHelp(t *testing.T) {
	// Cursor inside `Email{|` — exercised on the wired token-based path
	// (the text-based GetSignatureHelp entry point was deleted, issue #40).
	tokens := lexer.NewLexer("oEmail := Email{").Tokenize()
	help := GetSignatureHelpWithProcedures(tokens, nil, 1, 17)
	if help == nil {
		t.Fatal("expected signature help inside Email{ ..., got nil")
	}
	if len(help.Signatures) == 0 {
		t.Fatal("expected at least one constructor signature for Email")
	}
	first := help.Signatures[0]
	if !strings.Contains(first.Label, "Email{") {
		t.Errorf("expected first signature label to start with Email{, got %q", first.Label)
	}
}

func TestClassNameCollisionDiagnostic(t *testing.T) {
	source := ":CLASS Email;\n:DECLARE _sFoo;\n"
	tokens := lexer.NewLexer(source).Tokenize()
	diags := checkClassNameCollision(tokens)
	if len(diags) == 0 {
		t.Fatal("expected collision diagnostic for ':CLASS Email;', got none")
	}
	if !strings.Contains(diags[0].Message, "Email") {
		t.Errorf("expected diagnostic to mention Email, got %q", diags[0].Message)
	}

	// Non-built-in name should NOT trigger
	tokens2 := lexer.NewLexer(":CLASS InvoiceManager;\n").Tokenize()
	if diags2 := checkClassNameCollision(tokens2); len(diags2) != 0 {
		t.Errorf("expected no diagnostic for user-defined :CLASS InvoiceManager, got %d", len(diags2))
	}
}

func TestInventoryTotalsAlignWithGenerated(t *testing.T) {
	if got := constants.InventoryTotals.Functions; got != len(constants.SSLFunctionNames) {
		t.Errorf("inventory total Functions=%d but SSLFunctionNames has %d entries",
			got, len(constants.SSLFunctionNames))
	}
	if got := constants.InventoryTotals.Classes; got != len(constants.SSLClassNames) {
		t.Errorf("inventory total Classes=%d but SSLClassNames has %d entries",
			got, len(constants.SSLClassNames))
	}
	if constants.InventoryTotals.All != 460 {
		t.Errorf("expected 460 total elements, got %d", constants.InventoryTotals.All)
	}
	if got := constants.InventoryTotals.Returns; got != len(constants.GeneratedReturnsObjectNames) {
		t.Errorf("inventory total Returns=%d but GeneratedReturnsObjectNames has %d entries",
			got, len(constants.GeneratedReturnsObjectNames))
	}
}

func TestReturnsObjectsGenerated(t *testing.T) {
	// Issue #123: the returns category (12 objects) must survive the
	// vendored refresh — the generator now fails on unhandled totals keys,
	// and this pins the wired result.
	if got := len(constants.GeneratedReturnsObjectDetails); got != 12 {
		t.Errorf("expected 12 returns objects, got %d", got)
	}
	for _, name := range []string{"httpclient", "httpresponse", "soapclient", "sslrequest", "sslresponse"} {
		if _, ok := constants.GeneratedReturnsObjectDetails[name]; !ok {
			t.Errorf("expected returns object %q in GeneratedReturnsObjectDetails", name)
		}
		if !constants.IsReturnsObject(name) {
			t.Errorf("IsReturnsObject(%q) = false, want true", name)
		}
	}
	if constants.IsReturnsObject("email") {
		t.Error("IsReturnsObject should not match class names")
	}
	// Meta parity: returns objects index into LookupMeta automatically.
	for _, name := range []string{"HttpResponse", "SoapClient"} {
		if _, ok := constants.LookupMeta(name); !ok {
			t.Errorf("expected LookupMeta(%q) to find returns-object meta", name)
		}
	}
	// New special forms from the refresh.
	for _, slug := range []string{"request", "response"} {
		if _, ok := constants.GeneratedSpecialFormDetails[slug]; !ok {
			t.Errorf("expected special form %q after refresh", slug)
		}
	}
}

func TestGeneratedMethodNamesAreClean(t *testing.T) {
	// Issue #123 (R1/R2): 42 method rows used the "method" JSON key (emitted
	// as Name: "") and 35 more carried paren signatures that flowed verbatim
	// into completion InsertText. All method names must now be bare.
	check := func(owner, name string) {
		if name == "" {
			t.Errorf("%s: empty method name", owner)
		}
		if strings.Contains(name, "(") {
			t.Errorf("%s: method name %q contains a paren signature", owner, name)
		}
	}
	for class, det := range constants.GeneratedClassDetails {
		for _, m := range det.Methods {
			check("class "+class, m.Name)
		}
	}
	for obj, det := range constants.GeneratedReturnsObjectDetails {
		for _, m := range det.Methods {
			check("returns object "+obj, m.Name)
		}
	}
	// The worst-affected class: every WebServices method row used the
	// "method" key and was invisible to completion.
	ws := constants.GeneratedClassDetails["webservices"]
	if len(ws.Methods) == 0 {
		t.Fatal("expected WebServices methods")
	}
	for _, m := range ws.Methods {
		if m.Name == "" {
			t.Error("WebServices method with empty name survived normalization")
		}
	}
}
