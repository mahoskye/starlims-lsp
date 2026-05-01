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
	// Cursor inside `Email{|`
	help := GetSignatureHelp("oEmail := Email{", 1, 17)
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
	if constants.InventoryTotals.All != 446 {
		t.Errorf("expected 446 total elements, got %d", constants.InventoryTotals.All)
	}
}
