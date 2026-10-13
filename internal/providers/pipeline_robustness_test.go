package providers

import (
	"testing"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// [spec feature.diagnostics_pipeline/A7] A check that panics mid-collection
// keeps everything collected before it, post-processed as usual, plus one
// internal_error (issue #258: the recovery used to discard the lot). The
// panic is real: the block-depth check dereferences a nil AST, which only
// a direct collectDiagnostics call can supply — the public entry points
// always parse one.
func TestCollectDiagnostics_PanicKeepsCollectedDiagnostics(t *testing.T) {
	text := "nValue := `x`;\n:IF .T.;\n"
	tokens := lexer.NewLexer(text).Tokenize()
	p := parser.NewParser(tokens)

	var got []Diagnostic
	func() {
		defer func() {
			if r := recover(); r != nil {
				t.Fatalf("panic propagated past collectDiagnostics: %v", r)
			}
		}()
		got = collectDiagnostics(tokens, nil, p, DefaultDiagnosticOptions())
	}()

	if n := countCode(got, CodeInternalError); n != 1 {
		t.Fatalf("want exactly one internal_error, got %d: %+v", n, got)
	}
	for _, d := range got {
		if d.Code == CodeInternalError && d.Severity != SeverityError {
			t.Errorf("internal_error severity = %d, want error", d.Severity)
		}
	}
	// Collected before the panicking check (block depth runs after these).
	for _, code := range []string{CodeUnknownToken, CodeUnclosedBlock} {
		if countCode(got, code) == 0 {
			t.Errorf("diagnostic %s collected before the panic was discarded: %+v", code, got)
		}
	}

	// Post-processing still applies to what was kept.
	opts := DefaultDiagnosticOptions()
	opts.RuleOverrides = map[string]string{"unclosed_block": "off"}
	got = collectDiagnostics(tokens, nil, p, opts)
	if countCode(got, CodeUnclosedBlock) != 0 {
		t.Errorf("rule override not applied on the recovery path: %+v", got)
	}
	if countCode(got, CodeInternalError) != 1 {
		t.Errorf("internal_error missing after override: %+v", got)
	}
}

// [spec feature.diagnostics_pipeline/A2] Override keys match the rule slug
// case-insensitively, like suppression slugs, and "hint" remaps to hint.
func TestRuleOverrides_CaseInsensitiveKeysAndHint(t *testing.T) {
	text := "nValue := `x`;\n"

	opts := DefaultDiagnosticOptions()
	opts.RuleOverrides = map[string]string{" Unknown_Token ": "OFF"}
	if n := countCode(GetDiagnostics(text, opts), CodeUnknownToken); n != 0 {
		t.Errorf("mixed-case override key did not apply: %d unknown_token left", n)
	}

	opts.RuleOverrides = map[string]string{"unknown_token": "hint"}
	got := GetDiagnostics(text, opts)
	if countCode(got, CodeUnknownToken) == 0 {
		t.Fatalf("hint override dropped the diagnostic: %+v", got)
	}
	for _, d := range got {
		if d.Code == CodeUnknownToken && d.Severity != SeverityHint {
			t.Errorf("severity = %d, want hint", d.Severity)
		}
	}
}

// [spec feature.diagnostics_pipeline/A3] An unrecognized override value is
// ignored as if absent: the diagnostic passes through unchanged, and an
// info-tier rule is not exempted from the info gate by it.
func TestRuleOverrides_UnknownValueIsIgnored(t *testing.T) {
	text := "nValue := `x`;\n"
	opts := DefaultDiagnosticOptions()
	opts.RuleOverrides = map[string]string{"unknown_token": "bogus"}
	got := GetDiagnostics(text, opts)
	if countCode(got, CodeUnknownToken) == 0 {
		t.Fatalf("unknown value dropped the diagnostic")
	}
	for _, d := range got {
		if d.Code == CodeUnknownToken && d.Severity != SeverityError {
			t.Errorf("unknown value changed severity to %d", d.Severity)
		}
	}

	// equals_vs_strict_equals is info tier, dropped by default.
	info := ":DECLARE sA;\nsA := \"x\";\n:IF sA = \"x\";\n\tsA := \"y\";\n:ENDIF;\n"
	opts = DefaultDiagnosticOptions()
	opts.RuleOverrides = map[string]string{"equals_vs_strict_equals": "loud"}
	if n := countCode(GetDiagnostics(info, opts), CodeEqualsVsStrictEquals); n != 0 {
		t.Errorf("unrecognized value bypassed the info gate: %d diagnostics", n)
	}
	opts.RuleOverrides = map[string]string{"EQUALS_VS_STRICT_EQUALS": "info"}
	if n := countCode(GetDiagnostics(info, opts), CodeEqualsVsStrictEquals); n == 0 {
		t.Errorf("recognized override (mixed-case key) should bypass the info gate")
	}
}
