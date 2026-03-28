package constants

import (
	"strings"
	"testing"
)

func TestKeywordAndOperatorSets(t *testing.T) {
	if len(SSLKeywords) != 38 {
		t.Fatalf("expected 38 keywords, got %d", len(SSLKeywords))
	}
	if !IsKeyword("PROCEDURE") {
		t.Error("expected PROCEDURE to be a keyword")
	}
	if !IsKeyword("RESUME") {
		t.Error("expected RESUME to be a keyword")
	}
	if IsKeyword("NOT_A_KEYWORD") {
		t.Error("expected NOT_A_KEYWORD to be false")
	}

	if len(SSLOperators) != 32 {
		t.Fatalf("expected 32 operators, got %d", len(SSLOperators))
	}
	for _, op := range SSLLogicalOperators {
		if !IsSSLOperator(op) {
			t.Errorf("expected logical operator %q to be in operator list", op)
		}
		if !IsSSLLogicalOperator(op) {
			t.Errorf("expected %q to be a logical operator", op)
		}
	}

	for _, op := range SSLCompoundOperators {
		if !IsSSLOperator(op) {
			t.Errorf("expected compound operator %q to be in operator list", op)
		}
		if !IsSSLCompoundOperator(op) {
			t.Errorf("expected %q to be a compound operator", op)
		}
	}
}

func TestLiteralsAndAliases(t *testing.T) {
	for _, literal := range SSLLiterals {
		if !IsSSLLiteral(literal) {
			t.Errorf("expected literal %q to be recognized", literal)
		}
	}

	for alias, canonical := range SSLLiteralAliases {
		if !IsSSLLiteral(canonical) {
			t.Errorf("expected canonical literal %q for alias %q", canonical, alias)
		}
	}

	if IsSSLLiteral("true") || IsSSLLiteral("false") {
		t.Fatal("expected true/false to not be treated as SSL literals")
	}
}

func TestFunctionSignatureMappings(t *testing.T) {
	knownFunctions := make(map[string]struct{}, len(SSLFunctionNames))
	for _, name := range SSLFunctionNames {
		knownFunctions[strings.ToLower(name)] = struct{}{}
	}

	if _, ok := SSLFunctionSignatures["abs"]; !ok {
		t.Fatal("expected abs signature to exist")
	}

	for key, sig := range SSLFunctionSignatures {
		if strings.ToLower(sig.Name) != key {
			t.Errorf("signature name %q does not match key %q", sig.Name, key)
		}
		if _, ok := knownFunctions[key]; !ok {
			t.Errorf("signature %q missing from SSLFunctionNames", key)
		}
	}

	if !IsSSLFunction("Str") {
		t.Fatal("expected Str to remain in the built-in function list")
	}
	if _, ok := GetFunctionSignature("Str"); !ok {
		t.Fatal("expected Str to remain in the signature table")
	}
	if !IsSSLFunction("LimsString") {
		t.Fatal("expected LimsString to remain in the built-in function list")
	}
}

func TestCanonicalInventories(t *testing.T) {
	if len(SSLFunctionNames) != 354 {
		t.Fatalf("expected 354 canonical functions, got %d", len(SSLFunctionNames))
	}
	if len(SSLClassNames) != 22 {
		t.Fatalf("expected 22 canonical classes, got %d", len(SSLClassNames))
	}

	for _, name := range []string{"Branch", "Eval"} {
		switch name {
		default:
			if !IsSSLFunction(strings.ToLower(name)) {
				t.Fatalf("expected %s to be recognized case-insensitively as a built-in function", name)
			}
		}
	}

	for _, removed := range []string{"Break", "TryConnect", "EnterpriseImpExBase", "InList", "SSLError", "SSLSQLError", "CDataColumn", "SQLConnection"} {
		if IsSSLFunction(removed) || IsSSLClass(removed) {
			t.Fatalf("expected %s to be excluded from the canonical public inventory", removed)
		}
	}

	if sig, ok := GetFunctionSignature("branch"); !ok || sig.Name != "Branch" {
		t.Fatalf("expected Branch to have a canonical signature, got ok=%v sig=%+v", ok, sig)
	}
}

func TestClassContextForms(t *testing.T) {
	trueCases := []string{"Me", "me", "BASE", "constructor"}
	for _, form := range trueCases {
		if !IsSSLClassContextForm(form) {
			t.Errorf("expected IsSSLClassContextForm(%q) to be true", form)
		}
	}

	if IsSSLClassContextForm("NotAForm") {
		t.Error("expected IsSSLClassContextForm(\"NotAForm\") to be false")
	}
}

func TestKeywordAndOperatorDescriptions(t *testing.T) {
	// Source-of-truth alignment: keyword descriptions must reflect authoritative language rules

	// EXITCASE must mention fall-through / multi-match behavior
	desc := SSLKeywordDescriptions["EXITCASE"]
	if !strings.Contains(desc, "without it") || !strings.Contains(desc, "evaluated") {
		t.Errorf("EXITCASE description must explain that without it, later :CASE blocks are still evaluated; got: %q", desc)
	}

	// FINALLY must list all four restricted keywords
	desc = SSLKeywordDescriptions["FINALLY"]
	for _, kw := range []string{":RETURN", ":EXITFOR", ":EXITWHILE", ":LOOP"} {
		if !strings.Contains(desc, kw) {
			t.Errorf("FINALLY description must mention %s restriction; got: %q", kw, desc)
		}
	}

	// = operator must mention prefix/loose matching
	desc = SSLOperatorDescriptions["="]
	if !strings.Contains(desc, "prefix") || !strings.Contains(desc, "loose") {
		t.Errorf("= description must mention loose prefix matching; got: %q", desc)
	}

	// != operator must mention it negates == not =
	desc = SSLOperatorDescriptions["!="]
	if !strings.Contains(desc, "negates ==") {
		t.Errorf("!= description must mention it negates == (not =); got: %q", desc)
	}

	// $ operator must explain containment direction
	desc = SSLOperatorDescriptions["$"]
	if !strings.Contains(desc, "left") && !strings.Contains(desc, "right") {
		t.Errorf("$ description should clarify containment direction; got: %q", desc)
	}
}

func TestCompoundOperatorsExcludesAssignment(t *testing.T) {
	for _, op := range SSLCompoundOperators {
		if op == ":=" {
			t.Fatal(":= should NOT be in SSLCompoundOperators (it is simple assignment, not compound)")
		}
	}

	if IsSSLCompoundOperator(":=") {
		t.Fatal("IsSSLCompoundOperator should return false for ':='")
	}
}
