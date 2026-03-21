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

func TestSourceAlignedInventories(t *testing.T) {
	if len(SSLFunctionNames) != 354 {
		t.Fatalf("expected 354 source-aligned functions, got %d", len(SSLFunctionNames))
	}
	if len(SSLClassNames) != 21 {
		t.Fatalf("expected 21 source-aligned classes, got %d", len(SSLClassNames))
	}

	for _, name := range []string{"Branch", "Eval"} {
		switch name {
		default:
			if !IsSSLFunction(strings.ToLower(name)) {
				t.Fatalf("expected %s to be recognized case-insensitively as a built-in function", name)
			}
		}
	}

	for _, removed := range []string{"Break", "TryConnect", "EnterpriseImpExBase", "InList", "SSLError", "SSLSQLError", "CDataTable", "SQLConnection"} {
		if IsSSLFunction(removed) || IsSSLClass(removed) {
			t.Fatalf("expected %s to be excluded from the source-aligned public inventory", removed)
		}
	}

	if sig, ok := GetFunctionSignature("branch"); !ok || sig.Name != "Branch" {
		t.Fatalf("expected Branch to have a source-aligned signature, got ok=%v sig=%+v", ok, sig)
	}
}
