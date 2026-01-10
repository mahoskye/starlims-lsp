package constants

import (
	"strings"
	"testing"
)

func TestKeywordAndOperatorSets(t *testing.T) {
	if len(SSLKeywords) != 37 {
		t.Fatalf("expected 37 keywords, got %d", len(SSLKeywords))
	}
	if !IsKeyword("PROCEDURE") {
		t.Error("expected PROCEDURE to be a keyword")
	}
	if IsKeyword("NOT_A_KEYWORD") {
		t.Error("expected NOT_A_KEYWORD to be false")
	}

	if len(SSLOperators) != 26 {
		t.Fatalf("expected 26 operators, got %d", len(SSLOperators))
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
}
