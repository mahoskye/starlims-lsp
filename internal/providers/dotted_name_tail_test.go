package providers

import "testing"

// The lexer splits a dotted name at a digit or an underscore, so the last
// piece of `Rules.Level2Base` is an identifier token `Base`. It is the rest
// of the name, not the `Base` or `Me` keyword.
func TestMeAndBase_TailOfADottedName(t *testing.T) {
	clean := []struct{ name, script string }{
		{"parent class ending in Base after a digit", ":CLASS A;\n:INHERIT Rules.Level2Base;\n"},
		{"parent class ending in Base after an underscore", ":CLASS A;\n:INHERIT Rules.Rule_Base;\n"},
		{"included script ending in Me after a digit", ":INCLUDE Lib.Round2Me;\n"},
		{"included script ending in Me after an underscore", ":INCLUDE Lib.Call_Me;\n"},
	}
	for _, tc := range clean {
		t.Run(tc.name, func(t *testing.T) {
			got := codeCounts(GetDiagnostics(tc.script, DefaultDiagnosticOptions()))
			if got[CodeBaseStandalone] != 0 || got[CodeMeOutsideClass] != 0 {
				t.Errorf("a name's tail is not Me or Base: %v", got)
			}
		})
	}

	// The keywords themselves are still judged.
	if got := codeCounts(GetDiagnostics(":CLASS A;\n:INHERIT Rules.B;\n:PROCEDURE Run;\n\t:RETURN Base;\n:ENDPROC;\n", DefaultDiagnosticOptions())); got[CodeBaseStandalone] != 1 {
		t.Errorf("a bare Base is still base_standalone: %v", got)
	}
	if got := codeCounts(GetDiagnostics(":DECLARE vX;\nvX := Me:value;\n", DefaultDiagnosticOptions())); got[CodeMeOutsideClass] != 1 {
		t.Errorf("Me outside a class is still me_outside_class: %v", got)
	}
}
