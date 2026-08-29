package providers

import (
	"strings"
	"testing"
)

// codesAt returns the 0-indexed lines carrying the given diagnostic code.
func codesAt(t *testing.T, code, script string) []int {
	t.Helper()
	var lines []int
	for _, d := range GetDiagnostics(script, DefaultDiagnosticOptions()) {
		if d.Code == code {
			lines = append(lines, d.Range.Start.Line)
		}
	}
	return lines
}

func TestFormatArgNotArrayTypedArguments(t *testing.T) {
	// Multi-token second arguments used to be unprovable and stayed
	// silent; with expression typing (issue #184) each of these infers to
	// a definite non-array type and flags.
	script := `:PROCEDURE Greet;
:PARAMETERS sA, sB, nCount;
:DECLARE sFmt, sMsg;
sMsg := sFmt:Format("{0}", sA + sB);
sMsg := sFmt:Format("{0}", AllTrim(sA));
sMsg := sFmt:Format("{0}", Len(sA));
sMsg := sFmt:Format("{0}", nCount > 3);
sMsg := sFmt:Format("{0}", Today());
:ENDPROC;`

	got := codesAt(t, CodeFormatArgNotArray, script)
	want := []int{3, 4, 5, 6, 7}
	if len(got) != len(want) {
		t.Fatalf("expected %v, got %v", want, got)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("expected %v, got %v", want, got)
		}
	}
}

func TestFormatArgNotArrayTypedReceivers(t *testing.T) {
	// The receiver is typed rather than name-matched: a string-returning
	// call and a Hungarian-string member both qualify, while an array
	// element and an object receiver make no claim.
	script := `:PROCEDURE Greet;
:PARAMETERS sA;
:DECLARE sMsg, aTemplates, oCfg;
sMsg := AllTrim(sA):Format("{0}", sA);
sMsg := Me:sTemplate:Format("{0}", sA);
sMsg := aTemplates[1]:Format("{0}", sA);
sMsg := oCfg:Format("{0}", sA);
:ENDPROC;`

	got := codesAt(t, CodeFormatArgNotArray, script)
	if len(got) != 2 || got[0] != 3 || got[1] != 4 {
		t.Fatalf("expected flags on lines 3 and 4 only, got %v", got)
	}
}

func TestFormatArgNotArrayUnknownStaysSilent(t *testing.T) {
	// The #184 constraint: an expression whose type cannot be resolved
	// degrades to no claim rather than to a guess. `x`/`v` names, user
	// procedures, array elements, and "any"-returning builtins all stay
	// silent.
	script := `:PROCEDURE Greet;
:PARAMETERS xThing, vThing;
:DECLARE sFmt, sMsg, aList;
sMsg := sFmt:Format("{0}", xThing);
sMsg := sFmt:Format("{0}", vThing);
sMsg := sFmt:Format("{0}", MyHelper(1));
sMsg := sFmt:Format("{0}", aList[1]);
sMsg := sFmt:Format("{0}", Eval(fnBuild));
:ENDPROC;`

	if got := codesAt(t, CodeFormatArgNotArray, script); len(got) != 0 {
		t.Fatalf("expected no diagnostics, got lines %v", got)
	}
}

func TestFormatArgNotArrayComposedReceiverMessage(t *testing.T) {
	// A receiver with no name of its own drops the prefix instead of
	// naming a token that is not the receiver.
	script := `:PROCEDURE Greet;
:PARAMETERS sA;
sMsg := AllTrim(sA):Format("{0}", sA);
:ENDPROC;`

	var msgs []string
	for _, d := range GetDiagnostics(script, DefaultDiagnosticOptions()) {
		if d.Code == CodeFormatArgNotArray {
			msgs = append(msgs, d.Message)
		}
	}
	if len(msgs) != 1 {
		t.Fatalf("expected one diagnostic, got %d", len(msgs))
	}
	if !strings.HasPrefix(msgs[0], "Format takes ONE array") {
		t.Errorf("expected an unprefixed message for a composed receiver, got %q", msgs[0])
	}
}

func TestBuiltinExcessArgumentsSkipCommaSurplus(t *testing.T) {
	// Regression: a surplus run ending in skipped slots used to index
	// past the argument list and panic (recovered as an internal_error
	// diagnostic). Argument subtrees carry their own ranges, so the
	// surplus span is always well-formed.
	script := `:PROCEDURE Main;
:DECLARE sPrefix, sText, nA, nB;
sPrefix := Left(sText, nA, nB,,);
:ENDPROC;`

	var internal int
	got := []int{}
	for _, d := range GetDiagnostics(script, DefaultDiagnosticOptions()) {
		switch d.Code {
		case CodeBuiltinExcessArguments:
			got = append(got, d.Range.Start.Line)
		case "internal_error":
			internal++
		}
	}
	if internal != 0 {
		t.Fatalf("check panicked: %d internal_error diagnostics", internal)
	}
	if len(got) != 1 || got[0] != 2 {
		t.Fatalf("expected one builtin_excess_arguments on line 2, got %v", got)
	}
}

func TestBuiltinExcessArgumentsNestedAndQualified(t *testing.T) {
	// Calls nested inside other calls are seen; a member call is the
	// receiver's method, never the builtin, however deep the receiver is.
	script := `:PROCEDURE Main;
:DECLARE sOut, sText, oDoc, aDocs;
sOut := AllTrim(Left(sText, 10, 99));
sOut := aDocs[1]:Left("a", "b", "c");
sOut := GetDoc():Left("a", "b", "c");
:ENDPROC;`

	got := codesAt(t, CodeBuiltinExcessArguments, script)
	if len(got) != 1 || got[0] != 2 {
		t.Fatalf("expected one diagnostic on line 2, got %v", got)
	}
}
