package providers

import (
	"strings"
	"testing"
)

// diag.visibility_annotation_usage leaves the `/*@private;` of a raise-only
// helper alone: the convention requires that one (issue #371).
func TestVisibilityAnnotationUsage_RaiseOnlyHelper(t *testing.T) {
	proc := func(annotation string, body ...string) string {
		return annotation + "\n:PROCEDURE Helper;\n\t:PARAMETERS sId;\n\t" +
			strings.Join(body, "\n\t") + "\n:ENDPROC;\n"
	}
	tests := []struct {
		name string
		src  string
		want int // usage notes
	}{
		{"private helper that raises",
			proc("/*@private;", ":IF Empty(sId);", "\tRaiseError(\"An id is required\");", ":ENDIF;"), 0},
		{"private helper that raises and returns a value",
			proc("/*@private;", ":IF Empty(sId);", "\tRaiseError(\"An id is required\");", ":ENDIF;",
				":RETURN Upper(sId);"), 0},
		{"raise in a :TRY with only :FINALLY leaves the procedure",
			proc("/*@private;", ":TRY;", "\tRaiseError(\"x\");", ":FINALLY;", "\tsId := \"\";", ":ENDTRY;"), 0},
		{"raise in the :CATCH leaves the procedure",
			proc("/*@private;", ":TRY;", "\tsId := Upper(sId);", ":CATCH;", "\tRaiseError(\"x\");", ":ENDTRY;"), 0},
		{"raise after its own :TRY is closed",
			proc("/*@private;", ":TRY;", "\tsId := Upper(sId);", ":CATCH;", "\tsId := \"\";", ":ENDTRY;",
				"RaiseError(\"x\");"), 0},
		{"lowercase call is the same built-in",
			proc("/*@private;", "raiseerror(\"x\");"), 0},

		{"private helper that does not raise",
			proc("/*@private;", ":RETURN Upper(sId);"), 1},
		{"private helper that catches its own raise",
			proc("/*@private;", ":TRY;", "\tRaiseError(\"x\");", ":CATCH;", "\t:RETURN .F.;", ":ENDTRY;",
				":RETURN .T.;"), 1},
		{"raise in an inner :TRY that an outer :CATCH encloses",
			proc("/*@private;", ":TRY;", "\t:TRY;", "\t\tRaiseError(\"x\");", "\t:FINALLY;", "\t\tsId := \"\";",
				"\t:ENDTRY;", ":CATCH;", "\t:RETURN .F.;", ":ENDTRY;", ":RETURN .T.;"), 1},
		{"protected is not the required annotation",
			proc("/*@protected;", "RaiseError(\"x\");"), 1},
		{"a member named RaiseError is not the built-in",
			proc("/*@private;", "oLog:RaiseError(\"x\");"), 1},
		{"a raise in another procedure does not count",
			proc("/*@private;", ":RETURN Upper(sId);") +
				":PROCEDURE Other;\n\tRaiseError(\"x\");\n:ENDPROC;\n", 1},
		{"only the helper that raises is exempt",
			proc("/*@private;", "RaiseError(\"x\");") +
				"/*@private;\n:PROCEDURE Quiet;\n\t:RETURN 1;\n:ENDPROC;\n", 1},
	}
	opts := infoEnabledOptions()
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := codeCounts(GetDiagnostics(tc.src, opts))
			if got[CodeVisibilityAnnotationUsage] != tc.want {
				t.Errorf("want %d usage note(s), got %d\n%s", tc.want, got[CodeVisibilityAnnotationUsage], tc.src)
			}
			if got[CodeVisibilityAnnotation] != 0 {
				t.Errorf("an effective annotation never gets the inert-annotation warning: %v", got)
			}
		})
	}
}

// The note names the exception, so a reader of a remaining finding knows
// when the annotation would be right.
func TestVisibilityAnnotationUsage_MessageNamesTheException(t *testing.T) {
	src := "/*@private;\n:PROCEDURE Helper;\n\t:RETURN 1;\n:ENDPROC;\n"
	for _, d := range GetDiagnostics(src, infoEnabledOptions()) {
		if d.Code != CodeVisibilityAnnotationUsage {
			continue
		}
		if !strings.Contains(d.Message, "raise-only helper") {
			t.Errorf("message does not name the exception: %s", d.Message)
		}
		return
	}
	t.Fatal("expected a visibility_annotation_usage note")
}
