package providers

import (
	"strings"
	"testing"
)

// productionScript is a server script in the shape the style baseline
// asks for: banner, procedure doc blocks stating the return contract,
// an explicit empty-string contract, parameterized SQL, TRY/CATCH with
// read-before-clear, and DoProc for a same-file procedure.
//
// Most of the corpus evidence behind the strict profile came from data
// sources (1,610 of them) and from archive fixtures that are
// deliberately adversarial. Production-shaped *scripts* were the thin
// spot, so this pins the shape an agent is asked to produce: it must
// validate clean under the full strict profile and be formatter-stable,
// and each defect the profile exists to catch must actually be caught
// when seeded into it (issue #249).
const productionScript = `/* =============================================================================;
/* SCRIPT:  SampleLookup;
/* PURPOSE: Resolves a sample id to its display name;
/* =============================================================================;

:PROCEDURE GetSampleName;
	/* Returns the sample's name for nSampleId.;
	/* On success: the trimmed name. On no match or failure: an empty string,;
	/* never NIL, so callers can concatenate without guarding;
	:PARAMETERS nSampleId;
	:DEFAULT nSampleId, 0;
	:DECLARE sName, sSql, oRow;

	sName := "";

	:IF Empty(nSampleId);
		:RETURN sName;
	:ENDIF;

	:TRY;
		sSql := "SELECT sample_name FROM sample WHERE sample_id = ?nSampleId?";
		oRow := SQLExecute(sSql);

		:IF .NOT. Empty(oRow);
			sName := AllTrim(oRow);
		:ENDIF;
	:CATCH;
		/* Read the error before anything can clear it, then fall through to;
		/* the empty-string contract above;
		ErrorMes("SampleLookup", DoProc("GetSampleNameError"));
	:ENDTRY;

	:RETURN sName;
:ENDPROC;

:PROCEDURE GetSampleNameError;
	/* Formats the current error for the log;
	:DECLARE oErr;

	oErr := GetLastSSLError();

	:RETURN "GetSampleName failed: " + LimsString(oErr);
:ENDPROC;
`

func strictScriptOptions() DiagnosticOptions {
	opts := DefaultDiagnosticOptions()
	opts.IncludeInfoDiagnostics = true
	opts.CheckHungarianTypes = true
	opts.CheckUndeclaredVars = true
	opts.CheckUnusedVars = true
	opts.CheckSQLParams = true
	return opts
}

func TestProductionScript_CleanUnderStrictProfile(t *testing.T) {
	for _, d := range GetDiagnostics(productionScript, strictScriptOptions()) {
		t.Errorf("%s at line %d: %s", d.Code, d.Range.Start.Line+1, d.Message)
	}
}

func TestProductionScript_FormatterStable(t *testing.T) {
	opts := DefaultFormattingOptions()
	fmtOnce := func(text string) string {
		edits := FormatDocument(text, opts)
		if len(edits) == 0 {
			return text
		}
		return edits[0].NewText
	}
	once := fmtOnce(productionScript)
	if once != productionScript {
		t.Errorf("the fixture is not in formatted form; regenerate it:\n%s", once)
	}
	if twice := fmtOnce(once); twice != once {
		t.Error("formatting a production-shaped script is not idempotent")
	}
}

// Each seeded defect must be caught. A profile that is quiet on the
// clean script proves nothing unless it also speaks up on a broken one.
func TestProductionScript_SeededDefectsAreCaught(t *testing.T) {
	cases := []struct {
		name string
		from string
		to   string
		want string
	}{
		{
			name: "comment closed C-style swallows the next statement",
			from: "/* Formats the current error for the log;",
			to:   "/* Formats the current error for the log\n*/",
			want: CodeCommentSwallowsCode,
		},
		{
			name: "typo'd variable read",
			from: "sName := AllTrim(oRow);",
			to:   "sName := AllTrim(oRowTypo);",
			want: CodeUndeclaredVariable,
		},
		{
			name: "SQL placeholder naming nothing in scope",
			from: "WHERE sample_id = ?nSampleId?",
			to:   "WHERE sample_id = ?nMissingParam?",
			want: CodeInvalidSqlParam,
		},
		{
			name: "declaration nothing reads",
			from: ":DECLARE sName, sSql, oRow;",
			to:   ":DECLARE sName, sSql, oRow, sNeverUsed;",
			want: CodeUnusedVariable,
		},
		{
			name: "direct call instead of DoProc",
			from: `DoProc("GetSampleNameError")`,
			to:   "GetSampleNameError()",
			want: CodeDirectProcedureCall,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if !strings.Contains(productionScript, tc.from) {
				t.Fatalf("fixture no longer contains %q — update the seed", tc.from)
			}
			broken := strings.Replace(productionScript, tc.from, tc.to, 1)
			for _, d := range GetDiagnostics(broken, strictScriptOptions()) {
				if d.Code == tc.want {
					return
				}
			}
			t.Errorf("seeded defect went unreported; expected %s", tc.want)
		})
	}
}

// A negative: a leading negative in an array literal is ordinary SSL and
// must not draw an arithmetic finding inside a real script.
func TestProductionScript_UnaryNegativeIsNotArithmetic(t *testing.T) {
	withArray := strings.Replace(productionScript,
		"sName := \"\";",
		"sName := \"\";\n\taFlags := {-1, 0};",
		1)
	withArray = strings.Replace(withArray,
		":DECLARE sName, sSql, oRow;",
		":DECLARE sName, sSql, oRow, aFlags;",
		1)
	for _, d := range GetDiagnostics(withArray, strictScriptOptions()) {
		if d.Code == CodeArithmeticTypeMismatch {
			t.Errorf("unary negative in an array literal reported as arithmetic: %s", d.Message)
		}
	}
}
