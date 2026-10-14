package providers

import (
	"strings"
	"testing"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// commentTerminationDiags runs the comment checks alone over text.
func commentTerminationDiags(text string) []Diagnostic {
	tokens := lexer.NewLexer(text).Tokenize()
	diags, _ := checkCommentTermination(tokens, parser.ExtractStatementExpressions(tokens))
	return diags
}

// Issue #6: a comment-only line following another comment must not be flagged
// as "comment_text_after_terminator". The multi-line heuristic was over-eager
// across paragraph breaks.
func TestCheckCommentTermination_Issue6_CommentChainSuppressed(t *testing.T) {
	// Multi-line comment ends early due to internal ;, but is followed by a
	// blank line and a standalone comment before any real code. The "broken-
	// out keyword" pattern that follows must not be reported because the
	// paragraph break signals the user ended the comment intentionally.
	text := `:PROCEDURE Demo;
:PARAMETERS sName;

/* Heading text;
   With more body
   that explains things */;

/* SQL Execution wrapper;

Parameters sName;

:ENDPROC;`

	diags := commentTerminationDiags(text)

	// The paragraph-break guard protects the SECOND comment (line 8,
	// 0-based 7): its broken-out "Parameters" keyword sits across a blank
	// line and must not be reported. The FIRST comment (0-based line 3)
	// is reported: its "With more body" lines are stranded as code with no
	// paragraph break in between, and they do not parse.
	for _, d := range diags {
		if d.Code == CodeCommentTextAfterTerminator && d.Range.Start.Line == 7 {
			t.Errorf("expected no comment_text_after_terminator across paragraph break, got: line=%d msg=%s",
				d.Range.Start.Line, d.Message)
		}
	}
}

// Sanity: the original positive case must still fire — a multi-line comment
// terminated mid-stream by a stray ; with broken-out bare keywords on the very
// next line (no paragraph break) is still a real bug.
func TestCheckCommentTermination_PositiveStillFires(t *testing.T) {
	text := `:PROCEDURE Demo;
/* multiline header notes; that closes early
Parameters sName;
   something */
:ENDPROC;`

	diags := commentTerminationDiags(text)

	found := false
	for _, d := range diags {
		if d.Code == CodeCommentTextAfterTerminator && strings.Contains(d.Message, "ends at this ';'") &&
			d.Severity == SeverityError {
			found = true
		}
	}
	if !found {
		t.Errorf("expected comment_text_after_terminator to fire on stray-semicolon multi-line comment, got %d diags", len(diags))
	}
}

// Issue #25: a multi-line comment whose interior line ends in ; strands the
// remaining prose lines as code. When the prose contains no bare keyword the
// bare-keyword signal is blind, but the next significant line starting with
// two consecutive bare identifiers never parses, so it must fire — as an
// error since issue #343.
func TestCheckCommentTermination_Issue25_OrphanedProseFires(t *testing.T) {
	text := `:PROCEDURE Demo;
/* This header explains the module
and this line accidentally ends with one;
so these words are now parsed as code
;
:ENDPROC;`

	diags := commentTerminationDiags(text)

	found := false
	for _, d := range diags {
		if d.Code == CodeCommentTextAfterTerminator {
			found = true
			if d.Severity != SeverityError {
				t.Errorf("stranded prose must be an error (issue #343), got severity %v", d.Severity)
			}
		}
	}
	if !found {
		t.Errorf("expected comment_text_after_terminator to fire on orphaned prose after mid-comment semicolon, got %d diags", len(diags))
	}
}

// Issue #25 boundary: a single bare identifier followed by code on the next
// line is not blamed on the comment. The statement fails only on its second
// line, which reads as a missing ';' (unexpected_token's finding).
func TestCheckCommentTermination_Issue25_SingleIdentifierDoesNotFire(t *testing.T) {
	text := `:PROCEDURE Demo;
/* a multi-line note
that spans two lines;
orphan
nCount := 1;
:ENDPROC;`

	diags := commentTerminationDiags(text)

	for _, d := range diags {
		if d.Code == CodeCommentTextAfterTerminator {
			t.Errorf("single bare identifier followed by valid code must not flag, got: line=%d msg=%s",
				d.Range.Start.Line, d.Message)
		}
	}
}

// Issue #25 boundary: prose after a paragraph break (blank line) must not
// fire the orphaned-prose signal — the issue #6 suppression applies to it
// exactly as to the bare-keyword signal.
func TestCheckCommentTermination_Issue25_ParagraphBreakSuppressesProse(t *testing.T) {
	text := `:PROCEDURE Demo;
/* a multi-line note
that spans two lines;

these words follow a paragraph break
;
:ENDPROC;`

	diags := commentTerminationDiags(text)

	for _, d := range diags {
		if d.Code == CodeCommentTextAfterTerminator {
			t.Errorf("prose after a paragraph break must not flag, got: line=%d msg=%s",
				d.Range.Start.Line, d.Message)
		}
	}
}

// Issue #25 boundary: legitimate code after a multi-line comment (assignment,
// call) must not fire — an operator or parenthesis after the first identifier
// means a valid statement, not prose.
func TestCheckCommentTermination_Issue25_ValidCodeDoesNotFire(t *testing.T) {
	text := `:PROCEDURE Demo;
/* a multi-line note
that spans two lines;
nCount := 1;
DoProc("X");
:ENDPROC;`

	diags := commentTerminationDiags(text)

	for _, d := range diags {
		if d.Code == CodeCommentTextAfterTerminator {
			t.Errorf("valid code after a terminated multi-line comment must not flag, got: line=%d msg=%s",
				d.Range.Start.Line, d.Message)
		}
	}
}

// Issue #25's original report: the comment's ';' lands on its FIRST line,
// so the comment token is single-line and the continuation prose on the
// next line is stranded as code. The orphaned-prose signal must fire.
func TestCheckCommentTermination_Issue25_SingleLineCommentStrandsProse(t *testing.T) {
	text := `/* Client address: treat as a single composite object;
   if any component changes, the whole address is flagged;`

	diags := commentTerminationDiags(text)

	found := false
	for _, d := range diags {
		if d.Code == CodeCommentTextAfterTerminator {
			found = true
			if d.Severity != SeverityError {
				t.Errorf("expected error severity for stranded prose (issue #343), got %v", d.Severity)
			}
		}
	}
	if !found {
		t.Errorf("expected orphaned-prose warning for single-line comment shape, got: %+v", diags)
	}
}

// [spec diag.comment_text_after_terminator] Issue #343: one error at the
// comment's ';'; valid code after the ';' on the same line is not
// reported; a stranded doc line that parses is; and the parse-level rules
// defer inside the stranded statement.
func TestCommentTextAfterTerminator_Issue343(t *testing.T) {
	// Valid same-line code is not a finding any more.
	if got := commentTerminationDiags(":DECLARE nX;\n/* note; nX := 1;\n"); len(got) != 0 {
		t.Errorf("valid code after ';': want none, got %+v", got)
	}

	// Stranded prose: one error, ranged on the ';', and no duplicates.
	text := ":DECLARE nCount;\n/* check the totals; then the rows\n;\nnCount := 0;\n"
	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	diags := GetDiagnostics(text, opts)
	got := diagsWithCode(diags, CodeCommentTextAfterTerminator)
	if len(got) != 1 {
		t.Fatalf("want 1 finding, got %+v", diags)
	}
	if r := got[0].Range; r.Start.Line != 1 || r.Start.Character != 19 || r.End.Character != 20 || got[0].Severity != SeverityError {
		t.Errorf("got %+v, want an error on the ';' at 1:19", got[0])
	}
	for _, code := range []string{CodeUnexpectedToken, CodeUndeclaredVariable} {
		if n := len(diagsWithCode(diags, code)); n != 0 {
			t.Errorf("%s should defer inside stranded text, got %d", code, n)
		}
	}

	// A single-line comment followed by a doc line that parses.
	got = diagsWithCode(GetDiagnostics("/* Purpose: totals;\n   Parameters: none;\n", opts), CodeCommentTextAfterTerminator)
	if len(got) != 1 || got[0].Range.Start.Line != 0 || got[0].Range.Start.Character != 18 {
		t.Errorf("stranded doc line: want 1 on the ';', got %+v", got)
	}
	// Member access written the usual way is code.
	if got := commentTerminationDiags(":DECLARE oRec;\n/* read the name;\noRec:Name;\n"); len(got) != 0 {
		t.Errorf("member access: want none, got %+v", got)
	}

	// Prose as code, with no comment involved: unexpected_token only.
	diags = GetDiagnostics("check the totals;\n", opts)
	if n := len(diagsWithCode(diags, CodeUndeclaredVariable)); n != 0 {
		t.Errorf("undeclared_variable on a line that does not parse: got %d", n)
	}
	if n := len(diagsWithCode(diags, CodeUnexpectedToken)); n != 1 {
		t.Errorf("want 1 unexpected_token, got %d", n)
	}
}
