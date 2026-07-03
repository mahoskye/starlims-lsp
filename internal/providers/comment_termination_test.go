package providers

import (
	"strings"
	"testing"

	"starlims-lsp/internal/lexer"
)

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

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	diags := checkCommentTermination(tokens)

	// The paragraph-break guard protects the SECOND comment (line 8,
	// 0-based 7): its broken-out "Parameters" keyword sits across a blank
	// line and must not be reported. The FIRST comment (0-based line 3)
	// legitimately warns since issue #25's orphaned-prose signal landed —
	// its "With more body" lines are genuinely stranded as code with no
	// paragraph break in between.
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

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	diags := checkCommentTermination(tokens)

	found := false
	for _, d := range diags {
		if d.Code == CodeCommentTextAfterTerminator && strings.Contains(d.Message, "terminated early") {
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
// two consecutive bare identifiers is the signature of orphaned prose and
// must fire — as a warning, since keyword-less prose is a weaker signal than
// a keyword match.
func TestCheckCommentTermination_Issue25_OrphanedProseFires(t *testing.T) {
	text := `:PROCEDURE Demo;
/* This header explains the module
and this line accidentally ends with one;
so these words are now parsed as code
;
:ENDPROC;`

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	diags := checkCommentTermination(tokens)

	found := false
	for _, d := range diags {
		if d.Code == CodeCommentTextAfterTerminator {
			found = true
			if d.Severity != SeverityWarning {
				t.Errorf("orphaned-prose path must be a warning, got severity %v", d.Severity)
			}
		}
	}
	if !found {
		t.Errorf("expected comment_text_after_terminator to fire on orphaned prose after mid-comment semicolon, got %d diags", len(diags))
	}
}

// Issue #25 boundary: a single bare identifier followed by valid code must
// not fire the orphaned-prose signal — one identifier can legitimately start
// a statement continued on the next line, so the second identifier must
// share the first one's line.
func TestCheckCommentTermination_Issue25_SingleIdentifierDoesNotFire(t *testing.T) {
	text := `:PROCEDURE Demo;
/* a multi-line note
that spans two lines;
orphan
nCount := 1;
:ENDPROC;`

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	diags := checkCommentTermination(tokens)

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

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	diags := checkCommentTermination(tokens)

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

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	diags := checkCommentTermination(tokens)

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

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	diags := checkCommentTermination(tokens)

	found := false
	for _, d := range diags {
		if d.Code == CodeCommentTextAfterTerminator {
			found = true
			if d.Severity != SeverityWarning {
				t.Errorf("expected warning severity for orphaned prose, got %v", d.Severity)
			}
		}
	}
	if !found {
		t.Errorf("expected orphaned-prose warning for single-line comment shape, got: %+v", diags)
	}
}
