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

	for _, d := range diags {
		if d.Code == CodeCommentTextAfterTerminator {
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
