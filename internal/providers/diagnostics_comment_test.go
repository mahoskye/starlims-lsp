package providers

import "testing"

// [spec diag.comment_swallows_code] An SSL comment ends at ';', never at
// '*/', so a C-style close runs the comment on into the next statement.
// The validator used to report nothing for this (issue #249).
func TestCommentSwallowsCode(t *testing.T) {
	// c_style_comment_closer is info-tier, so the gate must be open for
	// the assertions that check it is still delivered.
	base := DefaultDiagnosticOptions()
	base.IncludeInfoDiagnostics = true
	has := func(script, code string) bool {
		for _, d := range GetDiagnostics(script, base) {
			if d.Code == code {
				return true
			}
		}
		return false
	}

	swallowsAssignment := ":PROCEDURE Demo;\n:DECLARE nA, nB;\n/* banner\n*/\nnA := 1;\nnB := nA;\n:ENDPROC;\n"
	if !has(swallowsAssignment, CodeCommentSwallowsCode) {
		t.Error("a swallowed assignment was not reported")
	}

	swallowsDeclare := ":PROCEDURE Demo;\n/* banner\n*/\n:DECLARE nA;\nnA := 1;\n:ENDPROC;\n"
	if !has(swallowsDeclare, CodeCommentSwallowsCode) {
		t.Error("a swallowed :DECLARE was not reported")
	}

	// `*/;` really does end the comment: inert, and already covered by
	// c_style_comment_closer at info severity.
	inert := ":PROCEDURE Demo;\n:DECLARE nA;\n/* banner\n*/;\nnA := 1;\n:ENDPROC;\n"
	if has(inert, CodeCommentSwallowsCode) {
		t.Error("the inert */; form must not report comment_swallows_code")
	}
	if !has(inert, CodeCStyleCommentCloser) {
		t.Error("the inert */; form lost its c_style_comment_closer advisory")
	}

	// A correctly terminated comment is clean.
	fine := ":PROCEDURE Demo;\n:DECLARE nA;\n/* banner;\nnA := 1;\n:ENDPROC;\n"
	if has(fine, CodeCommentSwallowsCode) || has(fine, CodeCStyleCommentCloser) {
		t.Error("a correctly terminated comment was flagged")
	}

	// Banner decoration after the */ is not a swallowed statement.
	decoration := ":PROCEDURE Demo;\n:DECLARE nA;\n/* banner\n*/\n;\nnA := 1;\n:ENDPROC;\n"
	if has(decoration, CodeCommentSwallowsCode) {
		t.Error("banner decoration was misread as swallowed code")
	}

	// SQL-mode data sources use real SQL comments; the rule must not
	// reach them.
	opts := base
	opts.IsDataSourceFile = true
	sqlDS := "/* DATA SOURCE: X\n*/\n:DSN := conn;\n\nSELECT a FROM b WHERE c = 1\n"
	for _, d := range GetDiagnostics(sqlDS, opts) {
		if d.Code == CodeCommentSwallowsCode {
			t.Error("fired on a SQL-mode data source, where /* */ is a real SQL comment")
		}
	}
}
