package providers

import "testing"

// [spec feature.formatting/A6]
// Issue #311: FuzzFormat found malformed shapes the formatter kept
// rewriting. Formatting now converges to a fixed point or declines, so its
// output always formats to itself (feature.formatting A6).
func TestFormat_FixedPointOnMalformedInput(t *testing.T) {
	opts := DefaultFormattingOptions()
	for _, in := range []string{
		"#/*;)",
		";0[; ; /*\n ",
		"{|\"",
		"00000000000000000000000000000000.AAAAAAAA0:REGION 0[;:ENDREGION",
		"$A:$00000000000000000#0#00000000#00000000000#0#00000000000000000000000$''0",
	} {
		first := in
		if edits := FormatDocument(in, opts); len(edits) > 0 {
			first = edits[0].NewText
		}
		second := first
		if edits := FormatDocument(first, opts); len(edits) > 0 {
			second = edits[0].NewText
		}
		if first != second {
			t.Errorf("%q: not idempotent\nfirst:  %q\nsecond: %q", in, first, second)
		}
	}
}
