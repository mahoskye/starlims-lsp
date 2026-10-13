package constants

import "testing"

// IsStatusKeyword matches every SSLStatusKeywords entry in any casing and
// nothing else (issue #305: the lookup had no test).
func TestIsStatusKeyword(t *testing.T) {
	for _, kw := range SSLStatusKeywords {
		if !IsStatusKeyword(kw) {
			t.Errorf("IsStatusKeyword(%q) = false, want true", kw)
		}
	}
	for _, s := range []string{"pending", "OOS_A", "oktosubmit"} {
		if !IsStatusKeyword(s) {
			t.Errorf("IsStatusKeyword(%q) = false, want true (case-insensitive)", s)
		}
	}
	for _, s := range []string{"", "Pend", "PendingX", "sStatus", "Len"} {
		if IsStatusKeyword(s) {
			t.Errorf("IsStatusKeyword(%q) = true, want false", s)
		}
	}
}
