package server

import "testing"

// [spec feature.cross_file_resolution/A4] The script-level :PARAMETERS may
// follow leading :PROCEDURE blocks (diag.parameters_first's script-level
// placement); the entry point is still that line (issue #258: the search
// used to stop at the first :PROCEDURE and fall back to line 0).
func TestResolveDispatch_EntryPointAfterLeadingProcedures(t *testing.T) {
	wi, dir := newResolverIndex(t)
	uri := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/RUNNER.srvscr", `:PROCEDURE Helper;
:PARAMETERS sInner;
:RETURN sInner;
:ENDPROC;

/* entry;
:PARAMETERS sMode, nLimit;
:RETURN DoProc("Helper", {sMode});`)

	res := wi.ResolveDispatchTarget("LIMS_UTILS.RUNNER")
	if len(res) != 1 || res[0].URI != uri || !res[0].IsEntry {
		t.Fatalf("expected one entry resolution in %s, got %+v", uri, res)
	}
	// :PARAMETERS on 1-based line 7 -> 0-based 6.
	if res[0].Line != 6 {
		t.Errorf("expected entry line 6 (:PARAMETERS after the leading procedure), got %d", res[0].Line)
	}
}
