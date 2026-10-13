package server

import (
	"strings"
	"testing"

	"starlims-lsp/internal/providers"
)

// undeclaredWithClosure runs the undeclared check on source the way
// validateDocument does: closure names and completeness both come from
// includeClosure.
func undeclaredWithClosure(t *testing.T, s *SSLServer, source string, version int) []string {
	t.Helper()
	s.documents.SetDocument(testURI, source, version)
	s.documentVersion[testURI] = version
	cache := s.documents.ParseDocument(testURI, version)

	opts := providers.DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	opts.IncludeDeclaredVariables, opts.IncludesResolved = (liveResolver{s}).includeClosure(cache.Tokens, testURI)
	var flagged []string
	for _, d := range providers.GetDiagnosticsFromTokens(cache.Tokens, cache.AST, opts) {
		if d.Code == providers.CodeUndeclaredVariable {
			flagged = append(flagged, d.Message)
		}
	}
	return flagged
}

// [spec feature.cross_file_resolution/A23] — undeclared_variable checks a
// document with an :INCLUDE only when the include closure is complete
// (issue #304).
func TestIncludeClosure_CompletenessGatesUndeclared(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/LIBB.srvscr", ":PUBLIC gShared;")

	body := "\n:PROCEDURE Demo;\nnTotal := gShared + nMissing;\n:ENDPROC;"

	// Resolved: the closure applies and the genuinely undeclared name flags.
	flagged := undeclaredWithClosure(t, s, ":INCLUDE LibB;"+body, 1)
	if len(flagged) != 1 || !strings.Contains(flagged[0], "nMissing") {
		t.Errorf("resolved include: expected one flag for nMissing, got %v", flagged)
	}

	// Unresolved target: the unread script may declare any name.
	if flagged := undeclaredWithClosure(t, s, ":INCLUDE NoSuchLib;"+body, 2); len(flagged) != 0 {
		t.Errorf("unresolved include: expected no undeclared findings, got %v", flagged)
	}

	// One resolved and one unresolved target: still incomplete.
	if flagged := undeclaredWithClosure(t, s, ":INCLUDE LibB;\n:INCLUDE NoSuchLib;"+body, 3); len(flagged) != 0 {
		t.Errorf("partly resolved includes: expected no undeclared findings, got %v", flagged)
	}

	// No :INCLUDE: checked single-file as before.
	flagged = undeclaredWithClosure(t, s, strings.TrimPrefix(body, "\n"), 4)
	if len(flagged) != 2 {
		t.Errorf("no include: expected flags for gShared and nMissing, got %v", flagged)
	}

	// No workspace index: an include cannot be read at all.
	s.workspaceIndex = nil
	if flagged := undeclaredWithClosure(t, s, ":INCLUDE LibB;"+body, 5); len(flagged) != 0 {
		t.Errorf("no workspace index: expected no undeclared findings, got %v", flagged)
	}
}
