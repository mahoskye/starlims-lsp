package server

// Tests for the server handlers and helpers that had no direct test
// (issue #305): the lifecycle and file-event handlers, prepare-rename and
// inlay-hint request edges, document-symbol conversion, the class-file
// dispatch verdicts behind diag.execfunction_class_target, cross-file hover
// rendering, and the workspace index's lookup maintenance and call-site
// scan.

import (
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/providers"

	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// waitUntil polls cond until it holds or the deadline passes. File events
// may be handled asynchronously, so index effects are awaited rather than
// assumed to be visible when the handler returns.
func waitUntil(t *testing.T, what string, cond func() bool) {
	t.Helper()
	deadline := time.Now().Add(5 * time.Second)
	for !cond() {
		if time.Now().After(deadline) {
			t.Fatalf("timed out waiting for %s", what)
		}
		time.Sleep(5 * time.Millisecond)
	}
}

// publishRecorder collects publishDiagnostics notifications.
type publishRecorder struct {
	mu        sync.Mutex
	published []protocol.PublishDiagnosticsParams
}

func (r *publishRecorder) context() *glsp.Context {
	return &glsp.Context{Notify: func(method string, params any) {
		if method != protocol.ServerTextDocumentPublishDiagnostics {
			return
		}
		if p, ok := params.(protocol.PublishDiagnosticsParams); ok {
			r.mu.Lock()
			r.published = append(r.published, p)
			r.mu.Unlock()
		}
	}}
}

func (r *publishRecorder) all() []protocol.PublishDiagnosticsParams {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]protocol.PublishDiagnosticsParams(nil), r.published...)
}

func diagCode(d protocol.Diagnostic) string {
	if d.Code == nil {
		return ""
	}
	if s, ok := d.Code.Value.(string); ok {
		return s
	}
	return ""
}

// --- Lifecycle: initialized / shutdown / WorkspaceIndex.Stop ---

func TestHandleInitialized_StartsIndexAndRegistersWatchers(t *testing.T) {
	dir := t.TempDir()
	writeTestFile(t, dir, "Main.ssl", ":PROCEDURE RunMain;\n:ENDPROC;\n")

	s := NewSSLServer()
	s.rootURIs = []string{pathToURI(dir)}

	type call struct {
		method string
		params any
	}
	calls := make(chan call, 1)
	ctx := &glsp.Context{Call: func(method string, params any, result any) {
		calls <- call{method, params}
	}}

	if err := s.handleInitialized(ctx, &protocol.InitializedParams{}); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if s.workspaceIndex == nil {
		t.Fatal("expected a workspace index once roots are known")
	}

	select {
	case <-s.workspaceIndex.doneCh:
	case <-time.After(10 * time.Second):
		t.Fatal("background indexing timed out")
	}
	if got := s.workspaceIndex.FileCount(); got != 1 {
		t.Errorf("expected the root's one SSL file to be indexed, got %d", got)
	}
	if s.workspaceIndex.IsIndexing() {
		t.Error("indexing flag still set after the walk finished")
	}

	var c call
	select {
	case c = <-calls:
	case <-time.After(5 * time.Second):
		t.Fatal("expected a client/registerCapability call")
	}
	if c.method != string(protocol.ServerClientRegisterCapability) {
		t.Fatalf("registered via %q, want %q", c.method, protocol.ServerClientRegisterCapability)
	}
	reg, ok := c.params.(protocol.RegistrationParams)
	if !ok || len(reg.Registrations) != 1 {
		t.Fatalf("expected one registration, got %#v", c.params)
	}
	r := reg.Registrations[0]
	if r.Method != string(protocol.MethodWorkspaceDidChangeWatchedFiles) {
		t.Errorf("registration method = %q, want %q", r.Method, protocol.MethodWorkspaceDidChangeWatchedFiles)
	}
	opts, ok := r.RegisterOptions.(protocol.DidChangeWatchedFilesRegistrationOptions)
	if !ok {
		t.Fatalf("unexpected register options %#v", r.RegisterOptions)
	}
	var globs []string
	for _, w := range opts.Watchers {
		globs = append(globs, w.GlobPattern)
	}
	// One watcher per indexed extension, so a watched file is always one
	// the background walk would also have indexed.
	if len(globs) != len(sslFileExtensions) {
		t.Errorf("watchers = %v, want one per extension %v", globs, sslFileExtensions)
	}
	for _, ext := range sslFileExtensions {
		if !containsLabel(globs, "**/*"+ext) {
			t.Errorf("no watcher for %s in %v", ext, globs)
		}
	}

	if err := s.handleShutdown(nil); err != nil {
		t.Fatalf("shutdown: %v", err)
	}
}

func TestHandleInitialized_NoRootsNoIndex(t *testing.T) {
	s := NewSSLServer()
	ctx := &glsp.Context{Call: func(string, any, any) {
		t.Error("no watcher registration is expected without a workspace root")
	}}
	if err := s.handleInitialized(ctx, &protocol.InitializedParams{}); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if s.workspaceIndex != nil {
		t.Error("expected no workspace index without roots")
	}
	// Shutdown with no index is a no-op.
	if err := s.handleShutdown(nil); err != nil {
		t.Fatalf("shutdown: %v", err)
	}
}

func TestHandleShutdown_StopsBackgroundIndex(t *testing.T) {
	dir := t.TempDir()
	for i := 0; i < 20; i++ {
		writeTestFile(t, dir, "File"+string(rune('A'+i))+".ssl", ":PROCEDURE P;\n:ENDPROC;\n")
	}
	s := NewSSLServer()
	s.workspaceIndex = NewWorkspaceIndex([]string{pathToURI(dir)})
	s.workspaceIndex.StartBackgroundIndex()

	done := make(chan error, 1)
	go func() { done <- s.handleShutdown(nil) }()
	select {
	case err := <-done:
		if err != nil {
			t.Fatalf("shutdown: %v", err)
		}
	case <-time.After(10 * time.Second):
		t.Fatal("shutdown did not return: Stop must wait for the walk and then release")
	}
	if s.workspaceIndex.IsIndexing() {
		t.Error("indexing flag still set after Stop returned")
	}
	// Stop waited on doneCh, so the walk goroutine has exited.
	select {
	case <-s.workspaceIndex.doneCh:
	default:
		t.Error("Stop returned before the background walk finished")
	}
}

func TestWorkspaceIndex_StopAfterWalkCompleted(t *testing.T) {
	dir := t.TempDir()
	writeTestFile(t, dir, "Main.ssl", ":PROCEDURE RunMain;\n:ENDPROC;\n")
	wi := NewWorkspaceIndex([]string{pathToURI(dir)})
	wi.StartBackgroundIndex()
	select {
	case <-wi.doneCh:
	case <-time.After(10 * time.Second):
		t.Fatal("background indexing timed out")
	}

	stopped := make(chan struct{})
	go func() { wi.Stop(); close(stopped) }()
	select {
	case <-stopped:
	case <-time.After(5 * time.Second):
		t.Fatal("Stop blocked on an index whose walk already finished")
	}
	// The index stays readable after Stop.
	if got := wi.FileCount(); got != 1 {
		t.Errorf("FileCount after Stop = %d, want 1", got)
	}
}

// --- didSave ---

func TestHandleDidSave_RevalidatesOpenDocument(t *testing.T) {
	s := newTestServerWithDocument(":PROCEDURE Demo;\n:DECLARE sName;\nsName := \"x\"\n:ENDPROC;\n")
	rec := &publishRecorder{}

	if err := s.handleDidSave(rec.context(), &protocol.DidSaveTextDocumentParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
	}); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	got := rec.all()
	if len(got) != 1 || got[0].URI != testURI {
		t.Fatalf("expected one publish for %s, got %+v", testURI, got)
	}
	if len(got[0].Diagnostics) == 0 {
		t.Error("expected the missing-semicolon document to publish diagnostics on save")
	}
}

func TestHandleDidSave_UnknownDocumentPublishesNothing(t *testing.T) {
	s := NewSSLServer()
	rec := &publishRecorder{}
	if err := s.handleDidSave(rec.context(), &protocol.DidSaveTextDocumentParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: "file:///never-opened.ssl"},
	}); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if got := rec.all(); len(got) != 0 {
		t.Errorf("expected no publish for a document that is not open, got %+v", got)
	}
}

// --- didChangeWatchedFiles ---

func TestHandleDidChangeWatchedFiles_CreateChangeDelete(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi

	path := filepath.Join(dir, "Helpers.ssl")
	uri := pathToURI(path)
	if err := os.WriteFile(path, []byte(":PROCEDURE OldName;\n:ENDPROC;\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Created: the file is indexed.
	if err := s.handleDidChangeWatchedFiles(nil, &protocol.DidChangeWatchedFilesParams{
		Changes: []protocol.FileEvent{{URI: uri, Type: protocol.FileChangeTypeCreated}},
	}); err != nil {
		t.Fatal(err)
	}
	waitUntil(t, "the created file to be indexed", func() bool {
		return len(wi.ResolveDispatchTarget("Helpers.OldName")) == 1
	})

	// Changed: the file is re-indexed from disk and the old procedure name
	// leaves the lookups.
	if err := os.WriteFile(path, []byte(":PROCEDURE NewName;\n:ENDPROC;\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := s.handleDidChangeWatchedFiles(nil, &protocol.DidChangeWatchedFilesParams{
		Changes: []protocol.FileEvent{{URI: uri, Type: protocol.FileChangeTypeChanged}},
	}); err != nil {
		t.Fatal(err)
	}
	waitUntil(t, "the changed file to be re-indexed", func() bool {
		return len(wi.ResolveDispatchTarget("Helpers.NewName")) == 1
	})
	if got := wi.ResolveDispatchTarget("Helpers.OldName"); len(got) != 0 {
		t.Errorf("the renamed procedure still resolves after re-index: %+v", got)
	}
	if got := wi.ResolveDispatchTarget("Cat.Sub.OldName"); len(got) != 0 {
		t.Errorf("the unique-procedure fallback still finds the old name: %+v", got)
	}

	// Deleted: the file leaves the index.
	if err := s.handleDidChangeWatchedFiles(nil, &protocol.DidChangeWatchedFilesParams{
		Changes: []protocol.FileEvent{{URI: uri, Type: protocol.FileChangeTypeDeleted}},
	}); err != nil {
		t.Fatal(err)
	}
	waitUntil(t, "the deleted file to leave the index", func() bool {
		_, ok := wi.FileSymbolsFor(uri)
		return !ok
	})
	if got := wi.ScriptsNamed("Helpers"); len(got) != 0 {
		t.Errorf("deleted script still listed by name: %+v", got)
	}
}

func TestHandleDidChangeWatchedFiles_OpenDocumentNotReindexed(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi

	openPath := filepath.Join(dir, "Open.ssl")
	otherPath := filepath.Join(dir, "Other.ssl")
	for _, p := range []string{openPath, otherPath} {
		if err := os.WriteFile(p, []byte(":PROCEDURE OnDisk;\n:ENDPROC;\n"), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	openURI, otherURI := pathToURI(openPath), pathToURI(otherPath)
	// The editor's buffer is newer than the disk copy.
	s.documents.SetDocument(openURI, ":PROCEDURE InBuffer;\n:ENDPROC;\n", 1)
	s.documentVersion[openURI] = 1

	if err := s.handleDidChangeWatchedFiles(nil, &protocol.DidChangeWatchedFilesParams{
		Changes: []protocol.FileEvent{
			{URI: openURI, Type: protocol.FileChangeTypeChanged},
			{URI: otherURI, Type: protocol.FileChangeTypeCreated},
		},
	}); err != nil {
		t.Fatal(err)
	}
	waitUntil(t, "the closed file to be indexed", func() bool {
		_, ok := wi.FileSymbolsFor(otherURI)
		return ok
	})
	time.Sleep(50 * time.Millisecond) // let any straggling event land
	if _, ok := wi.FileSymbolsFor(openURI); ok {
		t.Error("a watcher event for an open document must not index the disk copy")
	}
}

func TestHandleDidChangeWatchedFiles_NoIndex(t *testing.T) {
	s := NewSSLServer()
	if err := s.handleDidChangeWatchedFiles(nil, &protocol.DidChangeWatchedFilesParams{
		Changes: []protocol.FileEvent{{URI: "file:///x.ssl", Type: protocol.FileChangeTypeCreated}},
	}); err != nil {
		t.Fatalf("expected a nil-index no-op, got %v", err)
	}
}

// --- prepareRename ---

func prepareRenameAt(t *testing.T, s *SSLServer, line, char uint32) any {
	t.Helper()
	result, err := s.handlePrepareRename(nil, &protocol.PrepareRenameParams{TextDocumentPositionParams: protocol.TextDocumentPositionParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
		Position:     protocol.Position{Line: line, Character: char},
	}})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	return result
}

func TestHandlePrepareRename_UnknownDocument(t *testing.T) {
	s := NewSSLServer()
	if got := prepareRenameAt(t, s, 0, 0); got != nil {
		t.Errorf("expected nil for a document that is not open, got %#v", got)
	}
}

// [spec feature.rename/A3] [spec feature.rename/A4]
func TestHandlePrepareRename_RejectsKeywordAndComment(t *testing.T) {
	s := newTestServerWithDocument(":DECLARE sName;\n/* sName note;\nsName := Len(sName);")
	cases := []struct {
		name       string
		line, char uint32
	}{
		{"keyword", 0, 3},
		{"comment", 1, 5},
		{"built-in function", 2, 10},
	}
	for _, tc := range cases {
		if got := prepareRenameAt(t, s, tc.line, tc.char); got != nil {
			t.Errorf("%s: expected the rename to be rejected, got %#v", tc.name, got)
		}
	}
}

func TestHandlePrepareRename_VariableReturnsRangeAndPlaceholder(t *testing.T) {
	s := newTestServerWithDocument(":DECLARE sName;\nsName := \"x\";")
	rp, ok := prepareRenameAt(t, s, 1, 2).(protocol.RangeWithPlaceholder)
	if !ok {
		t.Fatal("expected a RangeWithPlaceholder for a declared variable")
	}
	if rp.Placeholder != "sName" {
		t.Errorf("placeholder = %q, want sName", rp.Placeholder)
	}
	if rp.Range.Start != (protocol.Position{Line: 1, Character: 0}) || rp.Range.End != (protocol.Position{Line: 1, Character: 5}) {
		t.Errorf("range = %+v, want line 1, 0-5", rp.Range)
	}
}

// [spec feature.rename/A9] [spec feature.rename/A15]
func TestHandlePrepareRename_DispatchSegments(t *testing.T) {
	line := `ExecFunction("CAT.SCRIPT.CalcTotal", {});`
	s := newTestServerWithDocument(line)
	segStart := uint32(strings.Index(line, "CalcTotal"))

	rp, ok := prepareRenameAt(t, s, 0, segStart+2).(protocol.RangeWithPlaceholder)
	if !ok {
		t.Fatal("expected the last dispatch segment to be renameable")
	}
	if rp.Placeholder != "CalcTotal" {
		t.Errorf("placeholder = %q, want CalcTotal", rp.Placeholder)
	}
	if rp.Range.Start.Character != segStart || rp.Range.End.Character != segStart+uint32(len("CalcTotal")) {
		t.Errorf("range = %+v, want the CalcTotal segment", rp.Range)
	}

	for _, seg := range []string{"CAT", "SCRIPT"} {
		at := uint32(strings.Index(line, seg)) + 1
		if got := prepareRenameAt(t, s, 0, at); got != nil {
			t.Errorf("segment %s: expected rejection, got %#v", seg, got)
		}
	}
}

// --- inlayHint ---

func inlayHintsFor(t *testing.T, s *SSLServer, uri string, startLine, endLine uint32) []InlayHint {
	t.Helper()
	hints, err := s.handleInlayHint(nil, &InlayHintParams{
		TextDocument: TextDocumentIdentifier{URI: uri},
		Range:        InlayHintRange{Start: InlayHintPosition{Line: startLine}, End: InlayHintPosition{Line: endLine}},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	return hints
}

func TestHandleInlayHint_UnknownDocument(t *testing.T) {
	s := NewSSLServer()
	if got := inlayHintsFor(t, s, "file:///never-opened.ssl", 0, 10); got != nil {
		t.Errorf("expected nil for a document that is not open, got %+v", got)
	}
}

// [spec feature.inlay_hints/A1]
func TestHandleInlayHint_ParameterHintsBeforeArguments(t *testing.T) {
	line := `sX := SubStr("Hello", 1, 5);`
	s := newTestServerWithDocument(":DECLARE sX;\n" + line)

	hints := inlayHintsFor(t, s, testURI, 0, 2)
	if len(hints) != 3 {
		t.Fatalf("expected three parameter hints, got %+v", hints)
	}
	args := []string{`"Hello"`, "1, 5", "5)"}
	for i, h := range hints {
		if h.Kind == nil || *h.Kind != InlayHintKindParameter {
			t.Errorf("hint %d kind = %v, want Parameter", i, h.Kind)
		}
		if !strings.HasSuffix(h.Label, ":") || !h.PaddingRight {
			t.Errorf("hint %d = %+v, want a padded `name:` label", i, h)
		}
		if h.Position.Line != 1 {
			t.Errorf("hint %d on line %d, want 1", i, h.Position.Line)
		}
		if want := uint32(strings.Index(line, args[i])); h.Position.Character != want {
			t.Errorf("hint %d at character %d, want %d (before its argument)", i, h.Position.Character, want)
		}
	}
}

// [spec feature.inlay_hints/A6] [spec feature.inlay_hints/A7]
func TestHandleInlayHint_EmptyResultIsNil(t *testing.T) {
	content := ":DECLARE sX;\nsX := SubStr(\"Hello\", 1, 5);\n\n\nsX := Left(sX, 2);"
	s := newTestServerWithDocument(content)

	// A range holding no call yields nil, not an empty slice.
	if got := inlayHintsFor(t, s, testURI, 2, 3); got != nil {
		t.Errorf("expected nil for a range without calls, got %+v", got)
	}
	// A narrower range keeps only the calls inside it.
	if got := inlayHintsFor(t, s, testURI, 4, 4); len(got) != 2 || got[0].Position.Line != 4 {
		t.Errorf("expected the two Left hints on line 4 only, got %+v", got)
	}

	s.settings.InlayHints.Enabled = false
	if got := inlayHintsFor(t, s, testURI, 0, 5); got != nil {
		t.Errorf("expected nil with inlay hints disabled, got %+v", got)
	}
}

// --- documentSymbol conversion ---

// [spec feature.document_symbols/A3] [spec feature.document_symbols/A8]
func TestHandleDocumentSymbol_RegionChildrenConverted(t *testing.T) {
	s := newTestServerWithDocument(`/* region Outer;
/* region Inner;
:PROCEDURE Calc;
:PARAMETERS nQty, nPrice;
:ENDPROC;
/* endregion;
/* endregion;
:PROCEDURE Loose;
:ENDPROC;`)

	result, err := s.handleDocumentSymbol(nil, &protocol.DocumentSymbolParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	symbols := result.([]protocol.DocumentSymbol)

	var outer *protocol.DocumentSymbol
	for i := range symbols {
		if symbols[i].Name == "Outer" {
			outer = &symbols[i]
		}
		if symbols[i].Name == "Calc" {
			t.Error("Calc must appear only under its region, not at the top level")
		}
	}
	if outer == nil {
		t.Fatalf("expected a top-level Outer region, got %+v", symbols)
	}
	if outer.Detail == nil || *outer.Detail != "region" {
		t.Errorf("Outer detail = %v, want region", outer.Detail)
	}
	if len(outer.Children) != 1 || outer.Children[0].Name != "Inner" {
		t.Fatalf("expected Inner as Outer's only child, got %+v", outer.Children)
	}
	inner := outer.Children[0]
	if len(inner.Children) != 1 || inner.Children[0].Name != "Calc" {
		t.Fatalf("expected Calc under Inner, got %+v", inner.Children)
	}
	calc := inner.Children[0]
	if calc.Detail == nil || !strings.Contains(*calc.Detail, "nQty") {
		t.Errorf("Calc detail = %v, want its parameter list", calc.Detail)
	}
	if calc.Range.Start.Line < inner.Range.Start.Line || calc.Range.End.Line > inner.Range.End.Line {
		t.Errorf("child range %+v escapes its region %+v", calc.Range, inner.Range)
	}
}

func TestConvertDocumentSymbol_WidensRangeToSelection(t *testing.T) {
	sym := providers.DocumentSymbol{
		Name:           "Wide",
		Kind:           12,
		Range:          providers.Range{Start: providers.Position{Line: 2, Character: 4}, End: providers.Position{Line: 2, Character: 6}},
		SelectionRange: providers.Range{Start: providers.Position{Line: 2, Character: 0}, End: providers.Position{Line: 3, Character: 1}},
		Children: []providers.DocumentSymbol{{
			Name:           "Child",
			Kind:           13,
			Detail:         "parameter",
			Range:          providers.Range{Start: providers.Position{Line: 3, Character: 0}, End: providers.Position{Line: 3, Character: 5}},
			SelectionRange: providers.Range{Start: providers.Position{Line: 3, Character: 0}, End: providers.Position{Line: 3, Character: 5}},
		}},
	}

	got := convertDocumentSymbol(nil, sym)
	// The LSP requires selectionRange to lie inside range.
	if got.Range.Start != (protocol.Position{Line: 2, Character: 0}) || got.Range.End != (protocol.Position{Line: 3, Character: 1}) {
		t.Errorf("range = %+v, want it widened to contain the selection", got.Range)
	}
	if got.Detail != nil {
		t.Errorf("an empty detail must stay nil, got %q", *got.Detail)
	}
	if len(got.Children) != 1 || got.Children[0].Detail == nil || *got.Children[0].Detail != "parameter" {
		t.Errorf("children not converted: %+v", got.Children)
	}
	if got.Children[0].Children != nil {
		t.Error("a leaf symbol must not carry an empty children slice")
	}
}

// --- classFileDispatchTargets (diag.execfunction_class_target) ---

const sampleToolsClass = `:CLASS SampleTools;
:INHERIT LIMS.ToolBase;

:PROCEDURE Recalculate;
:ENDPROC;
`

const classTargetCaller = `:PROCEDURE Demo;
ExecFunction("LIMS.SampleTools", {1});
ExecFunction("lims.sampletools", {1});
ExecFunction("LIMS.SampleTools.Recalculate", {1});
ExecFunction("LIMS.HELPERS", {1});
DoProc("SampleTools.Recalculate", {1});
ExecFunction("No.Such.Target", {1});
:ENDPROC;
`

func classTargetServer(t *testing.T) *SSLServer {
	t.Helper()
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS/SAMPLETOOLS.srvscr", sampleToolsClass)
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS/HELPERS.srvscr", helperScript)
	return s
}

// [spec feature.cross_file_resolution/A20]
func TestClassFileDispatchTargets_PopulatedIndex(t *testing.T) {
	s := classTargetServer(t)
	tokens := lexer.NewLexer(classTargetCaller).Tokenize()

	got := (liveResolver{s}).classFileDispatchTargets(tokens)
	want := []string{"LIMS.SampleTools", "LIMS.SampleTools.Recalculate"}
	if strings.Join(got, "|") != strings.Join(want, "|") {
		// DoProc sites are skipped whatever they target, case variants of
		// one target are resolved once (first spelling kept), and ordinary
		// or unresolvable targets never qualify.
		t.Errorf("classFileDispatchTargets = %v, want %v", got, want)
	}

	s.workspaceIndex = nil
	if got := (liveResolver{s}).classFileDispatchTargets(tokens); got != nil {
		t.Errorf("expected nil without a workspace index, got %v", got)
	}
}

// End to end through validateDocument: the verdicts reach the published
// diagnostics, and data-source documents skip the resolution.
func TestValidateDocument_ExecFunctionClassTargetPublished(t *testing.T) {
	s := classTargetServer(t)
	s.documents.SetDocument(testURI, classTargetCaller, 1)
	s.documentVersion[testURI] = 1
	rec := &publishRecorder{}
	s.validateDocument(rec.context(), testURI)

	published := rec.all()
	if len(published) != 1 {
		t.Fatalf("expected one publish, got %d", len(published))
	}
	var lines []uint32
	for _, d := range published[0].Diagnostics {
		if diagCode(d) == providers.CodeExecFunctionClassTarget {
			lines = append(lines, d.Range.Start.Line)
		}
	}
	// Lines 1-3 dispatch to the class; 4 is an ordinary script, 5 a DoProc,
	// 6 unresolvable.
	if len(lines) != 3 || lines[0] != 1 || lines[1] != 2 || lines[2] != 3 {
		t.Errorf("execfunction_class_target on lines %v, want [1 2 3]", lines)
	}

	dsURI := "file:///query.ds"
	s.documents.SetDocument(dsURI, ":PARAMETERS sX;\n"+classTargetCaller, 1)
	s.documentVersion[dsURI] = 1
	s.validateDocument(rec.context(), dsURI)
	for _, p := range rec.all() {
		if p.URI != dsURI {
			continue
		}
		for _, d := range p.Diagnostics {
			if diagCode(d) == providers.CodeExecFunctionClassTarget {
				t.Errorf("data-source document flagged a class target: %+v", d)
			}
		}
	}
}

// --- includeHoverMarkdown / renderResolutionHover ---

func TestIncludeHoverMarkdown_Edges(t *testing.T) {
	s := NewSSLServer()
	if got := (liveResolver{s}).includeHoverMarkdown("SharedLib"); got != "" {
		t.Errorf("expected no hover without a workspace index, got %q", got)
	}

	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/SHAREDLIB.srvscr", helperScript)

	r := liveResolver{s}
	if got := r.includeHoverMarkdown("NoSuchLib"); got != "" {
		t.Errorf("expected no hover for an unresolvable include, got %q", got)
	}
	md := r.includeHoverMarkdown("LIMS_UTILS.SharedLib")
	if !strings.Contains(md, "LIMS_UTILS.SHAREDLIB") || !strings.Contains(md, "sMode") {
		t.Errorf("include hover missing the script identity or entry parameters:\n%s", md)
	}
}

const hoverDefOnDisk = `/*
 * Description: Disk description.
;
:PROCEDURE CalculateTotal;
:PARAMETERS nQty;
:ENDPROC;
`

// [spec feature.hover/A10] [spec feature.cross_file_resolution/A12] — an
// open definition file hovers from its live buffer, not the index.
func TestHandleHover_CrossFileProcedurePrefersLiveBuffer(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	defURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", hoverDefOnDisk)

	live := strings.Replace(hoverDefOnDisk, "Disk description.", "Buffer description.", 1)
	live = strings.Replace(live, ":PARAMETERS nQty;", ":PARAMETERS nQty, nPrice;", 1)
	s.documents.SetDocument(defURI, live, 2)
	s.documentVersion[defURI] = 2

	source := `nTotal := ExecFunction("LIMS_UTILS.HELPERS.CalculateTotal", {});`
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: uint32(strings.Index(source, "Calculate"))},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover == nil {
		t.Fatal("expected a cross-file procedure hover")
	}
	md := hover.Contents.(protocol.MarkupContent).Value
	for _, want := range []string{"Buffer description.", "nPrice", "LIMS_UTILS.HELPERS"} {
		if !strings.Contains(md, want) {
			t.Errorf("hover missing %q (live buffer):\n%s", want, md)
		}
	}
	if strings.Contains(md, "Disk description.") {
		t.Errorf("hover used the indexed docblock instead of the live buffer:\n%s", md)
	}
}

func TestRenderResolutionHover_NullCases(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	defURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", hoverDefOnDisk)
	r := liveResolver{s}

	// The first candidate is not in the index.
	if got := r.renderResolutionHover([]IndexResolution{{URI: "file:///gone.srvscr", ProcName: "CalculateTotal"}}); got != "" {
		t.Errorf("expected no hover for an unindexed candidate, got %q", got)
	}
	// The indexed file lacks the procedure.
	if got := r.renderResolutionHover([]IndexResolution{{URI: defURI, ProcName: "Missing"}}); got != "" {
		t.Errorf("expected no hover for a procedure the file lacks, got %q", got)
	}
	// Closed file: the indexed procedure renders, with the extra-candidate count.
	md := r.renderResolutionHover([]IndexResolution{{URI: defURI, ProcName: "calculatetotal"}, {URI: "file:///other.srvscr"}})
	if !strings.Contains(md, "Disk description.") {
		t.Errorf("expected the indexed procedure hover, got:\n%s", md)
	}
	// Open file whose buffer no longer has the procedure: truthful null.
	s.documents.SetDocument(defURI, ":PROCEDURE Renamed;\n:ENDPROC;\n", 2)
	s.documentVersion[defURI] = 2
	if got := r.renderResolutionHover([]IndexResolution{{URI: defURI, ProcName: "CalculateTotal"}}); got != "" {
		t.Errorf("expected no hover once the open buffer drops the procedure, got %q", got)
	}
}

// --- isClassFile (URI) and isClassFileFromTokens ---

func TestIsClassFileFromTokens(t *testing.T) {
	cases := []struct {
		name, src string
		want      bool
	}{
		{"class", ":CLASS Widget;", true},
		{"leading comment and blank lines", "/* header;\n\n:CLASS Widget;", true},
		{"lowercase keyword", ":class Widget;", true},
		{"script", ":PROCEDURE Run;\n:ENDPROC;", false},
		{"class after a statement", ":DECLARE sX;\n:CLASS Widget;", false},
		{"empty", "", false},
		{"comment only", "/* nothing here;", false},
	}
	for _, tc := range cases {
		if got := isClassFileFromTokens(lexer.NewLexer(tc.src).Tokenize()); got != tc.want {
			t.Errorf("%s: isClassFileFromTokens = %v, want %v", tc.name, got, tc.want)
		}
	}
	if isClassFileFromTokens(nil) {
		t.Error("a nil token stream is not a class file")
	}
}

func TestIsClassMethodContext_UsesSharedClassPredicate(t *testing.T) {
	src := "/* header;\n:CLASS Widget;\n:PROCEDURE Build;\n:ENDPROC;\n"
	tokens := lexer.NewLexer(src).Tokenize()
	s := newTestServerWithDocument(src)
	procs := s.documents.ParseDocument(testURI, 1).Procedures
	if !isClassMethodContext(tokens, procs, 3) {
		t.Error("a line inside a class method is class-method context")
	}
	if isClassMethodContext(tokens, procs, 1) {
		t.Error("a line outside every procedure is not class-method context")
	}
	script := ":PROCEDURE Build;\n:ENDPROC;\n"
	if isClassMethodContext(lexer.NewLexer(script).Tokenize(), procs, 1) {
		t.Error("a procedure in an ordinary script is not class-method context")
	}
}

// [spec feature.rename/A12] — the class-file gate cross-file rename uses.
func TestServerIsClassFile(t *testing.T) {
	s := NewSSLServer()
	if s.isClassFile("file:///x.srvscr") {
		t.Error("expected false with no index and no open document")
	}

	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	classURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS/SAMPLETOOLS.srvscr", sampleToolsClass)
	scriptURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS/HELPERS.srvscr", helperScript)

	if !s.isClassFile(classURI) {
		t.Error("closed class file: expected true from the index")
	}
	if s.isClassFile(scriptURI) {
		t.Error("closed script: expected false from the index")
	}
	if s.isClassFile("file:///not-indexed.srvscr") {
		t.Error("unindexed URI: expected false")
	}

	// An open buffer wins over the index in both directions.
	s.documents.SetDocument(classURI, ":PROCEDURE Recalculate;\n:ENDPROC;\n", 2)
	s.documentVersion[classURI] = 2
	if s.isClassFile(classURI) {
		t.Error("open buffer no longer a class: expected false")
	}
	s.documents.SetDocument(scriptURI, "/* now a class;\n:CLASS Helpers;\n", 2)
	s.documentVersion[scriptURI] = 2
	if !s.isClassFile(scriptURI) {
		t.Error("open buffer turned into a class: expected true")
	}
}

// --- WorkspaceIndex lookups: removeFromLookupsLocked ---

func TestWorkspaceIndex_RemoveFileMaintainsCategoryLookups(t *testing.T) {
	wi, dir := newResolverIndex(t)
	helpersURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr",
		":PROCEDURE OnlyInHelpers;\n:ENDPROC;\n:PROCEDURE Shared;\n:ENDPROC;\n")
	otherURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/OTHER.srvscr",
		":PROCEDURE Shared;\n:ENDPROC;\n")

	wi.RemoveFile(helpersURI)

	if got := wi.CategoryNames(); len(got) != 1 || got[0] != "LIMS_UTILS" {
		t.Errorf("category must survive while another script is in it, got %v", got)
	}
	if got := wi.ScriptsInCategory("LIMS_UTILS"); len(got) != 1 || got[0].URI != otherURI {
		t.Errorf("ScriptsInCategory = %+v, want only OTHER", got)
	}
	if got := wi.ScriptsNamed("HELPERS"); len(got) != 0 {
		t.Errorf("removed script still listed by name: %+v", got)
	}
	if got := wi.ResolveDispatchTarget("LIMS_UTILS.HELPERS"); len(got) != 0 {
		t.Errorf("removed script still resolves: %+v", got)
	}
	// The removed file's only procedure leaves the procedure lookup; the
	// shared name keeps the surviving file and becomes workspace-unique.
	if got := wi.ResolveDispatchTarget("X.Y.OnlyInHelpers"); len(got) != 0 {
		t.Errorf("removed procedure still resolves: %+v", got)
	}
	if got := wi.ResolveDispatchTarget("X.Y.Shared"); len(got) != 1 || got[0].URI != otherURI {
		t.Errorf("Shared should now resolve uniquely to OTHER, got %+v", got)
	}

	wi.RemoveFile(otherURI)
	if got := wi.CategoryNames(); len(got) != 0 {
		t.Errorf("empty category must be dropped, got %v", got)
	}
	wi.mu.RLock()
	defer wi.mu.RUnlock()
	if len(wi.byCategory) != 0 || len(wi.byScriptName) != 0 || len(wi.byProcName) != 0 {
		t.Errorf("lookups not emptied: byCategory=%v byScriptName=%v byProcName=%v",
			wi.byCategory, wi.byScriptName, wi.byProcName)
	}
}

func TestWorkspaceIndex_ReindexReplacesLookupEntries(t *testing.T) {
	wi, dir := newResolverIndex(t)
	uri := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", ":PROCEDURE Before;\n:ENDPROC;\n")
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", ":PROCEDURE After;\n:ENDPROC;\n")
	if err := wi.IndexFile(uri); err != nil { // a third pass with unchanged content
		t.Fatal(err)
	}

	wi.mu.RLock()
	byName := append([]string(nil), wi.byScriptName["helpers"]...)
	byCat := append([]string(nil), wi.byCategory["lims_utils"]["helpers"]...)
	_, hasBefore := wi.byProcName["before"]
	after := append([]string(nil), wi.byProcName["after"]...)
	wi.mu.RUnlock()

	if len(byName) != 1 || len(byCat) != 1 || len(after) != 1 {
		t.Errorf("re-indexing must not duplicate URIs: byScriptName=%v byCategory=%v byProcName[after]=%v", byName, byCat, after)
	}
	if hasBefore {
		t.Error("the replaced procedure name must leave the lookup")
	}
	wi.RemoveFile("file:///never-indexed.ssl") // no-op, must not panic
	if got := wi.FileCount(); got != 1 {
		t.Errorf("FileCount = %d, want 1", got)
	}
}

// --- WorkspaceIndex.CallSitesFor ---

// [spec feature.cross_file_resolution/A14] — 1-part targets are same-file
// calls and never cross-file call-site candidates.
func TestWorkspaceIndex_CallSitesFor(t *testing.T) {
	wi, dir := newResolverIndex(t)
	callerA := writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/A_CALLER.srvscr", `:INCLUDE LIMS_UTILS.CalcTotal;
:PROCEDURE Run;
ExecFunction("LIMS_UTILS.HELPERS.Other", {});
DoProc("CalcTotal", {1});
aRows := RunDS("QUERIES.CalcTotal");
ExecFunction("B.CalcTotal", {}); ExecFunction("A.calctotal", {});
ExecFunction("LIMS_UTILS.HELPERS.CalcTotal", {1});
:ENDPROC;
`)
	callerB := writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/B_CALLER.srvscr", `:PROCEDURE Go;
DoProc("HELPERS.CALCTOTAL", {});
:ENDPROC;
`)
	skipped := writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/C_SKIPPED.srvscr", `:PROCEDURE Go;
ExecFunction("LIMS_UTILS.HELPERS.CalcTotal", {});
:ENDPROC;
`)

	got := wi.CallSitesFor("CalcTotal", map[string]struct{}{skipped: {}})

	type siteKey struct {
		uri  string
		raw  string
		line int
	}
	want := []siteKey{
		{callerA, "B.CalcTotal", 5},
		{callerA, "A.calctotal", 5},
		{callerA, "LIMS_UTILS.HELPERS.CalcTotal", 6},
		{callerB, "HELPERS.CALCTOTAL", 1},
	}
	if len(got) != len(want) {
		t.Fatalf("CallSitesFor returned %d sites, want %d: %+v", len(got), len(want), got)
	}
	for i, w := range want {
		g := got[i]
		if g.URI != w.uri || g.Site.Raw != w.raw || g.Site.Line != w.line {
			t.Errorf("site %d = {%s %q line %d}, want {%s %q line %d}",
				i, g.URI, g.Site.Raw, g.Site.Line, w.uri, w.raw, w.line)
		}
		if g.Site.Kind != providers.CallDispatch {
			t.Errorf("site %d kind = %v, want dispatch only", i, g.Site.Kind)
		}
	}
	// Same line: ordered by start character.
	if got[0].Site.StartChar >= got[1].Site.StartChar {
		t.Errorf("same-line sites out of order: %d then %d", got[0].Site.StartChar, got[1].Site.StartChar)
	}

	if got := wi.CallSitesFor("NoSuchProc", nil); len(got) != 0 {
		t.Errorf("expected no sites for an unused name, got %+v", got)
	}
}
