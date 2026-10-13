package server

import (
	"bytes"
	"errors"
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// The tests in this file drive the server the way VS Code does: every URI
// is percent-escaped (`Server%20Scripts/...`) while the workspace index
// builds its keys from paths on disk (issue #258).

// clientURI spells a path the way VS Code sends it, using net/url's
// escaping rather than the server's own helper. On Windows VS Code also
// lower-cases the drive letter and escapes its colon.
func clientURI(t *testing.T, path string) string {
	t.Helper()
	p := filepath.ToSlash(path)
	drive := ""
	if hasDriveLetter(p, 0) {
		drive, p = "/"+strings.ToLower(p[:1])+"%3A", p[2:]
	}
	if !strings.HasPrefix(p, "/") {
		p = "/" + p
	}
	uri := (&url.URL{Scheme: "file", Path: p}).String()
	uri = "file://" + drive + strings.TrimPrefix(uri, "file://")
	if !strings.Contains(uri, "%20") {
		t.Fatalf("test path must contain an escaped space, got %q", uri)
	}
	return uri
}

// escapedWorkspace writes the rename fixtures under "Server Scripts/",
// indexes them with the real background walk, and returns the server
// plus the client spellings of the definition and caller files.
func escapedWorkspace(t *testing.T) (s *SSLServer, defURI, callerURI string) {
	t.Helper()
	dir := filepath.Join(t.TempDir(), "My Workspace")
	defPath := filepath.Join(dir, "Server Scripts", "LIMS_UTILS", "HELPERS.srvscr")
	callerPath := filepath.Join(dir, "Server Scripts", "ORDERS", "PROCESS.srvscr")
	for path, content := range map[string]string{defPath: renameDefScript, callerPath: renameCallerScript} {
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
			t.Fatal(err)
		}
	}

	s = NewSSLServer()
	if _, err := s.handleInitialize(nil, &protocol.InitializeParams{
		WorkspaceFolders: []protocol.WorkspaceFolder{{URI: clientURI(t, dir), Name: "ws"}},
	}); err != nil {
		t.Fatal(err)
	}
	s.workspaceIndex = NewWorkspaceIndex(s.rootURIs)
	s.workspaceIndex.StartBackgroundIndex()
	select {
	case <-s.workspaceIndex.doneCh:
	case <-time.After(10 * time.Second):
		t.Fatal("background indexing timed out")
	}
	if n := s.workspaceIndex.FileCount(); n != 2 {
		t.Fatalf("expected 2 indexed files, got %d", n)
	}
	return s, clientURI(t, defPath), clientURI(t, callerPath)
}

var nopContext = &glsp.Context{Notify: func(string, any) {}}

func openClientDoc(t *testing.T, s *SSLServer, uri, text string) {
	t.Helper()
	if err := s.handleDidOpen(nopContext, &protocol.DidOpenTextDocumentParams{TextDocument: protocol.TextDocumentItem{
		URI: uri, Version: 1, Text: text,
	}}); err != nil {
		t.Fatal(err)
	}
}

// indexedURIs lists the URIs the index holds a CalcTotal procedure under.
func calcTotalEntries(s *SSLServer) int {
	n := 0
	for _, r := range s.workspaceIndex.SearchSymbols("CalcTotal", nil) {
		if r.Name == "CalcTotal" {
			n++
		}
	}
	return n
}

// [spec feature.cross_file_resolution/A21] Cross-file rename from an open
// definition file reaches the closed caller, and every WorkspaceEdit key is
// the client's own spelling.
func TestEscapedURI_CrossFileRename(t *testing.T) {
	s, defURI, callerURI := escapedWorkspace(t)
	openClientDoc(t, s, defURI, renameDefScript)

	edit, err := s.handleRename(nil, renameParamsAt(defURI, 1, 14, "ComputeTotal"))
	if err != nil || edit == nil {
		t.Fatalf("expected a workspace edit, got %v, %v", edit, err)
	}
	if len(edit.Changes) != 2 {
		t.Errorf("expected edits in exactly 2 files, got keys %v", keys(edit.Changes))
	}
	if len(edit.Changes[protocol.DocumentUri(defURI)]) == 0 {
		t.Errorf("no edits under the definition's client URI %q; keys %v", defURI, keys(edit.Changes))
	}
	if got := len(edit.Changes[protocol.DocumentUri(callerURI)]); got != 1 {
		t.Errorf("expected 1 caller edit under %q, got %d; keys %v", callerURI, got, keys(edit.Changes))
	}
}

func keys[V any](m map[protocol.DocumentUri]V) []string {
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, string(k))
	}
	return out
}

// [spec feature.cross_file_resolution/A21] An open file's procedures are
// listed once: the open copy replaces the indexed one.
func TestEscapedURI_WorkspaceSymbolsListOpenFileOnce(t *testing.T) {
	s, defURI, _ := escapedWorkspace(t)
	openClientDoc(t, s, defURI, renameDefScript)

	syms, err := s.handleWorkspaceSymbol(nil, &protocol.WorkspaceSymbolParams{Query: "CalcTotal"})
	if err != nil {
		t.Fatal(err)
	}
	if len(syms) != 1 {
		t.Fatalf("expected CalcTotal once, got %d: %+v", len(syms), syms)
	}
	if string(syms[0].Location.URI) != defURI {
		t.Errorf("symbol URI %q, want the client's %q", syms[0].Location.URI, defURI)
	}
}

// [spec feature.cross_file_resolution/A21] Closing a document re-indexes
// it under the key the walk used, so dispatch stays unambiguous.
func TestEscapedURI_DidCloseReindexKeepsOneEntry(t *testing.T) {
	s, defURI, _ := escapedWorkspace(t)
	openClientDoc(t, s, defURI, renameDefScript)
	if err := s.handleDidClose(nopContext, &protocol.DidCloseTextDocumentParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: defURI},
	}); err != nil {
		t.Fatal(err)
	}
	s.workspaceIndex.pending.Wait()

	if n := s.workspaceIndex.FileCount(); n != 2 {
		t.Errorf("expected 2 indexed files after didClose, got %d", n)
	}
	if n := calcTotalEntries(s); n != 1 {
		t.Errorf("expected CalcTotal indexed once, got %d", n)
	}
	if res := s.workspaceIndex.ResolveDispatchTarget("LIMS_UTILS.HELPERS.CalcTotal"); len(res) != 1 {
		t.Errorf("expected one dispatch candidate, got %+v", res)
	}
}

// [spec feature.cross_file_resolution/A21] Watched-file events in the
// client's spelling update the entry the walk created.
func TestEscapedURI_WatchedFileEvents(t *testing.T) {
	s, _, callerURI := escapedWorkspace(t)

	event := func(typ protocol.UInteger) {
		t.Helper()
		if err := s.handleDidChangeWatchedFiles(nopContext, &protocol.DidChangeWatchedFilesParams{
			Changes: []protocol.FileEvent{{URI: callerURI, Type: typ}},
		}); err != nil {
			t.Fatal(err)
		}
		s.workspaceIndex.pending.Wait()
	}

	event(protocol.FileChangeTypeDeleted)
	if n := s.workspaceIndex.FileCount(); n != 1 {
		t.Fatalf("Delete must remove the entry: expected 1 indexed file, got %d", n)
	}
	event(protocol.FileChangeTypeCreated)
	if n := s.workspaceIndex.FileCount(); n != 2 {
		t.Fatalf("Create must re-add one entry: expected 2 indexed files, got %d", n)
	}
	event(protocol.FileChangeTypeChanged)
	if n := s.workspaceIndex.FileCount(); n != 2 {
		t.Errorf("Change must replace the entry: expected 2 indexed files, got %d", n)
	}
}

// [spec feature.cross_file_resolution/A12] [spec feature.cross_file_resolution/A21]
// The open-document overlay applies to a document the client opened under
// its escaped URI.
func TestEscapedURI_OpenDocumentOverlay(t *testing.T) {
	s, defURI, callerURI := escapedWorkspace(t)
	// Two blank lines above: CalcTotal moves from 0-based line 1 to 3.
	openClientDoc(t, s, defURI, "\n\n"+renameDefScript)
	openClientDoc(t, s, callerURI, renameCallerScript)

	result, err := s.handleDefinition(nil, &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: callerURI},
			Position:     protocol.Position{Line: 1, Character: 43},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	loc, ok := result.(protocol.Location)
	if !ok {
		t.Fatalf("expected one location, got %T %+v", result, result)
	}
	if string(loc.URI) != defURI {
		t.Errorf("location URI %q, want %q", loc.URI, defURI)
	}
	if loc.Range.Start.Line != 3 {
		t.Errorf("expected the live-buffer line 3, got %d", loc.Range.Start.Line)
	}
}

// Every URI a handler reads from the client goes through canonicalURI.
// A new handler that reads params.TextDocument.URI raw fails here.
func TestHandlersCanonicalizeIncomingURIs(t *testing.T) {
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatal(err)
	}
	raw := regexp.MustCompile(`(params\.TextDocument\.URI|event\.URI|folder\.URI|\*params\.RootURI)\b`)
	wrapped := regexp.MustCompile(`canonicalURI\((params\.TextDocument\.URI|event\.URI|folder\.URI|\*params\.RootURI)\)`)
	for _, f := range files {
		if strings.HasSuffix(f, "_test.go") {
			continue
		}
		data, err := os.ReadFile(f)
		if err != nil {
			t.Fatal(err)
		}
		for i, line := range strings.Split(string(data), "\n") {
			if n := len(raw.FindAllString(line, -1)); n != len(wrapped.FindAllString(line, -1)) {
				t.Errorf("%s:%d reads a client URI without canonicalURI: %s", f, i+1, strings.TrimSpace(line))
			}
		}
	}
}

// --- Background indexing (issue #258) ---

// [spec feature.cross_file_resolution/A22] A file whose parse panics is
// skipped; the rest of the workspace is indexed and the process survives.
func TestWorkspaceIndex_PanickingFileSkipped(t *testing.T) {
	dir := t.TempDir()
	writeTestFile(t, dir, "Good1.ssl", testSSLContent)
	writeTestFile(t, dir, "Bad.ssl", testSSLContent)
	writeTestFile(t, dir, "Good2.ssl", testSSLContent)

	indexFileHook = func(uri string) {
		if strings.HasSuffix(uri, "/Bad.ssl") {
			panic("injected parse failure")
		}
	}
	t.Cleanup(func() { indexFileHook = nil })
	log := &syncBuffer{}
	indexLog = log
	t.Cleanup(func() { indexLog = os.Stderr })

	wi := NewWorkspaceIndex([]string{pathToURI(dir)})
	wi.StartBackgroundIndex()
	select {
	case <-wi.doneCh:
	case <-time.After(10 * time.Second):
		t.Fatal("background indexing timed out")
	}
	if n := wi.FileCount(); n != 2 {
		t.Errorf("expected the 2 good files indexed, got %d", n)
	}
	if got := log.String(); !strings.Contains(got, "Bad.ssl, file skipped: injected parse failure") {
		t.Errorf("expected the panic logged with the file name, got %q", got)
	}

	// The re-index path recovers too.
	wi.ReindexAsync(pathToURI(filepath.Join(dir, "Bad.ssl")))
	wi.pending.Wait()
	if n := wi.FileCount(); n != 2 {
		t.Errorf("expected 2 indexed files after a panicking re-index, got %d", n)
	}
}

// syncBuffer is a bytes.Buffer safe for concurrent writers.
type syncBuffer struct {
	mu  sync.Mutex
	buf bytes.Buffer
}

func (b *syncBuffer) Write(p []byte) (int, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.buf.Write(p)
}

func (b *syncBuffer) String() string {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.buf.String()
}

// fillWorkers occupies every background-index slot, so queued work waits
// until the returned release is called.
func fillWorkers(wi *WorkspaceIndex) (release func()) {
	for i := 0; i < cap(wi.sem); i++ {
		wi.sem <- struct{}{}
	}
	return func() {
		for i := 0; i < cap(wi.sem); i++ {
			<-wi.sem
		}
	}
}

// [spec feature.cross_file_resolution/A22] Watched-file Created/Changed
// events return without parsing: the work is queued on the background
// worker.
func TestHandleDidChangeWatchedFiles_ReindexIsAsync(t *testing.T) {
	dir := t.TempDir()
	path := writeTestFile(t, dir, "Main.ssl", testSSLContent)
	s := NewSSLServer()
	wi := NewWorkspaceIndex([]string{pathToURI(dir)})
	close(wi.doneCh)
	s.workspaceIndex = wi

	release := fillWorkers(wi)
	done := make(chan error, 1)
	go func() {
		done <- s.handleDidChangeWatchedFiles(nopContext, &protocol.DidChangeWatchedFilesParams{
			Changes: []protocol.FileEvent{{URI: pathToURI(path), Type: protocol.FileChangeTypeCreated}},
		})
	}()
	select {
	case err := <-done:
		if err != nil {
			t.Fatal(err)
		}
	case <-time.After(5 * time.Second):
		release()
		t.Fatal("the handler blocked on indexing")
	}
	if n := wi.FileCount(); n != 0 {
		t.Errorf("nothing may be indexed while the workers are busy, got %d", n)
	}
	release()
	wi.pending.Wait()
	if n := wi.FileCount(); n != 1 {
		t.Errorf("expected the queued file indexed, got %d", n)
	}
}

// [spec feature.cross_file_resolution/A22] A Delete that arrives while a
// re-index of the same file is queued or parsing wins: the file is not
// added back.
func TestWorkspaceIndex_DeleteSupersedesInFlightReindex(t *testing.T) {
	dir := t.TempDir()
	path := writeTestFile(t, dir, "Main.ssl", testSSLContent)
	uri := pathToURI(path)
	s := NewSSLServer()
	wi := NewWorkspaceIndex([]string{pathToURI(dir)})
	close(wi.doneCh)
	s.workspaceIndex = wi
	if err := wi.IndexFile(uri); err != nil {
		t.Fatal(err)
	}

	send := func(typ protocol.UInteger) {
		t.Helper()
		if err := s.handleDidChangeWatchedFiles(nopContext, &protocol.DidChangeWatchedFilesParams{
			Changes: []protocol.FileEvent{{URI: uri, Type: typ}},
		}); err != nil {
			t.Fatal(err)
		}
	}

	// Queued: Changed, then Deleted before the worker runs. The file is
	// still on disk, as it is when the parse reads it just before the
	// delete lands.
	release := fillWorkers(wi)
	send(protocol.FileChangeTypeChanged)
	send(protocol.FileChangeTypeDeleted)
	release()
	wi.pending.Wait()
	if n := wi.FileCount(); n != 0 {
		t.Errorf("a queued re-index re-added a deleted file: %d indexed", n)
	}

	// Mid-parse: the Delete lands after the parse has started.
	if err := wi.IndexFile(uri); err != nil {
		t.Fatal(err)
	}
	started := make(chan struct{})
	proceed := make(chan struct{})
	indexFileHook = func(string) {
		close(started)
		<-proceed
	}
	t.Cleanup(func() { indexFileHook = nil })
	send(protocol.FileChangeTypeChanged)
	<-started
	send(protocol.FileChangeTypeDeleted)
	close(proceed)
	wi.pending.Wait()
	if n := wi.FileCount(); n != 0 {
		t.Errorf("an in-flight re-index re-added a deleted file: %d indexed", n)
	}

	// A later Create indexes it again.
	indexFileHook = nil
	send(protocol.FileChangeTypeCreated)
	wi.pending.Wait()
	if n := wi.FileCount(); n != 1 {
		t.Errorf("expected the re-created file indexed, got %d", n)
	}
	if _, err := os.Stat(path); errors.Is(err, os.ErrNotExist) {
		t.Fatal("fixture vanished")
	}
}
