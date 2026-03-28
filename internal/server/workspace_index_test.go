package server

import (
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"
)

const testSSLContent = `:DECLARE sName;

:PROCEDURE GetName;
:PARAMETERS sInput;
:DECLARE sResult;
sResult := sInput;
:RETURN sResult;
:ENDPROC;

:PROCEDURE SetValue;
:PARAMETERS sKey, sVal;
:ENDPROC;
`

const testClassContent = `:CLASS MyClass;
:INHERIT BaseClass;
:DECLARE sField;

:PROCEDURE GetField;
:RETURN Me:sField;
:ENDPROC;

:PROCEDURE Constructor;
Me:sField := "";
:ENDPROC;
`

func writeTestFile(t *testing.T, dir, name, content string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestWorkspaceIndex_IndexFile(t *testing.T) {
	dir := t.TempDir()
	path := writeTestFile(t, dir, "test.srvscr", testSSLContent)
	uri := pathToURI(path)

	wi := NewWorkspaceIndex(nil)
	close(wi.doneCh) // no background indexing

	if err := wi.IndexFile(uri); err != nil {
		t.Fatal(err)
	}

	if wi.FileCount() != 1 {
		t.Fatalf("expected 1 file, got %d", wi.FileCount())
	}

	wi.mu.RLock()
	fs := wi.files[uri]
	wi.mu.RUnlock()

	if fs == nil {
		t.Fatal("expected file symbols to be stored")
	}

	if len(fs.Procedures) != 2 {
		t.Fatalf("expected 2 procedures, got %d", len(fs.Procedures))
	}

	if fs.Procedures[0].Name != "GetName" {
		t.Errorf("expected first procedure 'GetName', got %q", fs.Procedures[0].Name)
	}
	if fs.Procedures[1].Name != "SetValue" {
		t.Errorf("expected second procedure 'SetValue', got %q", fs.Procedures[1].Name)
	}

	if len(fs.Procedures[0].Parameters) != 1 || fs.Procedures[0].Parameters[0] != "sInput" {
		t.Errorf("expected GetName params [sInput], got %v", fs.Procedures[0].Parameters)
	}

	if fs.IsClass {
		t.Error("expected IsClass to be false for non-class file")
	}
}

func TestWorkspaceIndex_IndexFile_Class(t *testing.T) {
	dir := t.TempDir()
	path := writeTestFile(t, dir, "myclass.srvscr", testClassContent)
	uri := pathToURI(path)

	wi := NewWorkspaceIndex(nil)
	close(wi.doneCh)

	if err := wi.IndexFile(uri); err != nil {
		t.Fatal(err)
	}

	wi.mu.RLock()
	fs := wi.files[uri]
	wi.mu.RUnlock()

	if !fs.IsClass {
		t.Error("expected IsClass to be true for class file")
	}

	if len(fs.Procedures) != 2 {
		t.Fatalf("expected 2 procedures, got %d", len(fs.Procedures))
	}
}

func TestWorkspaceIndex_SearchSymbols(t *testing.T) {
	dir := t.TempDir()

	path1 := writeTestFile(t, dir, "script1.srvscr", testSSLContent)
	path2 := writeTestFile(t, dir, "script2.srvscr", `:PROCEDURE FindItems;
:ENDPROC;

:PROCEDURE DeleteItem;
:ENDPROC;
`)

	wi := NewWorkspaceIndex(nil)
	close(wi.doneCh)

	wi.IndexFile(pathToURI(path1))
	wi.IndexFile(pathToURI(path2))

	// Search for all
	results := wi.SearchSymbols("", nil)
	if len(results) != 4 {
		t.Fatalf("expected 4 results for empty query, got %d", len(results))
	}

	// Search with filter
	results = wi.SearchSymbols("Get", nil)
	if len(results) != 1 {
		t.Fatalf("expected 1 result for 'Get', got %d", len(results))
	}
	if results[0].Name != "GetName" {
		t.Errorf("expected 'GetName', got %q", results[0].Name)
	}

	// Case insensitive
	results = wi.SearchSymbols("getname", nil)
	if len(results) != 1 {
		t.Fatalf("expected 1 result for case-insensitive search, got %d", len(results))
	}

	// No match
	results = wi.SearchSymbols("NonExistent", nil)
	if len(results) != 0 {
		t.Fatalf("expected 0 results, got %d", len(results))
	}

	// Search for "Item" should match FindItems and DeleteItem
	results = wi.SearchSymbols("Item", nil)
	if len(results) != 2 {
		t.Fatalf("expected 2 results for 'Item', got %d", len(results))
	}
}

func TestWorkspaceIndex_SearchSkipsOpenURIs(t *testing.T) {
	dir := t.TempDir()

	path1 := writeTestFile(t, dir, "open.srvscr", testSSLContent)
	path2 := writeTestFile(t, dir, "closed.srvscr", `:PROCEDURE Helper;
:ENDPROC;
`)

	uri1 := pathToURI(path1)
	uri2 := pathToURI(path2)

	wi := NewWorkspaceIndex(nil)
	close(wi.doneCh)

	wi.IndexFile(uri1)
	wi.IndexFile(uri2)

	// With open URI set, skip open file
	openURIs := map[string]struct{}{uri1: {}}
	results := wi.SearchSymbols("", openURIs)

	// Should only get results from closed.srvscr (1 procedure)
	if len(results) != 1 {
		t.Fatalf("expected 1 result (open file skipped), got %d", len(results))
	}
	if results[0].Name != "Helper" {
		t.Errorf("expected 'Helper', got %q", results[0].Name)
	}
}

func TestWorkspaceIndex_RemoveFile(t *testing.T) {
	dir := t.TempDir()
	path := writeTestFile(t, dir, "test.srvscr", testSSLContent)
	uri := pathToURI(path)

	wi := NewWorkspaceIndex(nil)
	close(wi.doneCh)

	wi.IndexFile(uri)
	if wi.FileCount() != 1 {
		t.Fatalf("expected 1 file, got %d", wi.FileCount())
	}

	wi.RemoveFile(uri)
	if wi.FileCount() != 0 {
		t.Fatalf("expected 0 files after removal, got %d", wi.FileCount())
	}

	results := wi.SearchSymbols("", nil)
	if len(results) != 0 {
		t.Fatalf("expected 0 results after removal, got %d", len(results))
	}
}

func TestWorkspaceIndex_BackgroundIndex(t *testing.T) {
	dir := t.TempDir()

	// Create subdirectories mimicking a STARLIMS workspace
	subdir := filepath.Join(dir, "Server Scripts", "TEST_CATEGORY")
	if err := os.MkdirAll(subdir, 0755); err != nil {
		t.Fatal(err)
	}

	writeTestFile(t, subdir, "SCRIPT1.srvscr", testSSLContent)
	writeTestFile(t, subdir, "SCRIPT2.srvscr", `:PROCEDURE RunTest;
:ENDPROC;
`)
	// Non-SSL file should be ignored
	writeTestFile(t, subdir, "notes.txt", "not an SSL file")

	wi := NewWorkspaceIndex([]string{pathToURI(dir)})
	wi.StartBackgroundIndex()

	// Wait for indexing to complete (with timeout)
	select {
	case <-wi.doneCh:
	case <-time.After(10 * time.Second):
		t.Fatal("background indexing timed out")
	}

	if wi.IsIndexing() {
		t.Error("expected indexing to be complete")
	}

	if wi.FileCount() != 2 {
		t.Fatalf("expected 2 indexed files, got %d", wi.FileCount())
	}

	results := wi.SearchSymbols("", nil)
	if len(results) != 3 { // GetName, SetValue, RunTest
		t.Fatalf("expected 3 procedures across 2 files, got %d", len(results))
	}
}

func TestWorkspaceIndex_BackgroundIndex_MultipleExtensions(t *testing.T) {
	dir := t.TempDir()

	writeTestFile(t, dir, "a.srvscr", `:PROCEDURE ProcA; :ENDPROC;`)
	writeTestFile(t, dir, "b.ssl", `:PROCEDURE ProcB; :ENDPROC;`)
	writeTestFile(t, dir, "c.ssl.txt", `:PROCEDURE ProcC; :ENDPROC;`)
	writeTestFile(t, dir, "d.ds", `:PROCEDURE ProcD; :ENDPROC;`)
	writeTestFile(t, dir, "e.ds.txt", `:PROCEDURE ProcE; :ENDPROC;`)
	writeTestFile(t, dir, "f.go", `package main`) // should be ignored

	wi := NewWorkspaceIndex([]string{pathToURI(dir)})
	wi.StartBackgroundIndex()

	select {
	case <-wi.doneCh:
	case <-time.After(10 * time.Second):
		t.Fatal("background indexing timed out")
	}

	if wi.FileCount() != 5 {
		t.Fatalf("expected 5 indexed files (all SSL extensions), got %d", wi.FileCount())
	}
}

func TestWorkspaceIndex_ConcurrentAccess(t *testing.T) {
	dir := t.TempDir()

	// Create several files
	for i := 0; i < 10; i++ {
		name := filepath.Join(dir, "script"+string(rune('A'+i))+".srvscr")
		os.WriteFile(name, []byte(`:PROCEDURE Proc`+string(rune('A'+i))+`;
:ENDPROC;
`), 0644)
	}

	wi := NewWorkspaceIndex(nil)
	close(wi.doneCh)

	var wg sync.WaitGroup

	// Concurrent indexing
	for i := 0; i < 10; i++ {
		wg.Add(1)
		go func(idx int) {
			defer wg.Done()
			name := filepath.Join(dir, "script"+string(rune('A'+idx))+".srvscr")
			wi.IndexFile(pathToURI(name))
		}(i)
	}

	// Concurrent searching while indexing
	for i := 0; i < 5; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			wi.SearchSymbols("Proc", nil)
		}()
	}

	wg.Wait()

	if wi.FileCount() != 10 {
		t.Fatalf("expected 10 files after concurrent indexing, got %d", wi.FileCount())
	}
}

func TestWorkspaceIndex_ClassSymbolKind(t *testing.T) {
	dir := t.TempDir()

	writeTestFile(t, dir, "script.srvscr", testSSLContent)
	writeTestFile(t, dir, "class.srvscr", testClassContent)

	wi := NewWorkspaceIndex(nil)
	close(wi.doneCh)

	wi.IndexFile(pathToURI(filepath.Join(dir, "script.srvscr")))
	wi.IndexFile(pathToURI(filepath.Join(dir, "class.srvscr")))

	results := wi.SearchSymbols("", nil)

	functionCount := 0
	methodCount := 0
	for _, r := range results {
		switch r.Kind {
		case 12: // SymbolKindFunction
			functionCount++
		case 6: // SymbolKindMethod
			methodCount++
		}
	}

	if functionCount != 2 {
		t.Errorf("expected 2 functions, got %d", functionCount)
	}
	if methodCount != 2 {
		t.Errorf("expected 2 methods (from class file), got %d", methodCount)
	}
}

func TestURIPathConversion(t *testing.T) {
	tests := []struct {
		path string
	}{
		{"/home/user/project/test.srvscr"},
		{"/tmp/Server Scripts/CATEGORY/SCRIPT.srvscr"},
	}

	for _, tt := range tests {
		uri := pathToURI(tt.path)
		if !isValidFileURI(uri) {
			t.Errorf("pathToURI(%q) = %q, not a valid file URI", tt.path, uri)
		}

		roundTripped := uriToPath(uri)
		if roundTripped != tt.path {
			t.Errorf("round-trip failed: %q -> %q -> %q", tt.path, uri, roundTripped)
		}
	}
}

func TestURIToPath_NonFileURI(t *testing.T) {
	// Plain path should be returned as-is
	path := uriToPath("/some/path")
	if path != "/some/path" {
		t.Errorf("expected plain path returned as-is, got %q", path)
	}
}

func isValidFileURI(uri string) bool {
	return len(uri) > 7 && uri[:7] == "file://"
}

func TestIsSSLFile(t *testing.T) {
	tests := []struct {
		path string
		want bool
	}{
		{"test.srvscr", true},
		{"test.ssl", true},
		{"test.ssl.txt", true},
		{"test.ds", true},
		{"test.ds.txt", true},
		{"test.go", false},
		{"test.txt", false},
		{"TEST.SRVSCR", true}, // case-insensitive
		{"test.SSL", true},
	}

	for _, tt := range tests {
		if got := isSSLFile(tt.path); got != tt.want {
			t.Errorf("isSSLFile(%q) = %v, want %v", tt.path, got, tt.want)
		}
	}
}
