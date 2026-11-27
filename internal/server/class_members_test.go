package server

import (
	"sort"
	"strings"
	"testing"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// Class members after `Me:` and `Base:` (issue #380,
// catalog/features/completion.md "Class members").

// counterClass is the class of issue #380, declared under the given name.
func counterClass(name string) string {
	return ":CLASS " + name + ";\n" +
		":DECLARE nCount;\n" +
		"\n" +
		":PROCEDURE Bump;\n" +
		"\t:PARAMETERS nBy;\n" +
		"\tMe:\n" +
		"\t:RETURN nCount;\n" +
		":ENDPROC;\n" +
		"\n" +
		":PROCEDURE Current;\n" +
		"\t:RETURN nCount;\n" +
		":ENDPROC;\n"
}

// completeAfter requests completion in the open document uri directly
// after the first occurrence of marker (`Me:` or `Base:`), as the typed `:`
// (trigger true) or as an explicit invocation.
func completeAfter(t *testing.T, s *SSLServer, uri, marker string, trigger bool) []protocol.CompletionItem {
	t.Helper()
	content, ok := s.documents.GetDocument(uri)
	if !ok {
		t.Fatalf("document %s is not open", uri)
	}
	at := strings.Index(content, marker)
	if at < 0 {
		t.Fatalf("marker %q not in document", marker)
	}
	before := content[:at+len(marker)]
	line := strings.Count(before, "\n")
	char := utf16Len(before[strings.LastIndex(before, "\n")+1:])

	params := &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: uri},
			Position:     protocol.Position{Line: uint32(line), Character: char},
		},
	}
	if trigger {
		colon := ":"
		params.Context = &protocol.CompletionContext{
			TriggerKind:      protocol.CompletionTriggerKindTriggerCharacter,
			TriggerCharacter: &colon,
		}
	}
	result, err := s.handleCompletion(nil, params)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	return items
}

func sortedLabels(items []protocol.CompletionItem) []string {
	labels := make([]string, 0, len(items))
	for _, item := range items {
		labels = append(labels, item.Label)
	}
	sort.Strings(labels)
	return labels
}

func wantLabels(t *testing.T, what string, items []protocol.CompletionItem, want ...string) {
	t.Helper()
	sort.Strings(want)
	if got := sortedLabels(items); strings.Join(got, ",") != strings.Join(want, ",") {
		t.Errorf("%s: got %v, want %v", what, got, want)
	}
}

func openDocument(s *SSLServer, uri, content string) {
	s.documents.SetDocument(uri, content, 1)
	s.documentVersion[uri] = 1
}

// [spec feature.completion/A4] — `Me:` in a class the workspace declares
// lists that class's own fields and methods, on the `:` trigger and on
// explicit invocation alike, and nothing else (issue #380).
func TestHandleCompletion_MeColonListsOwnClassMembers(t *testing.T) {
	for _, trigger := range []bool{true, false} {
		s := newTestServerWithDocument(counterClass("Counter"))
		items := completeAfter(t, s, testURI, "Me:", trigger)
		wantLabels(t, "Me: in Counter", items, "nCount", "Bump", "Current")

		field := findCompletionItem(items, "nCount")
		if field == nil || field.Kind == nil || *field.Kind != protocol.CompletionItemKindField {
			t.Errorf("nCount must be a field item, got %+v", field)
		} else if field.InsertText == nil || *field.InsertText != "nCount" {
			t.Errorf("nCount must insert its name, got %v", field.InsertText)
		}
		for label, insert := range map[string]string{"Bump": "Bump(${1:nBy})", "Current": "Current()"} {
			item := findCompletionItem(items, label)
			if item == nil || item.Kind == nil || *item.Kind != protocol.CompletionItemKindMethod {
				t.Errorf("%s must be a method item, got %+v", label, item)
				continue
			}
			if item.InsertText == nil || *item.InsertText != insert {
				t.Errorf("%s must insert %q, got %v", label, insert, item.InsertText)
			}
			if item.InsertTextFormat == nil || *item.InsertTextFormat != protocol.InsertTextFormatSnippet {
				t.Errorf("%s must be a snippet", label)
			}
		}
	}
}

// [spec feature.completion/A15] — the class is the one the document
// declares, whatever its name: `:CLASS Email` lists its own members, not
// the built-in Email class's. The old A4 test used this name and passed
// with or without the class's members (issue #380).
func TestHandleCompletion_MeColonClassNamedLikeBuiltin(t *testing.T) {
	s := newTestServerWithDocument(counterClass("Email"))
	items := completeAfter(t, s, testURI, "Me:", true)
	wantLabels(t, "Me: in a class named Email", items, "nCount", "Bump", "Current")
	for _, builtin := range []string{"Subject", "Send", "Attachments"} {
		if containsCompletionLabel(items, builtin) {
			t.Errorf("built-in Email member %q must not be offered for the document's own class", builtin)
		}
	}
}

const (
	rootClass = ":CLASS Root;\n" +
		"\n" +
		":PROCEDURE Audit;\n" +
		"\t:PARAMETERS sEvent;\n" +
		":ENDPROC;\n"

	parentClass = ":CLASS Parent;\n" +
		":INHERIT Lab.Root;\n" +
		":DECLARE sName;\n" +
		"\n" +
		":PROCEDURE Constructor;\n" +
		"\t:PARAMETERS sInitial;\n" +
		"\tsName := sInitial;\n" +
		":ENDPROC;\n" +
		"\n" +
		":PROCEDURE Describe;\n" +
		"\t:RETURN sName;\n" +
		":ENDPROC;\n" +
		"\n" +
		":PROCEDURE Bump;\n" +
		"\t:PARAMETERS nStep, bQuiet;\n" +
		"\t:RETURN nStep;\n" +
		":ENDPROC;\n"

	childClass = ":CLASS Child;\n" +
		":INHERIT Lab.Parent;\n" +
		":DECLARE nCount;\n" +
		"\n" +
		":PROCEDURE Bump;\n" +
		"\t:PARAMETERS nBy;\n" +
		"\tMe:\n" +
		"\tBase:\n" +
		":ENDPROC;\n"
)

func detailOf(items []protocol.CompletionItem, label string) string {
	if item := findCompletionItem(items, label); item != nil && item.Detail != nil {
		return *item.Detail
	}
	return ""
}

// [spec feature.completion/A16] — `Me:` adds what the class inherits, up
// the :INHERIT chain, the class's own declaration winning a shared name;
// `Base:` lists the inherited members only.
func TestHandleCompletion_InheritedClassMembers(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Server Scripts/Lab/Root.srvscr", rootClass)
	writeAndIndex(t, wi, dir, "Server Scripts/Lab/Parent.srvscr", parentClass)
	childURI := writeAndIndex(t, wi, dir, "Server Scripts/Lab/Child.srvscr", childClass)
	openDocument(s, childURI, childClass)

	me := completeAfter(t, s, childURI, "Me:", true)
	wantLabels(t, "Me: in Child", me, "nCount", "Bump", "sName", "Describe", "Audit")
	if got := detailOf(me, "Bump"); got != "Child method" {
		t.Errorf("Bump after Me: must be Child's own, got detail %q", got)
	}
	if insert := findCompletionItem(me, "Bump").InsertText; insert == nil || *insert != "Bump(${1:nBy})" {
		t.Errorf("Bump after Me: must insert Child's signature, got %v", insert)
	}
	for label, detail := range map[string]string{
		"sName":    "Lab.Parent field (inherited)",
		"Describe": "Lab.Parent method (inherited)",
		"Audit":    "Lab.Root method (inherited)",
	} {
		if got := detailOf(me, label); got != detail {
			t.Errorf("%s after Me:: detail %q, want %q", label, got, detail)
		}
	}

	base := completeAfter(t, s, childURI, "Base:", true)
	wantLabels(t, "Base: in Child", base, "sName", "Describe", "Bump", "Constructor", "Audit")
	if got := detailOf(base, "Bump"); got != "Lab.Parent method (inherited)" {
		t.Errorf("Bump after Base: must be Parent's, got detail %q", got)
	}
	if insert := findCompletionItem(base, "Bump").InsertText; insert == nil || *insert != "Bump(${1:nStep}, ${2:bQuiet})" {
		t.Errorf("Bump after Base: must insert Parent's signature, got %v", insert)
	}
	if insert := findCompletionItem(base, "Constructor").InsertText; insert == nil || *insert != "Constructor(${1:sInitial})" {
		t.Errorf("Constructor after Base: must insert the parent constructor's call, got %v", insert)
	}
}

// [spec feature.completion/A16] — classes that inherit each other in a
// circle end the walk; nothing is listed twice.
func TestHandleCompletion_InheritanceCycleEnds(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Server Scripts/Lab/Ping.srvscr", ":CLASS Ping;\n:INHERIT Lab.Pong;\n:PROCEDURE FromPing;\n\tBase:\n:ENDPROC;\n")
	pongURI := writeAndIndex(t, wi, dir, "Server Scripts/Lab/Pong.srvscr", ":CLASS Pong;\n:INHERIT Lab.Ping;\n:PROCEDURE FromPong;\n\tMe:\n:ENDPROC;\n")
	openDocument(s, pongURI, ":CLASS Pong;\n:INHERIT Lab.Ping;\n:PROCEDURE FromPong;\n\tMe:\n:ENDPROC;\n")

	wantLabels(t, "Me: in a class of an inheritance circle", completeAfter(t, s, pongURI, "Me:", true), "FromPong", "FromPing")
}

// [spec feature.completion/A17] — a parent that resolves to nothing, or to
// more than one file, contributes nothing: `Me:` keeps the class's own
// members and `Base:` offers no member at all.
func TestHandleCompletion_UnresolvedOrAmbiguousParent(t *testing.T) {
	// No workspace index at all.
	s := newTestServerWithDocument(childClass)
	wantLabels(t, "Me: without a workspace", completeAfter(t, s, testURI, "Me:", true), "nCount", "Bump")
	wantLabels(t, "Base: without a workspace", completeAfter(t, s, testURI, "Base:", true))

	// An index that does not hold the parent.
	s = NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	childURI := writeAndIndex(t, wi, dir, "Server Scripts/Lab/Child.srvscr", childClass)
	openDocument(s, childURI, childClass)
	wantLabels(t, "Me: with an unresolved parent", completeAfter(t, s, childURI, "Me:", true), "nCount", "Bump")
	wantLabels(t, "Base: with an unresolved parent", completeAfter(t, s, childURI, "Base:", true))

	// Two class files answer to Lab.Parent.
	writeAndIndex(t, wi, dir, "Server Scripts/Lab/Parent.srvscr", parentClass)
	writeAndIndex(t, wi, dir, "copy/Server Scripts/Lab/Parent.srvscr", ":CLASS Parent;\n:PROCEDURE OnlyInTheCopy;\n:ENDPROC;\n")
	wantLabels(t, "Me: with an ambiguous parent", completeAfter(t, s, childURI, "Me:", true), "nCount", "Bump")
	wantLabels(t, "Base: with an ambiguous parent", completeAfter(t, s, childURI, "Base:", true))
}

// [spec feature.completion/A18] — the procedures of a script the class
// file includes are reached as Me:Name(...): `Me:` lists them, without the
// ones a visibility annotation in force hides; `Base:` does not.
func TestHandleCompletion_IncludedProcedureMembers(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Server Scripts/Lab/Helpers.srvscr",
		":DECLARE sShared;\n\n:PROCEDURE Normalize;\n\t:PARAMETERS sText;\n\t:RETURN sText;\n:ENDPROC;\n\n/*@private;\n:PROCEDURE Internal;\n:ENDPROC;\n")
	class := ":CLASS Report;\n:INCLUDE Lab.Helpers;\n:DECLARE sTitle;\n\n:PROCEDURE Render;\n\tMe:\n\tBase:\n:ENDPROC;\n"
	classURI := writeAndIndex(t, wi, dir, "Server Scripts/Lab/Report.srvscr", class)
	openDocument(s, classURI, class)

	me := completeAfter(t, s, classURI, "Me:", true)
	wantLabels(t, "Me: in a class with an include", me, "sTitle", "Render", "Normalize")
	item := findCompletionItem(me, "Normalize")
	if item == nil || item.Kind == nil || *item.Kind != protocol.CompletionItemKindMethod {
		t.Fatalf("Normalize must be a method item, got %+v", item)
	}
	if got := detailOf(me, "Normalize"); got != "Lab.Helpers procedure (included)" {
		t.Errorf("Normalize: detail %q, want it marked as included", got)
	}
	if item.InsertText == nil || *item.InsertText != "Normalize(${1:sText})" {
		t.Errorf("Normalize must insert its call, got %v", item.InsertText)
	}

	wantLabels(t, "Base: in a class with an include and no parent", completeAfter(t, s, classURI, "Base:", true))
}

// [spec feature.completion/A19] — an open parent is read from its buffer:
// a method it has only there is offered in the child.
func TestHandleCompletion_InheritedMembersFromOpenBuffer(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Server Scripts/Lab/Root.srvscr", rootClass)
	parentURI := writeAndIndex(t, wi, dir, "Server Scripts/Lab/Parent.srvscr", parentClass)
	childURI := writeAndIndex(t, wi, dir, "Server Scripts/Lab/Child.srvscr", childClass)
	openDocument(s, childURI, childClass)
	openDocument(s, parentURI, parentClass+"\n:PROCEDURE Unsaved;\n:ENDPROC;\n")

	if me := completeAfter(t, s, childURI, "Me:", true); !containsCompletionLabel(me, "Unsaved") {
		t.Errorf("Me: must list the parent's unsaved method, got %v", sortedLabels(me))
	}
	if base := completeAfter(t, s, childURI, "Base:", true); !containsCompletionLabel(base, "Unsaved") {
		t.Errorf("Base: must list the parent's unsaved method, got %v", sortedLabels(base))
	}
}
