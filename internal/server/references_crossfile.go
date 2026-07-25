package server

import (
	"os"
	"regexp"
	"sort"
	"strings"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
	"starlims-lsp/internal/providers"
)

// Cross-file references orchestration (issue #125, feature.references
// A10-A14). The index discovers candidate files; open documents are scanned
// from their live parse. Reference subjects are procedures reachable through
// string dispatch — the v1 scope, in parity with go-to-definition.

// maxCrossFileReferences caps the cross-file location count per request.
const maxCrossFileReferences = 500

// crossFileProcedureReferences returns all references to the procedure
// procName defined in defURI: the definition file's own references
// (declaration handling per includeDeclaration, dotted self-sites included)
// plus every matching dispatch site across the workspace.
func (s *SSLServer) crossFileProcedureReferences(defURI, procName string, includeDeclaration bool) []providers.Location {
	content, procs, vars, ok := s.definitionFileSymbols(defURI)
	if !ok {
		return nil
	}

	// Position the same-file pass on the declaration name.
	declLine, declCol := procedureNamePosition(content, procs, procName)
	if declLine == 0 {
		return nil
	}
	locations := providers.FindReferencesWithScope(
		content, declLine, declCol, defURI, includeDeclaration, procs, vars)

	locations = append(locations, s.dispatchSiteReferences(defURI, procName, locations)...)
	return locations
}

// dispatchSiteReferences returns the dotted dispatch sites across the
// workspace (including dotted self-sites in the definition file, which the
// same-file whole-content match cannot see) that resolve to (defURI,
// procName). existing is used to dedupe against already-collected
// locations in defURI.
func (s *SSLServer) dispatchSiteReferences(defURI, procName string, existing []providers.Location) []providers.Location {
	if s.workspaceIndex == nil {
		return nil
	}
	resolver := liveResolver{s}

	type locKey struct {
		uri        string
		line, char int
	}
	seen := make(map[locKey]struct{}, len(existing))
	for _, loc := range existing {
		seen[locKey{loc.URI, loc.Range.Start.Line, loc.Range.Start.Character}] = struct{}{}
	}

	var out []providers.Location
	add := func(uri string, line, startChar, endChar int) {
		if len(out) >= maxCrossFileReferences {
			return
		}
		k := locKey{uri, line, startChar}
		if _, dup := seen[k]; dup {
			return
		}
		seen[k] = struct{}{}
		out = append(out, providers.Location{
			URI: uri,
			Range: providers.Range{
				Start: providers.Position{Line: line, Character: startChar},
				End:   providers.Position{Line: line, Character: endChar},
			},
		})
	}

	matches := func(raw string) bool {
		parts := strings.Split(raw, ".")
		if len(parts) < 2 || !strings.EqualFold(parts[len(parts)-1], procName) {
			return false
		}
		matched, _ := resolver.siteTargetsDefinition(raw, defURI, procName)
		return matched
	}

	// Phase 1: open documents from the live parse (deterministic order).
	openURIs := s.documents.OpenURIs()
	openList := make([]string, 0, len(openURIs))
	for uri := range openURIs {
		openList = append(openList, uri)
	}
	sort.Strings(openList)
	for _, uri := range openList {
		cache := s.documents.ParseDocument(uri, s.documentVersion[uri])
		for _, site := range providers.ExtractCallSites(cache.Tokens) {
			if site.Kind != providers.CallDispatch || !matches(site.Raw) {
				continue
			}
			add(uri, site.Range.Start.Line, site.Range.Start.Character, site.Range.End.Character)
		}
	}

	// Phase 2: indexed files, skipping open URIs.
	for _, cand := range s.workspaceIndex.CallSitesFor(procName, openURIs) {
		if !matches(cand.Site.Raw) {
			continue
		}
		add(cand.URI, cand.Site.Line, cand.Site.StartChar, cand.Site.EndChar)
	}

	return out
}

// definitionFileSymbols returns the definition file's current content and
// parsed symbols — the live buffer for open documents, disk otherwise.
func (s *SSLServer) definitionFileSymbols(defURI string) (string, []parser.ProcedureInfo, []parser.VariableInfo, bool) {
	if content, open := s.documents.GetDocument(defURI); open {
		cache := s.documents.ParseDocument(defURI, s.documentVersion[defURI])
		return content, cache.Procedures, cache.Variables, true
	}
	path := uriToPath(defURI)
	if path == "" {
		return "", nil, nil, false
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return "", nil, nil, false
	}
	content := string(data)
	tokens := lexer.NewLexer(content).Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	return content, p.ExtractProcedures(ast), p.ExtractVariables(ast), true
}

// procedureSubjectAt reports whether the word at the cursor line genuinely
// names a procedure subject of this document — and not a local variable or
// parameter that shadows the procedure's name inside the cursor's enclosing
// procedure (v0.14.0 review F3: a shadowed cursor must keep the scope-aware
// single-file path, or a local-variable rename silently mutates other
// files). The declaration line itself is always the procedure.
func procedureSubjectAt(cache *DocumentCache, word string, line int) (string, bool) {
	var subject string
	declLine := 0
	for _, proc := range cache.Procedures {
		if strings.EqualFold(proc.Name, word) {
			subject = proc.Name
			declLine = proc.StartLine
			break
		}
	}
	if subject == "" {
		return "", false
	}
	if line == declLine {
		return subject, true
	}
	if cursorProc := parser.FindProcedureAtLine(cache.Procedures, line); cursorProc != nil {
		for _, v := range cache.Variables {
			if strings.EqualFold(v.Name, word) &&
				(v.Scope == parser.ScopeLocal || v.Scope == parser.ScopeParameter) &&
				v.Line >= cursorProc.StartLine && v.Line <= cursorProc.EndLine {
				return "", false
			}
		}
	}
	return subject, true
}

// procedureNamePosition locates the (1-based) line and column of the
// procedure's name on its :PROCEDURE line; (0, 0) when not found. The name
// is matched as a whole word AFTER the :PROCEDURE keyword — a plain
// substring search would land inside the keyword itself for names like
// "Proc" and hand the reference search the word "PROCEDURE" (v0.14.0
// review F1).
func procedureNamePosition(content string, procs []parser.ProcedureInfo, procName string) (int, int) {
	for _, proc := range procs {
		if !strings.EqualFold(proc.Name, procName) {
			continue
		}
		lines := strings.Split(content, "\n")
		if proc.StartLine < 1 || proc.StartLine > len(lines) {
			return 0, 0
		}
		lineText := lines[proc.StartLine-1]
		searchFrom := 0
		if kw := strings.Index(strings.ToLower(lineText), ":procedure"); kw >= 0 {
			searchFrom = kw + len(":procedure")
		}
		re := regexp.MustCompile(`(?i)\b` + regexp.QuoteMeta(proc.Name) + `\b`)
		if m := re.FindStringIndex(lineText[searchFrom:]); m != nil {
			return proc.StartLine, searchFrom + m[0] + 1
		}
		return proc.StartLine, 1
	}
	return 0, 0
}
