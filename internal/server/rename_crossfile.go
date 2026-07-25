package server

import (
	"os"
	"sort"
	"strings"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/providers"
)

// Cross-file rename orchestration (issue #125 Phase B, feature.rename
// A9-A15). The write side is conservative where references are liberal:
// only unambiguously-resolving sites are edited (D1), class-file
// procedures refuse the cross-file path entirely (D8), and every edit is
// computed from the file's CURRENT content — open buffers from the live
// parse, closed files re-read and re-extracted from disk at request time —
// never from indexed positions (stale-index safety).

// crossFileRename renames the procedure procName defined in defURI to
// newName across the workspace. Returns nil when the rename is refused
// (invalid name, class-file definition, unresolvable declaration).
func (s *SSLServer) crossFileRename(defURI, procName, newName string) map[string][]providers.TextEdit {
	if !providers.IsValidNewName(newName) {
		return nil
	}
	// D8: procedures in class files are callable cross-file through
	// obj:Method()/Base:Method() bare identifiers this design cannot see —
	// a workspace rename would silently break them. Refused; the caller
	// may still fall back to the same-file path for in-file subjects.
	if s.isClassFile(defURI) {
		return nil
	}

	content, procs, vars, ok := s.definitionFileSymbols(defURI)
	if !ok {
		return nil
	}
	declLine, declCol := procedureNamePosition(content, procs, procName)
	if declLine == 0 {
		return nil
	}

	// Definition-file edits: declaration, identifier uses, and 1-part
	// dispatch strings, via the existing scope-aware provider.
	result := providers.Rename(content, declLine, declCol, newName, defURI, procs, vars)
	if result == nil {
		return nil
	}
	changes := result.Changes

	// Dispatch-site edits: dotted sites everywhere (including dotted
	// self-sites in the definition file, which the provider's whole-content
	// match cannot see), last segment only, unambiguous resolutions only.
	for uri, edits := range s.dispatchSiteEdits(defURI, procName, newName) {
		changes[uri] = append(changes[uri], edits...)
	}
	return changes
}

// dispatchSiteEdits computes last-segment rename edits for every dispatch
// site across the workspace that unambiguously resolves to (defURI,
// procName). Sites are re-extracted from current content per file.
func (s *SSLServer) dispatchSiteEdits(defURI, procName, newName string) map[string][]providers.TextEdit {
	if s.workspaceIndex == nil {
		return nil
	}
	resolver := liveResolver{s}

	// Candidate files: every open document plus every indexed file holding
	// a candidate site. The index only nominates files — the sites
	// themselves are re-extracted fresh below.
	openURIs := s.documents.OpenURIs()
	candidates := make(map[string]struct{}, len(openURIs)+4)
	for uri := range openURIs {
		candidates[uri] = struct{}{}
	}
	for _, cand := range s.workspaceIndex.CallSitesFor(procName, openURIs) {
		candidates[cand.URI] = struct{}{}
	}

	uris := make([]string, 0, len(candidates))
	for uri := range candidates {
		uris = append(uris, uri)
	}
	sort.Strings(uris)

	edits := make(map[string][]providers.TextEdit)
	for _, uri := range uris {
		var tokens []lexer.Token
		if _, open := openURIs[uri]; open {
			tokens = s.documents.ParseDocument(uri, s.documentVersion[uri]).Tokens
		} else {
			path := uriToPath(uri)
			if path == "" {
				continue
			}
			data, err := os.ReadFile(path)
			if err != nil {
				continue
			}
			tokens = lexer.NewLexer(string(data)).Tokenize()
		}

		for _, site := range providers.ExtractCallSites(tokens) {
			if site.Kind != providers.CallDispatch {
				continue
			}
			parts := strings.Split(site.Raw, ".")
			if len(parts) < 2 || !strings.EqualFold(parts[len(parts)-1], procName) {
				continue
			}
			// D1: the write side edits only sites whose resolution is a
			// single candidate equal to the renamed definition.
			if _, unambiguous := resolver.siteTargetsDefinition(site.Raw, defURI, procName); !unambiguous {
				continue
			}
			lastSeg := parts[len(parts)-1]
			segStart := site.Range.End.Character - len(lastSeg)
			edits[uri] = append(edits[uri], providers.TextEdit{
				Range: providers.Range{
					Start: providers.Position{Line: site.Range.Start.Line, Character: segStart},
					End:   providers.Position{Line: site.Range.Start.Line, Character: site.Range.End.Character},
				},
				NewText: newName,
			})
		}
	}
	return edits
}

// isClassFile reports whether defURI is a :CLASS script — from the live
// tokens for open documents, the index otherwise.
func (s *SSLServer) isClassFile(defURI string) bool {
	if _, open := s.documents.OpenURIs()[defURI]; open {
		cache := s.documents.ParseDocument(defURI, s.documentVersion[defURI])
		return isClassFileFromTokens(cache.Tokens)
	}
	if s.workspaceIndex != nil {
		if fs, ok := s.workspaceIndex.FileSymbolsFor(defURI); ok {
			return fs.IsClass
		}
	}
	return false
}
