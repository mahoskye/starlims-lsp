package server

import (
	"strings"

	"starlims-lsp/internal/parser"
	"starlims-lsp/internal/providers"
)

// liveResolver implements providers.WorkspaceResolver on top of the
// workspace index, overlaying positions from open documents so unsaved
// edits never produce stale jump targets
// (spec feature.cross_file_resolution/A12). Nil-index-safe: without a
// workspace index (no workspace root), every resolution is empty.
type liveResolver struct {
	s *SSLServer
}

func (r liveResolver) ResolveDispatch(target string) []providers.ResolvedTarget {
	if r.s.workspaceIndex == nil {
		return nil
	}
	return r.overlay(r.s.workspaceIndex.ResolveDispatchTarget(target))
}

func (r liveResolver) ResolveInclude(target string) []providers.ResolvedTarget {
	if r.s.workspaceIndex == nil {
		return nil
	}
	return r.overlay(r.s.workspaceIndex.ResolveIncludeTarget(target))
}

// overlay converts index resolutions to provider targets, re-deriving
// positions from the live document cache for URIs that are currently open.
func (r liveResolver) overlay(resolutions []IndexResolution) []providers.ResolvedTarget {
	if len(resolutions) == 0 {
		return nil
	}
	openURIs := r.s.documents.OpenURIs()

	out := make([]providers.ResolvedTarget, 0, len(resolutions))
	for _, res := range resolutions {
		target := providers.ResolvedTarget{URI: res.URI, Line: res.Line, Kind: providers.ResolvedScriptEntry}
		if !res.IsEntry {
			target.Kind = providers.ResolvedProcedure
		}

		if _, open := openURIs[res.URI]; open {
			cache := r.s.documents.ParseDocument(res.URI, r.s.documentVersion[res.URI])
			if res.IsEntry {
				_, line := parser.NewParser(cache.Tokens).ExtractTopLevelParameters(cache.AST)
				if line > 0 {
					target.Line = line - 1
				} else {
					target.Line = 0
				}
			} else {
				found := false
				for _, proc := range cache.Procedures {
					if strings.EqualFold(proc.Name, res.ProcName) {
						target.Line = proc.StartLine - 1
						found = true
						break
					}
				}
				// The live buffer no longer contains the procedure — the
				// index is stale against unsaved edits. Truthful null for
				// this candidate.
				if !found {
					continue
				}
			}
		}
		out = append(out, target)
	}
	return out
}
