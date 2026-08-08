package server

import (
	"strings"

	"starlims-lsp/internal/lexer"
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

func (r liveResolver) ResolveDataSource(target string) []providers.ResolvedTarget {
	if r.s.workspaceIndex == nil {
		return nil
	}
	return r.overlay(r.s.workspaceIndex.ResolveDataSourceTarget(target))
}

// classFileDispatchTargets pre-resolves the document's ExecFunction
// dispatch targets against the workspace index and returns those that
// resolve to class files only (diag.execfunction_class_target, issue
// #143). The verdicts feed DiagnosticOptions so the check runs inside
// diagnostic collection, where suppression and overrides apply.
func (r liveResolver) classFileDispatchTargets(tokens []lexer.Token) []string {
	if r.s.workspaceIndex == nil {
		return nil
	}
	var out []string
	seen := make(map[string]bool)
	for _, site := range providers.ExtractCallSites(tokens) {
		if site.Kind != providers.CallDispatch || site.IsDoProc {
			continue
		}
		key := strings.ToLower(site.Raw)
		if seen[key] {
			continue
		}
		seen[key] = true
		if r.s.workspaceIndex.DispatchResolvesToClassOnly(site.Raw) {
			out = append(out, site.Raw)
		}
	}
	return out
}

// overlayResolutions applies the open-document overlay to raw index
// resolutions, keeping ProcName/IsEntry intact (issue #125): positions are
// re-derived from the live parse cache for open URIs, and procedures
// deleted in unsaved edits are dropped (truthful null). The site→definition
// matcher consumes these directly — the ResolvedTarget conversion below
// would lose the fields it matches on.
func (r liveResolver) overlayResolutions(resolutions []IndexResolution) []IndexResolution {
	if len(resolutions) == 0 {
		return nil
	}
	openURIs := r.s.documents.OpenURIs()

	out := make([]IndexResolution, 0, len(resolutions))
	for _, res := range resolutions {
		if _, open := openURIs[res.URI]; open {
			cache := r.s.documents.ParseDocument(res.URI, r.s.documentVersion[res.URI])
			if res.IsEntry {
				_, line := parser.NewParser(cache.Tokens).ExtractTopLevelParameters(cache.AST)
				if line > 0 {
					res.Line = line - 1
				} else {
					res.Line = 0
				}
			} else {
				found := false
				for _, proc := range cache.Procedures {
					if strings.EqualFold(proc.Name, res.ProcName) {
						res.Line = proc.StartLine - 1
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
		out = append(out, res)
	}
	return out
}

// overlay converts index resolutions to provider targets, re-deriving
// positions from the live document cache for URIs that are currently open.
func (r liveResolver) overlay(resolutions []IndexResolution) []providers.ResolvedTarget {
	overlaid := r.overlayResolutions(resolutions)
	if len(overlaid) == 0 {
		return nil
	}
	out := make([]providers.ResolvedTarget, 0, len(overlaid))
	for _, res := range overlaid {
		kind := providers.ResolvedScriptEntry
		if !res.IsEntry {
			kind = providers.ResolvedProcedure
		}
		out = append(out, providers.ResolvedTarget{URI: res.URI, Line: res.Line, Kind: kind})
	}
	return out
}

// resolutionsTargetDefinition reports whether the definition (defURI,
// procName) is among the candidate resolutions (matched) and is the only
// candidate (unambiguous). An empty procName matches the script entry.
func resolutionsTargetDefinition(res []IndexResolution, defURI, procName string) (matched, unambiguous bool) {
	for _, cand := range res {
		if cand.URI != defURI {
			continue
		}
		if procName == "" {
			if cand.IsEntry {
				matched = true
			}
		} else if strings.EqualFold(cand.ProcName, procName) {
			matched = true
		}
	}
	return matched, matched && len(res) == 1
}

// siteTargetsDefinition resolves a dispatch-site target string through the
// open-document overlay and reports whether the definition (defURI,
// procName) is among its candidates (matched) and is its only candidate
// (unambiguous). References include matched-but-ambiguous sites (D2).
func (r liveResolver) siteTargetsDefinition(raw, defURI, procName string) (matched, unambiguous bool) {
	if r.s.workspaceIndex == nil {
		return false, false
	}
	return resolutionsTargetDefinition(
		r.overlayResolutions(r.s.workspaceIndex.ResolveDispatchTarget(raw)), defURI, procName)
}

// siteUnambiguousForWrite is rename's stricter D1 gate (v0.14.0 review
// F4): the site must resolve to the single definition under BOTH the
// live-buffer overlay and the raw disk/index view. The overlay's
// truthful-null drop is right for read features, but an UNSAVED buffer
// that deletes a competing procedure must not make a write to disk look
// unambiguous — the buffer may never be saved.
func (r liveResolver) siteUnambiguousForWrite(raw, defURI, procName string) bool {
	if r.s.workspaceIndex == nil {
		return false
	}
	res := r.s.workspaceIndex.ResolveDispatchTarget(raw)
	if _, diskOK := resolutionsTargetDefinition(res, defURI, procName); !diskOK {
		return false
	}
	_, liveOK := resolutionsTargetDefinition(r.overlayResolutions(res), defURI, procName)
	return liveOK
}

// includeDeclaredVariables collects the variable names declared by a
// document's resolved :INCLUDE targets — transitively, cycle-guarded, with
// all resolution candidates unioned (spec
// feature.cross_file_resolution/A18-A19). :INCLUDE splices the included
// script's full text, so its declarations belong to the including file.
// Open included documents contribute their live-buffer declarations.
func (r liveResolver) includeDeclaredVariables(tokens []lexer.Token, selfURI string) []string {
	if r.s.workspaceIndex == nil {
		return nil
	}
	targets := providers.ExtractIncludeTargets(tokens)
	if len(targets) == 0 {
		return nil
	}

	openURIs := r.s.documents.OpenURIs()
	visited := map[string]bool{selfURI: true}
	var names []string

	var walk func(targets []string)
	walk = func(targets []string) {
		for _, target := range targets {
			for _, res := range r.s.workspaceIndex.ResolveIncludeTarget(target) {
				if visited[res.URI] {
					continue
				}
				visited[res.URI] = true

				if _, open := openURIs[res.URI]; open {
					cache := r.s.documents.ParseDocument(res.URI, r.s.documentVersion[res.URI])
					for _, v := range cache.Variables {
						names = append(names, v.Name)
					}
					walk(providers.ExtractIncludeTargets(cache.Tokens))
					continue
				}

				fs, ok := r.s.workspaceIndex.FileSymbolsFor(res.URI)
				if !ok {
					continue
				}
				names = append(names, fs.DeclaredVars...)
				walk(fs.IncludeTargets)
			}
		}
	}
	walk(targets)
	return names
}

// scriptDisplayName renders a file's identity for hover panels:
// "Category.Script" when the category is known, else the bare script name.
func scriptDisplayName(fs *FileSymbols) string {
	if fs.Category != "" {
		return fs.Category + "." + fs.ScriptName
	}
	return fs.ScriptName
}

// dispatchHoverMarkdown renders hover content for a dotted dispatch target,
// or "" when the target does not resolve (the caller falls through to the
// normal string-hover suppression). Ambiguity shows the first candidate
// plus a count — go-to-definition is where multi-candidate UX lives.
func (r liveResolver) dispatchHoverMarkdown(target string) string {
	if r.s.workspaceIndex == nil {
		return ""
	}
	resolutions := r.s.workspaceIndex.ResolveDispatchTarget(target)
	if len(resolutions) == 0 {
		return ""
	}
	return r.renderResolutionHover(resolutions)
}

// dataSourceHoverMarkdown renders hover content for a RunDS target, or ""
// when it does not resolve (feature.hover A14).
func (r liveResolver) dataSourceHoverMarkdown(target string) string {
	if r.s.workspaceIndex == nil {
		return ""
	}
	resolutions := r.s.workspaceIndex.ResolveDataSourceTarget(target)
	if len(resolutions) == 0 {
		return ""
	}
	fs, ok := r.s.workspaceIndex.FileSymbolsFor(resolutions[0].URI)
	if !ok {
		return ""
	}
	return providers.RenderDataSourceHover(scriptDisplayName(fs), fs.EntryParameters, len(resolutions)-1)
}

// includeHoverMarkdown renders hover content for an :INCLUDE target, or "".
func (r liveResolver) includeHoverMarkdown(target string) string {
	if r.s.workspaceIndex == nil {
		return ""
	}
	resolutions := r.s.workspaceIndex.ResolveIncludeTarget(target)
	if len(resolutions) == 0 {
		return ""
	}
	return r.renderResolutionHover(resolutions)
}

func (r liveResolver) renderResolutionHover(resolutions []IndexResolution) string {
	first := resolutions[0]
	extra := len(resolutions) - 1

	fs, ok := r.s.workspaceIndex.FileSymbolsFor(first.URI)
	if !ok {
		return ""
	}
	display := scriptDisplayName(fs)

	if first.IsEntry {
		return providers.RenderScriptEntryHover(display, fs.EntryParameters, fs.IsClass, len(fs.Procedures), extra)
	}

	// Prefer the live buffer's procedure data for open documents.
	openURIs := r.s.documents.OpenURIs()
	if _, open := openURIs[first.URI]; open {
		cache := r.s.documents.ParseDocument(first.URI, r.s.documentVersion[first.URI])
		for _, proc := range cache.Procedures {
			if strings.EqualFold(proc.Name, first.ProcName) {
				return providers.RenderCrossFileProcedureHover(providers.WorkspaceProcInfo{
					Name:       proc.Name,
					Parameters: proc.Parameters,
					Doc:        proc.Doc,
					StartLine:  proc.StartLine,
					EndLine:    proc.EndLine,
				}, display, extra)
			}
		}
		return ""
	}

	for _, proc := range fs.Procedures {
		if strings.EqualFold(proc.Name, first.ProcName) {
			return providers.RenderCrossFileProcedureHover(providers.WorkspaceProcInfo{
				Name:       proc.Name,
				Parameters: proc.Parameters,
				Doc:        proc.Doc,
				StartLine:  proc.StartLine,
				EndLine:    proc.EndLine,
			}, display, extra)
		}
	}
	return ""
}

// dispatchCompletionContext enumerates dispatch-target segment candidates
// for the string prefix typed so far (feature.completion A7-A10). Level 0
// (no dot) offers same-file procedures plus category names only — the
// deliberate noise floor; scripts appear after "Category." and procedures
// after "Category.Script." or flat "Script.". Private/protected procedures
// are excluded from workspace-sourced lists.
func (r liveResolver) dispatchCompletionContext(prefix string, sameFile []parser.ProcedureInfo) providers.DispatchCompletionContext {
	parts := strings.Split(prefix, ".")
	completed := parts[:len(parts)-1]

	ctx := providers.DispatchCompletionContext{}
	if len(completed) == 0 {
		ctx.SameFileProcs = sameFile
	}
	if r.s.workspaceIndex == nil {
		return ctx
	}
	wi := r.s.workspaceIndex

	switch len(completed) {
	case 0:
		ctx.Categories = wi.CategoryNames()
	case 1:
		seen := map[string]bool{}
		for _, fs := range wi.ScriptsInCategory(completed[0]) {
			if seen[strings.ToLower(fs.ScriptName)] {
				continue
			}
			seen[strings.ToLower(fs.ScriptName)] = true
			ctx.Scripts = append(ctx.Scripts, providers.ScriptCompletion{
				Name:      fs.ScriptName,
				Display:   scriptDisplayName(fs),
				IsClass:   fs.IsClass,
				ProcCount: len(fs.Procedures),
			})
		}
		// The first segment may also be a script name (flat layout):
		// offer its procedures alongside any category scripts.
		r.appendTargetProcs(&ctx, wi.ScriptsNamed(completed[0]))
	default:
		// "Cat.Script." — prefer the category chain, degrade to basename.
		var scripts []*FileSymbols
		for _, fs := range wi.ScriptsInCategory(strings.Join(completed[:len(completed)-1], ".")) {
			if strings.EqualFold(fs.ScriptName, completed[len(completed)-1]) {
				scripts = append(scripts, fs)
			}
		}
		if len(scripts) == 0 {
			scripts = wi.ScriptsNamed(completed[len(completed)-1])
		}
		r.appendTargetProcs(&ctx, scripts)
	}
	return ctx
}

func (r liveResolver) appendTargetProcs(ctx *providers.DispatchCompletionContext, scripts []*FileSymbols) {
	for _, fs := range scripts {
		if ctx.ScriptDisplay == "" {
			ctx.ScriptDisplay = scriptDisplayName(fs)
		}
		for _, proc := range fs.Procedures {
			if proc.IsPrivate {
				continue
			}
			ctx.TargetProcs = append(ctx.TargetProcs, providers.WorkspaceProcInfo{
				Name:       proc.Name,
				Parameters: proc.Parameters,
				Doc:        proc.Doc,
				StartLine:  proc.StartLine,
				EndLine:    proc.EndLine,
			})
		}
	}
}
