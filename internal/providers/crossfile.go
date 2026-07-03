// Cross-file resolution contract. Providers stay pure: they extract targets
// and render results, while the server implements WorkspaceResolver on top
// of its workspace index (providers must never import the server package).
// Normative behavior: catalog/features/cross_file_resolution.md.
package providers

// ResolvedTargetKind classifies what a cross-file resolution landed on.
type ResolvedTargetKind int

const (
	// ResolvedScriptEntry is a script's entry point — the target of a
	// 2-part ExecFunction("Category.Script") call or an :INCLUDE.
	ResolvedScriptEntry ResolvedTargetKind = iota
	// ResolvedProcedure is a named procedure inside a target script.
	ResolvedProcedure
)

// ResolvedTarget is one candidate location for a cross-file target.
type ResolvedTarget struct {
	URI  string
	Line int // 0-based target line
	Kind ResolvedTargetKind
}

// WorkspaceResolver resolves dispatch and include targets across the
// workspace. Implementations must be nil-input-safe and return candidates
// in normative order (anchored-layout matches first, then
// path-lexicographic), capped per the catalog entry.
type WorkspaceResolver interface {
	// ResolveDispatch resolves a dotted DoProc/ExecFunction target string
	// ("Cat.Script", "Cat.Script.Proc", "Script.Proc", ...). Bare 1-part
	// targets are the caller's same-file concern and return nil here.
	ResolveDispatch(target string) []ResolvedTarget
	// ResolveInclude resolves an :INCLUDE target ("Name" or "Cat.Script",
	// already unquoted) to candidate files.
	ResolveInclude(target string) []ResolvedTarget
}
