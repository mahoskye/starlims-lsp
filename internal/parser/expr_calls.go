package parser

// Call-site index over the expression AST (issue #184, milestone: the
// first tree consumers). Diagnostics that need to know what a call is —
// which callee, which receiver, which arguments, where each argument
// starts and ends — walk this instead of re-deriving argument boundaries
// from tokens. Nothing here runs during the structural Parse(); the trees
// are built on demand, so the fast structural path stays fast.

import "starlims-lsp/internal/lexer"

// CallSite is one call expression found in a token stream, together with
// the context a consumer needs to judge it.
type CallSite struct {
	// Call is the ExprCall node; Start/End bracket the whole call
	// including its closing paren.
	Call *Expr
	// Name is the callee's name: the identifier for a bare `Name(...)`
	// call, or the member name for a qualified `recv:Name(...)` call.
	Name string
	// Receiver is the member-access receiver of a qualified call, and nil
	// for a bare call. `Me:Foo()` carries an ExprIdentifier "Me"; the
	// receiver may be any expression (`Templates(1):Foo()`).
	Receiver *Expr
	// Args are the argument expressions in source order. A skipped slot
	// (`f(a,,c)`) is an ExprSkipped node, so indices stay aligned with the
	// argument positions the call actually names.
	Args []*Expr
	// Complete reports whether the owning statement parsed end to end
	// (StatementExprs.Complete). The call node itself is always
	// well-formed — an unclosed argument list never produces a CallSite —
	// so this matters only to consumers reasoning about the statement.
	Complete bool
}

// Qualified reports whether the call is a member call (`recv:Name(...)`)
// rather than a bare `Name(...)` call.
func (c CallSite) Qualified() bool { return c.Receiver != nil }

// EffectiveArgs returns the arguments with a single trailing skipped slot
// dropped: `f(a,)` names one argument, not two — the trailing comma adds
// no argument (see diag.trailing_skip_commas). Interior skips are kept,
// since `f(a,,c)` does pass three.
func (c CallSite) EffectiveArgs() []*Expr {
	if n := len(c.Args); n > 0 && c.Args[n-1].Kind == ExprSkipped {
		return c.Args[:n-1]
	}
	return c.Args
}

// CollectCalls returns every call expression in the token stream, in
// source order (a nested call follows the call it sits inside). Regions
// the expression parser could not resolve simply yield no call sites —
// unknown is never reported as evidence.
func CollectCalls(tokens []lexer.Token) []CallSite {
	var out []CallSite
	for _, se := range ExtractStatementExpressions(tokens) {
		for _, e := range se.Exprs {
			collectCallsInto(e, se.Complete, &out)
		}
	}
	return out
}

func collectCallsInto(e *Expr, complete bool, out *[]CallSite) {
	if e == nil {
		return
	}
	if e.Kind == ExprCall && len(e.Children) > 0 {
		callee := e.Children[0]
		site := CallSite{Call: e, Args: e.Children[1:], Complete: complete}
		switch callee.Kind {
		case ExprIdentifier:
			site.Name = callee.Name
		case ExprMember:
			site.Name = callee.Name
			if len(callee.Children) > 0 {
				site.Receiver = callee.Children[0]
			}
		}
		// parsePostfix only builds a call over an identifier or member
		// callee, so Name is always set; the guard keeps a future callee
		// shape from producing a nameless site.
		if site.Name != "" {
			*out = append(*out, site)
		}
	}
	for _, c := range e.Children {
		collectCallsInto(c, complete, out)
	}
}
