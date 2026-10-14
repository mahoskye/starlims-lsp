// Package providers implements LSP feature providers for SSL.
package providers

import (
	"fmt"
	"os"
	"regexp"
	"runtime/debug"
	"sort"
	"strings"
	"unicode"
	"unicode/utf8"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// DiagnosticSeverity represents the severity of a diagnostic.
type DiagnosticSeverity int

const (
	SeverityError   DiagnosticSeverity = 1
	SeverityWarning DiagnosticSeverity = 2
	SeverityInfo    DiagnosticSeverity = 3
	SeverityHint    DiagnosticSeverity = 4
)

// Range represents a range in a text document.
type Range struct {
	Start Position
	End   Position
}

// Position represents a position in a text document.
type Position struct {
	Line      int
	Character int
}

// Diagnostic represents a diagnostic message.
//
// Code is a stable, machine-readable identifier for the rule that produced
// this diagnostic. Clients use it to wire quick-fix code actions, suppression
// comments, and per-rule severity overrides. Where the schema in
// ssl-style-guide.schema.yaml defines a `lints` rule slug, Code uses that
// slug verbatim (snake_case). Otherwise, Code is derived from the producing
// check function. See diagnostic_codes.go for the canonical list.
type Diagnostic struct {
	Range    Range
	Severity DiagnosticSeverity
	Message  string
	Source   string
	Code     string
}

// DiagnosticOptions configures diagnostic checking.
type DiagnosticOptions struct {
	CheckUnclosedBlocks    bool
	CheckUnmatchedParens   bool
	CheckUndeclaredVars    bool
	CheckUnusedVars        bool
	CheckSQLParams         bool
	CheckHungarianNotation bool
	// CheckHungarianTypes gates diag.hungarian_type_mismatch, separately
	// from CheckHungarianNotation (issue #184 follow-up). The two rules
	// are independent findings that happened to share one switch: the
	// notation check enforces a house convention, and reports every name
	// a codebase that opted out of it ever declared; the type cross-check
	// reports a contradiction — a prefix promising one type over an
	// expression producing another — and only ever looks at names that
	// carry a prefix in the first place. A consumer wanting the
	// correctness signal without the convention audit enables this alone.
	CheckHungarianTypes bool
	HungarianPrefixes   []string
	GlobalVariables     []string
	MaxBlockDepth       int
	IsDataSourceFile    bool
	// IsEndpointFile marks the file as an SSL endpoint script. When true,
	// `Request` and `Response` are treated as pre-injected runtime
	// ambients (not declared, not flagged as undeclared, not assignable).
	IsEndpointFile bool

	// IncludeInfoDiagnostics enables the info severity tier (issue #208
	// discussion): info diagnostics are advisory detail — style
	// observations and idiom notes aimed at assistant/LLM consumers and
	// teams that want the full picture — and are dropped by default so
	// the everyday surface stays errors/warnings/hints. A rule explicitly
	// configured in RuleOverrides always shows regardless of this gate.
	IncludeInfoDiagnostics bool

	// IncludeDeclaredVariables carries variable names declared by the
	// file's resolved :INCLUDE targets (full-splice semantics, supplied by
	// the server's workspace closure — spec
	// feature.cross_file_resolution/A18-A19). They count as declared for
	// undeclared_variable and invalid_sql_param.
	IncludeDeclaredVariables []string

	// IncludesResolved reports that the caller read every :INCLUDE target
	// in the file's include closure (the server sets it from its workspace
	// index). When a file has an :INCLUDE and this is false — always so in
	// workspace-less consumers like --validate — the declarations the
	// include splices in are unknown, so undeclared_variable stays silent
	// for that file (DECISIONS.md D10, issue #304).
	IncludesResolved bool

	// ClassFileDispatchTargets lists this document's dispatch target
	// strings that resolve through the workspace index to class files only
	// (diag.execfunction_class_target, issue #143). Matched
	// case-insensitively against ExecFunction targets. Empty/nil — as in
	// workspace-less consumers like --validate — disables the check.
	ClassFileDispatchTargets []string

	// RuleOverrides maps a diagnostic Code (rule slug) to a severity override.
	// Recognized values: "off" (drop the diagnostic), "hint", "info",
	// "warn", "warning", "error". Keys and values are matched
	// case-insensitively; an entry with any other value is ignored as if
	// absent. Diagnostics whose Code is not in the map pass through
	// unchanged.
	RuleOverrides map[string]string
}

// DefaultDiagnosticOptions returns default diagnostic options.
func DefaultDiagnosticOptions() DiagnosticOptions {
	return DiagnosticOptions{
		CheckUnclosedBlocks:    true,
		CheckUnmatchedParens:   true,
		CheckUndeclaredVars:    false,
		CheckUnusedVars:        false,
		CheckSQLParams:         false,
		CheckHungarianNotation: false,
		CheckHungarianTypes:    false,
		HungarianPrefixes:      []string{"a", "b", "d", "fn", "n", "o", "s", "v"},
		MaxBlockDepth:          4,
	}
}

// GetDiagnostics returns all diagnostics for a document.
// concatDiagnostics joins diagnostic slices without the append-spread
// idiom, which mutates its first argument's backing array.
func concatDiagnostics(groups ...[]Diagnostic) []Diagnostic {
	var out []Diagnostic
	for _, g := range groups {
		out = append(out, g...)
	}
	return out
}

func GetDiagnostics(text string, opts DiagnosticOptions) (result []Diagnostic) {
	opts.RuleOverrides = normalizeRuleOverrides(opts.RuleOverrides)
	// collectDiagnostics contains its own panics; this covers the rest of
	// the text path — data-source SQL-mode classification and its SQL-body
	// checks, lexing, and parsing (issue #258).
	defer func() {
		if r := recover(); r != nil {
			result = applyRuleOverrides([]Diagnostic{internalErrorDiagnostic("GetDiagnostics", r)}, opts.RuleOverrides)
		}
	}()

	// A data-source document whose content is plain SQL gets no SSL
	// diagnostics at all — every SSL check would false-flag SQL syntax
	// such as dot-qualified column names (feature.diagnostics_pipeline
	// A10-A12, issue #77). The server routes data-source documents through
	// this text path in validateDocument.
	// SQL-mode bodies carry no SSL tokens, so suppression comments cannot
	// apply there; rule overrides still must (the config surface may not
	// diverge between SSL and SQL-mode diagnostics), hence the explicit
	// applyRuleOverrides on both SQL-mode paths below.
	var sqlBodyDiagnostics []Diagnostic
	if opts.IsDataSourceFile {
		// The strict trio models SSL scoping, which a data source does
		// not follow: its `:PARAMETERS` carry inline `:=` defaults, its
		// builder directives take bare identifiers as *values*
		// (`:DSN := starlims;` — `starlims` is a DSN name, not a variable
		// read), and its SQL body names columns and keywords that are not
		// SSL identifiers at all. Left on, every data source reported its
		// own directive values and SQL keywords as undeclared. The
		// data-source dialect gets its own placeholder check
		// (checkDataSourceUndeclaredPlaceholders) instead (issue #249).
		opts.CheckUndeclaredVars = false
		opts.CheckUnusedVars = false
		opts.CheckSQLParams = false

		// A data-source file is SQL by default and only stays in SSL mode
		// when its body carries a strong SSL marker (issue #153): a
		// non-directive colon keyword or a `:=` assignment, or the document
		// leads with an unterminated SSL comment. The directive /
		// :PARAMETERS header is split off first so its keywords and inline
		// `:=` defaults never read as SSL. Plain SQL (A10), a comment-only
		// stub (A16), and the hybrid header-then-SQL shapes all lack those
		// markers and route here; a real SSL data source (A11) has one and
		// falls through to the full SSL pipeline below.
		header, body := SplitDataSourceHeader(text)
		if IsSQLModeDataSourceSplit(text, header, body) {
			if strings.TrimSpace(header) != "" {
				// Hybrid shape: the directive / :PARAMETERS header keeps its
				// SSL and data-source checks; the SQL body gets only the
				// SQL-body checks — the statement-separator warning (issue
				// #154) and the undeclared @name placeholder check against
				// the header's :PARAMETERS names — offset past the header.
				// The header is a position-preserving prefix, so ranges line
				// up unchanged (issues #104, #148).
				offset := strings.Count(header, "\n")
				sqlBodyDiagnostics = applyInfoGate(applyRuleOverrides(concatDiagnostics(
					checkDataSourceSQLSemicolons(body, offset),
					checkDataSourceBodySQLContent(body, offset),
					checkDataSourceUndeclaredPlaceholders(body, dataSourceParameterNames(header), offset),
					checkDataSourceQuestionPlaceholders(body, offset),
					// The header region, unmasked. The split blanks its
					// comments and swallows a banner's trailing `;`, so
					// the SSL pipeline below never sees either.
					checkDataSourceHeaderSemicolons(text[:len(header)]),
				), opts.RuleOverrides), opts)
				text = header
			} else {
				// Whole-document SQL body (or a comment-only stub): no SSL
				// diagnostics, only the SQL-body checks. With no header, no
				// @name placeholders are declared.
				return applyInfoGate(applyRuleOverrides(concatDiagnostics(
					checkDataSourceSQLSemicolons(text, 0),
					checkDataSourceBodySQLContent(text, 0),
					checkDataSourceUndeclaredPlaceholders(text, nil, 0),
					checkDataSourceQuestionPlaceholders(text, 0),
				), opts.RuleOverrides), opts)
			}
		}
	}

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	return append(collectDiagnostics(tokens, ast, p, opts), sqlBodyDiagnostics...)
}

// GetDiagnosticsFromTokens returns diagnostics using cached tokens/AST.
// Note: A parser instance is created even when AST is provided because
// ExtractVariables requires parser helper methods to traverse the AST.
// Parser creation is O(1) as it just stores a reference to the tokens.
func GetDiagnosticsFromTokens(tokens []lexer.Token, ast *parser.Node, opts DiagnosticOptions) (result []Diagnostic) {
	if len(tokens) == 0 {
		return nil
	}
	opts.RuleOverrides = normalizeRuleOverrides(opts.RuleOverrides)
	defer func() {
		if r := recover(); r != nil {
			result = applyRuleOverrides([]Diagnostic{internalErrorDiagnostic("GetDiagnosticsFromTokens", r)}, opts.RuleOverrides)
		}
	}()

	p := parser.NewParser(tokens)
	if ast == nil {
		ast = p.Parse()
	}

	return collectDiagnostics(tokens, ast, p, opts)
}

func collectDiagnostics(tokens []lexer.Token, ast *parser.Node, p *parser.Parser, opts DiagnosticOptions) (result []Diagnostic) {
	var diagnostics []Diagnostic

	// Defense in depth: if a check function panics, surface it as an
	// internal_error diagnostic instead of letting the panic propagate and
	// kill the LSP server (which manifests on the client as
	// "all goroutines are asleep - deadlock"). Everything the checks before
	// the panicking one collected is kept, post-processed as usual (issue
	// #258: the recovery used to discard it). The stack goes to stderr so
	// it shows up in the editor's LSP output channel.
	defer func() {
		if r := recover(); r != nil {
			result = finishAfterPanic(tokens, diagnostics, internalErrorDiagnostic("collectDiagnostics", r), opts)
		}
	}()

	// Check for lexer-level issues
	diagnostics = append(diagnostics, checkTokenErrors(tokens)...)
	diagnostics = append(diagnostics, checkCommentTermination(tokens)...)
	diagnostics = append(diagnostics, checkCStyleCommentClosers(tokens)...)
	diagnostics = append(diagnostics, checkUnterminatedStrings(tokens)...)

	// Check for unmatched parentheses/brackets
	if opts.CheckUnmatchedParens {
		diagnostics = append(diagnostics, checkUnmatchedDelimiters(tokens)...)
	}

	// Check for unclosed blocks
	if opts.CheckUnclosedBlocks {
		diagnostics = append(diagnostics, checkUnclosedBlocks(tokens)...)
	}

	// Check block depth
	if opts.MaxBlockDepth > 0 {
		diagnostics = append(diagnostics, checkBlockDepth(ast, opts.MaxBlockDepth)...)
	}

	variables := p.ExtractVariables()
	typeInfo := buildSimpleTypeInfo(tokens, variables)

	// Expression-AST consumers (issue #184): the statement trees and the
	// call-site index derived from them are built once here and shared by
	// every tree-driven rule.
	stmtExprs := parser.ExtractStatementExpressions(tokens)
	calls := parser.CollectCallsIn(stmtExprs)

	// Check for Hungarian notation (opt-in)
	if opts.CheckHungarianNotation {
		diagnostics = append(diagnostics, checkHungarianNotation(variables, opts.HungarianPrefixes)...)
	}
	if opts.CheckHungarianTypes {
		diagnostics = append(diagnostics, checkHungarianTypeMismatch(tokens, stmtExprs)...)
	}
	diagnostics = append(diagnostics, checkInvalidNativeMember(tokens, stmtExprs)...)

	// SSL language rule enforcement (always enabled)
	if opts.IsDataSourceFile {
		diagnostics = append(diagnostics, checkKeywordFormsDataSource(tokens)...)
		diagnostics = append(diagnostics, checkDataSourceDefaultUsage(tokens)...)
		diagnostics = append(diagnostics, checkDataSourceParameterDefaults(tokens)...)
	} else {
		diagnostics = append(diagnostics, checkKeywordForms(tokens)...)
		diagnostics = append(diagnostics, checkInlineDefaultOutsideDataSource(tokens)...)
		diagnostics = append(diagnostics, checkDefaultOnDeclareLine(tokens)...)
		diagnostics = append(diagnostics, checkParameterPlacement(tokens)...)
		diagnostics = append(diagnostics, checkDefaultPlacement(tokens)...)
		// Statement grammar (diag.unexpected_token, issue #240). Gated off
		// for data-source files: their SSL is directive syntax outside the
		// expression grammar and their bodies are SQL.
		diagnostics = append(diagnostics, checkUnexpectedTokens(tokens, stmtExprs)...)
	}
	diagnostics = append(diagnostics, checkDeclareInitializer(tokens)...)
	diagnostics = append(diagnostics, checkMissingExitCase(tokens)...)
	diagnostics = append(diagnostics, checkExitCaseLastInBranch(tokens)...)
	diagnostics = append(diagnostics, checkMissingOtherwise(tokens)...)
	diagnostics = append(diagnostics, checkBareLogicalOperators(tokens)...)
	diagnostics = append(diagnostics, checkInvalidOperatorSequences(tokens)...)
	diagnostics = append(diagnostics, checkIncludePlacement(tokens)...)
	diagnostics = append(diagnostics, checkIncludeQuotedTarget(tokens)...)
	diagnostics = append(diagnostics, checkInlineCodeNaming(tokens)...)
	diagnostics = append(diagnostics, checkInheritQualifiedName(tokens)...)
	diagnostics = append(diagnostics, checkBeginCaseHasCase(tokens)...)
	diagnostics = append(diagnostics, checkTryStructure(tokens)...)
	diagnostics = append(diagnostics, checkErrorHandlerStructure(tokens)...)
	diagnostics = append(diagnostics, checkCatchClauseForm(tokens)...)
	diagnostics = append(diagnostics, checkRaiseErrorInCatch(tokens, calls)...)
	diagnostics = append(diagnostics, checkMixedErrorHandlingFamilies(tokens)...)
	diagnostics = append(diagnostics, checkExitCaseAfterReturn(tokens)...)
	diagnostics = append(diagnostics, checkExecFunctionClassTargets(tokens, opts.ClassFileDispatchTargets)...)
	diagnostics = append(diagnostics, checkForNumericValues(tokens, stmtExprs)...)
	diagnostics = append(diagnostics, checkLoopAndFinallyControl(tokens)...)
	diagnostics = append(diagnostics, checkDeprecatedKeywords(tokens)...)
	diagnostics = append(diagnostics, checkNotPreferredOperators(tokens)...)
	diagnostics = append(diagnostics, checkLiteralTypeSafety(tokens, typeInfo)...)
	diagnostics = append(diagnostics, checkNilInOperations(tokens, stmtExprs)...)
	diagnostics = append(diagnostics, checkMixedTypePlus(tokens, stmtExprs)...)
	diagnostics = append(diagnostics, checkOperatorOperandTypes(tokens, stmtExprs)...)
	diagnostics = append(diagnostics, checkEqualsEmptyString(tokens, stmtExprs)...)
	diagnostics = append(diagnostics, checkEmptyOptionalParamArrays(tokens)...)
	trailingRuns := trailingSkipCommaRuns(tokens, calls)
	diagnostics = append(diagnostics, checkTrailingSkipCommas(tokens, trailingRuns)...)
	diagnostics = append(diagnostics, checkSpacedSkipCommas(tokens, trailingRuns)...)
	diagnostics = append(diagnostics, checkFormatArgNotArray(tokens, calls)...)
	diagnostics = append(diagnostics, checkBuiltinExcessArguments(tokens, calls)...)
	diagnostics = append(diagnostics, checkPublicVariables(tokens)...)
	procedures := p.ExtractProcedures(ast)
	diagnostics = append(diagnostics, checkProcedureParameterCounts(procedures)...)
	diagnostics = append(diagnostics, checkNameLengths(variables, procedures, opts.HungarianPrefixes)...)
	diagnostics = append(diagnostics, checkRedeclaredVariables(tokens)...)
	diagnostics = append(diagnostics, checkNestedIIF(tokens)...)
	diagnostics = append(diagnostics, checkNegativeLogic(tokens, stmtExprs)...)
	diagnostics = append(diagnostics, checkVisibilityAnnotations(tokens)...)
	diagnostics = append(diagnostics, checkNilMethodCalls(tokens)...)
	diagnostics = append(diagnostics, checkParameterListAndDirectives(tokens, opts.IsDataSourceFile)...)

	// Check for assignment to global variables.
	// Always runs to catch writes to built-in predefined globals (e.g. MYUSERNAME).
	// Also enforces user-configured globals when provided.
	diagnostics = append(diagnostics, checkGlobalAssignment(tokens, variables, opts.GlobalVariables)...)

	// Check for undeclared variable usage (opt-in)
	// An :INCLUDE the caller could not read splices in declarations this
	// check cannot see, so it stays silent rather than guess (issue #304).
	if opts.CheckUndeclaredVars && (opts.IncludesResolved || len(ExtractIncludeTargets(tokens)) == 0) {
		diagnostics = append(diagnostics, checkUndeclaredVariables(tokens, p, opts.GlobalVariables, opts.IncludeDeclaredVariables, opts.IsEndpointFile)...)
	}

	// Check for unused variable declarations (opt-in)
	if opts.CheckUnusedVars {
		diagnostics = append(diagnostics, checkUnusedVariables(tokens, ast, p)...)
	}

	// Check for SQL parameter validation (opt-in)
	if opts.CheckSQLParams {
		diagnostics = append(diagnostics, checkSQLParameterValidation(tokens, ast, p, opts.GlobalVariables, opts.IncludeDeclaredVariables)...)
	}

	// SSL gotcha detection (always enabled)
	diagnostics = append(diagnostics, checkAssignmentInCondition(tokens)...)
	diagnostics = append(diagnostics, checkDotPropertyAccess(tokens)...)
	diagnostics = append(diagnostics, checkClassInstantiationSyntax(tokens)...)
	diagnostics = append(diagnostics, checkCreateUdObjectBuiltinClassMisuse(tokens)...)
	diagnostics = append(diagnostics, checkZeroBasedArrayIndex(tokens, stmtExprs)...)
	diagnostics = append(diagnostics, checkNamedSQLParamsWithWrongFunction(tokens)...)
	diagnostics = append(diagnostics, checkComplexSQLPlaceholders(tokens)...)
	diagnostics = append(diagnostics, checkUDObjectArrayInClause(tokens)...)
	diagnostics = append(diagnostics, checkRunSQLNonDML(tokens, calls)...)
	diagnostics = append(diagnostics, checkSQLAdvisories(tokens, opts)...)
	diagnostics = append(diagnostics, checkUnicodeLiteralPrefix(tokens)...)
	diagnostics = append(diagnostics, checkCollateJustification(tokens)...)
	diagnostics = append(diagnostics, checkProcedureDeclarationSyntax(tokens)...)
	diagnostics = append(diagnostics, checkDirectProcedureCalls(tokens, ast, p)...)
	diagnostics = append(diagnostics, checkMissingQuotesInExecFunction(tokens)...)
	diagnostics = append(diagnostics, checkBranchTargetLabels(tokens)...)
	diagnostics = append(diagnostics, checkClassContextRules(tokens, ast, p)...)
	diagnostics = append(diagnostics, checkClassNameCollision(tokens)...)
	diagnostics = append(diagnostics, checkClassReferenceForms(tokens)...)
	diagnostics = append(diagnostics, checkScientificNotation(tokens)...)
	diagnostics = append(diagnostics, checkStepSpacing(tokens)...)
	diagnostics = append(diagnostics, checkStepZeroLiteral(tokens, stmtExprs)...)
	diagnostics = append(diagnostics, checkInvalidLimsTypeExComparison(tokens)...)
	diagnostics = append(diagnostics, checkRegionEndMismatch(tokens)...)
	diagnostics = append(diagnostics, checkCodeBlockStructure(tokens)...)
	diagnostics = append(diagnostics, checkSQLConcatenationInjection(tokens, calls)...)

	diagnostics = applySuppressionComments(tokens, diagnostics)
	diagnostics = applyRuleOverrides(diagnostics, opts.RuleOverrides)
	diagnostics = applyInfoGate(diagnostics, opts)

	result = diagnostics
	return
}

// internalErrorDiagnostic logs a recovered panic with its stack to stderr
// and returns the internal_error diagnostic reporting it. Call it from the
// deferred function that recovered, so the stack still shows the panic.
func internalErrorDiagnostic(where string, r any) Diagnostic {
	fmt.Fprintf(os.Stderr, "ssl-lsp: panic in %s: %v\n%s\n", where, r, debug.Stack())
	return Diagnostic{
		Severity: SeverityError,
		Range: Range{
			Start: Position{Line: 0, Character: 0},
			End:   Position{Line: 0, Character: 1},
		},
		Message: fmt.Sprintf("ssl-lsp internal error: %v. Other diagnostics for this file may be missing. Please file an issue with the file contents — full stack trace is in the LSP output channel.", r),
		Source:  "ssl-lsp",
		Code:    CodeInternalError,
	}
}

// finishAfterPanic completes collection after a check panicked: the
// diagnostics collected so far plus the internal_error go through
// suppression, rule overrides, and the info gate like any other result.
// Should post-processing itself panic, the unprocessed list is returned.
func finishAfterPanic(tokens []lexer.Token, collected []Diagnostic, internal Diagnostic, opts DiagnosticOptions) (out []Diagnostic) {
	raw := append(append([]Diagnostic(nil), collected...), internal)
	out = raw
	defer func() {
		if r := recover(); r != nil {
			fmt.Fprintf(os.Stderr, "ssl-lsp: panic post-processing recovered diagnostics: %v\n", r)
			out = raw
		}
	}()
	processed := append([]Diagnostic(nil), raw...)
	processed = applySuppressionComments(tokens, processed)
	processed = applyRuleOverrides(processed, opts.RuleOverrides)
	return applyInfoGate(processed, opts)
}

// normalizeRuleOverrides returns overrides keyed by the lower-cased,
// trimmed rule slug with lower-cased, trimmed values, keeping only entries
// whose value applyRuleOverrides recognizes (issue #258). Codes are
// lower-case snake_case, so `"Hungarian_Notation"` now configures the rule
// as suppression slugs already did; and an unrecognized value such as a
// typo is dropped here, so it neither changes the severity nor exempts the
// rule from the info gate — an entry that does nothing must not do half a
// thing.
func normalizeRuleOverrides(overrides map[string]string) map[string]string {
	if len(overrides) == 0 {
		return overrides
	}
	out := make(map[string]string, len(overrides))
	for k, v := range overrides {
		v = strings.ToLower(strings.TrimSpace(v))
		switch v {
		case "off", "hint", "info", "warn", "warning", "error":
			out[strings.ToLower(strings.TrimSpace(k))] = v
		}
	}
	return out
}

// applyInfoGate drops info-severity diagnostics unless the info tier is
// enabled (ssl.diagnostics.infoDiagnostics). Rules the user explicitly
// configured in RuleOverrides are exempt — an explicit per-rule severity
// choice always wins over the blanket gate, in both directions (a rule
// remapped *to* info stays visible; a rule remapped away from info was
// never info to begin with).
func applyInfoGate(diagnostics []Diagnostic, opts DiagnosticOptions) []Diagnostic {
	if opts.IncludeInfoDiagnostics {
		return diagnostics
	}
	filtered := diagnostics[:0]
	for _, d := range diagnostics {
		if d.Severity == SeverityInfo {
			if _, configured := opts.RuleOverrides[d.Code]; !configured {
				continue
			}
		}
		filtered = append(filtered, d)
	}
	return filtered
}

// applyRuleOverrides drops or remaps severities for diagnostics whose Code
// matches an entry in `overrides`. Diagnostics with no Code or no matching
// entry pass through unchanged. Recognized override values:
//
//	"off"                  — drop the diagnostic entirely
//	"hint"                 — remap to SeverityHint
//	"info"                 — remap to SeverityInformation
//	"warn" / "warning"     — remap to SeverityWarning
//	"error"                — remap to SeverityError
//
// Any other value is treated as no-op rather than silently dropping. The
// pipeline entry points normalize the map first (normalizeRuleOverrides),
// which drops such entries and makes keys case-insensitive.
func applyRuleOverrides(diagnostics []Diagnostic, overrides map[string]string) []Diagnostic {
	if len(overrides) == 0 {
		return diagnostics
	}
	out := make([]Diagnostic, 0, len(diagnostics))
	for _, d := range diagnostics {
		if d.Code == "" {
			out = append(out, d)
			continue
		}
		raw, ok := overrides[d.Code]
		if !ok {
			out = append(out, d)
			continue
		}
		switch strings.ToLower(strings.TrimSpace(raw)) {
		case "off":
			continue
		case "hint":
			d.Severity = SeverityHint
		case "info":
			d.Severity = SeverityInfo
		case "warn", "warning":
			d.Severity = SeverityWarning
		case "error":
			d.Severity = SeverityError
		}
		out = append(out, d)
	}
	return out
}

// applySuppressionComments drops diagnostics silenced by user-authored
// suppression comments embedded in the source. Two forms are recognized:
//
//	/* @ssl-disable <rule_slug>[, <rule_slug>...] [-- reason];
//	    File-scope suppression. Any diagnostic with a matching Code is dropped.
//	/* @ssl-disable-next-line <rule_slug>[, <rule_slug>...] [-- reason];
//	    Line-scope: applies to diagnostics on the line immediately following
//	    the comment.
//
// The comment ends at its `;` like any SSL comment. An optional reason
// follows the rule list after `--`, `—` or `:` (issue: suppressions should
// say why); it is ignored by matching.
//
// Slug `*` matches any code and silences every coded diagnostic in scope.
// Diagnostics without a Code (defensive — every emit should set one) bypass
// suppression so the user sees them.
func applySuppressionComments(tokens []lexer.Token, diagnostics []Diagnostic) []Diagnostic {
	fileScope := map[string]bool{}
	lineScope := map[int]map[string]bool{} // 0-based diagnostic line -> slug set

	addLineRule := func(line int, slug string) {
		if lineScope[line] == nil {
			lineScope[line] = map[string]bool{}
		}
		lineScope[line][slug] = true
	}

	for _, t := range tokens {
		if t.Type != lexer.TokenComment {
			continue
		}
		body := t.Text
		if idx := strings.Index(body, "@ssl-disable-next-line"); idx >= 0 {
			tail := body[idx+len("@ssl-disable-next-line"):]
			// Comments may span lines, so suppress on the line immediately
			// after the comment ends (not after it starts). Token.Line is
			// 1-based; Diagnostic.Range.Start.Line is 0-based; the line
			// directly below a 1-based line N is the 0-based line N. So
			// adding (start_line + extra_lines_in_text) yields the right
			// 0-based key for the line below the comment's last line.
			extraLines := strings.Count(body, "\n")
			for _, s := range parseRuleList(tail) {
				addLineRule(t.Line+extraLines, s)
			}
			continue
		}
		if idx := strings.Index(body, "@ssl-disable"); idx >= 0 {
			tail := body[idx+len("@ssl-disable"):]
			for _, s := range parseRuleList(tail) {
				fileScope[s] = true
			}
		}
	}

	if len(fileScope) == 0 && len(lineScope) == 0 {
		return diagnostics
	}

	out := make([]Diagnostic, 0, len(diagnostics))
	for _, d := range diagnostics {
		if d.Code == "" {
			out = append(out, d)
			continue
		}
		if fileScope[d.Code] || fileScope["*"] {
			continue
		}
		if line, ok := lineScope[d.Range.Start.Line]; ok {
			if line[d.Code] || line["*"] {
				continue
			}
		}
		out = append(out, d)
	}
	return out
}

// parseRuleList extracts comma-separated rule slugs from text following an
// `@ssl-disable[-next-line]` directive. Stops at the comment-terminating
// `;`. Whitespace is trimmed, empty entries dropped, and slugs lowercased
// since the canonical form in diagnostic_codes.go is snake_case lowercase.
func parseRuleList(s string) []string {
	if i := strings.Index(s, ";"); i >= 0 {
		s = s[:i]
	}
	// An optional reason follows the rule list after `--`, `—` or `:`.
	for _, sep := range []string{"--", "—", ":"} {
		if i := strings.Index(s, sep); i >= 0 {
			s = s[:i]
		}
	}
	var out []string
	for _, part := range strings.Split(s, ",") {
		slug := strings.ToLower(strings.TrimSpace(part))
		if slug != "" {
			out = append(out, slug)
		}
	}
	return out
}

// knownInvalidKeyword reports colon forms that are not SSL keywords but are
// known, so they are definite compile errors rather than possible gaps in
// the keyword list (issue #269): :ENDCLASS (a class runs to the end of its
// file and has no closer), and a data-source builder directive outside a
// data source file.
func knownInvalidKeyword(token lexer.Token, normalized string, isDataSource bool) (Diagnostic, bool) {
	switch {
	case normalized == "ENDCLASS":
		return Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(token),
			Message:  "':ENDCLASS' is not valid — a class extends to the end of its file and has no closing keyword",
			Source:   "ssl-lsp",
			Code:     CodeEndClassInvalid,
		}, true
	case !isDataSource && constants.IsBuilderDirective(normalized):
		return Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(token),
			Message:  fmt.Sprintf("'%s' is a data source builder directive and is valid only in a data source file", token.Text),
			Source:   "ssl-lsp",
			Code:     CodeBuilderDirectiveOutsideDatasource,
		}, true
	}
	return Diagnostic{}, false
}

// checkKeywordForms enforces colon-prefixed uppercase keywords and flags unknown colon forms.
func checkKeywordForms(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}

		text := token.Text
		normalized := strings.ToUpper(strings.TrimPrefix(text, ":"))

		if strings.HasPrefix(text, ":") {
			if isLegacyLabelKeywordForm(text) {
				if !strings.HasPrefix(text, ":LABEL") {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  "SSL label keyword forms are case-sensitive: use ':LABEL Name;' or ':LABELName;'",
						Source:   "ssl-lsp",
						Code:     CodeLabelKeywordForm,
					})
				}
				continue
			}

			if !constants.IsKeyword(normalized) {
				// Special case: :ENDFOR is a recognized token but NOT usable — use :NEXT
				if normalized == "ENDFOR" {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  "':ENDFOR' is not valid — FOR loops must be terminated with ':NEXT'",
						Source:   "ssl-lsp",
						Code:     CodeEndForInvalid,
					})
				} else if d, ok := knownInvalidKeyword(token, normalized, false); ok {
					diagnostics = append(diagnostics, d)
				} else {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityWarning,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Unknown SSL keyword: '%s'", text),
						Source:   "ssl-lsp",
						Code:     CodeUnknownKeyword,
					})
				}
				continue
			}

			canonical := ":" + normalized
			if text != canonical {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  fmt.Sprintf("SSL keywords are case-sensitive and must be uppercase: use '%s'", canonical),
					Source:   "ssl-lsp",
					Code:     CodeKeywordUppercase,
				})
			}
		}
	}

	return diagnostics
}

func isLegacyLabelKeywordForm(text string) bool {
	if !strings.HasPrefix(text, ":") {
		return false
	}

	trimmed := strings.TrimPrefix(text, ":")
	return strings.HasPrefix(strings.ToUpper(trimmed), "LABEL") && len(trimmed) > len("LABEL")
}

// checkTokenErrors checks for token-level errors.
// Skips TokenUnknown that look like dot property access (handled by checkDotPropertyAccess).
func checkTokenErrors(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		if token.Type == lexer.TokenUnknown {
			// Skip dot property access patterns - they have their own diagnostic
			if strings.HasPrefix(token.Text, ".") && len(token.Text) > 1 {
				rest := token.Text[1:]
				// Check if at least the start of rest looks like an identifier
				if len(rest) > 0 && len(extractIdentifier(rest)) > 0 {
					// Check if preceded by a receiver
					if dotAccessReceiverBefore(tokens, i) {
						continue // Skip - will be reported by checkDotPropertyAccess
					}
				}
			}

			// A character no SSL token class accepts cannot compile, so this
			// is an error; unexpected_token defers to this rule, so it must
			// be the one that fails validation (issue #291).
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("Unknown token: '%s'", token.Text),
				Source:   "ssl-lsp",
				Code:     CodeUnknownToken,
			})
		}
	}

	return diagnostics
}

// checkCommentTermination detects block comments where a semicolon inside the
// text terminates the comment prematurely, causing the remaining text to become
// executable code. This is one of the most destructive errors in SSL — a single
// stray semicolon in a header comment can corrupt the entire file's parse.
func checkCommentTermination(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		if token.Type != lexer.TokenComment {
			continue
		}

		if !strings.HasSuffix(token.Text, ";") {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "SSL comments must end with a semicolon ';'",
				Source:   "ssl-lsp",
				Code:     CodeCommentTermination,
			})
			continue
		}

		// Skip region marker comments — the semicolon after the region name
		// is intentional and does not indicate premature termination.
		trimmed := strings.TrimSpace(strings.TrimPrefix(token.Text, "/*"))
		trimmed = strings.TrimSpace(strings.TrimPrefix(trimmed, "//"))
		lower := strings.ToLower(trimmed)
		if strings.HasPrefix(lower, "region") || strings.HasPrefix(lower, "endregion") {
			continue
		}

		nextIdx := nextSignificantTokenIndex(tokens, i+1)
		if nextIdx < 0 {
			continue
		}

		nextToken := tokens[nextIdx]

		// Same-line continuation: the semicolon terminated the comment
		// before the line ended. The remaining text becomes executable code,
		// which may be intentional or may be hiding code accidentally.
		if nextToken.Line == token.Line {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(token),
				Message:  "Comment terminated early by semicolon. Text after the ';' becomes executable code and may be unintentionally hidden",
				Source:   "ssl-lsp",
				Code:     CodeCommentTextAfterTerminator,
			})
			continue
		}

		if !strings.HasPrefix(token.Text, "/*") {
			continue
		}
		// Issue #6: suppress when there's a paragraph break (blank line or
		// another standalone comment) between the comment-end and the alleged
		// "broken-out" token. That gap indicates the user deliberately ended
		// the comment — the heuristic is firing on a benign comment-after-
		// comment chain rather than an actual mid-comment terminator.
		if commentChainBreaksBeforeNext(tokens, i, nextIdx) {
			continue
		}

		// Multi-line detection: if a /* comment spans multiple lines (contains
		// newlines in its token text) and the next token is an identifier whose
		// name matches a keyword (e.g. "Parameters", "Default", "For"), the
		// semicolon almost certainly terminated the comment prematurely.
		if strings.Contains(token.Text, "\n") &&
			nextToken.Type == lexer.TokenIdentifier && constants.IsKeyword(strings.ToUpper(nextToken.Text)) &&
			!startsAssignment(tokens, nextIdx) {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "Comment likely terminated early by semicolon. The text on the following lines may be intended as comment content but is being parsed as code. Rewrite the comment to avoid internal semicolons",
				Source:   "ssl-lsp",
				Code:     CodeCommentTextAfterTerminator,
			})
			continue
		}

		// Issue #25: orphaned-prose signal. When the stranded lines are plain
		// prose with no keyword-named word up front, the bare-keyword check
		// above sees nothing. But prose betrays itself: the next significant
		// line starts with two or more consecutive bare identifiers with
		// nothing between them, which never forms a valid SSL statement
		// (assignments, calls, and keyword statements all place an operator,
		// parenthesis, or keyword between/before names). Applies to both
		// multi-line comments and a comment whose ';' lands on its first
		// line (the issue's original shape) — the stranded prose is on the
		// following lines either way. Weaker signal than the bare-keyword
		// break-out, so this path warns rather than errors.
		if startsOrphanedProse(tokens, nextIdx) {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(token),
				Message:  "Comment likely terminated early by semicolon. The following lines read as prose but are being parsed as code. Rewrite the comment to avoid internal semicolons",
				Source:   "ssl-lsp",
				Code:     CodeCommentTextAfterTerminator,
			})
		}
	}

	return diagnostics
}

// startsAssignment reports whether the identifier at idx is the target of
// an assignment — the next significant token on its line is `:=` or a
// compound assignment operator. A keyword-named variable assigned on the
// line after a comment (`parameters := ...`) is code, not a doc header cut
// in half (issue #304).
func startsAssignment(tokens []lexer.Token, idx int) bool {
	next := nextSignificantTokenIndex(tokens, idx+1)
	if next < 0 || tokens[next].Line != tokens[idx].Line || tokens[next].Type != lexer.TokenOperator {
		return false
	}
	switch tokens[next].Text {
	case ":=", "+=", "-=", "*=", "/=", "^=", "%=":
		return true
	}
	return false
}

// startsOrphanedProse reports whether the significant token at idx begins a
// run of two consecutive bare identifiers on the same line with nothing
// between them — the signature of comment prose stranded as code (issue #25).
// A single identifier is not enough: it could be the start of a legitimate
// statement continued on the next line, so the second identifier must share
// the first one's line. Any operator, parenthesis, bracket, or keyword after
// the first identifier means legitimate code and does not match.
func startsOrphanedProse(tokens []lexer.Token, idx int) bool {
	first := tokens[idx]
	if first.Type != lexer.TokenIdentifier {
		return false
	}
	secondIdx := nextSignificantTokenIndex(tokens, idx+1)
	if secondIdx < 0 {
		return false
	}
	second := tokens[secondIdx]
	return second.Type == lexer.TokenIdentifier && second.Line == first.Line
}

// commentChainBreaksBeforeNext reports whether the run of tokens between
// commentIdx (a comment that terminates with ;) and nextIdx (the next
// significant code token) contains a paragraph break — either a blank line
// (whitespace token spanning more than one newline) or another standalone
// comment on its own line. Such breaks indicate the original comment ended
// deliberately and what follows is unrelated; the multi-line "broken-out
// keyword" heuristic should not fire across them. See issue #6.
func commentChainBreaksBeforeNext(tokens []lexer.Token, commentIdx, nextIdx int) bool {
	for j := commentIdx + 1; j < nextIdx && j < len(tokens); j++ {
		t := tokens[j]
		if t.Type == lexer.TokenWhitespace && strings.Count(t.Text, "\n") >= 2 {
			return true
		}
		if t.Type == lexer.TokenComment {
			return true
		}
	}
	return false
}

// checkAssignmentInCondition detects := assignment operator used in IF/WHILE/CASE conditions.
// This is usually a mistake - the developer likely meant = or == for comparison.
// Gotcha #9 in gotchas.md.
func checkAssignmentInCondition(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Track when we're inside a condition (between IF/WHILE/CASE and semicolon)
	inCondition := false
	var conditionKeyword *lexer.Token
	// Bracket depth inside the condition. Only a top-level `:=` reads as a
	// mistyped comparison: a parenthesized assignment (`(x := f()) != NIL`)
	// is the deliberate assign-and-test idiom, and an assignment inside a
	// call argument or array literal is an argument, not the condition
	// (issue #255).
	depth := 0

	for i := range tokens {
		token := &tokens[i]

		// Skip whitespace and comments
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}

		// Detect condition-starting keywords
		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
			if normalized == "IF" || normalized == "WHILE" || normalized == "CASE" {
				inCondition = true
				conditionKeyword = token
				depth = 0
				continue
			}
		}

		if inCondition && token.Type == lexer.TokenPunctuation {
			switch token.Text {
			case "(", "[", "{":
				depth++
			case ")", "]", "}":
				if depth > 0 {
					depth--
				}
			}
		}

		// End of condition
		if token.Type == lexer.TokenPunctuation && token.Text == ";" {
			inCondition = false
			conditionKeyword = nil
			continue
		}

		// Detect := in condition
		if inCondition && depth == 0 && token.Type == lexer.TokenOperator && token.Text == ":=" {
			keywordName := "condition"
			if conditionKeyword != nil {
				keywordName = strings.ToUpper(strings.TrimPrefix(conditionKeyword.Text, ":"))
			}
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(*token),
				Message:  fmt.Sprintf("Assignment ':=' used in %s condition - did you mean '=' or '=='?", keywordName),
				Source:   "ssl-lsp",
				Code:     CodeAssignmentInCondition,
			})
		}
	}

	return diagnostics
}

// dotAccessReceiverBefore reports whether the significant token before a
// `.name` fragment can end a receiver: an identifier, or the `)` / `]`
// closing a call or subscript.
func dotAccessReceiverBefore(tokens []lexer.Token, i int) bool {
	prev := previousSignificantTokenIndex(tokens, i-1)
	if prev < 0 {
		return false
	}
	t := tokens[prev]
	return t.Type == lexer.TokenIdentifier ||
		(t.Type == lexer.TokenPunctuation && (t.Text == ")" || t.Text == "]"))
}

// isUnquotedDispatchPath reports whether the `.name` fragment at i follows
// the bare first argument of an ExecFunction/DoProc call
// (`ExecFunction(Module.Proc, ...)`).
func isUnquotedDispatchPath(tokens []lexer.Token, i int) bool {
	ident := previousSignificantTokenIndex(tokens, i-1)
	if ident < 0 || tokens[ident].Type != lexer.TokenIdentifier {
		return false
	}
	open := previousSignificantTokenIndex(tokens, ident-1)
	if open < 0 || tokens[open].Type != lexer.TokenPunctuation || tokens[open].Text != "(" {
		return false
	}
	callee := previousSignificantTokenIndex(tokens, open-1)
	if callee < 0 || tokens[callee].Type != lexer.TokenIdentifier {
		return false
	}
	upper := strings.ToUpper(tokens[callee].Text)
	return upper == "EXECFUNCTION" || upper == "DOPROC"
}

// checkDotPropertyAccess detects identifier.identifier patterns that look like
// property access using dot notation (common in other languages).
// SSL uses colon notation: object:property instead of object.property.
// Gotcha #8 in gotchas.md.
func checkDotPropertyAccess(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Issue #56: skip dots inside :INCLUDE module paths. These can be deep
	// (e.g. :INCLUDE A.B.C.D;) and the lexer breaks them into Unknown
	// chunks like ".B.", so a single-token lookback is insufficient. Track
	// :INCLUDE statement scope explicitly. :INHERIT qualified base names
	// (:INHERIT Category.ScriptName;) get the same exemption (issue #149).
	inInclude := false

	for i, token := range tokens {
		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
			if normalized == "INCLUDE" || normalized == "INHERIT" {
				inInclude = true
				continue
			}
		}
		if inInclude {
			if token.Type == lexer.TokenPunctuation && token.Text == ";" {
				inInclude = false
				continue
			}
			// Also handle the case where the trailing ';' is glued to the
			// last Unknown token (lexer emits ".D;" as one token).
			if token.Type == lexer.TokenUnknown && strings.HasSuffix(token.Text, ";") {
				inInclude = false
			}
			continue
		}

		// Look for TokenUnknown that starts with a dot followed by identifier chars
		if token.Type != lexer.TokenUnknown {
			continue
		}

		// Check if it looks like .identifier
		if !strings.HasPrefix(token.Text, ".") {
			continue
		}

		rest := token.Text[1:]
		if len(rest) == 0 {
			continue
		}

		// Extract identifier portion from rest (may have trailing non-identifier chars like semicolons)
		propName := extractIdentifier(rest)
		if len(propName) == 0 {
			continue
		}

		// Preceded by a receiver: a name, or a call or subscript result
		// (`GetObj().Name`, `aRows[1].Name`, issue #255). An unquoted
		// ExecFunction/DoProc path is execfunction_missing_quotes' finding,
		// with the fix-it message, so it is not reported twice.
		if dotAccessReceiverBefore(tokens, i) && !isUnquotedDispatchPath(tokens, i) {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("SSL uses colon ':' for property access, not dot '.'. Use 'object:%s' instead of 'object.%s'", propName, propName),
				Source:   "ssl-lsp",
				Code:     CodeDotPropertyAccess,
			})
		}
	}

	return diagnostics
}

// extractIdentifier extracts the identifier portion from the start of a string.
func extractIdentifier(s string) string {
	var result strings.Builder
	for i, ch := range s {
		if i == 0 {
			if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_' {
				result.WriteRune(ch)
			} else {
				break
			}
		} else {
			if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') || ch == '_' {
				result.WriteRune(ch)
			} else {
				break
			}
		}
	}
	return result.String()
}

// checkClassInstantiationSyntax detects ClassName() patterns for SSL built-in classes.
// SSL uses curly braces for class instantiation: Email{}, SSLRegex{}, etc.
// Gotcha #15 in gotchas.md.
func checkClassInstantiationSyntax(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Build a case-insensitive set of class names
	classNames := make(map[string]string) // uppercase -> original
	for _, cls := range constants.SSLClassNames {
		classNames[strings.ToUpper(cls)] = cls
	}

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		// Check if this identifier is a class name
		originalName, isClass := classNames[strings.ToUpper(token.Text)]
		if !isClass {
			continue
		}

		// Skip qualified access: oSvc:Email(...) is a member call on a user
		// object, not an instantiation of the built-in class.
		if prevIdx := previousSignificantTokenIndex(tokens, i-1); prevIdx >= 0 {
			p := tokens[prevIdx]
			if p.Type == lexer.TokenPunctuation && p.Text == ":" {
				continue
			}
		}

		// Look ahead for '(' (skip whitespace)
		for j := i + 1; j < len(tokens); j++ {
			if tokens[j].Type == lexer.TokenWhitespace || tokens[j].Type == lexer.TokenComment {
				continue
			}
			if tokens[j].Type == lexer.TokenPunctuation && tokens[j].Text == "(" {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  fmt.Sprintf("SSL built-in class '%s' uses curly braces for instantiation: '%s{}' not '%s()'", originalName, originalName, originalName),
					Source:   "ssl-lsp",
					Code:     CodeClassInstantiationCurly,
				})
			}
			break
		}
	}

	return diagnostics
}

// checkCreateUdObjectBuiltinClassMisuse detects CreateUdObject("BuiltInClass")
// patterns. The source guide reserves CreateUdObject string dispatch for
// user-defined :CLASS objects; built-in classes must use curly braces.
func checkCreateUdObjectBuiltinClassMisuse(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenIdentifier || !strings.EqualFold(token.Text, "CreateUdObject") {
			continue
		}
		// oFactory:CreateUdObject("Email") is a method on some receiver,
		// not the builtin (D10: member access never warns; #258).
		if prevIdx := previousSignificantTokenIndex(tokens, i-1); prevIdx >= 0 &&
			tokens[prevIdx].Type == lexer.TokenPunctuation && tokens[prevIdx].Text == ":" {
			continue
		}

		openParenIdx := nextSignificantTokenIndex(tokens, i+1)
		if openParenIdx < 0 || tokens[openParenIdx].Type != lexer.TokenPunctuation || tokens[openParenIdx].Text != "(" {
			continue
		}

		argStarts, argEnds, closeIdx := parseTopLevelCallArguments(tokens, openParenIdx)
		if closeIdx < 0 || len(argStarts) == 0 || len(argEnds) == 0 {
			continue
		}

		argStart := argStarts[0]
		argEnd := argEnds[0]
		if argStart < 0 || argEnd != argStart || tokens[argStart].Type != lexer.TokenString {
			continue
		}

		className := strings.TrimSpace(unquoteSSLString(tokens[argStart].Text))
		if className == "" || !constants.IsSSLClass(className) {
			continue
		}

		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(tokens[argStart]),
			Message:  fmt.Sprintf("Built-in SSL class '%s' must use curly-brace construction ('%s{}'), not CreateUdObject(\"%s\")", className, className, className),
			Source:   "ssl-lsp",
			Code:     CodeCreateUdObjectBuiltinMisuse,
		})
	}

	return diagnostics
}

// netDerivedRHS reports whether an assignment's right-hand side (starting
// after the `:=`, ending at the statement's `;`) produces a value from the
// .NET interop surface: a colon member call (oInt:ToByteArray()) or a
// LimsNetConnect/LimsNetCast result. Such values may be 0-based collections
// (issue #166).
func netDerivedRHS(tokens []lexer.Token, start int) bool {
	for j := start; j < len(tokens); j++ {
		t := tokens[j]
		if t.Type == lexer.TokenPunctuation && t.Text == ";" {
			break
		}
		if t.Type == lexer.TokenPunctuation && t.Text == ":" {
			return true
		}
		if t.Type == lexer.TokenIdentifier {
			upper := strings.ToUpper(t.Text)
			if upper == "LIMSNETCONNECT" || upper == "LIMSNETCAST" {
				return true
			}
		}
	}
	return false
}

// zeroSubscript is one literal `0` subscript found in the expression tree.
type zeroSubscript struct {
	// base is the identifier at the root of the subscripted receiver
	// (`aRows` in `aRows[1][0]`), or "" when the receiver is reached
	// through member access.
	base string
	// member is true when the receiver is reached through `:` member
	// access (`dataSet:Tables[0]`).
	member bool
	// dataTables is true when the receiver chain passes through a
	// `:Tables` member: the .NET DataSet's table collection and the
	// DataTable members reached from it, all 0-based (issue #304).
	dataTables bool
}

// throughTablesMember reports whether a subscript receiver's member chain
// passes through a `:Tables` member (`ds:Tables[0]`,
// `ds:Tables[0]:Rows[0]`). No SSL built-in class has a Tables member; it
// is the .NET DataSet's 0-based table collection, so index 0 there and on
// the DataTable members reached from it is valid (issue #304).
func throughTablesMember(e *parser.Expr) bool {
	for e != nil {
		if e.Kind == parser.ExprMember && strings.EqualFold(e.Name, "Tables") {
			return true
		}
		switch e.Kind {
		case parser.ExprMember, parser.ExprIndex, parser.ExprCall:
			if len(e.Children) == 0 {
				return false
			}
			e = e.Children[0]
		default:
			return false
		}
	}
	return false
}

// collectZeroSubscripts records every subscript that is exactly the
// literal `0`, in every dimension of `a[i, j]` and `a[i][j]`, keyed by the
// literal's token index. Receivers that are calls, groups, or anything
// else without a provable SSL-array reading are skipped: unknown is never
// evidence.
func collectZeroSubscripts(tokens []lexer.Token, stmts []parser.StatementExprs) map[int]zeroSubscript {
	found := make(map[int]zeroSubscript)
	var walk func(e *parser.Expr)
	walk = func(e *parser.Expr) {
		if e == nil {
			return
		}
		if e.Kind == parser.ExprIndex && len(e.Children) > 1 {
			recv := e.Children[0]
			for recv.Kind == parser.ExprIndex && len(recv.Children) > 0 {
				recv = recv.Children[0]
			}
			var site zeroSubscript
			ok := true
			switch recv.Kind {
			case parser.ExprIdentifier:
				site.base = strings.ToUpper(recv.Name)
			case parser.ExprMember:
				site.member = true
				site.dataTables = throughTablesMember(recv)
			default:
				ok = false
			}
			if ok {
				for _, sub := range e.Children[1:] {
					if sub.Kind == parser.ExprLiteral && sub.Name == "0" &&
						sub.Start >= 0 && sub.Start < len(tokens) && tokens[sub.Start].Type == lexer.TokenNumber {
						found[sub.Start] = site
					}
				}
			}
		}
		for _, c := range e.Children {
			walk(c)
		}
	}
	for i := range stmts {
		for _, e := range stmts[i].Exprs {
			walk(e)
		}
	}
	return found
}

// checkZeroBasedArrayIndex detects a literal `0` subscript. SSL arrays are
// 1-based, so index 0 on a native array is invalid (gotcha #5). The
// subscripts come from the expression tree, so every dimension of a
// multi-dimensional access is checked (issue #255). A value reached through
// `:` member access, or a variable whose most recent assignment was
// .NET-derived, may be a 0-based .NET collection — `dataSet:Tables[0]` is
// the prescribed form there — so those sites get an info-tier advisory
// instead of the error (issues #152/#166/#255).
func checkZeroBasedArrayIndex(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var diagnostics []Diagnostic

	sites := collectZeroSubscripts(tokens, stmts)
	if len(sites) == 0 {
		return diagnostics
	}

	// Most recent assignment per variable (file order): true when the RHS
	// was .NET-derived, making a later [0] on that variable plausibly valid
	// (issue #166: aBytes := oInt:ToByteArray(); aBytes[0]).
	netDerived := make(map[string]bool)

	for i, token := range tokens {
		// Track `ident := RHS;` assignments (property assignments
		// obj:Prop := ... track the object's member, not a variable — skip).
		if token.Type == lexer.TokenIdentifier {
			if n := nextSignificantTokenIndex(tokens, i+1); n >= 0 &&
				tokens[n].Type == lexer.TokenOperator && tokens[n].Text == ":=" {
				if k := previousSignificantTokenIndex(tokens, i-1); !(k >= 0 &&
					tokens[k].Type == lexer.TokenPunctuation && tokens[k].Text == ":") {
					netDerived[strings.ToUpper(token.Text)] = netDerivedRHS(tokens, n+1)
				}
			}
		}

		site, ok := sites[i]
		if !ok || site.dataTables {
			continue
		}
		if site.member || netDerived[site.base] {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range:    tokenToRange(token),
				Message:  "Index 0 on a .NET-derived value may be valid — .NET collections are 0-based. Native SSL arrays are 1-based; verify which one this is.",
				Source:   "ssl-lsp",
				Code:     CodeZeroBasedArrayIndex,
			})
			continue
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(token),
			Message:  "SSL arrays are 1-based; index 0 is invalid. Use index 1 for the first element.",
			Source:   "ssl-lsp",
			Code:     CodeZeroBasedArrayIndex,
		})
	}

	return diagnostics
}

// checkNamedSQLParamsWithWrongFunction detects ?varName? syntax used with
// functions that don't support named parameters.
// Per the SSL style guide, only SQLExecute supports ?varName? syntax.
// Gotcha #7 in gotchas.md.
func checkNamedSQLParamsWithWrongFunction(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Build set of functions that DON'T support named params
	parameterizedFuncs := make(map[string]bool)
	for _, fn := range constants.ParameterizedSQLFunctions {
		parameterizedFuncs[strings.ToUpper(fn)] = true
	}

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		// Check if this is a parameterized SQL function
		if !parameterizedFuncs[strings.ToUpper(token.Text)] {
			continue
		}
		// A member call (oDb:RunSQL(...)) is the object's own method, not
		// the built-in (DECISIONS.md D10).
		if prev := prevSignificantTokenIndex(tokens, i-1); prev >= 0 &&
			tokens[prev].Type == lexer.TokenPunctuation && tokens[prev].Text == ":" {
			continue
		}

		funcName := token.Text

		// Look ahead for '(' then find the first string argument
		inCall := false
		parenDepth := 0
		for j := i + 1; j < len(tokens); j++ {
			t := tokens[j]

			if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
				continue
			}

			if t.Type == lexer.TokenPunctuation && t.Text == "(" {
				if !inCall {
					inCall = true
				}
				parenDepth++
				continue
			}

			if t.Type == lexer.TokenPunctuation && t.Text == ")" {
				parenDepth--
				if parenDepth <= 0 {
					break
				}
				continue
			}

			// Found a string in the function call
			if inCall && parenDepth == 1 && t.Type == lexer.TokenString {
				// Check for named parameters in this string
				content := t.Text
				if len(content) >= 2 {
					content = content[1 : len(content)-1] // Remove quotes
				}
				placeholders := ParseSQLPlaceholders(content)
				for _, ph := range placeholders {
					if ph.IsNamed {
						diagnostics = append(diagnostics, Diagnostic{
							Severity: SeverityError,
							Range:    tokenToRange(t),
							Message:  fmt.Sprintf("Named SQL parameter '?%s?' not supported by '%s'. Use positional '?' with value array, or use 'SQLExecute' for named parameters.", ph.Name, funcName),
							Source:   "ssl-lsp",
							Code:     CodeNamedSqlParamUnsupported,
						})
						break // One warning per string is enough
					}
				}
				break // Only check first string argument
			}
		}
	}

	return diagnostics
}

// checkComplexSQLPlaceholders warns when SQLExecute calls contain named placeholders
// with complex expressions (property access, array indexing, function calls).
// These are evaluated on every query execution and should be pre-computed into variables.
// Gotcha #20 in gotchas.md.
func checkComplexSQLPlaceholders(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		if !strings.EqualFold(token.Text, "SQLExecute") {
			continue
		}

		// Look ahead for '(' then find the first string argument
		inCall := false
		parenDepth := 0
		for j := i + 1; j < len(tokens); j++ {
			t := tokens[j]

			if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
				continue
			}

			if t.Type == lexer.TokenPunctuation && t.Text == "(" {
				if !inCall {
					inCall = true
				}
				parenDepth++
				continue
			}

			if t.Type == lexer.TokenPunctuation && t.Text == ")" {
				parenDepth--
				if parenDepth <= 0 {
					break
				}
				continue
			}

			// Found a string in the function call — check its placeholders
			if inCall && parenDepth == 1 && t.Type == lexer.TokenString {
				content := t.Text
				if len(content) >= 2 {
					content = content[1 : len(content)-1]
				}
				placeholders := ParseSQLPlaceholders(content)
				for _, ph := range placeholders {
					if ph.IsNamed && !isSimpleNamedPlaceholder(ph.Name) {
						diagnostics = append(diagnostics, Diagnostic{
							Severity: SeverityInfo,
							Range:    stringContentRange(t, ph.Start, len(ph.Name)+2),
							Message:  fmt.Sprintf("Complex expression '?%s?' in SQLExecute placeholder is evaluated on every execution. Pre-compute into a variable for better performance.", ph.Name),
							Source:   "ssl-lsp",
							Code:     CodeComplexSqlPlaceholder,
						})
					}
				}
				break // Only check first string argument
			}
		}
	}

	return diagnostics
}

// asciiUpper upper-cases ASCII letters only, so the result has exactly the
// byte length and offsets of s — safe to slice by offsets computed on s.
func asciiUpper(s string) string {
	b := []byte(s)
	for i, c := range b {
		if c >= 'a' && c <= 'z' {
			b[i] = c - ('a' - 'A')
		}
	}
	return string(b)
}

// stringContentRange returns the source range of the `length` bytes that
// start at byte offset `start` of a string token's content (the text
// between its delimiters). A multi-line string is handled: the line
// advances past every newline before the offset, and the character
// restarts from that line's first column (issue #255 — the range used to
// stay on the token's first line, past its end).
func stringContentRange(tok lexer.Token, start, length int) Range {
	line := tok.Line - 1
	// tok.Column is 1-based; +1 skips the opening delimiter.
	char := tok.Column + start
	if end := 1 + start; end <= len(tok.Text) {
		before := tok.Text[1:end]
		if nl := strings.LastIndexByte(before, '\n'); nl >= 0 {
			line += strings.Count(before, "\n")
			char = len(before) - nl - 1
		}
	}
	return Range{
		Start: Position{Line: line, Character: char},
		End:   Position{Line: line, Character: char + length},
	}
}

// checkUDObjectArrayInClause detects UDObject property access in SQL IN clause
// array expansion placeholders. Using ?oObj:ArrayProp? directly in an IN clause
// causes runtime error "The current array has more than 1 dimmension." —
// the array must be copied to a local variable first. Scalar properties are fine
// outside IN clauses, so this only flags property-access placeholders inside IN(...).
func checkUDObjectArrayInClause(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenString {
			continue
		}

		content := token.Text
		if len(content) < 2 {
			continue
		}
		// Strip surrounding quotes
		content = content[1 : len(content)-1]

		// ASCII-only upper-casing keeps every byte offset aligned with
		// content: strings.ToUpper can change a string's byte length (a
		// Turkish dotless ı is two bytes but upper-cases to the one-byte I),
		// and slicing the result by a content offset panicked (issue #255).
		upper := asciiUpper(content)
		placeholders := ParseSQLPlaceholders(content)

		for _, ph := range placeholders {
			if !ph.IsNamed {
				continue
			}
			// Only flag property access (contains ':')
			if !strings.Contains(ph.Name, ":") {
				continue
			}
			// Check if this placeholder sits inside an IN(...) clause
			// Look backwards from the placeholder start for "IN" keyword
			prefix := strings.TrimRight(upper[:ph.Start], " \t")
			if !strings.HasSuffix(prefix, "(") {
				continue
			}
			prefix = strings.TrimRight(prefix[:len(prefix)-1], " \t")
			if !strings.HasSuffix(prefix, "IN") {
				continue
			}
			// Verify "IN" is not part of a longer word
			inStart := len(prefix) - 2
			if inStart > 0 {
				ch := prefix[inStart-1]
				if (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_' {
					continue
				}
			}

			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    stringContentRange(token, ph.Start, len(ph.Name)+2),
				Message: fmt.Sprintf(
					"UDObject array property '?%s?' in IN clause causes runtime error "+
						"'The current array has more than 1 dimmension.' — "+
						"copy the array to a local variable first.",
					ph.Name,
				),
				Source: "ssl-lsp",
				Code:   CodeUdObjectArrayInClause,
			})
		}
	}

	return diagnostics
}

// checkProcedureDeclarationSyntax flags two malformed procedure declarations:
//   - bare "PROCEDURE Name(...)" with no leading colon
//   - ":PROCEDURE Name(...)" with parentheses after the name
//
// Both shapes are common typos from users coming from C-style languages.
// SSL declares procedures as ":PROCEDURE Name;" and accepts arguments via a
// separate ":PARAMETERS ..." statement. Running this check ahead of
// checkDirectProcedureCalls ensures the user sees the syntax error rather
// than a misleading "custom procedures cannot be called directly" message.
func checkProcedureDeclarationSyntax(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		tok := tokens[i]
		isKeywordProc := tok.Type == lexer.TokenKeyword &&
			strings.EqualFold(strings.TrimPrefix(tok.Text, ":"), "PROCEDURE")
		isBareProc := tok.Type == lexer.TokenIdentifier && strings.EqualFold(tok.Text, "PROCEDURE")
		if !isKeywordProc && !isBareProc {
			continue
		}

		nameIdx := nextSignificantTokenIndex(tokens, i+1)
		if nameIdx < 0 || tokens[nameIdx].Type != lexer.TokenIdentifier {
			continue
		}

		// For the bare-PROCEDURE case, only flag if the user is actually
		// trying to declare a procedure: the name must be followed by `(`.
		// For the keyword case, the parens are the diagnostic trigger.
		parenIdx := nextSignificantTokenIndex(tokens, nameIdx+1)
		if parenIdx < 0 {
			continue
		}
		paren := tokens[parenIdx]
		if !(paren.Type == lexer.TokenPunctuation && paren.Text == "(") {
			continue
		}

		if isBareProc {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(tok),
				Message:  `Procedure declarations require a leading colon. Use ":PROCEDURE Name;" (no parentheses; declare arguments via ":PARAMETERS").`,
				Source:   "ssl-lsp",
				Code:     CodeProcedureDeclarationSyntax,
			})
		} else {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(paren),
				Message:  `Procedure declarations don't take parentheses. Use ":PROCEDURE Name;" and declare arguments via ":PARAMETERS".`,
				Source:   "ssl-lsp",
				Code:     CodeProcedureDeclarationSyntax,
			})
		}
	}

	return diagnostics
}

// checkDirectProcedureCalls detects attempts to call procedures directly.
// SSL requires DoProc("name", {params}) or ExecFunction("Module.name", {params}).
// Gotcha #1 in gotchas.md.
// Severity is tiered (issue #167): calling a procedure declared in this file
// is a definite misuse (error); an unknown bare callable cannot be
// distinguished from a vendor built-in missing from the published inventory
// (SetLocationSQLServer, LimsCleanUp, SetAMPM in SYSTEMINIT-era stock
// scripts), so it warns instead.
func checkDirectProcedureCalls(tokens []lexer.Token, ast *parser.Node, p *parser.Parser) []Diagnostic {
	var diagnostics []Diagnostic

	inFileProcs := make(map[string]bool)
	procs := p.ExtractProcedures(ast)
	for _, proc := range procs {
		inFileProcs[strings.ToUpper(proc.Name)] = true
	}
	// insideOwnProcedure reports whether a call on line `line` sits in the
	// body of the procedure it names. That is the shape of a procedure
	// wrapping a same-named built-in the published inventory does not
	// list, so the error tier's proof (the target is this file's
	// procedure) does not hold there (issue #304).
	insideOwnProcedure := func(upperName string, line int) bool {
		for _, proc := range procs {
			if strings.ToUpper(proc.Name) == upperName && line > proc.StartLine && line <= proc.EndLine {
				return true
			}
		}
		return false
	}

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		if constants.IsSSLFunction(token.Text) || constants.IsSSLClass(token.Text) {
			continue
		}

		previousIdx := previousSignificantTokenIndex(tokens, i-1)
		if previousIdx >= 0 {
			prev := tokens[previousIdx]
			if prev.Type == lexer.TokenPunctuation && prev.Text == ":" {
				continue
			}
		}

		upperName := strings.ToUpper(token.Text)
		if upperName == "DOPROC" || upperName == "EXECFUNCTION" || upperName == "EXECUDF" || upperName == "EVAL" {
			continue
		}

		// `bA AND (bB)` / `NOT (bA)`: a bare logical word in operator
		// position is diag.bare_logical_operator's finding, not a call
		// (issue #255).
		if bareLogicalOperatorPosition(tokens, i) {
			continue
		}

		for j := i + 1; j < len(tokens); j++ {
			if tokens[j].Type == lexer.TokenWhitespace || tokens[j].Type == lexer.TokenComment {
				continue
			}
			if tokens[j].Type == lexer.TokenPunctuation && tokens[j].Text == "(" {
				isDeclaration := false
				for k := i - 1; k >= 0; k-- {
					if tokens[k].Type == lexer.TokenWhitespace {
						if strings.Contains(tokens[k].Text, "\n") {
							break
						}
						continue
					}
					if tokens[k].Type == lexer.TokenKeyword {
						normalized := strings.ToUpper(strings.TrimPrefix(tokens[k].Text, ":"))
						if normalized == "PROCEDURE" {
							isDeclaration = true
						}
					}
					// Defense in depth: malformed "PROCEDURE Name(" (no leading
					// colon) is reported by checkProcedureDeclarationSyntax;
					// also recognise it here so the direct-call rule doesn't
					// double-fire on the same typo.
					if tokens[k].Type == lexer.TokenIdentifier &&
						strings.EqualFold(tokens[k].Text, "PROCEDURE") {
						isDeclaration = true
					}
					break
				}

				if !isDeclaration {
					severity := SeverityError
					message := fmt.Sprintf("Custom procedures cannot be called directly. Use DoProc(\"%s\", {args}) for same-file script procedures, ExecFunction(...) for external script procedures, or Me:/Base: inside classes.", token.Text)
					switch {
					case !inFileProcs[upperName]:
						// Unknown callable: possibly an uncataloged vendor
						// built-in rather than a custom procedure (issue #167).
						severity = SeverityWarning
						message = fmt.Sprintf("'%s' is not a known built-in function or in-file procedure. If it is a custom procedure, dispatch it via DoProc/ExecFunction; if it is a legacy vendor built-in, this call is valid as written.", token.Text)
					case insideOwnProcedure(upperName, token.Line):
						// A procedure calling its own name: recursion
						// written C-style, or a wrapper around a same-named
						// vendor built-in (issue #304).
						severity = SeverityWarning
						message = fmt.Sprintf("'%s' is called inside its own procedure. If this is recursion, dispatch it via DoProc(\"%s\", {args}); if the procedure wraps a vendor built-in of the same name, this call is valid as written.", token.Text, token.Text)
					}
					diagnostics = append(diagnostics, Diagnostic{
						Severity: severity,
						Range:    tokenToRange(token),
						Message:  message,
						Source:   "ssl-lsp",
						Code:     CodeDirectProcedureCall,
					})
				}
			}
			break
		}
	}

	return diagnostics
}

// unquotedPathEnd returns the index of the last token of an unquoted
// namespace path whose first `.segment` token is tokens[k]: the path runs
// on through glued identifiers and `.`-led tokens (`Billing.Tax.Rate`
// lexes as Billing, `.Tax.`, Rate).
func unquotedPathEnd(tokens []lexer.Token, k int) int {
	last := k
	for j := k + 1; j < len(tokens); j++ {
		t := tokens[j]
		if t.Type == lexer.TokenIdentifier || (t.Type == lexer.TokenUnknown && strings.HasPrefix(t.Text, ".")) ||
			(t.Type == lexer.TokenPunctuation && t.Text == ".") {
			last = j
			continue
		}
		break
	}
	return last
}

// unquotedPathTokens marks the tokens of every unquoted ExecFunction /
// DoProc namespace path (execfunction_missing_quotes), so other rules
// leave its segments to that finding — `Billing` and `Rate` in
// `ExecFunction(Billing.Tax.Rate)` are not undeclared variables
// (issue #328).
func unquotedPathTokens(tokens []lexer.Token) map[int]bool {
	marked := map[int]bool{}
	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier {
			continue
		}
		upper := strings.ToUpper(token.Text)
		if upper != "EXECFUNCTION" && upper != "DOPROC" {
			continue
		}
		j := i + 1
		for j < len(tokens) && (tokens[j].Type == lexer.TokenWhitespace || tokens[j].Type == lexer.TokenComment) {
			j++
		}
		if j >= len(tokens) || tokens[j].Text != "(" {
			continue
		}
		j++
		for j < len(tokens) && (tokens[j].Type == lexer.TokenWhitespace || tokens[j].Type == lexer.TokenComment) {
			j++
		}
		if j+1 >= len(tokens) || tokens[j].Type != lexer.TokenIdentifier ||
			tokens[j+1].Type != lexer.TokenUnknown || !strings.HasPrefix(tokens[j+1].Text, ".") {
			continue
		}
		for k := j; k <= unquotedPathEnd(tokens, j+1); k++ {
			marked[k] = true
		}
	}
	return marked
}

// checkMissingQuotesInExecFunction detects ExecFunction(Module.Proc, ...) patterns
// where the namespace path is not quoted.
// Related to Gotcha #8 (dot notation).
func checkMissingQuotesInExecFunction(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		// Check if this is ExecFunction or DoProc
		upper := strings.ToUpper(token.Text)
		if upper != "EXECFUNCTION" && upper != "DOPROC" {
			continue
		}

		funcName := token.Text

		// Look ahead for '(' then check first argument
		inCall := false
		for j := i + 1; j < len(tokens); j++ {
			t := tokens[j]

			if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
				continue
			}

			if t.Type == lexer.TokenPunctuation && t.Text == "(" {
				inCall = true
				continue
			}

			if inCall {
				// First non-whitespace token after '(' should be the first argument
				// If it's an identifier followed by TokenUnknown starting with '.', that's the error
				if t.Type == lexer.TokenIdentifier {
					// Look ahead for .identifier pattern (TokenUnknown)
					for k := j + 1; k < len(tokens); k++ {
						if tokens[k].Type == lexer.TokenWhitespace || tokens[k].Type == lexer.TokenComment {
							continue
						}
						if tokens[k].Type == lexer.TokenUnknown && strings.HasPrefix(tokens[k].Text, ".") {
							// Found identifier.something pattern without quotes.
							// The range runs to the path's last segment
							// (`Billing.Tax.Rate`, issue #328).
							last := unquotedPathEnd(tokens, k)
							diagnostics = append(diagnostics, Diagnostic{
								Severity: SeverityError,
								Range: Range{
									Start: Position{Line: t.Line - 1, Character: t.Column - 1},
									End:   Position{Line: tokens[last].Line - 1, Character: tokens[last].Column - 1 + len(tokens[last].Text)},
								},
								Message: fmt.Sprintf("Namespace path must be quoted: %s(\"Module.Procedure\", ...) not %s(Module.Procedure, ...)", funcName, funcName),
								Source:  "ssl-lsp",
								Code:    CodeExecFunctionMissingQuotes,
							})
						}
						break
					}
				}
				break
			}
		}
	}

	return diagnostics
}

// checkClassContextRules validates class-specific constraints from the updated style guide.
func checkClassContextRules(tokens []lexer.Token, ast *parser.Node, p *parser.Parser) []Diagnostic {
	var diagnostics []Diagnostic

	var classTokens []lexer.Token
	for _, token := range tokens {
		if token.Type == lexer.TokenKeyword &&
			strings.ToUpper(strings.TrimPrefix(token.Text, ":")) == "CLASS" {
			classTokens = append(classTokens, token)
		}
	}

	if len(classTokens) == 0 {
		procedures := p.ExtractProcedures(ast)
		for _, proc := range procedures {
			if strings.EqualFold(proc.Name, "Constructor") {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range: Range{
						Start: Position{Line: proc.StartLine - 1, Character: 0},
						End:   Position{Line: proc.StartLine - 1, Character: len(proc.Name)},
					},
					Message: "'Constructor' is only meaningful inside a ':CLASS' definition",
					Source:  "ssl-lsp",
					Code:    CodeConstructorOutsideClass,
				})
			}
		}
		return diagnostics
	}

	if len(classTokens) > 1 {
		for _, token := range classTokens[1:] {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "Only one ':CLASS' definition is allowed per file",
				Source:   "ssl-lsp",
				Code:     CodeOneClassPerFile,
			})
		}
	}

	classStartLine := classTokens[0].Line
	if !isFirstSignificantStatementKeyword(tokens, "CLASS") {
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(classTokens[0]),
			Message:  "A file is either a ':CLASS' definition or a script; ':CLASS' must be the first significant statement",
			Source:   "ssl-lsp",
			Code:     CodeClassOrScript,
		})
	}

	diagnostics = append(diagnostics, checkClassMemberOrder(tokens, classTokens[0])...)

	procedures := p.ExtractProcedures(ast)
	for _, proc := range procedures {
		isClassMethod := proc.StartLine > classStartLine
		if strings.EqualFold(proc.Name, "Constructor") && !isClassMethod {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range: Range{
					Start: Position{Line: proc.StartLine - 1, Character: 0},
					End:   Position{Line: proc.StartLine - 1, Character: len(proc.Name)},
				},
				Message: "'Constructor' is only meaningful inside a ':CLASS' definition",
				Source:  "ssl-lsp",
				Code:    CodeConstructorOutsideClass,
			})
		}
	}

	classMethodRanges := make([]parser.ProcedureInfo, 0, len(procedures))
	for _, proc := range procedures {
		if proc.StartLine > classStartLine {
			classMethodRanges = append(classMethodRanges, proc)
		}
	}

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier || !strings.EqualFold(token.Text, "DoProc") {
			continue
		}

		if !tokenInProcedureRange(token, classMethodRanges) {
			continue
		}

		for j := i + 1; j < len(tokens); j++ {
			if tokens[j].Type == lexer.TokenWhitespace || tokens[j].Type == lexer.TokenComment {
				continue
			}
			if tokens[j].Type == lexer.TokenPunctuation && tokens[j].Text == "(" {
				// Only a string-literal target without a '.' qualifier is
				// provably a class-local/base call; qualified
				// "Category.Script.Procedure" references to deployed
				// procedures are valid inside class methods, and
				// non-literal targets are not provable (issue #151,
				// ssl-style-guide#49).
				if isUnqualifiedStringTarget(tokens, j+1) {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  "Unqualified DoProc targets are a compile-time error inside class methods. Use Me:MethodName() / Base:MethodName(), or a fully qualified \"Category.Script.Procedure\" reference for a deployed procedure.",
						Source:   "ssl-lsp",
						Code:     CodeDoProcInClass,
					})
				}
			}
			break
		}
	}

	diagnostics = append(diagnostics, checkUnqualifiedFieldAssignment(tokens, classStartLine, classMethodRanges)...)

	for _, proc := range classMethodRanges {
		if !strings.EqualFold(proc.Name, "Constructor") {
			continue
		}

		for i := range tokens {
			token := tokens[i]
			if token.Line < proc.StartLine || token.Line > proc.EndLine {
				continue
			}
			if token.Type != lexer.TokenKeyword ||
				strings.ToUpper(strings.TrimPrefix(token.Text, ":")) != "RETURN" {
				continue
			}

			hasReturnValue := false
			for j := i + 1; j < len(tokens); j++ {
				next := tokens[j]
				if next.Line > proc.EndLine {
					break
				}
				if next.Type == lexer.TokenWhitespace || next.Type == lexer.TokenComment {
					continue
				}
				if next.Type == lexer.TokenPunctuation && next.Text == ";" {
					break
				}
				hasReturnValue = true
				break
			}

			if hasReturnValue {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':RETURN' inside a Constructor cannot return a value",
					Source:   "ssl-lsp",
					Code:     CodeConstructorReturnValue,
				})
			}
		}
	}

	return diagnostics
}

// checkUnqualifiedFieldAssignment flags bare uses of a class field inside
// a class method. Inside a method a bare identifier follows ordinary
// variable lookup — local, then the callers' scopes, then publics — and
// never reaches the :DECLAREd field (ssl-style-guide field_access_via_me).
// So a bare write (`nTotal := 0`, `nTotal++`, `aItems[1] := x`) leaves the
// field unchanged (unqualified_field_assignment), and a bare read
// (`:RETURN nTotal`) raises `Variable [nTotal] is undefined!` or silently
// reads someone else's variable (unqualified_field_read, issue #260). The
// fix for both is `Me:fieldName`.
//
// Targets and reads come from the expression tree (issue #255), so every
// assignment form is seen: plain and compound assignment, chained
// assignment, `++`/`--`, element writes, and `:FOR` loop variables.
func checkUnqualifiedFieldAssignment(tokens []lexer.Token, classStartLine int, classMethodRanges []parser.ProcedureInfo) []Diagnostic {
	var diagnostics []Diagnostic
	if classStartLine <= 0 || len(classMethodRanges) == 0 {
		return diagnostics
	}

	// First method's start line bounds the class-body field region.
	firstMethodStart := classMethodRanges[0].StartLine
	for _, m := range classMethodRanges {
		if m.StartLine < firstMethodStart {
			firstMethodStart = m.StartLine
		}
	}

	// Collect class fields: identifiers appearing on :DECLARE lines
	// between the :CLASS line and the first method's start.
	fields := make(map[string]bool)
	for i, tok := range tokens {
		if tok.Type != lexer.TokenKeyword {
			continue
		}
		if tok.Line <= classStartLine || tok.Line >= firstMethodStart {
			continue
		}
		if strings.ToUpper(strings.TrimPrefix(tok.Text, ":")) != "DECLARE" {
			continue
		}
		// Collect identifiers on the same logical declaration (until ';').
		for j := i + 1; j < len(tokens); j++ {
			t := tokens[j]
			if t.Type == lexer.TokenPunctuation && t.Text == ";" {
				break
			}
			if t.Type == lexer.TokenIdentifier {
				fields[strings.ToUpper(t.Text)] = true
			}
		}
	}
	if len(fields) == 0 {
		return diagnostics
	}

	builtins := buildBuiltinSet()
	stmts := parser.ExtractStatementExpressions(tokens)

	for _, proc := range classMethodRanges {
		// Method-local names (:DECLARE / :PARAMETERS / :PUBLIC inside the
		// method) shadow the field: the bare name is then evidently the
		// author's own variable.
		locals := make(map[string]bool)
		for i, tok := range tokens {
			if tok.Line < proc.StartLine || tok.Line > proc.EndLine {
				continue
			}
			if tok.Type != lexer.TokenKeyword {
				continue
			}
			norm := strings.ToUpper(strings.TrimPrefix(tok.Text, ":"))
			if norm != "DECLARE" && norm != "PARAMETERS" && norm != "PUBLIC" {
				continue
			}
			for j := i + 1; j < len(tokens); j++ {
				t := tokens[j]
				if t.Line > proc.EndLine {
					break
				}
				if t.Type == lexer.TokenPunctuation && t.Text == ";" {
					break
				}
				if t.Type == lexer.TokenIdentifier {
					locals[strings.ToUpper(t.Text)] = true
				}
			}
		}

		candidate := func(e *parser.Expr) (string, bool) {
			if e == nil || e.Kind != parser.ExprIdentifier || e.Start < 0 || e.Start >= len(tokens) {
				return "", false
			}
			upper := strings.ToUpper(e.Name)
			if !fields[upper] || locals[upper] || builtins[upper] {
				return "", false
			}
			return upper, true
		}

		// Once a bare write has happened, the name is a method-local from
		// then on, so later reads of it are the local's — not reported.
		written := make(map[string]bool)

		for si := range stmts {
			st := &stmts[si]
			if st.Start < 0 || st.Start >= len(tokens) {
				continue
			}
			line := tokens[st.Start].Line
			if line < proc.StartLine || line > proc.EndLine {
				continue
			}

			var writes, reads []*parser.Expr
			// writeKind records how each written identifier is written,
			// for the message: "" for a plain `:=` (which creates a local),
			// or "compound", "increment", "element" — forms that act on a
			// variable the bare lookup must already find.
			writeKind := make(map[*parser.Expr]string)
			kind := ""
			var visit func(e *parser.Expr, target bool)
			// target marks the expression being assigned: a bare
			// identifier there is a write; the root of an indexed target
			// (`aItems[1] := x`) is written through, and its subscripts
			// are reads.
			visit = func(e *parser.Expr, target bool) {
				if e == nil {
					return
				}
				switch e.Kind {
				case parser.ExprIdentifier:
					if target {
						writes = append(writes, e)
						writeKind[e] = kind
					} else {
						reads = append(reads, e)
					}
					return
				case parser.ExprIndex:
					if len(e.Children) > 0 {
						outer := kind
						if target {
							kind = "element"
						}
						visit(e.Children[0], target)
						kind = outer
						for _, c := range e.Children[1:] {
							visit(c, false)
						}
					}
					return
				case parser.ExprBinary:
					if len(e.Children) == 2 && isFieldAssignmentOp(e.Op) {
						outer := kind
						kind = ""
						if e.Op != ":=" {
							kind = "compound"
						}
						visit(e.Children[0], true)
						kind = outer
						visit(e.Children[1], false)
						return
					}
				case parser.ExprIncrement:
					if len(e.Children) == 1 {
						outer := kind
						kind = "increment"
						visit(e.Children[0], true)
						kind = outer
					}
					return
				case parser.ExprCall:
					// A bare callee is a function name, not a variable.
					if len(e.Children) > 0 {
						if e.Children[0].Kind != parser.ExprIdentifier {
							visit(e.Children[0], false)
						}
						for _, c := range e.Children[1:] {
							visit(c, false)
						}
					}
					return
				}
				for _, c := range e.Children {
					visit(c, false)
				}
			}

			switch st.Kind {
			case parser.StmtAssign, parser.StmtFor:
				for k, e := range st.Exprs {
					kind = ""
					if st.Kind == parser.StmtAssign && st.Assign != ":=" {
						kind = "compound"
					}
					visit(e, k == 0)
				}
				kind = ""
			case parser.StmtDefault:
				// The target is a parameter name, already a local.
				for _, e := range st.Exprs[min(1, len(st.Exprs)):] {
					visit(e, false)
				}
			default:
				for _, e := range st.Exprs {
					visit(e, false)
				}
			}

			flaggedHere := make(map[string]bool)
			for _, w := range writes {
				name, ok := candidate(w)
				if !ok || written[name] {
					continue
				}
				tok := tokens[w.Start]
				msg := fmt.Sprintf("Bare assignment to '%s' inside a class method creates a local — use 'Me:%s' to assign the class field", tok.Text, tok.Text)
				switch writeKind[w] {
				case "element":
					msg = fmt.Sprintf("Bare '%s' inside a class method does not reach the class field, so this element write misses it — use 'Me:%s'", tok.Text, tok.Text)
				case "compound":
					msg = fmt.Sprintf("Bare '%s' inside a class method does not reach the class field, so this compound assignment misses it — use 'Me:%s'", tok.Text, tok.Text)
				case "increment":
					msg = fmt.Sprintf("Bare '%s' inside a class method does not reach the class field, so this increment misses it — use 'Me:%s'", tok.Text, tok.Text)
				}
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range:    tokenToRange(tok),
					Message:  msg,
					Source:   "ssl-lsp",
					Code:     CodeUnqualifiedFieldAssignment,
				})
				flaggedHere[name] = true
			}
			for _, r := range reads {
				name, ok := candidate(r)
				if !ok || written[name] || flaggedHere[name] {
					continue
				}
				tok := tokens[r.Start]
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range:    tokenToRange(tok),
					Message:  fmt.Sprintf("Bare '%s' inside a class method does not read the class field — it resolves to a local, a caller's variable, or a public, and raises 'Variable [%s] is undefined!' when none exists. Use 'Me:%s'", tok.Text, tok.Text, tok.Text),
					Source:   "ssl-lsp",
					Code:     CodeUnqualifiedFieldRead,
				})
			}
			// After any bare write the name has been reported once; later
			// reads in the method are the same mistake (or, after a plain
			// `:=`, a genuine local) and are not reported again.
			for _, w := range writes {
				if name, ok := candidate(w); ok {
					written[name] = true
				}
			}
		}
	}

	return diagnostics
}

// isFieldAssignmentOp reports whether an operator assigns: `:=` or one of
// the compound forms.
func isFieldAssignmentOp(op string) bool {
	switch op {
	case ":=", "+=", "-=", "*=", "/=", "^=", "%=":
		return true
	}
	return false
}

// isProcedureLibraryFile reports whether the file consists solely of
// :PROCEDURE blocks (plus comments and paste-time :INCLUDE directives) with
// no top-level statements — the shape of an include library whose procedures
// are compiled into a class via :INCLUDE (issue #171).
func isProcedureLibraryFile(tokens []lexer.Token) bool {
	depth := 0
	sawProcedure := false
	startOfStatement := true
	for _, t := range tokens {
		if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment || t.Type == lexer.TokenEOF {
			continue
		}
		if t.Type == lexer.TokenPunctuation && t.Text == ";" {
			startOfStatement = true
			continue
		}
		if !startOfStatement {
			continue
		}
		startOfStatement = false
		if t.Type != lexer.TokenKeyword {
			if depth == 0 {
				return false
			}
			continue
		}
		switch strings.ToUpper(strings.TrimPrefix(t.Text, ":")) {
		case "PROCEDURE":
			sawProcedure = true
			depth++
		case "ENDPROC":
			if depth > 0 {
				depth--
			}
		case "INCLUDE":
			// Paste-time directive, fine at top level of a library.
		default:
			if depth == 0 {
				return false
			}
		}
	}
	return sawProcedure
}

// checkClassReferenceForms validates source-of-truth rules for Me and Base.
func checkClassReferenceForms(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	var classToken *lexer.Token
	hasInherit := false
	for i := range tokens {
		token := &tokens[i]
		if token.Type != lexer.TokenKeyword {
			continue
		}

		switch strings.ToUpper(strings.TrimPrefix(token.Text, ":")) {
		case "CLASS":
			if classToken == nil {
				classToken = token
			}
		case "INHERIT":
			if classToken != nil {
				hasInherit = true
			}
		}
	}

	isProcedureLibrary := classToken == nil && isProcedureLibraryFile(tokens)
	// Declared names anywhere in a :DECLARE / :PARAMETERS / :PUBLIC list,
	// not only the first one after the keyword (issue #255).
	roles := parser.IdentifierRoles(tokens)

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenIdentifier || isDeclarationIdentifier(tokens, i) {
			continue
		}

		switch {
		case strings.EqualFold(token.Text, "Me"):
			if tokenInClassRange(token, classToken) {
				continue
			}
			// Skip qualified access: oObj:Me is a member named Me, not the
			// self-reference (same guard as checkUnqualifiedFieldAssignment).
			if prevIdx := previousSignificantTokenIndex(tokens, i-1); prevIdx >= 0 {
				p := tokens[prevIdx]
				if p.Type == lexer.TokenPunctuation && p.Text == ":" {
					continue
				}
			}
			// A file of nothing but :PROCEDURE blocks may be an include
			// library compiled into a class via :INCLUDE, where Me is valid
			// at runtime — single-file analysis cannot tell, so warn instead
			// of error (issue #171).
			severity := SeverityError
			message := "'Me' can only be used inside a ':CLASS' definition"
			if classToken == nil && isProcedureLibrary {
				severity = SeverityWarning
				message = "'Me' outside a ':CLASS' definition — valid only if this file is an include library compiled into a class via ':INCLUDE'"
			}
			diagnostics = append(diagnostics, Diagnostic{
				Severity: severity,
				Range:    tokenToRange(token),
				Message:  message,
				Source:   "ssl-lsp",
				Code:     CodeMeOutsideClass,
			})

		case strings.EqualFold(token.Text, "Base"):
			if i < len(roles) && roles[i] == parser.RoleDeclaredName {
				continue
			}
			// oUri:Base is a member named Base on some receiver, not the
			// parent reference (D10: member access never warns; #258).
			if prevIdx := previousSignificantTokenIndex(tokens, i-1); prevIdx >= 0 &&
				tokens[prevIdx].Type == lexer.TokenPunctuation && tokens[prevIdx].Text == ":" {
				continue
			}
			nextIdx := nextSignificantTokenIndex(tokens, i+1)
			if nextIdx < 0 || tokens[nextIdx].Type != lexer.TokenPunctuation || tokens[nextIdx].Text != ":" {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "'Base' must be used as 'Base:MemberName' and cannot stand alone",
					Source:   "ssl-lsp",
					Code:     CodeBaseStandalone,
				})
				continue
			}

			if !tokenInClassRange(token, classToken) {
				// Same include-library tier as Me (issue #171): a file of
				// nothing but :PROCEDURE blocks may compile inside an
				// inheriting class via :INCLUDE, where Base:Member is valid
				// (issue #255).
				severity := SeverityError
				message := "'Base:MemberName' can only be used inside a ':CLASS' definition"
				if classToken == nil && isProcedureLibrary {
					severity = SeverityWarning
					message = "'Base:MemberName' outside a ':CLASS' definition — valid only if this file is an include library compiled into an inheriting class via ':INCLUDE'"
				}
				diagnostics = append(diagnostics, Diagnostic{
					Severity: severity,
					Range:    tokenToRange(token),
					Message:  message,
					Source:   "ssl-lsp",
					Code:     CodeBaseOutsideClass,
				})
				continue
			}

			if !hasInherit {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "'Base:MemberName' requires ':INHERIT' in the current ':CLASS' definition",
					Source:   "ssl-lsp",
					Code:     CodeBaseRequiresInherit,
				})
			}
		}
	}

	return diagnostics
}

func checkClassMemberOrder(tokens []lexer.Token, classToken lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	const orderMessage = "Class members must be ordered as ':DECLARE' fields, then methods"

	const (
		classOrderInherit = 1
		classOrderDeclare = 2
		classOrderMethod  = 3
	)

	seenClass := false
	startOfStatement := true
	inMethod := false
	maxOrder := 0
	// statementIndex counts class-level statements after :CLASS; the
	// :INHERIT position rule needs to know whether it is the first.
	statementIndex := 0

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]

		if token.Type == lexer.TokenWhitespace {
			continue
		}
		if token.Type == lexer.TokenComment {
			startOfStatement = true
			continue
		}

		if !seenClass {
			if token.Line == classToken.Line && token.Column == classToken.Column && token.Text == classToken.Text {
				seenClass = true
				// The class name and its `;` belong to the :CLASS
				// statement, not to the first member.
				startOfStatement = false
				continue
			}
			if token.Type == lexer.TokenPunctuation && token.Text == ";" {
				startOfStatement = true
			}
			continue
		}

		if inMethod {
			if token.Type == lexer.TokenKeyword &&
				strings.ToUpper(strings.TrimPrefix(token.Text, ":")) == "ENDPROC" {
				inMethod = false
				startOfStatement = false
			}
			if token.Type == lexer.TokenPunctuation && token.Text == ";" {
				startOfStatement = true
			}
			continue
		}

		if !startOfStatement {
			if token.Type == lexer.TokenPunctuation && token.Text == ";" {
				startOfStatement = true
			}
			continue
		}

		startOfStatement = false
		statementIndex++
		if token.Type != lexer.TokenKeyword {
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		order := 0

		switch normalized {
		case "INHERIT":
			// SSL accepts :INHERIT only directly after the :CLASS
			// statement (reference: keywords/INHERIT). Anything else in
			// front of it is a compile error, reported under its own code
			// rather than as an ordering note (issue #255).
			if statementIndex > 1 {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':INHERIT' must directly follow the ':CLASS' statement",
					Source:   "ssl-lsp",
					Code:     CodeInheritPosition,
				})
			}
			if classOrderInherit > maxOrder {
				maxOrder = classOrderInherit
			}
			continue
		case "DECLARE":
			order = classOrderDeclare
		case "PROCEDURE":
			order = classOrderMethod
			inMethod = true
		default:
			continue
		}

		if order < maxOrder {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range:    tokenToRange(token),
				Message:  orderMessage,
				Source:   "ssl-lsp",
				Code:     CodeClassMemberOrder,
			})
		}
		if order > maxOrder {
			maxOrder = order
		}
	}

	return diagnostics
}

// checkUnmatchedDelimiters checks for unmatched parentheses and brackets.
func checkUnmatchedDelimiters(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	type stackItem struct {
		char  string
		token lexer.Token
	}
	var stack []stackItem

	pairs := map[string]string{
		"(": ")",
		"[": "]",
		"{": "}",
	}

	closers := map[string]string{
		")": "(",
		"]": "[",
		"}": "{",
	}

	unmatched := func(token lexer.Token) {
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(token),
			Message:  fmt.Sprintf("Unmatched '%s'", token.Text),
			Source:   "ssl-lsp",
			Code:     CodeUnmatchedDelimiter,
		})
	}

	for i, token := range tokens {
		if token.Type != lexer.TokenPunctuation {
			continue
		}
		if _, isOpener := pairs[token.Text]; isOpener {
			stack = append(stack, stackItem{char: token.Text, token: token})
			continue
		}
		expected, isCloser := closers[token.Text]
		if !isCloser {
			continue
		}
		if len(stack) == 0 {
			unmatched(token)
			continue
		}
		top := stack[len(stack)-1]
		if top.char == expected {
			stack = stack[:len(stack)-1]
			continue
		}

		// The closer does not match the innermost opener. One mistake
		// should yield one finding (issue #255):
		//
		//   - a matching opener deeper in the stack means the openers above
		//     it were never closed (`Len(aV[1);` is missing its `]`):
		//     report those and resume at the match, as checkUnclosedBlocks
		//     does for block keywords;
		//   - with no matching opener anywhere, a closer followed by the
		//     innermost opener's own closer is a stray extra
		//     (`{a, b}});`): report it alone and leave the opener open;
		//   - otherwise it is a wrong-kind closer standing in for the
		//     innermost one (`(1 + 2];`): report the crossed pair and pop.
		match := -1
		for j := len(stack) - 2; j >= 0; j-- {
			if stack[j].char == expected {
				match = j
				break
			}
		}
		if match >= 0 {
			for j := len(stack) - 1; j > match; j-- {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(stack[j].token),
					Message:  fmt.Sprintf("Unclosed '%s'", stack[j].char),
					Source:   "ssl-lsp",
					Code:     CodeUnclosedDelimiter,
				})
			}
			stack = stack[:match]
			continue
		}
		if next := nextSignificantTokenIndex(tokens, i+1); next >= 0 &&
			tokens[next].Type == lexer.TokenPunctuation && tokens[next].Text == pairs[top.char] {
			unmatched(token)
			continue
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(token),
			Message:  fmt.Sprintf("Expected '%s' but found '%s'", pairs[top.char], token.Text),
			Source:   "ssl-lsp",
			Code:     CodeMismatchedDelimiter,
		})
		stack = stack[:len(stack)-1]
	}

	// Report unclosed delimiters
	for _, item := range stack {
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(item.token),
			Message:  fmt.Sprintf("Unclosed '%s'", item.char),
			Source:   "ssl-lsp",
			Code:     CodeUnclosedDelimiter,
		})
	}

	return diagnostics
}

// middleKeywordOwner maps each middle clause keyword to the block it
// belongs inside (issue #292). `:EXITCASE` is not here: its placement is
// exitcase_last_in_branch's.
var middleKeywordOwner = map[string]string{
	"ELSE":      "IF",
	"CATCH":     "TRY",
	"FINALLY":   "TRY",
	"CASE":      "BEGINCASE",
	"OTHERWISE": "BEGINCASE",
}

// checkUnclosedBlocks checks for unclosed block statements, end keywords
// with no block to close, and middle clause keywords with no block to sit
// in.
func checkUnclosedBlocks(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	blockPairs := map[string][]string{
		"IF":              {"ENDIF"},
		"WHILE":           {"ENDWHILE"},
		"FOR":             {"NEXT"},
		"BEGINCASE":       {"ENDCASE"},
		"BEGININLINECODE": {"ENDINLINECODE"},
		"TRY":             {"ENDTRY"},
		"PROCEDURE":       {"ENDPROC"},
		"REGION":          {"ENDREGION"},
	}

	endToStart := make(map[string][]string)
	for start, ends := range blockPairs {
		for _, end := range ends {
			endToStart[end] = append(endToStart[end], start)
		}
	}
	// `:ENDFOR` is not SSL, but it is unmistakably meant to close a `:FOR`.
	// endfor_invalid reports the token; pairing it here keeps one typo to
	// one finding instead of also reporting the `:FOR` as unclosed
	// (issue #255). It never produces a pairing diagnostic of its own.
	endToStart["ENDFOR"] = []string{"FOR"}

	type stackItem struct {
		keyword string
		token   lexer.Token
	}
	var stack []stackItem

	for _, token := range tokens {
		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))

			// `:ENDFOR` is endfor_invalid's finding. When a :FOR is open on
			// top of the stack it is the intended terminator, so it closes
			// that loop for structure purposes instead of leaving a second
			// unclosed_block error on the :FOR (issue #255).
			if normalized == "ENDFOR" {
				if len(stack) > 0 && stack[len(stack)-1].keyword == "FOR" {
					stack = stack[:len(stack)-1]
				}
				continue
			}

			if _, isStart := blockPairs[normalized]; isStart {
				stack = append(stack, stackItem{keyword: normalized, token: token})
			} else if owner, isMiddle := middleKeywordOwner[normalized]; isMiddle {
				// A middle clause keyword needs its block open somewhere
				// in the current procedure (or inline-code unit). An
				// owner deeper in the stack is a pairing problem the
				// end-keyword recovery reports; no owner at all is a
				// stray clause no other rule names (issue #292).
				inBlock := false
				for i := len(stack) - 1; i >= 0; i-- {
					k := stack[i].keyword
					if k == owner {
						inBlock = true
						break
					}
					if k == "PROCEDURE" || k == "BEGININLINECODE" {
						break
					}
				}
				if !inBlock {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("':%s' is outside any ':%s' block", normalized, owner),
						Source:   "ssl-lsp",
						Code:     CodeStrayBlockKeyword,
					})
				}
			} else if validStarts, isEnd := endToStart[normalized]; isEnd {
				silent := normalized == "ENDFOR"
				if len(stack) == 0 {
					if silent {
						continue
					}
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Unexpected ':%s' without matching block start", normalized),
						Source:   "ssl-lsp",
						Code:     CodeUnmatchedBlockEnd,
					})
				} else {
					top := stack[len(stack)-1]
					if contains(validStarts, top.keyword) {
						stack = stack[:len(stack)-1]
					} else {
						// Try to find a matching opener further down the stack
						found := false
						for i := len(stack) - 1; i >= 0; i-- {
							if contains(validStarts, stack[i].keyword) {
								// Report missing closers for items above
								for j := len(stack) - 1; j > i; j-- {
									unclosed := stack[j]
									expectedEnd := blockPairs[unclosed.keyword][0]
									diagnostics = append(diagnostics, Diagnostic{
										Severity: SeverityError,
										Range:    tokenToRange(unclosed.token),
										Message:  fmt.Sprintf("Unclosed ':%s' - expected ':%s'", unclosed.keyword, expectedEnd),
										Source:   "ssl-lsp",
										Code:     CodeUnclosedBlock,
									})
								}
								stack = stack[:i]
								found = true
								break
							}
						}

						switch {
						case found || silent:
						case top.keyword == "PROCEDURE":
							// Nothing is open inside the procedure, so the
							// end keyword is stray, exactly as at top level
							// — not a crossed pair with the procedure
							// (issue #255).
							diagnostics = append(diagnostics, Diagnostic{
								Severity: SeverityError,
								Range:    tokenToRange(token),
								Message:  fmt.Sprintf("Unexpected ':%s' without matching block start", normalized),
								Source:   "ssl-lsp",
								Code:     CodeUnmatchedBlockEnd,
							})
						default:
							diagnostics = append(diagnostics, Diagnostic{
								Severity: SeverityError,
								Range:    tokenToRange(token),
								Message:  fmt.Sprintf("':%s' does not match ':%s'", normalized, top.keyword),
								Source:   "ssl-lsp",
								Code:     CodeMismatchedBlockEnd,
							})
						}
					}
				}
			}
		}
	}

	// Report any remaining unclosed blocks
	for _, item := range stack {
		expectedEnd := blockPairs[item.keyword][0]
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(item.token),
			Message:  fmt.Sprintf("Unclosed ':%s' - expected ':%s'", item.keyword, expectedEnd),
			Source:   "ssl-lsp",
			Code:     CodeUnclosedBlock,
		})
	}

	return diagnostics
}

// checkBlockDepth checks for excessive block nesting depth.
//
// Depth counts the blocks opened by a block-start keyword (:PROCEDURE,
// :IF, :WHILE, :FOR, :BEGINCASE, :TRY, :REGION, :BEGININLINECODE). The
// parser also opens a block after each middle keyword; those bodies are
// branches of the block they continue, not new nesting, so they never add
// a level of their own and are never reported separately:
//
//   - an :ELSE / :CATCH / :FINALLY body is a sibling of its :IF / :TRY
//     body and sits at the same depth — the opening block's report
//     already covers it;
//   - a :CASE / :OTHERWISE arm is a child of its :BEGINCASE block and
//     sits at the :BEGINCASE depth, because an arm body is indented one
//     level inside :BEGINCASE exactly as an :IF body is inside :IF.
func checkBlockDepth(ast *parser.Node, maxDepth int) []Diagnostic {
	var diagnostics []Diagnostic

	var checkNode func(node *parser.Node, depth int)
	checkNode = func(node *parser.Node, depth int) {
		for i, child := range node.Children {
			if child.Type != parser.NodeBlock {
				checkNode(child, depth)
				continue
			}

			starter := ""
			if i > 0 {
				starter = blockStarterKeyword(node.Children[i-1])
			}
			childDepth := depth + 1
			report := true
			switch starter {
			case "CASE", "OTHERWISE":
				childDepth = depth
				report = false
			case "ELSE", "CATCH", "FINALLY":
				report = false
			}

			if report && childDepth > maxDepth {
				// Guard against invalid line numbers
				line := child.StartLine - 1
				if line < 0 {
					line = 0
				}
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityInfo,
					Range: Range{
						Start: Position{Line: line, Character: 0},
						End:   Position{Line: line, Character: 0},
					},
					Message: fmt.Sprintf("Block nesting depth (%d) exceeds maximum (%d)", childDepth, maxDepth),
					Source:  "ssl-lsp",
					Code:    CodeMaxBlockDepth,
				})
			}
			checkNode(child, childDepth)
		}
	}

	checkNode(ast, 0)
	return diagnostics
}

// blockStarterKeyword returns the normalized keyword (no colon, upper
// case) that leads the statement node opening a block, or "" when the
// node does not start with a keyword.
func blockStarterKeyword(stmt *parser.Node) string {
	for _, t := range stmt.Tokens {
		if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
			continue
		}
		if t.Type != lexer.TokenKeyword {
			return ""
		}
		return strings.ToUpper(strings.TrimPrefix(t.Text, ":"))
	}
	return ""
}

func checkHungarianNotation(variables []parser.VariableInfo, prefixes []string) []Diagnostic {
	var diagnostics []Diagnostic

	if len(prefixes) == 0 {
		return diagnostics
	}

	validPrefixes := strings.Join(prefixes, ", ")

	for _, variable := range variables {
		if isHungarianExemptName(variable.Name) {
			continue
		}
		if _, ok := hasHungarianPrefix(variable.Name, prefixes); ok {
			continue
		}

		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range: Range{
				Start: Position{Line: variable.Line - 1, Character: variable.Column - 1},
				End:   Position{Line: variable.Line - 1, Character: variable.Column - 1 + len(variable.Name)},
			},
			Message: fmt.Sprintf("Variable '%s' should use a Hungarian notation prefix (%s)", variable.Name, validPrefixes),
			Source:  "ssl-lsp",
			Code:    CodeHungarianNotation,
		})
	}

	return diagnostics
}

func hasHungarianPrefix(name string, prefixes []string) (string, bool) {
	trimmed := strings.TrimLeft(name, "_")
	if trimmed == "" {
		return "", false
	}

	lower := strings.ToLower(trimmed)
	for _, prefix := range prefixes {
		if !strings.HasPrefix(lower, prefix) {
			continue
		}

		remainder := trimmed[len(prefix):]
		remainder = strings.TrimLeft(remainder, "_")
		if remainder == "" {
			continue
		}
		firstRune := []rune(remainder)[0]
		if unicode.IsUpper(firstRune) {
			return prefix, true
		}
	}

	return "", false
}

func isHungarianExemptName(name string) bool {
	trimmed := strings.TrimLeft(name, "_")
	if trimmed == "" {
		return true
	}

	switch trimmed {
	case "i", "j", "k", "x", "y", "z":
		return true
	}

	if strings.ToUpper(trimmed) == trimmed && strings.Contains(trimmed, "_") {
		return true
	}

	return false
}

// isUnqualifiedStringTarget reports whether the first significant token at
// or after idx is a string literal whose content has no '.' qualifier — the
// provably class-local DoProc target form. Qualified
// "Category.Script.Procedure" literals and non-literal targets return false
// (issue #151, ssl-style-guide#49).
func isUnqualifiedStringTarget(tokens []lexer.Token, idx int) bool {
	arg := nextSignificantTokenIndex(tokens, idx)
	if arg < 0 || tokens[arg].Type != lexer.TokenString {
		return false
	}
	return !strings.Contains(tokens[arg].Text, ".")
}

func tokenInProcedureRange(token lexer.Token, procedures []parser.ProcedureInfo) bool {
	for _, proc := range procedures {
		if token.Line >= proc.StartLine && token.Line <= proc.EndLine {
			return true
		}
	}
	return false
}

func tokenInClassRange(token lexer.Token, classToken *lexer.Token) bool {
	if classToken == nil {
		return false
	}
	return token.Line >= classToken.Line
}

func isDeclarationIdentifier(tokens []lexer.Token, idx int) bool {
	prevIdx := previousSignificantTokenIndex(tokens, idx-1)
	if prevIdx < 0 {
		return false
	}
	prev := tokens[prevIdx]
	if prev.Type != lexer.TokenKeyword {
		return false
	}

	switch strings.ToUpper(strings.TrimPrefix(prev.Text, ":")) {
	case "DECLARE", "PARAMETERS", "DEFAULT", "PUBLIC", "PROCEDURE", "CLASS", "INHERIT":
		return true
	default:
		return false
	}
}

func isFirstSignificantStatementKeyword(tokens []lexer.Token, keyword string) bool {
	startOfStatement := true

	for _, token := range tokens {
		if token.Type == lexer.TokenWhitespace {
			continue
		}

		if token.Type == lexer.TokenComment {
			if startOfStatement {
				continue
			}
			startOfStatement = true
			continue
		}

		if startOfStatement {
			if token.Type == lexer.TokenKeyword &&
				strings.ToUpper(strings.TrimPrefix(token.Text, ":")) == keyword {
				return true
			}
			return false
		}

		if token.Type == lexer.TokenPunctuation && token.Text == ";" {
			startOfStatement = true
		}
	}

	return false
}

// tokenToRange converts a token to a range, its end in runes like its
// start (issue #307). A token spanning lines (a multi-line comment or
// string) ends on its last line, after its last character there; it used
// to end on its first line at the start column plus the whole token's
// length, past that line's end (issue #328).
func tokenToRange(token lexer.Token) Range {
	// A token that runs to end of input carries the final line break; the
	// range ends at its last character, not on the empty line after it.
	text := strings.TrimRight(token.Text, "\r\n")
	if text == "" {
		text = token.Text
	}
	endLine := token.Line - 1
	endChar := token.Column - 1 + utf8.RuneCountInString(text)
	if nl := strings.LastIndex(text, "\n"); nl >= 0 {
		endLine += strings.Count(text, "\n")
		endChar = utf8.RuneCountInString(text[nl+1:])
	}
	return Range{
		Start: Position{Line: token.Line - 1, Character: token.Column - 1},
		End:   Position{Line: endLine, Character: endChar},
	}
}

// contains checks if a string slice contains a value.
func contains(slice []string, val string) bool {
	for _, s := range slice {
		if s == val {
			return true
		}
	}
	return false
}

// checkMissingExitCase checks that every :CASE and :OTHERWISE block ends with :EXITCASE.
// The source guide recommends this unless multi-match CASE behavior is intentional.
func checkMissingExitCase(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Use a stack to handle nested BEGINCASE blocks correctly
	type caseState struct {
		currentCaseToken *lexer.Token
		hasExitCase      bool
		lastStmtReturn   bool // clause's most recent statement starts with :RETURN
	}
	var stack []caseState

	reportMissing := func(caseToken *lexer.Token) {
		if caseToken != nil {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(*caseToken),
				Message:  fmt.Sprintf("':%s' block should end with ':EXITCASE;'", strings.ToUpper(strings.TrimPrefix(caseToken.Text, ":"))),
				Source:   "ssl-lsp",
				Code:     CodePreferExitCase,
			})
		}
	}

	statementStart := true
	for i := range tokens {
		token := &tokens[i]
		switch token.Type {
		case lexer.TokenWhitespace, lexer.TokenComment:
			continue
		}
		if token.Type == lexer.TokenPunctuation && token.Text == ";" {
			statementStart = true
			continue
		}
		isStart := statementStart
		statementStart = false

		normalized := ""
		if token.Type == lexer.TokenKeyword {
			normalized = strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		}

		// A :RETURN as the clause's FINAL statement satisfies the rule — the
		// :EXITCASE after it would be unreachable (issue #139). Track whether
		// the most recent statement in the open clause starts with :RETURN;
		// the case-structure keywords below are boundaries, not statements
		// (except BEGINCASE, which does start a statement in the enclosing
		// clause and must clear the flag before pushing).
		switch normalized {
		case "CASE", "OTHERWISE", "EXITCASE", "ENDCASE":
			// boundary handling below reads lastStmtReturn — don't touch it
		default:
			if isStart && len(stack) > 0 && stack[len(stack)-1].currentCaseToken != nil {
				stack[len(stack)-1].lastStmtReturn = normalized == "RETURN"
			}
		}

		switch normalized {
		case "BEGINCASE":
			stack = append(stack, caseState{})

		case "CASE", "OTHERWISE":
			if len(stack) > 0 {
				top := &stack[len(stack)-1]
				// If we had a previous CASE/OTHERWISE without EXITCASE, report it
				if !top.hasExitCase && !top.lastStmtReturn {
					reportMissing(top.currentCaseToken)
				}
				top.currentCaseToken = token
				top.hasExitCase = false
				top.lastStmtReturn = false
			}

		case "EXITCASE":
			if len(stack) > 0 {
				stack[len(stack)-1].hasExitCase = true
			}

		case "ENDCASE":
			if len(stack) > 0 {
				top := &stack[len(stack)-1]
				// Check the last CASE/OTHERWISE block
				if !top.hasExitCase && !top.lastStmtReturn {
					reportMissing(top.currentCaseToken)
				}
				stack = stack[:len(stack)-1]
			}
		}
	}

	return diagnostics
}

// checkExitCaseLastInBranch reports an :EXITCASE that is not the last
// statement directly in its :CASE/:OTHERWISE branch body (issue #265). The
// compiler accepts :EXITCASE only there: a statement after it in the same
// branch, an :EXITCASE nested inside a block (:IF, :WHILE, :FOR, :TRY)
// within the branch, and an :EXITCASE outside any :BEGINCASE all fail to
// compile with "unexpected token". Comments are transparent.
func checkExitCaseLastInBranch(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// One frame per open block. A case frame is a :BEGINCASE; any other
	// frame is a nested statement block (:IF, :WHILE, :FOR, :TRY).
	type frame struct {
		isCase       bool
		inBranch     bool // a :CASE/:OTHERWISE has opened a branch
		exited       bool // a direct :EXITCASE ended the branch
		reportedTail bool // the statement after it is already reported
	}
	var stack []frame
	insideCase := func(stack []frame) bool {
		for _, f := range stack {
			if f.isCase {
				return true
			}
		}
		return false
	}

	report := func(token lexer.Token, message string) {
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(token),
			Message:  message,
			Source:   "ssl-lsp",
			Code:     CodeExitCaseLastInBranch,
		})
	}

	statementStart := true
	for i := range tokens {
		token := tokens[i]
		switch token.Type {
		case lexer.TokenWhitespace, lexer.TokenComment:
			continue
		}
		if token.Type == lexer.TokenPunctuation && token.Text == ";" {
			statementStart = true
			continue
		}
		isStart := statementStart
		statementStart = false
		if !isStart {
			continue
		}

		normalized := ""
		if token.Type == lexer.TokenKeyword {
			normalized = strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		}

		// A statement after a direct :EXITCASE, before the branch ends.
		if n := len(stack); n > 0 && stack[n-1].isCase && stack[n-1].exited {
			switch normalized {
			case "CASE", "OTHERWISE", "ENDCASE":
			default:
				if !stack[n-1].reportedTail {
					stack[n-1].reportedTail = true
					report(token, "Statement after ':EXITCASE' does not compile: ':EXITCASE' must be the last statement of its ':CASE' or ':OTHERWISE' branch")
				}
			}
		}

		switch normalized {
		case "BEGINCASE":
			stack = append(stack, frame{isCase: true})
		case "IF", "WHILE", "FOR", "TRY":
			stack = append(stack, frame{})
		case "ENDIF", "ENDWHILE", "NEXT", "ENDTRY", "ENDCASE":
			if len(stack) > 0 {
				stack = stack[:len(stack)-1]
			}
		case "CASE", "OTHERWISE":
			if n := len(stack); n > 0 && stack[n-1].isCase {
				stack[n-1] = frame{isCase: true, inBranch: true}
			}
		case "EXITCASE":
			n := len(stack)
			switch {
			case n > 0 && stack[n-1].isCase && stack[n-1].inBranch:
				stack[n-1].exited = true
			case insideCase(stack):
				report(token, "':EXITCASE' inside a nested block does not compile: it must be the last statement directly in its ':CASE' or ':OTHERWISE' branch")
			default:
				report(token, "':EXITCASE' outside a ':BEGINCASE' branch does not compile")
			}
		}
	}

	return diagnostics
}

// checkMissingOtherwise warns when a :BEGINCASE block has no :OTHERWISE clause.
// Style guide recommends including :OTHERWISE for default handling (advisory).
func checkMissingOtherwise(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	var beginCaseToken *lexer.Token
	hasOtherwise := false
	depth := 0

	for i := range tokens {
		token := &tokens[i]
		if token.Type != lexer.TokenKeyword {
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))

		switch normalized {
		case "BEGINCASE":
			if depth == 0 {
				beginCaseToken = token
				hasOtherwise = false
			}
			depth++
		case "OTHERWISE":
			if depth == 1 {
				hasOtherwise = true
			}
		case "ENDCASE":
			depth--
			if depth == 0 && beginCaseToken != nil && !hasOtherwise {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityHint,
					Range:    tokenToRange(*beginCaseToken),
					Message:  "':BEGINCASE' has no ':OTHERWISE' clause; consider adding one for default handling",
					Source:   "ssl-lsp",
					Code:     CodeMissingOtherwise,
				})
			}
			if depth <= 0 {
				beginCaseToken = nil
				hasOtherwise = false
				depth = 0
			}
		}
	}

	return diagnostics
}

// checkBareLogicalOperators checks for AND, OR, NOT without enclosing periods.
// SSL requires .AND., .OR., .NOT. - bare operators are an error.
// operandEnd reports whether a token can end an operand expression — the
// left-hand side a binary operator would attach to.
func operandEnd(t lexer.Token) bool {
	switch t.Type {
	case lexer.TokenIdentifier, lexer.TokenNumber, lexer.TokenString, lexer.TokenCodeBlock:
		return true
	case lexer.TokenKeyword:
		// Dot-wrapped literals (.T., .F.) and NIL end an operand; control
		// keywords (:IF, :DECLARE, ...) do not.
		return strings.HasPrefix(t.Text, ".") || strings.EqualFold(t.Text, "NIL")
	case lexer.TokenPunctuation:
		return t.Text == ")" || t.Text == "]" || t.Text == "}"
	}
	return false
}

// operandStart reports whether a token can begin an operand expression —
// the right-hand side a logical operator would attach to.
func operandStart(t lexer.Token) bool {
	switch t.Type {
	case lexer.TokenIdentifier, lexer.TokenNumber, lexer.TokenString, lexer.TokenCodeBlock:
		return true
	case lexer.TokenKeyword:
		return strings.HasPrefix(t.Text, ".") || strings.EqualFold(t.Text, "NIL")
	case lexer.TokenPunctuation:
		return t.Text == "(" || t.Text == "{"
	case lexer.TokenOperator:
		// A prefix operator opens an operand: !x, .NOT. x.
		return t.Text == "!" || strings.EqualFold(t.Text, ".NOT.")
	}
	return false
}

// checkBareLogicalOperators flags bare And/Or/Not used as logical operators —
// SSL's logical operators exist only in dotted form (.AND., .OR., .NOT.).
// A bare And/Or/Not in an identifier slot is a legal identifier (issue #165:
// WSDL-generated proxy classes declare members named And/Or), so the check
// fires only in expression-operator positions: And/Or between two operands,
// Not as a prefix before an operand. Declaration lists, member access, and
// assignment targets never flag.
func checkBareLogicalOperators(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Bare logical operators that should be .AND., .OR., .NOT.
	bareOperators := map[string]string{
		"AND": ".AND.",
		"OR":  ".OR.",
		"NOT": ".NOT.",
	}

	for i, token := range tokens {
		// Only check identifiers - the lexer tokenizes bare AND/OR/NOT as identifiers
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		upper := strings.ToUpper(token.Text)
		correct, isBare := bareOperators[upper]
		if !isBare {
			continue
		}

		prevIdx := previousSignificantTokenIndex(tokens, i-1)
		nextIdx := nextSignificantTokenIndex(tokens, i+1)

		var prev, next lexer.Token
		if prevIdx >= 0 {
			prev = tokens[prevIdx]
		}
		if nextIdx >= 0 {
			next = tokens[nextIdx]
		}

		// Member access (obj:And) is an identifier slot, never an operator.
		if prev.Type == lexer.TokenPunctuation && prev.Text == ":" {
			continue
		}

		operatorPosition := false
		if upper == "NOT" {
			// Prefix position: an operand follows and no operand precedes
			// (`x Not y` is not a NOT expression; `Not := 1` has no operand).
			operatorPosition = nextIdx >= 0 && operandStart(next) && !(prevIdx >= 0 && operandEnd(prev))
		} else {
			// Infix position: operands on both sides.
			operatorPosition = prevIdx >= 0 && operandEnd(prev) &&
				nextIdx >= 0 && operandStart(next)
		}
		if !operatorPosition {
			continue
		}

		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(token),
			Message:  fmt.Sprintf("Use '%s' instead of '%s' for logical operations in SSL", correct, token.Text),
			Source:   "ssl-lsp",
			Code:     CodeBareLogicalOperator,
		})
	}

	return diagnostics
}

// checkInvalidOperatorSequences detects adjacent operator tokens that form
// invalid compound operators common in other languages (e.g. !==, ===, &&, ||).
func checkInvalidOperatorSequences(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Map of (op1 + op2) -> suggestion
	type opFix struct {
		combined    string
		suggestion  string
		description string
	}
	invalidPairs := []opFix{
		{"!==", "!=", "SSL uses '!=' or '<>' for inequality, not '!=='"},
		{"===", "==", "SSL uses '==' for exact equality, not '==='"},
	}

	// C-style logical operators with SSL equivalents
	invalidLogicalOperators := map[string]string{
		"&&": ".AND.",
		"||": ".OR.",
	}
	// Operators that are simply invalid in SSL (no direct equivalent)
	invalidOperatorSet := map[string]bool{
		"&": true,
		"|": true,
	}

	for i, token := range tokens {
		if token.Type == lexer.TokenOperator {
			if suggestion, ok := invalidLogicalOperators[token.Text]; ok {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  fmt.Sprintf("SSL uses '%s' instead of '%s'", suggestion, token.Text),
					Source:   "ssl-lsp",
					Code:     CodeInvalidOperatorSequence,
				})
				continue
			}
			if invalidOperatorSet[token.Text] {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  fmt.Sprintf("'%s' is not a valid SSL operator", token.Text),
					Source:   "ssl-lsp",
					Code:     CodeInvalidOperatorSequence,
				})
				continue
			}
		}

		if token.Type != lexer.TokenOperator {
			continue
		}

		// Check adjacent operator pairs for !== and ===
		if i+1 < len(tokens) && tokens[i+1].Type == lexer.TokenOperator {
			next := tokens[i+1]
			if token.Offset+len(token.Text) == next.Offset {
				combined := token.Text + next.Text
				for _, fix := range invalidPairs {
					if combined == fix.combined {
						diagnostics = append(diagnostics, Diagnostic{
							Severity: SeverityError,
							Range: Range{
								Start: Position{Line: token.Line - 1, Character: token.Column - 1},
								End:   Position{Line: next.Line - 1, Character: next.Column - 1 + len(next.Text)},
							},
							Message: fmt.Sprintf("%s. Use '%s' instead", fix.description, fix.suggestion),
							Source:  "ssl-lsp",
							Code:    CodeInvalidOperatorSequence,
						})
						break
					}
				}
			}
		}
	}

	return diagnostics
}

// checkIncludeQuotedTarget reports an :INCLUDE whose target is a string
// literal (issue #266). The target is an identifier or a qualified
// identifier (`:INCLUDE Common.Helpers;`); a quoted target does not compile.
// The range covers the string token, so a client quick fix can replace it
// with its unquoted text.
func checkIncludeQuotedTarget(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic
	for i, token := range tokens {
		if token.Type != lexer.TokenKeyword ||
			!strings.EqualFold(strings.TrimPrefix(token.Text, ":"), "INCLUDE") {
			continue
		}
		j := i + 1
		for j < len(tokens) && (tokens[j].Type == lexer.TokenWhitespace || tokens[j].Type == lexer.TokenComment) {
			j++
		}
		if j >= len(tokens) || tokens[j].Type != lexer.TokenString {
			continue
		}
		target := tokens[j].Text
		if len(target) >= 2 {
			target = target[1 : len(target)-1]
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(tokens[j]),
			Message:  fmt.Sprintf("':INCLUDE' target must be an identifier, not a string: write ':INCLUDE %s;'", target),
			Source:   "ssl-lsp",
			Code:     CodeIncludeQuotedTarget,
		})
	}
	return diagnostics
}

// checkIncludePlacement reports :INCLUDE directives that appear after other
// significant statements or inside procedure bodies.
// Recommended conventional order: :PARAMETERS, :DEFAULT, :INCLUDE, :PUBLIC, :DECLARE.
// :PARAMETERS and :DEFAULT are required to precede :INCLUDE, so they don't
// count as "non-include statements" for the late-placement warning.
func checkIncludePlacement(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	startOfStatement := true
	seenNonPreambleStatement := false
	procedureDepth := 0

	for _, token := range tokens {
		if token.Type == lexer.TokenWhitespace {
			continue
		}

		if token.Type == lexer.TokenComment {
			startOfStatement = true
			continue
		}

		// Track procedure nesting
		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
			if normalized == "PROCEDURE" {
				procedureDepth++
			} else if normalized == "ENDPROC" && procedureDepth > 0 {
				procedureDepth--
			}
		}

		if !startOfStatement {
			if token.Type == lexer.TokenPunctuation && token.Text == ";" {
				startOfStatement = true
			}
			continue
		}

		startOfStatement = false

		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))

			if normalized == "INCLUDE" {
				if procedureDepth > 0 {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityWarning,
						Range:    tokenToRange(token),
						Message:  "':INCLUDE' inside a procedure body is not supported",
						Source:   "ssl-lsp",
						Code:     CodeIncludeInProcedure,
					})
				} else if seenNonPreambleStatement {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityInfo,
						Range:    tokenToRange(token),
						Message:  "':INCLUDE' should appear early in the file. Recommended order: :PARAMETERS, :DEFAULT, :INCLUDE, :PUBLIC, :DECLARE",
						Source:   "ssl-lsp",
						Code:     CodeIncludeEarly,
					})
				}
				continue
			}

			// :PARAMETERS and :DEFAULT are required to precede :INCLUDE,
			// so they don't trigger the late-placement warning.
			if normalized == "PARAMETERS" || normalized == "DEFAULT" {
				continue
			}
		}

		seenNonPreambleStatement = true
	}

	return diagnostics
}

// checkDefaultOnDeclareLine flags a :DEFAULT whose target is not a
// :PARAMETERS name of the enclosing procedure — or, outside procedures, of
// the script (diag.default_on_declare_line). :DEFAULT only supplies a
// fallback for a parameter; the classic shape is defaulting a :DECLARE'd
// local. The slug predates the re-scope (issue #255), which replaced a
// same-line heuristic that co-fired with default_after_parameters and
// missed the defect on any other line.
func checkDefaultOnDeclareLine(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// scopeOf maps each token to its scope: the index of the enclosing
	// :PROCEDURE keyword, or -1 at script level.
	scopeOf := make([]int, len(tokens))
	scope := -1
	for i, t := range tokens {
		if t.Type == lexer.TokenKeyword {
			switch strings.ToUpper(strings.TrimPrefix(t.Text, ":")) {
			case "PROCEDURE":
				scope = i
			case "ENDPROC":
				scopeOf[i] = scope
				scope = -1
				continue
			}
		}
		scopeOf[i] = scope
	}

	// First pass: every :PARAMETERS name, per scope.
	params := map[int]map[string]bool{}
	for i, t := range tokens {
		if t.Type != lexer.TokenKeyword || !strings.EqualFold(strings.TrimPrefix(t.Text, ":"), "PARAMETERS") {
			continue
		}
		names := params[scopeOf[i]]
		if names == nil {
			names = map[string]bool{}
			params[scopeOf[i]] = names
		}
		expectName := true
		depth := 0
		for j := i + 1; j < len(tokens); j++ {
			tok := tokens[j]
			if tok.Type == lexer.TokenWhitespace || tok.Type == lexer.TokenComment {
				continue
			}
			if tok.Type == lexer.TokenPunctuation {
				switch tok.Text {
				case "(", "[", "{":
					depth++
				case ")", "]", "}":
					depth--
				}
				if depth == 0 && tok.Text == ";" {
					break
				}
				if depth == 0 && tok.Text == "," {
					expectName = true
					continue
				}
			}
			if tok.Type == lexer.TokenKeyword {
				break
			}
			if expectName && tok.Type == lexer.TokenIdentifier {
				names[strings.ToUpper(tok.Text)] = true
			}
			expectName = false
		}
	}

	// Second pass: judge each :DEFAULT target.
	for i, t := range tokens {
		if t.Type != lexer.TokenKeyword || !strings.EqualFold(strings.TrimPrefix(t.Text, ":"), "DEFAULT") {
			continue
		}
		targetIdx := nextSignificantTokenIndex(tokens, i+1)
		if targetIdx < 0 || tokens[targetIdx].Type != lexer.TokenIdentifier {
			continue
		}
		if params[scopeOf[i]][strings.ToUpper(tokens[targetIdx].Text)] {
			continue
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(tokens[targetIdx]),
			Message:  fmt.Sprintf("':DEFAULT' target '%s' is not a ':PARAMETERS' name here - ':DEFAULT' only supplies a fallback for a parameter; give a ':DECLARE'd variable its value with an assignment", tokens[targetIdx].Text),
			Source:   "ssl-lsp",
			Code:     CodeDefaultOnDeclareLine,
		})
	}

	return diagnostics
}

// checkDeclareInitializer flags inline initializers in :DECLARE statements
// (diag.declare_initializer, issue #138). Authoritative SSL accepts only
// :DECLARE ident(, ident)*; in every context — procedure locals, script
// level, class fields, and data-source files alike — so each := between the
// :DECLARE keyword and its terminating ; is a syntax error. Initialization
// belongs in a separate assignment statement (for class fields, in the
// Constructor).
func checkDeclareInitializer(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	inDeclare := false
	for _, token := range tokens {
		switch {
		case token.Type == lexer.TokenKeyword:
			inDeclare = strings.EqualFold(strings.TrimPrefix(token.Text, ":"), "DECLARE")
		case token.Type == lexer.TokenPunctuation && token.Text == ";":
			inDeclare = false
		case inDeclare && token.Type == lexer.TokenOperator && token.Text == ":=":
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "':DECLARE' accepts only a comma-separated list of variable names - assign the value in a separate statement",
				Source:   "ssl-lsp",
				Code:     CodeDeclareInitializer,
			})
		}
	}

	return diagnostics
}

// checkRaiseErrorInCatch flags RaiseError( calls whose nearest enclosing
// :TRY section is a :CATCH block (diag.raiseerror_in_catch, issue #142) —
// the RaiseError placement doctrine (ssl-style-guide schema
// error_handling.raise_error_doctrine): the error handler must not become
// the thing that crashes. A RaiseError inside a deeper :TRY body nested
// within the handler is fine; its own :CATCH contains it. Call sites come
// from the expression AST; a qualified `recv:RaiseError(...)` is a member
// call, never the built-in, and is not reported (D10).
func checkRaiseErrorInCatch(tokens []lexer.Token, calls []parser.CallSite) []Diagnostic {
	var diagnostics []Diagnostic

	// Section of the innermost open :TRY structure at each token index.
	const (
		sectionNone = iota
		sectionTry
		sectionCatch
		sectionFinally
	)
	sectionAt := make([]int, len(tokens))
	var stack []int
	for i := range tokens {
		token := &tokens[i]
		if token.Type == lexer.TokenKeyword {
			switch strings.ToUpper(strings.TrimPrefix(token.Text, ":")) {
			case "TRY":
				stack = append(stack, sectionTry)
			case "CATCH":
				if len(stack) > 0 {
					stack[len(stack)-1] = sectionCatch
				}
			case "FINALLY":
				if len(stack) > 0 {
					stack[len(stack)-1] = sectionFinally
				}
			case "ENDTRY":
				if len(stack) > 0 {
					stack = stack[:len(stack)-1]
				}
			}
		}
		if len(stack) > 0 {
			sectionAt[i] = stack[len(stack)-1]
		}
	}

	for _, site := range calls {
		if site.Qualified() || !strings.EqualFold(site.Name, "RaiseError") {
			continue
		}
		idx := site.Call.Children[0].Start
		if idx < 0 || idx >= len(tokens) || sectionAt[idx] != sectionCatch {
			continue
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range:    tokenToRange(tokens[idx]),
			Message:  "'RaiseError' inside ':CATCH' - the error handler should not raise; move the raise into the ':TRY' block it belongs to",
			Source:   "ssl-lsp",
			Code:     CodeRaiseErrorInCatch,
		})
	}

	return diagnostics
}

// checkExecFunctionClassTargets flags ExecFunction dispatch strings whose
// target the server resolved to class files only
// (diag.execfunction_class_target, issue #143): a class file has no script
// entry point, so the two-segment call fails at runtime and the
// three-segment form does not invoke the method. classTargets carries the
// pre-resolved verdicts; the check itself stays workspace-free.
func checkExecFunctionClassTargets(tokens []lexer.Token, classTargets []string) []Diagnostic {
	if len(classTargets) == 0 {
		return nil
	}
	targets := make(map[string]bool, len(classTargets))
	for _, t := range classTargets {
		targets[strings.ToLower(t)] = true
	}

	var diagnostics []Diagnostic
	for _, site := range ExtractCallSites(tokens) {
		if site.Kind != CallDispatch || site.IsDoProc {
			continue
		}
		if !targets[strings.ToLower(site.Raw)] {
			continue
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    site.Range,
			Message: fmt.Sprintf("'ExecFunction' cannot run '%s' - the target is a class file, which has no script entry point (and class methods are not invokable this way). Instantiate with CreateUdObject and call the method on the instance.",
				site.Raw),
			Source: "ssl-lsp",
			Code:   CodeExecFunctionClassTarget,
		})
	}
	return diagnostics
}

// paramScope tracks one open :PROCEDURE or :BEGININLINECODE block for
// checkParameterPlacement: whether the scope is still eligible for a
// leading :PARAMETERS statement.
type paramScope struct {
	kind    string // "PROCEDURE" or "BEGININLINECODE"
	waiting bool   // no statement seen yet — :PARAMETERS still allowed
}

// checkParameterPlacement enforces that procedure-level :PARAMETERS statements
// appear immediately after :PROCEDURE (likewise :BEGININLINECODE, whose named
// blocks take their own :PARAMETERS list — issue #168) and that script-level
// :PARAMETERS appears before top-level executable statements (leading
// procedures are allowed). :INCLUDE never counts as a statement at any level:
// it is resolved as a textual paste before the file runs, and the style
// guide's include_early rule wants it before :PARAMETERS (issue #168).
func checkParameterPlacement(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	startOfStatement := true
	var scopes []paramScope
	seenTopLevelStatement := false

	markStatement := func() {
		if len(scopes) > 0 {
			scopes[len(scopes)-1].waiting = false
		} else {
			seenTopLevelStatement = true
		}
	}

	for _, token := range tokens {
		if token.Type == lexer.TokenWhitespace {
			continue
		}

		if token.Type == lexer.TokenComment {
			// Comments are structurally transparent — they neither prevent
			// :PARAMETERS from being accepted after :PROCEDURE nor end the
			// enclosing statement: an inline comment mid-way through a
			// multi-line :PARAMETERS list must not make the next parameter
			// register as a body/top-level statement (issue #170). Only `;`
			// ends a statement.
			continue
		}

		if !startOfStatement {
			if token.Type == lexer.TokenPunctuation && token.Text == ";" {
				startOfStatement = true
			}
			continue
		}

		startOfStatement = false

		if token.Type != lexer.TokenKeyword {
			markStatement()
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))

		switch normalized {
		case "PROCEDURE":
			scopes = append(scopes, paramScope{kind: "PROCEDURE", waiting: true})
		case "BEGININLINECODE":
			scopes = append(scopes, paramScope{kind: "BEGININLINECODE", waiting: true})
		case "ENDPROC", "ENDINLINECODE":
			if len(scopes) > 0 {
				scopes = scopes[:len(scopes)-1]
			}
		case "INCLUDE":
			// Structurally transparent for placement purposes: a paste-time
			// directive, not an executable statement (issue #168).
		case "PARAMETERS":
			if len(scopes) > 0 {
				top := &scopes[len(scopes)-1]
				if !top.waiting {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("':PARAMETERS' must appear immediately after ':%s'", top.kind),
						Source:   "ssl-lsp",
						Code:     CodeParametersFirst,
					})
				}
				top.waiting = false
			} else if seenTopLevelStatement {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "Script-level ':PARAMETERS' must appear before top-level statements (leading ':PROCEDURE' blocks are allowed)",
					Source:   "ssl-lsp",
					Code:     CodeParametersFirst,
				})
			} else {
				// A script-level :PARAMETERS is itself a statement: a
				// second one is judged against it, as at block level.
				seenTopLevelStatement = true
			}
		default:
			markStatement()
		}

		if token.Type == lexer.TokenPunctuation && token.Text == ";" {
			startOfStatement = true
		}
	}

	return diagnostics
}

// checkDefaultPlacement enforces that :DEFAULT statements immediately follow
// their corresponding :PARAMETERS statement with no intervening statements.
func checkDefaultPlacement(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	startOfStatement := true
	defaultsAllowed := false

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]

		if token.Type == lexer.TokenWhitespace {
			continue
		}

		if token.Type == lexer.TokenComment {
			// Comments are structurally transparent — they neither break the
			// :PARAMETERS -> :DEFAULT sequence nor end the enclosing
			// statement: an inline comment mid-way through a multi-line
			// :PARAMETERS list must not make the next parameter look like a
			// new statement (issue #170). Only `;` ends a statement.
			continue
		}

		if startOfStatement {
			startOfStatement = false

			if token.Type == lexer.TokenKeyword {
				normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
				switch normalized {
				case "PARAMETERS":
					defaultsAllowed = true
				case "DEFAULT":
					if !defaultsAllowed {
						diagnostics = append(diagnostics, Diagnostic{
							Severity: SeverityError,
							Range:    tokenToRange(token),
							Message:  "':DEFAULT' must appear immediately after ':PARAMETERS'",
							Source:   "ssl-lsp",
							Code:     CodeDefaultAfterParameters,
						})
					}
				default:
					defaultsAllowed = false
				}
			} else {
				defaultsAllowed = false
			}
		}

		if token.Type == lexer.TokenPunctuation && token.Text == ";" {
			startOfStatement = true
		}
	}

	return diagnostics
}

// checkInheritQualifiedName reports an :INHERIT whose parent is a plain
// class name (issue #269). The parent must be written as the full
// Category.ClassName; a plain name fails when the class compiles with
// "Specify the full name of the class in :INHERIT". A quoted parent is not
// checked: its compile behavior is not documented (DECISIONS.md D10).
func checkInheritQualifiedName(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic
	for i, token := range tokens {
		if token.Type != lexer.TokenKeyword ||
			!strings.EqualFold(strings.TrimPrefix(token.Text, ":"), "INHERIT") {
			continue
		}
		var first, last *lexer.Token
		qualified, colonForm := false, false
		var written strings.Builder
		for j := i + 1; j < len(tokens); j++ {
			t := &tokens[j]
			if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
				continue
			}
			if t.Type == lexer.TokenPunctuation && t.Text == ";" {
				break
			}
			if first == nil && t.Type != lexer.TokenIdentifier {
				break
			}
			// The lexer reads `.ClassName` after the category as one token,
			// so look for the dot in the text, not as punctuation.
			if strings.Contains(t.Text, ".") {
				qualified = true
			}
			// `Category:ClassName` lexes the colon as member access or as a
			// fused `:ClassName` keyword token.
			if strings.Contains(t.Text, ":") {
				colonForm = true
			}
			written.WriteString(t.Text)
			if first == nil {
				first = t
			}
			last = t
		}
		if first == nil || qualified {
			continue
		}
		r := tokenToRange(*first)
		r.End = tokenToRange(*last).End
		message := fmt.Sprintf("':INHERIT' needs the parent's full 'Category.ClassName'; the plain name '%s' does not compile (Specify the full name of the class in :INHERIT)", first.Text)
		if colonForm {
			message = fmt.Sprintf("':INHERIT' separates category and class with '.', not ':'; write ':INHERIT %s;'", strings.ReplaceAll(written.String(), ":", "."))
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    r,
			Message:  message,
			Source:   "ssl-lsp",
			Code:     CodeInheritQualifiedName,
		})
	}
	return diagnostics
}

// checkInlineCodeNaming enforces the style-guide requirement that BEGININLINECODE
// blocks be named with either an identifier or a quoted string.
func checkInlineCodeNaming(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenKeyword {
			continue
		}
		if strings.ToUpper(strings.TrimPrefix(token.Text, ":")) != "BEGININLINECODE" {
			continue
		}

		nextIdx := nextSignificantTokenIndex(tokens, i+1)
		if nextIdx < 0 {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "':BEGININLINECODE' requires a name (identifier or quoted string)",
				Source:   "ssl-lsp",
				Code:     CodeInlineCodeNaming,
			})
			continue
		}

		next := tokens[nextIdx]
		if next.Type == lexer.TokenPunctuation && next.Text == ";" {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "':BEGININLINECODE' requires a name (identifier or quoted string)",
				Source:   "ssl-lsp",
				Code:     CodeInlineCodeNaming,
			})
			continue
		}

		if next.Type != lexer.TokenIdentifier && next.Type != lexer.TokenString {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(next),
				Message:  "':BEGININLINECODE' name must be an identifier or quoted string",
				Source:   "ssl-lsp",
				Code:     CodeInlineCodeNaming,
			})
		}
	}

	return diagnostics
}

// checkBeginCaseHasCase ensures each BEGINCASE contains at least one CASE block.
func checkBeginCaseHasCase(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	type caseState struct {
		token   lexer.Token
		sawCase bool
	}
	var stack []caseState

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		switch normalized {
		case "BEGINCASE":
			stack = append(stack, caseState{token: token})
		case "CASE":
			if len(stack) > 0 {
				stack[len(stack)-1].sawCase = true
			}
		case "ENDCASE":
			if len(stack) == 0 {
				continue
			}
			state := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			if !state.sawCase {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(state.token),
					Message:  "':BEGINCASE' requires at least one ':CASE' block",
					Source:   "ssl-lsp",
					Code:     CodeBeginCaseRequiresCase,
				})
			}
		}
	}

	return diagnostics
}

// checkTryStructure enforces TRY/CATCH/FINALLY structure rules.
func checkTryStructure(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	type tryState struct {
		token             lexer.Token
		hasCatch          bool
		hasFinally        bool
		bodyHasStatements bool
		inFinally         bool
		finallyToken      lexer.Token
		finallyHasBody    bool
		// inExtraFinally: inside a duplicate :FINALLY (diag.single_finally).
		// Its statements belong to no clause, so they must not stand in
		// for the body the first :FINALLY lacks (diag.finally_empty).
		inExtraFinally bool
	}
	var stack []tryState

	markStatement := func() {
		if len(stack) == 0 {
			return
		}

		if stack[len(stack)-1].inExtraFinally {
			return
		}

		if stack[len(stack)-1].inFinally {
			stack[len(stack)-1].finallyHasBody = true
			return
		}

		stack[len(stack)-1].bodyHasStatements = true
	}

	for _, token := range tokens {
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}

		if token.Type != lexer.TokenKeyword {
			if token.Type != lexer.TokenPunctuation || token.Text != ";" {
				markStatement()
			}
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		switch normalized {
		case "TRY":
			markStatement()
			stack = append(stack, tryState{token: token})
		case "CATCH":
			if len(stack) == 0 {
				continue
			}
			if !stack[len(stack)-1].bodyHasStatements {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(stack[len(stack)-1].token),
					Message:  "':TRY' requires at least one statement before ':CATCH' or ':FINALLY'",
					Source:   "ssl-lsp",
					Code:     CodeTryStructure,
				})
			}
			if stack[len(stack)-1].hasFinally {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':CATCH' must appear before ':FINALLY' in a ':TRY' block",
					Source:   "ssl-lsp",
					Code:     CodeCatchOrderBeforeFinally,
				})
				continue
			}
			if stack[len(stack)-1].hasCatch {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "Only one ':CATCH' block is allowed per ':TRY'",
					Source:   "ssl-lsp",
					Code:     CodeSingleCatch,
				})
				continue
			}
			stack[len(stack)-1].hasCatch = true
			stack[len(stack)-1].inFinally = false
		case "FINALLY":
			if len(stack) == 0 {
				continue
			}
			if !stack[len(stack)-1].bodyHasStatements {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(stack[len(stack)-1].token),
					Message:  "':TRY' requires at least one statement before ':CATCH' or ':FINALLY'",
					Source:   "ssl-lsp",
					Code:     CodeTryStructure,
				})
			}
			if stack[len(stack)-1].hasFinally {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "Only one ':FINALLY' block is allowed per ':TRY'",
					Source:   "ssl-lsp",
					Code:     CodeSingleFinally,
				})
				stack[len(stack)-1].inExtraFinally = true
				continue
			}
			stack[len(stack)-1].hasFinally = true
			stack[len(stack)-1].inFinally = true
			stack[len(stack)-1].finallyToken = token
		case "ENDTRY":
			if len(stack) == 0 {
				continue
			}
			state := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			if !state.hasCatch && !state.hasFinally {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(state.token),
					Message:  "':TRY' requires at least one ':CATCH' or ':FINALLY' block",
					Source:   "ssl-lsp",
					Code:     CodeTryRequiresHandler,
				})
			}
			// An empty protected body is reported once, at the first
			// handler clause (above); bodyHasStatements only ever turns
			// true, so a second report here could never be the only one
			// (issue #255).
			if state.hasFinally && !state.finallyHasBody {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(state.finallyToken),
					Message:  "':FINALLY' must contain at least one statement",
					Source:   "ssl-lsp",
					Code:     CodeFinallyEmpty,
				})
			}
		default:
			markStatement()
		}
	}

	return diagnostics
}

// checkErrorHandlerStructure enforces that :ERROR handlers contain at least one
// statement before :RESUME or the end of the current scope.
func checkErrorHandlerStructure(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenKeyword || strings.ToUpper(strings.TrimPrefix(token.Text, ":")) != "ERROR" {
			continue
		}

		nextIdx := -1
		for j := i + 1; j < len(tokens); j++ {
			next := tokens[j]
			if next.Type == lexer.TokenWhitespace || next.Type == lexer.TokenComment {
				continue
			}
			if next.Type == lexer.TokenPunctuation && next.Text == ";" {
				continue
			}
			nextIdx = j
			break
		}
		if nextIdx < 0 {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "':ERROR' must contain at least one statement before ':RESUME' or the end of the current scope",
				Source:   "ssl-lsp",
				Code:     CodeErrorHandlerStructure,
			})
			continue
		}

		next := tokens[nextIdx]
		if next.Type != lexer.TokenKeyword {
			continue
		}

		switch strings.ToUpper(strings.TrimPrefix(next.Text, ":")) {
		case "RESUME", "ENDPROC", "ENDTRY", "ENDWHILE", "NEXT", "ENDCASE", "ENDINLINECODE", "ENDREGION",
			// Any other block boundary also leaves the handler empty
			// (issue #255).
			"ENDIF", "ELSE", "CASE", "OTHERWISE", "CATCH", "FINALLY":
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "':ERROR' must contain at least one statement before ':RESUME' or the end of the current scope",
				Source:   "ssl-lsp",
				Code:     CodeErrorHandlerStructure,
			})
		}
	}

	return append(diagnostics, checkErrorHandlerPlacement(tokens)...)
}

// checkErrorHandlerPlacement reports the two placement defects of the
// legacy handler (issue #255, reference keywords/ERROR and RESUME): an
// `:ERROR;` with no executable statement before it in its procedure —
// the handler protects the statements before it, so placed first it
// protects nothing — and a `:RESUME;` in a procedure with no `:ERROR;`,
// which does not compile. Only the marker-statement forms count:
// `:ERROR` also appears in expression position (`LimsString(:ERROR)`).
func checkErrorHandlerPlacement(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	isMarker := func(i int) bool {
		n := nextSignificantTokenIndex(tokens, i+1)
		return n >= 0 && tokens[n].Type == lexer.TokenPunctuation && tokens[n].Text == ";"
	}

	type scopeState struct {
		protected bool // an executable statement has run before :ERROR
		hasError  bool
		resumes   []int
	}
	// Script-level statements surround the procedures, so the script
	// scope persists across them; each procedure gets its own.
	var script, proc scopeState
	inProc := false
	state := &script
	flush := func() {
		if !state.hasError {
			for _, idx := range state.resumes {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(tokens[idx]),
					Message:  "':RESUME' without an ':ERROR' handler in the same procedure does not compile",
					Source:   "ssl-lsp",
					Code:     CodeErrorHandlerStructure,
				})
			}
		}
		*state = scopeState{}
	}

	startOfStatement := true
	for i, t := range tokens {
		if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
			continue
		}
		if t.Type == lexer.TokenPunctuation && t.Text == ";" {
			startOfStatement = true
			continue
		}
		if !startOfStatement {
			continue
		}
		startOfStatement = false

		if t.Type != lexer.TokenKeyword {
			if !state.hasError {
				state.protected = true
			}
			continue
		}
		switch strings.ToUpper(strings.TrimPrefix(t.Text, ":")) {
		case "PROCEDURE":
			if inProc {
				flush()
			}
			inProc = true
			state = &proc
		case "ENDPROC":
			if inProc {
				flush()
			}
			inProc = false
			state = &script
		case "PARAMETERS", "DEFAULT", "DECLARE", "PUBLIC", "INCLUDE", "LABEL":
			// Declarations and markers protect nothing.
		case "ERROR":
			if !isMarker(i) {
				continue
			}
			if !state.hasError && !state.protected {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(t),
					Message:  "':ERROR' protects only the statements before it in the procedure; placed first it protects nothing. Move it to the end of the procedure",
					Source:   "ssl-lsp",
					Code:     CodeErrorHandlerStructure,
				})
			}
			state.hasError = true
		case "RESUME":
			if isMarker(i) {
				state.resumes = append(state.resumes, i)
			}
		default:
			if !state.hasError {
				state.protected = true
			}
		}
	}
	if inProc {
		flush()
	}
	state = &script
	flush()

	return diagnostics
}

// checkCatchClauseForm enforces the source-of-truth rule that :CATCH does not
// take an exception variable or other clause content.
func checkCatchClauseForm(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenKeyword || strings.ToUpper(strings.TrimPrefix(token.Text, ":")) != "CATCH" {
			continue
		}

		nextIdx := nextSignificantTokenIndex(tokens, i+1)
		if nextIdx < 0 {
			continue
		}

		next := tokens[nextIdx]
		if next.Type == lexer.TokenPunctuation && next.Text == ";" {
			continue
		}

		// The next token on a later line is the handler's first statement:
		// the clause lost its `;` rather than gained a variable (#255).
		message := "':CATCH' does not take an exception variable; call 'GetLastSSLError()' inside the block instead"
		rng := tokenToRange(next)
		if lineLeading(tokens, nextIdx) {
			message = "':CATCH' is missing its ';' terminator - write ':CATCH;'"
			rng = tokenToRange(token)
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    rng,
			Message:  message,
			Source:   "ssl-lsp",
			Code:     CodeCatchClauseForm,
		})
	}

	return diagnostics
}

// checkLoopAndFinallyControl validates loop control placement and FINALLY restrictions.
func checkLoopAndFinallyControl(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	type tryState struct {
		inFinally bool
	}
	var tryStack []tryState
	var loopStack []string

	inFinally := func() bool {
		for i := len(tryStack) - 1; i >= 0; i-- {
			if tryStack[i].inFinally {
				return true
			}
		}
		return false
	}

	hasLoop := func(kind string) bool {
		for i := len(loopStack) - 1; i >= 0; i-- {
			if kind == "" || loopStack[i] == kind {
				return true
			}
		}
		return false
	}

	popLoop := func(kind string) {
		for i := len(loopStack) - 1; i >= 0; i-- {
			if loopStack[i] == kind {
				loopStack = append(loopStack[:i], loopStack[i+1:]...)
				return
			}
		}
	}

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		switch normalized {
		case "TRY":
			tryStack = append(tryStack, tryState{})
		case "CATCH":
			if len(tryStack) > 0 {
				tryStack[len(tryStack)-1].inFinally = false
			}
		case "FINALLY":
			if len(tryStack) > 0 {
				tryStack[len(tryStack)-1].inFinally = true
			}
		case "ENDTRY":
			if len(tryStack) > 0 {
				tryStack = tryStack[:len(tryStack)-1]
			}
		case "FOR":
			loopStack = append(loopStack, "FOR")
		case "WHILE":
			loopStack = append(loopStack, "WHILE")
		case "NEXT", "ENDFOR":
			// ENDFOR is endfor_invalid's finding; it still ends the loop
			// it was meant to end (issue #255).
			popLoop("FOR")
		case "ENDWHILE":
			popLoop("WHILE")
		case "EXITFOR":
			if inFinally() {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':EXITFOR' inside a ':FINALLY' block is a compile-time error",
					Source:   "ssl-lsp",
					Code:     CodeExitForInFinally,
				})
			}
			if !hasLoop("FOR") {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':EXITFOR' must be inside a ':FOR' loop",
					Source:   "ssl-lsp",
					Code:     CodeExitForOutsideLoop,
				})
			}
		case "EXITWHILE":
			if inFinally() {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':EXITWHILE' inside a ':FINALLY' block is a compile-time error",
					Source:   "ssl-lsp",
					Code:     CodeExitWhileInFinally,
				})
			}
			if !hasLoop("WHILE") {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':EXITWHILE' must be inside a ':WHILE' loop",
					Source:   "ssl-lsp",
					Code:     CodeExitWhileOutsideLoop,
				})
			}
		case "LOOP":
			if inFinally() {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':LOOP' inside a ':FINALLY' block is a compile-time error",
					Source:   "ssl-lsp",
					Code:     CodeLoopInFinally,
				})
			}
			if !hasLoop("") {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':LOOP' must be inside a ':WHILE' or ':FOR' loop",
					Source:   "ssl-lsp",
					Code:     CodeLoopOutsideLoop,
				})
			}
		case "RETURN":
			if inFinally() {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':RETURN' inside a ':FINALLY' block is a compile-time error",
					Source:   "ssl-lsp",
					Code:     CodeReturnInFinally,
				})
			}
		}
	}

	return diagnostics
}

// checkDeprecatedKeywords reports legacy keywords that the style guide discourages in new code.
func checkDeprecatedKeywords(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		var message string
		switch {
		case normalized == "ERROR":
			message = "':ERROR' is legacy error handling. Prefer ':TRY' / ':CATCH' / ':FINALLY'"
		case normalized == "RESUME":
			message = "':RESUME' is legacy error handling. Prefer ':TRY' / ':CATCH' / ':FINALLY'"
		case normalized == "LABEL" || strings.HasPrefix(normalized, "LABEL"):
			message = "':LABEL' is legacy flow control used with Branch(); prefer structured control flow in new code"
		}

		if message != "" {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(token),
				Message:  message,
				Source:   "ssl-lsp",
				Code:     CodeDeprecatedKeyword,
			})
		}
	}

	return diagnostics
}

// checkNotPreferredOperators reports valid but discouraged operator forms.
func checkNotPreferredOperators(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenOperator {
			continue
		}

		switch token.Text {
		case "#":
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range:    tokenToRange(token),
				Message:  "Use '!=' instead of '#' for inequality",
				Source:   "ssl-lsp",
				Code:     CodeNotPreferredOperator,
			})
		case "<>":
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range:    tokenToRange(token),
				Message:  "Use '!=' instead of '<>' for inequality",
				Source:   "ssl-lsp",
				Code:     CodeNotPreferredOperator,
			})
		}
	}

	return diagnostics
}

// checkScientificNotation detects numbers immediately followed by an identifier
// starting with 'e' or 'E', which suggests the user intended scientific notation
// but omitted the required decimal point (e.g., 7e2 should be 7.0e2).
func checkScientificNotation(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens)-1; i++ {
		if tokens[i].Type != lexer.TokenNumber {
			continue
		}
		num := tokens[i].Text

		next := tokens[i+1]
		if next.Type != lexer.TokenIdentifier {
			continue
		}
		upper := strings.ToUpper(next.Text)

		// Case 1: number WITHOUT decimal followed by eN, e-N, e+N identifier
		// e.g., 7e2 -> tokens: "7" + "e2"; 1e-3 -> tokens: "1" + "e" + "-" + "3"
		if !strings.Contains(num, ".") {
			if len(upper) >= 2 && upper[0] == 'E' && (upper[1] >= '0' && upper[1] <= '9' || upper[1] == '+' || upper[1] == '-') {
				// The exponent sign, if any, is kept: either sign is valid
				// SSL (issue #246); only the decimal point is missing.
				suggested := next.Text
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(tokens[i]),
					Message:  fmt.Sprintf("SSL scientific notation requires a decimal point: use '%s.0%s' instead of '%s%s'", num, suggested, num, next.Text),
					Source:   "ssl-lsp",
					Code:     CodeScientificNotation,
				})
			}
			// Case 1b: 9E+1 -> tokens: "9" + "E" (single char) + "+" + "1"
			if upper == "E" && i+2 < len(tokens) {
				afterE := tokens[i+2]
				if afterE.Type == lexer.TokenOperator && (afterE.Text == "+" || afterE.Text == "-") {
					// Either exponent sign is valid SSL and kept (issue
					// #246); only the decimal point is missing.
					suggestedSign := afterE.Text
					suggestedExp := "..."
					if i+3 < len(tokens) && tokens[i+3].Type == lexer.TokenNumber {
						suggestedExp = tokens[i+3].Text
					}
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(tokens[i]),
						Message:  fmt.Sprintf("SSL scientific notation requires a decimal point: use '%s.0%s%s%s' instead of '%s%s%s...'", num, next.Text, suggestedSign, suggestedExp, num, next.Text, afterE.Text),
						Source:   "ssl-lsp",
						Code:     CodeScientificNotation,
					})
				}
			}
		}

		// Case 2: number WITH decimal but no digit before decimal (e.g., .5e1)
		// The lexer produces ".5" as a number token followed by "e1" identifier
		if strings.HasPrefix(num, ".") && upper[0] == 'E' {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(tokens[i]),
				Message:  fmt.Sprintf("SSL scientific notation requires a digit before the decimal point: use '0%s%s' instead of '%s%s'", num, next.Text, num, next.Text),
				Source:   "ssl-lsp",
				Code:     CodeScientificNotation,
			})
		}
	}

	return diagnostics
}

// checkLiteralTypeSafety reports type-safety gotchas from the style guide using
// conservative local type inference.
func checkLiteralTypeSafety(tokens []lexer.Token, typeInfo map[string]string) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenOperator {
			continue
		}

		prevIdx := previousSignificantTokenIndex(tokens, i-1)
		nextIdx := nextSignificantTokenIndex(tokens, i+1)
		if prevIdx < 0 || nextIdx < 0 {
			continue
		}

		left := tokens[prevIdx]
		right := tokens[nextIdx]

		switch token.Text {
		case "=":
			if isNilDefaultValueComparison(left, right) {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityInfo,
					Range:    tokenToRange(token),
					Message:  "NIL is not the same as empty string, zero, or .F. Declared variables initialize to empty string, not NIL",
					Source:   "ssl-lsp",
					Code:     CodeNilNotEmptyString,
				})
				continue
			}
			if left.Type == lexer.TokenString || right.Type == lexer.TokenString {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityInfo,
					Range:    tokenToRange(token),
					Message:  "For strings, '=' does prefix matching. Use '==' for exact string comparisons",
					Source:   "ssl-lsp",
					Code:     CodeEqualsVsStrictEquals,
				})
			}
		case "==":
			if isNilDefaultValueComparison(left, right) {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityInfo,
					Range:    tokenToRange(token),
					Message:  "NIL is not the same as empty string, zero, or .F. Declared variables initialize to empty string, not NIL",
					Source:   "ssl-lsp",
					Code:     CodeNilNotEmptyString,
				})
			}
		case "+", "-", "*", "/":
			// Operand types of arithmetic and concatenation are judged on
			// the expression tree: nil_in_operations (checkNilInOperations),
			// mixed_type_operator (checkMixedTypePlus) and
			// arithmetic_type_mismatch (checkOperatorOperandTypes).
			continue
		}
	}

	return diagnostics
}

func isNilDefaultValueComparison(left, right lexer.Token) bool {
	return (isNilLiteral(left) && isDefaultValueLiteral(right)) || (isNilLiteral(right) && isDefaultValueLiteral(left))
}

func isNilLiteral(token lexer.Token) bool {
	if token.Type != lexer.TokenKeyword {
		return false
	}

	canonical, ok := constants.CanonicalSSLLiteral(token.Text)
	return ok && canonical == "NIL"
}

func isBooleanLiteral(token lexer.Token) bool {
	if token.Type != lexer.TokenKeyword {
		return false
	}

	canonical, ok := constants.CanonicalSSLLiteral(token.Text)
	return ok && (canonical == ".T." || canonical == ".F.")
}

func isEmptyStringLiteral(token lexer.Token) bool {
	if token.Type != lexer.TokenString || len(token.Text) < 2 {
		return false
	}

	switch {
	case strings.HasPrefix(token.Text, "\"") && strings.HasSuffix(token.Text, "\""):
		return len(token.Text) == 2
	case strings.HasPrefix(token.Text, "'") && strings.HasSuffix(token.Text, "'"):
		return len(token.Text) == 2
	case strings.HasPrefix(token.Text, "[") && strings.HasSuffix(token.Text, "]"):
		return len(token.Text) == 2
	default:
		return false
	}
}

func isZeroNumberLiteral(token lexer.Token) bool {
	return token.Type == lexer.TokenNumber && token.Text == "0"
}

func isFalseLiteral(token lexer.Token) bool {
	if token.Type != lexer.TokenKeyword {
		return false
	}

	canonical, ok := constants.CanonicalSSLLiteral(token.Text)
	return ok && canonical == ".F."
}

func isDefaultValueLiteral(token lexer.Token) bool {
	return isEmptyStringLiteral(token) || isZeroNumberLiteral(token) || isFalseLiteral(token)
}

func buildSimpleTypeInfo(tokens []lexer.Token, variables []parser.VariableInfo) map[string]string {
	typeInfo := make(map[string]string, len(variables))

	for _, variable := range variables {
		if inferred := inferTypeFromName(variable.Name); inferred != "" {
			typeInfo[strings.ToLower(variable.Name)] = inferred
		}
	}

	for i := 0; i < len(tokens); i++ {
		if tokens[i].Type != lexer.TokenIdentifier {
			continue
		}

		prevIdx := previousSignificantTokenIndex(tokens, i-1)
		if prevIdx >= 0 && tokens[prevIdx].Type == lexer.TokenPunctuation && tokens[prevIdx].Text == ":" {
			continue
		}

		assignIdx := nextSignificantTokenIndex(tokens, i+1)
		if assignIdx < 0 || tokens[assignIdx].Type != lexer.TokenOperator || tokens[assignIdx].Text != ":=" {
			continue
		}

		stmtEnd := -1
		for j := assignIdx + 1; j < len(tokens); j++ {
			if tokens[j].Type == lexer.TokenPunctuation && tokens[j].Text == ";" {
				stmtEnd = j
				break
			}
		}
		if stmtEnd < 0 {
			continue
		}

		exprStart := nextSignificantTokenIndex(tokens, assignIdx+1)
		if exprStart < 0 || exprStart >= stmtEnd {
			continue
		}

		exprEnd := expressionEnd(tokens, exprStart, -1, stmtEnd)
		inferred := inferExpressionType(tokens, exprStart, exprEnd, typeInfo)
		if inferred == "" || inferred == "nil" {
			continue
		}

		typeInfo[strings.ToLower(tokens[i].Text)] = inferred
	}

	return typeInfo
}

func inferTypeFromName(name string) string {
	switch {
	case isHungarianExemptName(name):
		return "numeric"
	case hasStrictHungarianPrefix(name, "fn"):
		return "codeblock"
	case hasStrictHungarianPrefix(name, "s"):
		return "string"
	case hasStrictHungarianPrefix(name, "n"):
		return "numeric"
	case hasStrictHungarianPrefix(name, "b"):
		return "boolean"
	case hasStrictHungarianPrefix(name, "a"):
		return "array"
	case hasStrictHungarianPrefix(name, "o"):
		return "object"
	case hasStrictHungarianPrefix(name, "d"):
		return "date"
	default:
		return ""
	}
}

// hasStrictHungarianPrefix is the case-sensitive variant used for type
// inference. The original name must start with the lowercase prefix and the
// next non-underscore rune must be uppercase. This avoids classifying names
// like "DCUparseCat" as date-typed (its leading 'D' is the start of an
// acronym, not a Hungarian 'd' prefix).
func hasStrictHungarianPrefix(name, prefix string) bool {
	trimmed := strings.TrimLeft(name, "_")
	if !strings.HasPrefix(trimmed, prefix) {
		return false
	}
	remainder := trimmed[len(prefix):]
	remainder = strings.TrimLeft(remainder, "_")
	if remainder == "" {
		return false
	}
	return unicode.IsUpper([]rune(remainder)[0])
}

func inferExpressionType(tokens []lexer.Token, startIdx, endIdx int, typeInfo map[string]string) string {
	if startIdx < 0 || endIdx < startIdx || endIdx >= len(tokens) {
		return ""
	}

	startIdx = nextSignificantTokenIndex(tokens, startIdx)
	if startIdx < 0 || startIdx > endIdx {
		return ""
	}
	endIdx = previousSignificantTokenIndex(tokens, endIdx)
	if endIdx < startIdx {
		return ""
	}

	// Top-level operator scans must respect grouping. An operator inside
	// `(...)`, `[...]`, or `{...}` is part of a sub-expression (function-call
	// argument, indexed lookup, array literal) — picking it up as the
	// expression's outer operator misclassifies the whole expression.
	depth := 0
	for i := startIdx; i <= endIdx; i++ {
		token := tokens[i]
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}
		if token.Type == lexer.TokenPunctuation {
			switch token.Text {
			case "(", "[", "{":
				depth++
				continue
			case ")", "]", "}":
				if depth > 0 {
					depth--
				}
				continue
			}
		}
		if depth > 0 {
			continue
		}
		if token.Type == lexer.TokenOperator {
			switch token.Text {
			case "=", "==", "!=", "<>", "<", ">", "<=", ">=", "$", ".AND.", ".OR.", ".NOT.":
				return "boolean"
			}
		}
	}

	depth = 0
	for i := startIdx; i <= endIdx; i++ {
		token := tokens[i]
		if token.Type == lexer.TokenPunctuation {
			switch token.Text {
			case "(", "[", "{":
				depth++
				continue
			case ")", "]", "}":
				if depth > 0 {
					depth--
				}
				continue
			}
		}
		if depth > 0 {
			continue
		}
		if token.Type != lexer.TokenOperator {
			continue
		}

		switch token.Text {
		case "+", "-", "*", "/":
			leftType := inferExpressionType(tokens, startIdx, previousSignificantTokenIndex(tokens, i-1), typeInfo)
			rightType := inferExpressionType(tokens, nextSignificantTokenIndex(tokens, i+1), endIdx, typeInfo)
			if token.Text == "+" && (leftType == "string" || rightType == "string") {
				return "string"
			}
			return "numeric"
		}
	}

	if startIdx == endIdx {
		return inferSimpleType(tokens, startIdx, typeInfo)
	}

	startToken := tokens[startIdx]
	if startToken.Type == lexer.TokenPunctuation && startToken.Text == "{" {
		if isCodeBlockLiteralStart(tokens, startIdx) {
			return "codeblock"
		}
		return "array"
	}

	if startToken.Type == lexer.TokenIdentifier {
		nextIdx := nextSignificantTokenIndex(tokens, startIdx+1)
		if nextIdx >= 0 && nextIdx <= endIdx {
			if tokens[nextIdx].Type == lexer.TokenPunctuation && tokens[nextIdx].Text == "(" {
				return inferFunctionReturnType(startToken.Text)
			}
			if tokens[nextIdx].Type == lexer.TokenPunctuation && tokens[nextIdx].Text == "{" && constants.IsSSLClass(startToken.Text) {
				return "object"
			}
			// Indexed access (arr[i]) — element type is opaque, don't classify.
			if tokens[nextIdx].Type == lexer.TokenPunctuation && tokens[nextIdx].Text == "[" {
				return ""
			}
			// Member access (Me:Foo, obj:bar) — member type is opaque.
			if tokens[nextIdx].Type == lexer.TokenPunctuation && tokens[nextIdx].Text == ":" {
				return ""
			}
		}
	}

	return inferSimpleType(tokens, startIdx, typeInfo)
}

func inferFunctionReturnType(name string) string {
	sig, ok := constants.GetFunctionSignature(name)
	if !ok {
		return ""
	}

	switch strings.ToLower(sig.ReturnType) {
	case "string":
		return "string"
	case "double", "numeric", "number", "integer":
		return "numeric"
	case "boolean", "bool":
		return "boolean"
	case "array":
		return "array"
	case "date":
		return "date"
	case "object", "sslexpando", "ssldataset", "sslnetobject":
		return "object"
	case "sslfunction", "codeblock":
		return "codeblock"
	default:
		return ""
	}
}

func expressionEnd(tokens []lexer.Token, startIdx, upperBound, stmtEnd int) int {
	limit := stmtEnd
	if upperBound >= 0 && upperBound < limit {
		limit = upperBound
	}
	return previousSignificantTokenIndex(tokens, limit-1)
}

func inferSimpleType(tokens []lexer.Token, idx int, typeInfo map[string]string) string {
	if idx < 0 || idx >= len(tokens) {
		return ""
	}

	token := tokens[idx]

	switch token.Type {
	case lexer.TokenString:
		return "string"
	case lexer.TokenNumber:
		return "numeric"
	case lexer.TokenKeyword:
		if isNilLiteral(token) {
			return "nil"
		}
		if isBooleanLiteral(token) {
			return "boolean"
		}
	case lexer.TokenIdentifier:
		if inferred := typeInfo[strings.ToLower(token.Text)]; inferred != "" {
			return inferred
		}
		if inferred := inferTypeFromName(token.Text); inferred != "" {
			return inferred
		}

		nextIdx := nextSignificantTokenIndex(tokens, idx+1)
		if nextIdx >= 0 {
			if tokens[nextIdx].Type == lexer.TokenPunctuation && tokens[nextIdx].Text == "(" {
				return inferFunctionReturnType(token.Text)
			}
			if tokens[nextIdx].Type == lexer.TokenPunctuation && tokens[nextIdx].Text == "{" && constants.IsSSLClass(token.Text) {
				return "object"
			}
		}
	case lexer.TokenPunctuation:
		if token.Text == "{" && isCodeBlockLiteralStart(tokens, idx) {
			return "codeblock"
		}
		if token.Text == "{" {
			return "array"
		}
		if token.Text == "}" && isCodeBlockLiteralEnd(tokens, idx) {
			return "codeblock"
		}
	}

	return ""
}

func isCodeBlockLiteralStart(tokens []lexer.Token, idx int) bool {
	if idx < 0 || idx >= len(tokens) || tokens[idx].Type != lexer.TokenPunctuation || tokens[idx].Text != "{" {
		return false
	}

	nextIdx := nextSignificantTokenIndex(tokens, idx+1)
	return nextIdx >= 0 && tokens[nextIdx].Type == lexer.TokenUnknown && tokens[nextIdx].Text == "|"
}

func isCodeBlockLiteralEnd(tokens []lexer.Token, idx int) bool {
	if idx < 0 || idx >= len(tokens) || tokens[idx].Type != lexer.TokenPunctuation || tokens[idx].Text != "}" {
		return false
	}

	depth := 0
	for i := idx; i >= 0; i-- {
		token := tokens[i]
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}

		if token.Type == lexer.TokenPunctuation {
			switch token.Text {
			case "}":
				depth++
			case "{":
				depth--
				if depth == 0 {
					return isCodeBlockLiteralStart(tokens, i)
				}
			}
		}
	}

	return false
}

// checkEmptyOptionalParamArrays warns when callers pass {} for trailing optional array args
// that the style guide recommends omitting entirely.
func checkEmptyOptionalParamArrays(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	targets := map[string]int{
		"DOPROC":       2,
		"EXECFUNCTION": 2,
		"GETDATASET":   2,
	}

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		targetArgIndex, ok := targets[strings.ToUpper(token.Text)]
		if !ok {
			continue
		}

		callStart := nextSignificantTokenIndex(tokens, i+1)
		if callStart < 0 || tokens[callStart].Type != lexer.TokenPunctuation || tokens[callStart].Text != "(" {
			continue
		}

		argStarts, argEnds, closeIdx := parseTopLevelCallArguments(tokens, callStart)
		if closeIdx < 0 || len(argStarts) < targetArgIndex {
			continue
		}

		argPos := targetArgIndex - 1
		if !isEmptyArrayLiteral(tokens, argStarts[argPos], argEnds[argPos]) {
			continue
		}

		hasLaterArgs := false
		for j := argPos + 1; j < len(argStarts); j++ {
			if argStarts[j] >= 0 && argEnds[j] >= argStarts[j] {
				hasLaterArgs = true
				break
			}
		}
		if hasLaterArgs {
			continue
		}

		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityInfo,
			Range: Range{
				Start: Position{Line: tokens[argStarts[argPos]].Line - 1, Character: tokens[argStarts[argPos]].Column - 1},
				End:   Position{Line: tokens[argEnds[argPos]].Line - 1, Character: tokens[argEnds[argPos]].Column - 1 + utf8.RuneCountInString(tokens[argEnds[argPos]].Text)},
			},
			Message: fmt.Sprintf("Omit the trailing empty array for '%s' instead of passing '{}'", token.Text),
			Source:  "ssl-lsp",
			Code:    CodeEmptyOptionalParamArray,
		})
	}

	return diagnostics
}

// checkBranchTargetLabels validates literal Branch() targets
// (diag.branch_target_label). The runtime matches a target against the
// stored label text exactly — the keyword included, so `:LABEL SKIP;` is
// reached by "LABEL SKIP" — and resolves labels within the current
// procedure or script. A literal target is therefore checked against the
// labels declared in the same scope (issue #255).
func checkBranchTargetLabels(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenIdentifier || !strings.EqualFold(token.Text, "Branch") {
			continue
		}

		openParenIdx := nextSignificantTokenIndex(tokens, i+1)
		if openParenIdx < 0 || tokens[openParenIdx].Type != lexer.TokenPunctuation || tokens[openParenIdx].Text != "(" {
			continue
		}

		argStarts, argEnds, closeIdx := parseTopLevelCallArguments(tokens, openParenIdx)
		if closeIdx < 0 || len(argStarts) == 0 || len(argEnds) == 0 {
			continue
		}

		argStart := argStarts[0]
		argEnd := argEnds[0]
		if argStart < 0 || argEnd < argStart || tokens[argStart].Type != lexer.TokenString || argStart != argEnd {
			continue
		}

		target := collapseSpaces(unquoteSSLString(tokens[argStart].Text))
		if target == "" {
			continue
		}

		if !strings.HasPrefix(target, "LABEL") {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(tokens[argStart]),
				Message:  "Branch target string must include the label token text, such as \"LABEL SKIP\" or \"LABELSKIP\"",
				Source:   "ssl-lsp",
				Code:     CodeBranchTargetLabel,
			})
			continue
		}

		labels, ok := labelsInScope(tokens, i)
		if !ok || labels[target] {
			continue
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(tokens[argStart]),
			Message:  fmt.Sprintf("Branch target %q matches no ':LABEL' in this procedure; the target must equal the label text exactly, keyword included", target),
			Source:   "ssl-lsp",
			Code:     CodeBranchTargetLabel,
		})
	}

	return diagnostics
}

// labelsInScope returns the stored texts of the labels declared in the
// procedure enclosing token idx — or, outside any procedure, at script
// level — with whitespace runs collapsed. ok is false when the scope
// cannot be judged from this file alone (it pulls code in via :INCLUDE).
func labelsInScope(tokens []lexer.Token, idx int) (map[string]bool, bool) {
	// Scope bounds: the procedure that contains idx, else the whole file
	// outside procedures.
	scopeStart, scopeEnd := -1, -1
	depth := 0
	for j := 0; j < len(tokens); j++ {
		t := tokens[j]
		if t.Type != lexer.TokenKeyword {
			continue
		}
		switch strings.ToUpper(strings.TrimPrefix(t.Text, ":")) {
		case "PROCEDURE":
			if depth == 0 && j < idx {
				scopeStart = j
			}
			depth++
		case "ENDPROC":
			if depth > 0 {
				depth--
			}
			if depth == 0 && scopeStart >= 0 && scopeEnd < 0 && j > idx {
				scopeEnd = j
			}
			if depth == 0 && j < idx {
				scopeStart = -1
			}
		}
	}
	var depths []int
	if scopeStart < 0 {
		depths = procedureDepths(tokens)
	}
	inScope := func(j int) bool {
		if scopeStart >= 0 {
			return j > scopeStart && (scopeEnd < 0 || j < scopeEnd)
		}
		// Script level: outside every procedure.
		return depths[j] == 0
	}

	labels := map[string]bool{}
	for j := 0; j < len(tokens); j++ {
		t := tokens[j]
		if t.Type != lexer.TokenKeyword || !inScope(j) {
			continue
		}
		name := strings.TrimPrefix(t.Text, ":")
		if strings.EqualFold(name, "INCLUDE") {
			return nil, false
		}
		if !strings.HasPrefix(strings.ToUpper(name), "LABEL") {
			continue
		}
		var b strings.Builder
		b.WriteString(name)
		for k := j + 1; k < len(tokens); k++ {
			if tokens[k].Type == lexer.TokenPunctuation && tokens[k].Text == ";" {
				break
			}
			if tokens[k].Type == lexer.TokenComment {
				continue
			}
			b.WriteString(tokens[k].Text)
		}
		labels[collapseSpaces(b.String())] = true
	}
	return labels, true
}

// procedureDepths returns the :PROCEDURE nesting depth at each token; a
// :PROCEDURE or :ENDPROC keyword itself counts as inside.
func procedureDepths(tokens []lexer.Token) []int {
	depths := make([]int, len(tokens))
	depth := 0
	for k, t := range tokens {
		if t.Type == lexer.TokenKeyword {
			switch strings.ToUpper(strings.TrimPrefix(t.Text, ":")) {
			case "PROCEDURE":
				depth++
				depths[k] = depth
				continue
			case "ENDPROC":
				depths[k] = depth
				if depth > 0 {
					depth--
				}
				continue
			}
		}
		depths[k] = depth
	}
	return depths
}

// collapseSpaces trims s and folds every whitespace run to one space.
func collapseSpaces(s string) string {
	return strings.Join(strings.Fields(s), " ")
}

func unquoteSSLString(text string) string {
	if len(text) >= 2 {
		switch {
		case text[0] == '"' && text[len(text)-1] == '"':
			return text[1 : len(text)-1]
		case text[0] == '\'' && text[len(text)-1] == '\'':
			return text[1 : len(text)-1]
		case text[0] == '[' && text[len(text)-1] == ']':
			return text[1 : len(text)-1]
		}
	}
	return text
}

// checkPublicVariables warns on :PUBLIC usage because the style guide treats it
// as risky shared state.
func checkPublicVariables(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}
		if strings.ToUpper(strings.TrimPrefix(token.Text, ":")) != "PUBLIC" {
			continue
		}

		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityInfo,
			Range:    tokenToRange(token),
			Message:  "':PUBLIC' variables persist across procedures and risk namespace pollution. Prefer ':DECLARE' with parameter passing",
			Source:   "ssl-lsp",
			Code:     CodeLimitPublicVars,
		})
	}

	return diagnostics
}

// checkProcedureParameterCounts reports procedures whose parameter count
// exceeds the style guide's recommended maximum (8), with a sharper
// message past the platform's own warning threshold (20). Both tiers are
// info severity under one code; the range is the procedure name.
func checkProcedureParameterCounts(procedures []parser.ProcedureInfo) []Diagnostic {
	var diagnostics []Diagnostic

	for _, proc := range procedures {
		count := len(proc.Parameters)
		if count <= 8 {
			continue
		}
		message := fmt.Sprintf("Procedure '%s' has %d parameters; style guide recommends at most 8 per procedure", proc.Name, count)
		if count > 20 {
			message = fmt.Sprintf("Procedure '%s' has %d parameters; procedures with more than 20 parameters should be refactored", proc.Name, count)
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityInfo,
			Range:    procedureNameRange(proc),
			Message:  message,
			Source:   "ssl-lsp",
			Code:     CodeMaxParamsWarning,
		})
	}

	return diagnostics
}

// procedureNameRange ranges over the name token of a :PROCEDURE
// declaration. When the declaration node is unavailable it falls back to
// the start of the declaration line.
func procedureNameRange(proc parser.ProcedureInfo) Range {
	if proc.Node != nil {
		sawKeyword := false
		for _, t := range proc.Node.Tokens {
			if t.Type == lexer.TokenKeyword && strings.EqualFold(strings.TrimPrefix(t.Text, ":"), "PROCEDURE") {
				sawKeyword = true
				continue
			}
			if sawKeyword && t.Type == lexer.TokenIdentifier {
				return tokenToRange(t)
			}
		}
	}
	return Range{
		Start: Position{Line: proc.StartLine - 1, Character: 0},
		End:   Position{Line: proc.StartLine - 1, Character: len(proc.Name)},
	}
}

// checkNameLengths warns when variable or procedure names exceed style guide limits.
// Style guide: variable names max 20 characters (excluding Hungarian prefix),
// procedure names max 30 characters.
func checkNameLengths(variables []parser.VariableInfo, procedures []parser.ProcedureInfo, prefixes []string) []Diagnostic {
	var diagnostics []Diagnostic

	for _, v := range variables {
		// Strip Hungarian prefix to get the effective name length
		effectiveName := v.Name
		trimmed := strings.TrimLeft(v.Name, "_")
		if trimmed != "" {
			lower := strings.ToLower(trimmed)
			for _, prefix := range prefixes {
				if strings.HasPrefix(lower, prefix) {
					rest := trimmed[len(prefix):]
					if len(rest) > 0 && unicode.IsUpper([]rune(rest)[0]) {
						effectiveName = rest
						break
					}
				}
			}
		}

		if len(effectiveName) > 20 {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range: Range{
					Start: Position{Line: v.Line - 1, Character: v.Column - 1},
					End:   Position{Line: v.Line - 1, Character: v.Column - 1 + len(v.Name)},
				},
				Message: fmt.Sprintf("Variable name '%s' exceeds 20-character limit (effective length %d excluding prefix)", v.Name, len(effectiveName)),
				Source:  "ssl-lsp",
				Code:    CodeIdentifierTooLong,
			})
		}
	}

	for _, proc := range procedures {
		if len(proc.Name) > 30 {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range: Range{
					Start: Position{Line: proc.StartLine - 1, Character: 0},
					End:   Position{Line: proc.StartLine - 1, Character: len(proc.Name)},
				},
				Message: fmt.Sprintf("Procedure name '%s' exceeds 30-character limit (length %d)", proc.Name, len(proc.Name)),
				Source:  "ssl-lsp",
				Code:    CodeIdentifierTooLong,
			})
		}
	}

	return diagnostics
}

// checkVisibilityAnnotations validates /*@private; and /*@protected;
// annotations. The one effective placement is the exact lowercase form on
// its own line immediately before a script :PROCEDURE; everything else the
// author plainly meant as an annotation is inert and reported, because the
// procedure silently stays public (issue #255). Class methods are always
// public, so any annotation in a class file is inert too. An effective
// annotation instead gets the info-tier visibility_annotation_usage note
// (issue #198) for teams that prefer procedures unannotated. Exactly one
// of the two rules speaks per annotation, and the verdict matches
// parser.ProcedureInfo.IsPrivate.
func checkVisibilityAnnotations(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	inClass := false
	warn := func(token lexer.Token, msg string) {
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range:    tokenToRange(token),
			Message:  msg,
			Source:   "ssl-lsp",
			Code:     CodeVisibilityAnnotation,
		})
	}

	for i, token := range tokens {
		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
			if normalized == "CLASS" {
				inClass = true
			}
		}

		if token.Type != lexer.TokenComment {
			continue
		}

		content, exact := parser.ParseVisibilityAnnotation(token.Text)
		nearContent, nearMiss := parser.ParseNearMissVisibilityAnnotation(token.Text)
		if !exact && !nearMiss {
			continue
		}
		attach, nextIdx := parser.VisibilityAnnotationAttachment(tokens, i)

		if inClass {
			if exact || attach != parser.AnnotationNoProcedure {
				if !exact {
					content = nearContent
				}
				warn(token, fmt.Sprintf("Visibility annotation '/*@%s;' has no effect on class methods — class methods are always public/virtual", content))
			}
			continue
		}

		if nearMiss {
			// A near-miss spelling is ordinary comment text; only one that
			// stands before a :PROCEDURE is plainly an annotation attempt.
			if attach != parser.AnnotationNoProcedure {
				warn(token, fmt.Sprintf("'%s' is not a recognized visibility annotation, so the procedure stays public — write exactly '/*@%s;' (lowercase, no spaces)", strings.TrimSpace(token.Text), nearContent))
			}
			continue
		}

		switch attach {
		case parser.AnnotationAttached:
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("Visibility annotation '/*@%s;' - this team convention prefers procedures unannotated", content),
				Source:   "ssl-lsp",
				Code:     CodeVisibilityAnnotationUsage,
			})
		case parser.AnnotationDetached:
			warn(token, fmt.Sprintf("Visibility annotation '/*@%s;' must sit on its own line immediately before ':PROCEDURE' — a blank line, another comment, or a shared line breaks the association, so the procedure stays public", content))
		default:
			// Followed by another keyword: provably misplaced. Followed by
			// an expression statement or nothing: left alone, since
			// annotation-shaped comments also serve as prose markers.
			if nextIdx >= 0 && tokens[nextIdx].Type == lexer.TokenKeyword {
				warn(token, fmt.Sprintf("Visibility annotation '/*@%s;' should appear on its own line immediately before ':PROCEDURE'", content))
			}
		}
	}

	return diagnostics
}

// checkNilMethodCalls detects patterns where methods are called on NIL values.
// Style guide: "Do NOT call instance methods on NIL (raises error)".
// This uses conservative analysis — only flags cases where a variable is
// compared to NIL or known to be NIL from assignment context.
//
// Tracking replays assignments in source order, with two flow-aware
// guards (issue #304): a use guarded by a negated-emptiness test of the
// same variable (`!Empty(v)`, `v != NIL`) in an enclosing :IF/:WHILE
// condition or an earlier `.AND.` operand of the same condition is
// skipped, and a NIL assignment inside an :IF branch or loop body that
// ends in :RETURN does not reach code after that branch.
func checkNilMethodCalls(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Track variables assigned NIL
	nilVars := make(map[string]bool)
	flow := newNilFlow()
	// condGuards marks, per token index inside an :IF/:WHILE condition,
	// the variables an earlier `.AND.` operand proved non-empty.
	condGuards := make(map[int]map[string]bool)

	// isQualified reports whether the identifier at idx is a `:`-qualified
	// member (`Me:oClient`, `oOuter:oInner`) rather than a bare local —
	// members are object state, not the locals this check tracks
	// (issue #207: `Me:oClient := NIL;` in a teardown must not poison the
	// bare name for the whole file).
	isQualified := func(idx int) bool {
		prev := previousSignificantTokenIndex(tokens, idx-1)
		return prev >= 0 && tokens[prev].Type == lexer.TokenPunctuation && tokens[prev].Text == ":"
	}

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]

		// Locals live per procedure; tracking resets at each boundary.
		if token.Type == lexer.TokenKeyword {
			kw := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
			switch kw {
			case "PROCEDURE", "ENDPROC":
				nilVars = make(map[string]bool)
				flow = newNilFlow()
			case "IF", "WHILE":
				end, body, inCond := nilConditionGuards(tokens, i+1)
				for idx, vars := range inCond {
					condGuards[idx] = vars
				}
				flow.open(body, end)
			case "FOR":
				flow.open(nil, i)
			case "BEGINCASE", "TRY":
				flow.openOpaque()
			case "ELSE":
				flow.branch(nilVars)
			case "ENDIF", "ENDWHILE", "NEXT":
				flow.close(nilVars)
			case "ENDCASE", "ENDTRY":
				flow.closeOpaque()
			case "RETURN":
				flow.markReturned()
			case "EXITWHILE", "EXITFOR", "LOOP":
				flow.markJumped()
			}
		}

		// Track NIL assignments: x := NIL;
		if token.Type == lexer.TokenOperator && token.Text == ":=" {
			prevIdx := previousSignificantTokenIndex(tokens, i-1)
			nextIdx := nextSignificantTokenIndex(tokens, i+1)
			if prevIdx >= 0 && nextIdx >= 0 &&
				tokens[prevIdx].Type == lexer.TokenIdentifier && !isQualified(prevIdx) {
				nextTok := tokens[nextIdx]
				name := strings.ToUpper(tokens[prevIdx].Text)
				isNilAssign := strings.EqualFold(nextTok.Text, "NIL") &&
					(nextTok.Type == lexer.TokenIdentifier || nextTok.Type == lexer.TokenKeyword)
				if isNilAssign {
					flow.recordNil(name, nilVars[name])
					nilVars[name] = true
				} else {
					// Any non-NIL assignment clears the flag
					delete(nilVars, name)
				}
			}
		}

		// Check for method calls on NIL literal: NIL:Method()
		isNilToken := strings.EqualFold(token.Text, "NIL") &&
			(token.Type == lexer.TokenIdentifier || token.Type == lexer.TokenKeyword)
		if isNilToken {
			nextIdx := nextSignificantTokenIndex(tokens, i+1)
			if nextIdx >= 0 {
				nextTok := tokens[nextIdx]
				// Pattern 1: NIL : Method (colon as punctuation)
				isMemberAccess := nextTok.Type == lexer.TokenPunctuation && nextTok.Text == ":"
				// Pattern 2: NIL:Method (colon consumed into keyword token like :ToString)
				if !isMemberAccess && nextTok.Type == lexer.TokenKeyword && strings.HasPrefix(nextTok.Text, ":") {
					normalized := strings.ToUpper(strings.TrimPrefix(nextTok.Text, ":"))
					if !constants.IsKeyword(normalized) {
						isMemberAccess = true
					}
				}
				if isMemberAccess {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityWarning,
						Range:    tokenToRange(token),
						Message:  "Calling a method on NIL raises an error. Check for NIL before accessing members.",
						Source:   "ssl-lsp",
						Code:     CodeNilMethodCall,
					})
				}
			}
		}

		// Check for method calls on variables known to be NIL. A
		// `:`-qualified occurrence is a member in a chain
		// (`Me:oClient:Send(...)`), not the tracked local.
		upper := strings.ToUpper(token.Text)
		if token.Type == lexer.TokenIdentifier && nilVars[upper] && !isQualified(i) &&
			!condGuards[i][upper] && !flow.guarded(upper, i) {
			nextIdx := nextSignificantTokenIndex(tokens, i+1)
			if nextIdx >= 0 && tokens[nextIdx].Type == lexer.TokenPunctuation && tokens[nextIdx].Text == ":" {
				// Check it's a member access, not assignment
				memberIdx := nextSignificantTokenIndex(tokens, nextIdx+1)
				if memberIdx >= 0 && tokens[memberIdx].Type == lexer.TokenIdentifier {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityWarning,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Variable '%s' may be NIL at this point. Calling methods on NIL raises an error.", token.Text),
						Source:   "ssl-lsp",
						Code:     CodeNilMethodCall,
					})
				}
			}
		}
	}

	return diagnostics
}

// nilFrame is one open block for checkNilMethodCalls' flow guards.
type nilFrame struct {
	// opaque frames (:BEGINCASE, :TRY) neither guard nor restore: a CASE
	// branch can fall through to the next, and a :CATCH runs after part
	// of the :TRY body, so their sections are not isolated paths.
	opaque bool
	// guards are the variables the condition proved non-empty for the
	// current branch, active after token index guardsFrom (the
	// condition's `;`). Cleared at :ELSE and when the variable is
	// reassigned NIL inside the branch.
	guards     map[string]bool
	guardsFrom int
	// returned is set by a :RETURN directly in the current branch;
	// jumped by :EXITWHILE/:EXITFOR/:LOOP anywhere inside, which can
	// leave the branch before that :RETURN.
	returned, jumped bool
	// saved holds, for each variable assigned NIL in the current branch,
	// its state before the branch's first such assignment.
	saved map[string]bool
	// mayNil collects the NIL assignments that reach the end of an
	// earlier branch of this :IF, re-applied at :ENDIF.
	mayNil map[string]bool
}

type nilFlowState struct {
	frames []*nilFrame
}

func newNilFlow() *nilFlowState { return &nilFlowState{} }

func (f *nilFlowState) open(guards map[string]bool, guardsFrom int) {
	f.frames = append(f.frames, &nilFrame{
		guards:     guards,
		guardsFrom: guardsFrom,
		saved:      map[string]bool{},
		mayNil:     map[string]bool{},
	})
}

func (f *nilFlowState) openOpaque() {
	f.frames = append(f.frames, &nilFrame{opaque: true, saved: map[string]bool{}, mayNil: map[string]bool{}})
}

func (f *nilFlowState) top() *nilFrame {
	if len(f.frames) == 0 {
		return nil
	}
	return f.frames[len(f.frames)-1]
}

// recordNil notes a NIL assignment: the innermost non-opaque frame
// remembers the variable's prior state, and every open frame stops
// guarding it.
func (f *nilFlowState) recordNil(name string, prior bool) {
	for _, fr := range f.frames {
		delete(fr.guards, name)
	}
	if fr := f.top(); fr != nil {
		if _, seen := fr.saved[name]; !seen {
			fr.saved[name] = prior
		}
	}
}

func (f *nilFlowState) markReturned() {
	if fr := f.top(); fr != nil {
		fr.returned = true
	}
}

func (f *nilFlowState) markJumped() {
	for _, fr := range f.frames {
		fr.jumped = true
	}
}

// endBranch settles the current branch of the top frame: NIL assignments
// that reach its end are kept in mayNil, and every NIL assignment made in
// the branch is undone so the next branch (or the code after a branch
// that returned) starts from the state before it.
func (f *nilFlowState) endBranch(fr *nilFrame, nilVars map[string]bool) {
	reaches := !fr.returned || fr.jumped
	for name, prior := range fr.saved {
		if reaches && nilVars[name] {
			fr.mayNil[name] = true
		}
		if prior {
			nilVars[name] = true
		} else {
			delete(nilVars, name)
		}
	}
	fr.saved = map[string]bool{}
	fr.returned = false
}

// branch handles :ELSE: the THEN branch's NIL assignments cannot reach
// the ELSE branch, and the ELSE branch is not guarded by the condition.
func (f *nilFlowState) branch(nilVars map[string]bool) {
	fr := f.top()
	if fr == nil || fr.opaque {
		return
	}
	f.endBranch(fr, nilVars)
	fr.guards = nil
}

// close handles the end of an :IF or loop: assignments that reached the
// end of any branch apply after the block, and the frame's remaining
// prior states pass to the enclosing frame.
func (f *nilFlowState) close(nilVars map[string]bool) {
	fr := f.top()
	if fr == nil || fr.opaque {
		return
	}
	f.endBranch(fr, nilVars)
	f.frames = f.frames[:len(f.frames)-1]
	for name := range fr.mayNil {
		prior := nilVars[name]
		f.recordNil(name, prior)
		nilVars[name] = true
	}
}

func (f *nilFlowState) closeOpaque() {
	fr := f.top()
	if fr == nil || !fr.opaque {
		return
	}
	f.frames = f.frames[:len(f.frames)-1]
	// Assignments inside pass through unchanged; the enclosing frame
	// still owns their prior states.
	if parent := f.top(); parent != nil {
		for name, prior := range fr.saved {
			if _, seen := parent.saved[name]; !seen {
				parent.saved[name] = prior
			}
		}
	}
}

func (f *nilFlowState) guarded(name string, idx int) bool {
	for _, fr := range f.frames {
		if idx > fr.guardsFrom && fr.guards[name] {
			return true
		}
	}
	return false
}

// nilConditionGuards reads an :IF/:WHILE condition starting at token
// index start, through its terminating `;`. It returns the `;` index,
// the variables the whole condition proves non-empty (a top-level
// `.AND.` chain, no top-level `.OR.`, with an operand `!Empty(v)`,
// `.NOT. Empty(v)`, or `v != NIL`), and, per later token index, the
// variables proven by an earlier operand of the same `.AND.` chain —
// `.AND.` short-circuits, so `!Empty(v) .AND. v:Prop` never reaches
// `v:Prop` when v is empty (issue #304).
func nilConditionGuards(tokens []lexer.Token, start int) (int, map[string]bool, map[int]map[string]bool) {
	inCond := map[int]map[string]bool{}
	all := map[string]bool{}
	chain := map[string]bool{}
	hasOr := false
	depth := 0
	end := len(tokens)
	operandStart := true
	for j := start; j < len(tokens); j++ {
		t := tokens[j]
		if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
			continue
		}
		if t.Type == lexer.TokenPunctuation {
			switch t.Text {
			case "(", "[", "{":
				depth++
			case ")", "]", "}":
				depth--
			case ";":
				if depth <= 0 {
					end = j
				}
			}
			if end == j {
				break
			}
		}
		if depth == 0 && t.Type == lexer.TokenOperator {
			switch strings.ToUpper(t.Text) {
			case ".OR.":
				hasOr = true
				chain = map[string]bool{}
				operandStart = true
				continue
			case ".AND.":
				operandStart = true
				continue
			}
		}
		atOperandStart := operandStart
		operandStart = false
		if depth == 0 && atOperandStart && t.Type != lexer.TokenPunctuation {
			if name, next := nonEmptyOperand(tokens, j); name != "" {
				chain[name] = true
				all[name] = true
				j = next - 1
				continue
			}
		}
		if t.Type == lexer.TokenIdentifier && len(chain) > 0 {
			vars := make(map[string]bool, len(chain))
			for name := range chain {
				vars[name] = true
			}
			inCond[j] = vars
		}
	}
	if hasOr {
		all = nil
	}
	return end, all, inCond
}

// nonEmptyOperand matches a non-emptiness test starting at token index
// idx — `!Empty(v)`, `.NOT. Empty(v)`, `v != NIL` (or `<>`, `#`, and
// with NIL first) — and returns the upper-cased variable and the index
// just past the operand, or "" when the tokens are anything else.
func nonEmptyOperand(tokens []lexer.Token, idx int) (string, int) {
	sig := func(from int) int { return nextSignificantTokenIndex(tokens, from) }
	isNil := func(k int) bool {
		return k >= 0 && strings.EqualFold(tokens[k].Text, "NIL") &&
			(tokens[k].Type == lexer.TokenIdentifier || tokens[k].Type == lexer.TokenKeyword)
	}
	isNotEqual := func(k int) bool {
		if k < 0 || tokens[k].Type != lexer.TokenOperator {
			return false
		}
		switch tokens[k].Text {
		case "!=", "<>", "#":
			return true
		}
		return false
	}
	bareIdent := func(k int) bool {
		return k >= 0 && tokens[k].Type == lexer.TokenIdentifier && !strings.EqualFold(tokens[k].Text, "NIL")
	}
	// operandEnds requires the operand to finish at a top-level `.AND.`,
	// `.OR.`, `)` or the statement's `;`, so `v != NIL + x` is not read
	// as a test.
	operandEnds := func(k int) bool {
		if k < 0 {
			return true
		}
		t := tokens[k]
		if t.Type == lexer.TokenOperator {
			u := strings.ToUpper(t.Text)
			return u == ".AND." || u == ".OR."
		}
		return t.Type == lexer.TokenPunctuation && (t.Text == ";" || t.Text == ")")
	}

	t := tokens[idx]
	if t.Type == lexer.TokenOperator && (t.Text == "!" || strings.EqualFold(t.Text, ".NOT.")) {
		fn := sig(idx + 1)
		open := sig(fn + 1)
		if fn < 0 || open < 0 || !strings.EqualFold(tokens[fn].Text, "Empty") ||
			tokens[open].Type != lexer.TokenPunctuation || tokens[open].Text != "(" {
			return "", idx + 1
		}
		v := sig(open + 1)
		closeIdx := sig(v + 1)
		if !bareIdent(v) || closeIdx < 0 || tokens[closeIdx].Type != lexer.TokenPunctuation || tokens[closeIdx].Text != ")" {
			return "", idx + 1
		}
		if !operandEnds(sig(closeIdx + 1)) {
			return "", idx + 1
		}
		return strings.ToUpper(tokens[v].Text), closeIdx + 1
	}
	if bareIdent(idx) {
		op := sig(idx + 1)
		n := sig(op + 1)
		if isNotEqual(op) && isNil(n) && operandEnds(sig(n+1)) {
			return strings.ToUpper(t.Text), n + 1
		}
		return "", idx + 1
	}
	if isNil(idx) {
		op := sig(idx + 1)
		v := sig(op + 1)
		if isNotEqual(op) && bareIdent(v) && operandEnds(sig(v+1)) {
			return strings.ToUpper(tokens[v].Text), v + 1
		}
	}
	return "", idx + 1
}

func nextSignificantTokenIndex(tokens []lexer.Token, start int) int {
	for i := start; i < len(tokens); i++ {
		if tokens[i].Type == lexer.TokenWhitespace || tokens[i].Type == lexer.TokenComment {
			continue
		}
		return i
	}
	return -1
}

func parseTopLevelCallArguments(tokens []lexer.Token, openParenIdx int) ([]int, []int, int) {
	var argStarts []int
	var argEnds []int

	parenDepth := 0
	bracketDepth := 0
	braceDepth := 0
	argStart := -1

	for i := openParenIdx; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type == lexer.TokenComment || token.Type == lexer.TokenWhitespace {
			continue
		}

		if token.Type == lexer.TokenPunctuation {
			switch token.Text {
			case "(":
				parenDepth++
				if parenDepth == 1 {
					continue
				}
			case ")":
				if parenDepth == 1 {
					if argStart >= 0 {
						argStarts = append(argStarts, argStart)
						argEnds = append(argEnds, previousSignificantTokenIndex(tokens, i-1))
					}
					return argStarts, argEnds, i
				}
				parenDepth--
			case "[":
				bracketDepth++
			case "]":
				bracketDepth--
			case "{":
				braceDepth++
			case "}":
				braceDepth--
			case ",":
				if parenDepth == 1 && bracketDepth == 0 && braceDepth == 0 {
					if argStart >= 0 {
						argStarts = append(argStarts, argStart)
						argEnds = append(argEnds, previousSignificantTokenIndex(tokens, i-1))
						argStart = -1
					} else {
						argStarts = append(argStarts, -1)
						argEnds = append(argEnds, -1)
					}
					continue
				}
			}
		}

		if parenDepth == 1 && argStart < 0 {
			argStart = i
		}
	}

	return nil, nil, -1
}

func previousSignificantTokenIndex(tokens []lexer.Token, start int) int {
	for i := start; i >= 0; i-- {
		if tokens[i].Type == lexer.TokenWhitespace || tokens[i].Type == lexer.TokenComment {
			continue
		}
		return i
	}
	return -1
}

func isEmptyArrayLiteral(tokens []lexer.Token, startIdx, endIdx int) bool {
	if startIdx < 0 || endIdx < startIdx {
		return false
	}

	significant := make([]lexer.Token, 0, endIdx-startIdx+1)
	for i := startIdx; i <= endIdx; i++ {
		if tokens[i].Type == lexer.TokenWhitespace || tokens[i].Type == lexer.TokenComment {
			continue
		}
		significant = append(significant, tokens[i])
	}

	return len(significant) == 2 &&
		significant[0].Type == lexer.TokenPunctuation && significant[0].Text == "{" &&
		significant[1].Type == lexer.TokenPunctuation && significant[1].Text == "}"
}

// checkGlobalAssignment checks for assignment to global variables.
// Global variables are pre-declared and should not be assigned to.
// Always checks SSLWriteCheckedGlobals (MYUSERNAME); also checks user-configured globals.
// An in-file declaration (:DECLARE/:PARAMETERS/:PUBLIC) suppresses the check
// for that name (issue #169): a declared local that happens to collide with a
// status keyword (loop variable iS vs IS) is the author's own variable, and a
// :PUBLIC declaration marks this file as the initializer that creates the
// global — "globals are read-only" holds for consumers, not the declarer.
func checkGlobalAssignment(tokens []lexer.Token, declared []parser.VariableInfo, globals []string) []Diagnostic {
	var diagnostics []Diagnostic

	declaredSet := make(map[string]bool)
	for _, v := range declared {
		declaredSet[strings.ToUpper(v.Name)] = true
	}

	// Build a case-insensitive set of global variable names.
	// Always include built-in predefined globals and status keywords.
	globalSet := make(map[string]bool)
	for _, g := range constants.SSLWriteCheckedGlobals {
		globalSet[strings.ToUpper(g)] = true
	}
	for _, g := range constants.SSLStatusKeywords {
		globalSet[strings.ToUpper(g)] = true
	}
	for _, g := range globals {
		globalSet[strings.ToUpper(g)] = true
	}

	// Look for assignment patterns: identifier := value
	for i := 0; i < len(tokens); i++ {
		token := tokens[i]

		// Skip non-identifiers
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		// Check if this identifier is a global
		if !globalSet[strings.ToUpper(token.Text)] {
			continue
		}

		// A qualified member belongs to its object, even when its name
		// matches a predefined or configured global (oSummary:pending).
		if prev := previousSignificantTokenIndex(tokens, i-1); prev >= 0 &&
			tokens[prev].Type == lexer.TokenPunctuation && tokens[prev].Text == ":" {
			continue
		}

		// Declared in this file — the author's own variable, or the
		// initializer script that creates the global (issue #169).
		if declaredSet[strings.ToUpper(token.Text)] {
			continue
		}

		// Look ahead for := assignment operator
		j := i + 1
		for j < len(tokens) && tokens[j].Type == lexer.TokenWhitespace {
			j++
		}

		if j < len(tokens) && tokens[j].Type == lexer.TokenOperator && tokens[j].Text == ":=" {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("Assignment to global variable '%s' changes it for every routine that reads it", token.Text),
				Source:   "ssl-lsp",
				Code:     CodeGlobalAssignment,
			})
		}
	}

	return diagnostics
}

// checkUndeclaredVariables checks for usage of undeclared variables.
// This implements the logic specified in DIAGNOSTICS_SPECIFICATION.md Section 5.
// It addresses GitHub issues:
//   - Issue #55: Globals config should recognize variables as pre-declared
//   - Issue #56: :INCLUDE paths should be skipped from checking
//   - Issue #2: 'Me' should be recognized as a built-in identifier
//   - Issue #53: Function calls (identifier followed by '(') should be skipped
func checkUndeclaredVariables(tokens []lexer.Token, p *parser.Parser, globals []string, includeDeclared []string, isEndpoint bool) []Diagnostic {
	pathTokens := unquotedPathTokens(tokens)
	var diagnostics []Diagnostic

	// Build set of declared variables from the token stream
	declaredVars := make(map[string]bool)
	variables := p.ExtractVariables()
	for _, v := range variables {
		declaredVars[strings.ToUpper(v.Name)] = true
	}

	// Add configured globals to declared variables (Issue #55)
	for _, g := range globals {
		declaredVars[strings.ToUpper(g)] = true
	}

	// Names declared by resolved :INCLUDE targets count as declared —
	// :INCLUDE splices the included script's full text
	// (spec feature.cross_file_resolution/A18-A19).
	for _, g := range includeDeclared {
		declaredVars[strings.ToUpper(g)] = true
	}

	// Endpoint ambients: in endpoint scripts, Request and Response are
	// pre-injected runtime objects in scope. Treat them as declared so
	// they don't fire `undeclared_variable`. In non-endpoint files we
	// leave them out — using them there is a real bug.
	if isEndpoint {
		declaredVars["REQUEST"] = true
		declaredVars["RESPONSE"] = true
	}

	// Build set of built-in identifiers to skip
	builtins := buildBuiltinSet()

	// Token positions that bind a name rather than use one.
	declarationSpans := parser.DeclarationSpans(tokens)

	// Track which undeclared variables we've already reported (once per scope)
	reported := make(map[string]bool)

	// Track if we're inside an :INCLUDE / :INHERIT / :CLASS statement whose
	// identifiers are declarations or module references, not variable uses
	// (issues #56, #149, #155).
	inInclude := false

	// Process tokens
	for i := 0; i < len(tokens); i++ {
		token := tokens[i]

		// Skip whitespace and comments
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}

		// Detect :INCLUDE (issue #56) / :INHERIT (issue #149) / :CLASS
		// (issue #155) / :LABEL, :REGION, :BEGININLINECODE (issue #272)
		// keywords and skip until semicolon — the identifiers that follow
		// are module references (:INCLUDE, :INHERIT), the class-name
		// declaration (:CLASS), or a label or block name, not variable uses.
		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
			switch normalized {
			case "INCLUDE", "INHERIT", "CLASS", "LABEL", "REGION", "BEGININLINECODE":
				inInclude = true
			}
			// Other keywords are not variables
			continue
		}

		// End of :INCLUDE statement
		if inInclude {
			if token.Type == lexer.TokenPunctuation && token.Text == ";" {
				inInclude = false
			}
			// Skip all tokens in :INCLUDE path
			continue
		}

		// Only check identifiers
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		upperName := strings.ToUpper(token.Text)

		// Skip built-in identifiers (functions, classes, literals, operators)
		if builtins[upperName] {
			continue
		}

		// One typo is one finding (issue #255): a fragment another rule
		// already reports is not an undeclared variable too (issue #328) —
		// a bare AND/OR/NOT (bare_logical_operator), the exponent glued to
		// a number in `7e2` (scientific_notation), a segment of an unquoted
		// ExecFunction path (execfunction_missing_quotes).
		if upperName == "AND" || upperName == "OR" || upperName == "NOT" {
			continue
		}
		if i > 0 && tokens[i-1].Type == lexer.TokenNumber && (token.Text[0] == 'e' || token.Text[0] == 'E') {
			continue
		}
		if pathTokens[i] {
			continue
		}

		// Skip 'Me' - class self-reference (Issue #2)
		if upperName == "ME" {
			continue
		}

		// Check if preceded by ':' (property access, e.g., object:property)
		if i > 0 {
			prevIdx := i - 1
			for prevIdx > 0 && tokens[prevIdx].Type == lexer.TokenWhitespace {
				prevIdx--
			}
			if prevIdx >= 0 && tokens[prevIdx].Type == lexer.TokenPunctuation && tokens[prevIdx].Text == ":" {
				continue
			}
		}

		// Check if followed by '(' (function call) (Issue #53)
		nextIdx := i + 1
		for nextIdx < len(tokens) && tokens[nextIdx].Type == lexer.TokenWhitespace {
			nextIdx++
		}
		if nextIdx < len(tokens) && tokens[nextIdx].Type == lexer.TokenPunctuation && tokens[nextIdx].Text == "(" {
			continue
		}

		// A name followed by `{` on its line is class instantiation
		// (`FormDto{}`), not a variable read — the class may be one the
		// published inventory does not list (issue #304).
		if nextIdx < len(tokens) && tokens[nextIdx].Line == token.Line &&
			tokens[nextIdx].Type == lexer.TokenPunctuation && tokens[nextIdx].Text == "{" {
			continue
		}

		// Check if on left side of ':=' (assignment target - this declares the variable)
		if nextIdx < len(tokens) && tokens[nextIdx].Type == lexer.TokenOperator && tokens[nextIdx].Text == ":=" {
			// This is a dynamic declaration, add to declared set
			declaredVars[upperName] = true
			continue
		}

		// Skip positions inside a statement that binds names rather than
		// using them — a declaration, or a :PROCEDURE header
		// (issue #184). Read from the statement, not the line: a
		// declaration written as a bare `:DECLARE` with its names on
		// later lines used to flag every one of its own names.
		if declarationSpans[i] {
			continue
		}

		// Check if declared
		if declaredVars[upperName] {
			continue
		}

		// Report undeclared variable (once per name)
		if !reported[upperName] {
			reported[upperName] = true
			message := fmt.Sprintf("Variable '%s' is not declared", token.Text)
			if use, ok := constants.SSLNonBaseGlobals[upperName]; ok {
				message += fmt.Sprintf("; a base installation does not declare it as a global, use %s instead", use)
			}
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(token),
				Message:  message,
				Source:   "ssl-lsp",
				Code:     CodeUndeclaredVariable,
			})
		}
	}

	return diagnostics
}

// buildBuiltinSet creates a case-insensitive set of all built-in identifiers.
func buildBuiltinSet() map[string]bool {
	builtins := make(map[string]bool)

	// Add all SSL function names
	for _, fn := range constants.SSLFunctionNames {
		builtins[strings.ToUpper(fn)] = true
	}

	// Add all SSL class names
	for _, cls := range constants.SSLClassNames {
		builtins[strings.ToUpper(cls)] = true
	}

	// Add SSL literals
	for _, lit := range constants.SSLLiterals {
		builtins[strings.ToUpper(lit)] = true
	}

	// Add SSL operators (the text form)
	for _, op := range constants.SSLLogicalOperators {
		builtins[strings.ToUpper(op)] = true
	}

	// Add base-installation predefined globals (always recognized, never flagged as undeclared)
	for _, g := range constants.SSLPredefinedGlobals {
		builtins[strings.ToUpper(g)] = true
	}
	for _, g := range constants.SSLStatusKeywords {
		builtins[strings.ToUpper(g)] = true
	}

	// Add special identifiers
	builtins["ME"] = true          // Class self-reference
	builtins["BASE"] = true        // Parent-class reference
	builtins["CONSTRUCTOR"] = true // Reserved constructor identifier
	builtins["NIL"] = true         // Null value

	return builtins
}

// checkUnusedVariables checks for declared variables that are never used.
func checkUnusedVariables(tokens []lexer.Token, ast *parser.Node, p *parser.Parser) []Diagnostic {
	var diagnostics []Diagnostic

	// Extract all declared variables
	variables := p.ExtractVariables()
	if len(variables) == 0 {
		return diagnostics
	}

	// Extract procedures for scope awareness
	procedures := p.ExtractProcedures(ast)
	isClass := isFirstSignificantStatementKeyword(tokens, "CLASS")

	// Count usages for each declared variable
	for _, v := range variables {
		// A class-level :DECLARE is a field — part of the class's public
		// interface, read by callers in other files this check cannot see
		// (issue #328).
		if isClass && !insideProcedure(v.Line, procedures) {
			continue
		}
		usageCount := countVariableUsages(tokens, v, procedures)

		if usageCount == 0 {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityHint,
				Range: Range{
					Start: Position{Line: v.Line - 1, Character: v.Column - 1},
					End:   Position{Line: v.Line - 1, Character: v.Column - 1 + len(v.Name)},
				},
				Message: fmt.Sprintf("Variable '%s' is declared but never used", v.Name),
				Source:  "ssl-lsp",
				Code:    CodeUnusedVariable,
			})
		}
	}

	return diagnostics
}

// countVariableUsages counts how many times a variable is used in the code.
// For local/parameter variables, only counts usages within the same procedure.
// Returns the number of usages (excluding the declaration itself).
func countVariableUsages(tokens []lexer.Token, v parser.VariableInfo, procedures []parser.ProcedureInfo) int {
	usageCount := 0
	varNameUpper := strings.ToUpper(v.Name)

	// Determine scope for local/parameter variables
	var scopeProc *parser.ProcedureInfo
	if v.Scope == parser.ScopeLocal || v.Scope == parser.ScopeParameter {
		// Find the procedure that contains this variable
		for i := range procedures {
			if v.Line >= procedures[i].StartLine && v.Line <= procedures[i].EndLine {
				scopeProc = &procedures[i]
				break
			}
		}
	}

	for _, token := range tokens {
		// A ?name? marker in a SQL string reads the variable at run time
		// (SQLExecute substitution), so it counts as a use (issue #270).
		// Same string scan as invalid_sql_param, so the two rules agree on
		// which markers name a variable.
		if token.Type == lexer.TokenString {
			if scopeProc != nil && (token.Line < scopeProc.StartLine || token.Line > scopeProc.EndLine) {
				continue
			}
			usageCount += countSQLMarkerUsages(token.Text, varNameUpper)
			continue
		}

		// Only check identifiers
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		// Check if name matches (case-insensitive)
		if strings.ToUpper(token.Text) != varNameUpper {
			continue
		}

		// Skip if this is the declaration line and column
		if token.Line == v.Line && token.Column == v.Column {
			continue
		}

		// For scoped variables, only count usages within the procedure
		if scopeProc != nil {
			if token.Line < scopeProc.StartLine || token.Line > scopeProc.EndLine {
				continue
			}
		}

		// Check if this is a property access (preceded by ':')
		// We should count these as usages even though they're properties
		// Actually, if preceded by ':' it's accessing the property on an object,
		// not our variable, so we should skip these
		// But we need to find the preceding token...
		// For simplicity, we'll count all identifier matches as usages

		usageCount++
	}

	return usageCount
}

// countSQLMarkerUsages counts the named ?name? placeholders in a string
// literal whose base variable is varNameUpper.
func countSQLMarkerUsages(text, varNameUpper string) int {
	if len(text) < 2 || !strings.Contains(text, "?") {
		return 0
	}
	n := 0
	for _, ph := range ParseSQLPlaceholders(text[1 : len(text)-1]) {
		if !ph.IsNamed {
			continue
		}
		if !isSimpleNamedPlaceholder(ph.Name) || strings.Contains(ph.Name, "(") {
			// A complex placeholder or call (`?sPrefix + sSuffix?`,
			// `?Len(aIds)?`) is an SSL expression evaluated at run time:
			// every variable it names is read (issue #328).
			n += countExpressionReads(ph.Name, varNameUpper)
			continue
		}
		if strings.ToUpper(extractBaseVarName(ph.Name)) == varNameUpper {
			n++
		}
	}
	return n
}

// countExpressionReads counts the reads of a variable in an SSL expression:
// identifiers that are not a member name (after ':') or a called function
// (before '(').
func countExpressionReads(expr, varNameUpper string) int {
	toks := lexer.NewLexer(expr).Tokenize()
	var sig []lexer.Token
	for _, t := range toks {
		if t.Type != lexer.TokenWhitespace && t.Type != lexer.TokenEOF {
			sig = append(sig, t)
		}
	}
	n := 0
	for i, t := range sig {
		if t.Type != lexer.TokenIdentifier || strings.ToUpper(t.Text) != varNameUpper {
			continue
		}
		if i > 0 && sig[i-1].Text == ":" {
			continue
		}
		if i+1 < len(sig) && sig[i+1].Text == "(" {
			continue
		}
		n++
	}
	return n
}

// insideProcedure reports whether a 1-based line falls in a procedure.
func insideProcedure(line int, procedures []parser.ProcedureInfo) bool {
	for _, pr := range procedures {
		if line >= pr.StartLine && line <= pr.EndLine {
			return true
		}
	}
	return false
}

// checkSQLParameterValidation checks that SQL parameters (?param?) match declared variables.
// This validation ensures that named parameters in SQL strings reference variables
// that are actually declared in the current scope (case-insensitive).
func checkSQLParameterValidation(tokens []lexer.Token, ast *parser.Node, p *parser.Parser, globals []string, includeDeclared []string) []Diagnostic {
	var diagnostics []Diagnostic

	// Build set of all declared variables (case-insensitive)
	declaredVars := make(map[string]bool)
	variables := p.ExtractVariables()
	for _, v := range variables {
		declaredVars[strings.ToUpper(v.Name)] = true
	}

	// Names declared by resolved :INCLUDE targets count as declared —
	// :INCLUDE splices the included script's full text
	// (spec feature.cross_file_resolution/A18-A19).
	for _, g := range includeDeclared {
		declaredVars[strings.ToUpper(g)] = true
	}

	// Add built-in predefined globals (MYUSERNAME, etc.) and status keywords
	for _, g := range constants.SSLPredefinedGlobals {
		declaredVars[strings.ToUpper(g)] = true
	}
	for _, g := range constants.SSLStatusKeywords {
		declaredVars[strings.ToUpper(g)] = true
	}

	// Add configured globals
	for _, g := range globals {
		declaredVars[strings.ToUpper(g)] = true
	}

	// A variable created by assignment (`sName := ...;` with no
	// :DECLARE) exists at run time, so ?sName? substitutes it. This is the
	// same rule undeclared_variable applies: the assignment target counts
	// as declared. Without it the two --strict rules disagreed about one
	// name, and every ?name? marker for an assigned variable was reported.
	for k, tok := range tokens {
		if tok.Type != lexer.TokenIdentifier {
			continue
		}
		next := nextSignificantTokenIndex(tokens, k+1)
		if next < 0 || tokens[next].Type != lexer.TokenOperator || tokens[next].Text != ":=" {
			continue
		}
		if prev := prevSignificantTokenIndex(tokens, k-1); prev >= 0 &&
			tokens[prev].Type == lexer.TokenPunctuation && tokens[prev].Text == ":" {
			continue // a member assignment, oObj:Name := ...
		}
		declaredVars[strings.ToUpper(tok.Text)] = true
	}

	// Add procedure parameters to declared vars
	procedures := p.ExtractProcedures(ast)
	for _, proc := range procedures {
		for _, param := range proc.Parameters {
			declaredVars[strings.ToUpper(param)] = true
		}
	}

	// Track reported parameters to avoid duplicate warnings
	reported := make(map[string]map[int]bool) // paramName -> line -> reported

	// Scan all string tokens for SQL parameters
	for _, token := range tokens {
		if token.Type != lexer.TokenString {
			continue
		}

		// Extract string content (remove quotes)
		content := token.Text
		if len(content) < 2 {
			continue
		}
		content = content[1 : len(content)-1]

		// Parse SQL placeholders from the string
		placeholders := ParseSQLPlaceholders(content)

		for _, ph := range placeholders {
			// Only validate named parameters (skip complex expressions with operators)
			if !ph.IsNamed || !isSimpleNamedPlaceholder(ph.Name) {
				continue
			}

			// Extract base variable name from property/array/function access
			// e.g., oUser:ID -> oUser, aArr[1] -> aArr, Today() -> Today
			baseName := extractBaseVarName(ph.Name)
			paramUpper := strings.ToUpper(baseName)

			// Skip function calls (Today(), etc.) — they're not variables
			if strings.Contains(ph.Name, "(") {
				continue
			}

			// Initialize reported map for this parameter if needed
			if reported[paramUpper] == nil {
				reported[paramUpper] = make(map[int]bool)
			}

			// Skip if already reported on this line
			if reported[paramUpper][token.Line] {
				continue
			}

			// Check if the parameter matches a declared variable
			if !declaredVars[paramUpper] {
				reported[paramUpper][token.Line] = true

				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range:    stringContentRange(token, ph.Start, len(ph.Name)+2), // +2 for surrounding ?
					Message:  fmt.Sprintf("SQL parameter '%s' does not match any declared variable", ph.Name),
					Source:   "ssl-lsp",
					Code:     CodeInvalidSqlParam,
				})
			}
		}
	}

	return diagnostics
}

// extractBaseVarName extracts the root variable name from a SQL placeholder.
// For example: "oUser:ID" -> "oUser", "aArr[1]" -> "aArr", "Today()" -> "Today".
func extractBaseVarName(name string) string {
	for i, ch := range name {
		if ch == ':' || ch == '[' || ch == '(' {
			return name[:i]
		}
	}
	return name
}

// checkRedeclaredVariables warns when the same variable is declared more than once
// in the same scope. Per the schema, re-declaring is silently ignored by the runtime
// but is almost always a mistake.
func checkRedeclaredVariables(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Track declared variables per scope (procedure or global)
	type scopeInfo struct {
		declared map[string]lexer.Token // variable name -> first declaration token
	}

	currentScope := &scopeInfo{declared: make(map[string]lexer.Token)}
	scopeStack := []*scopeInfo{currentScope}

	for i, token := range tokens {
		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
			if normalized == "PROCEDURE" {
				// New scope
				currentScope = &scopeInfo{declared: make(map[string]lexer.Token)}
				scopeStack = append(scopeStack, currentScope)
			} else if normalized == "ENDPROC" {
				// Pop scope
				if len(scopeStack) > 1 {
					scopeStack = scopeStack[:len(scopeStack)-1]
					currentScope = scopeStack[len(scopeStack)-1]
				}
			} else if normalized == "DECLARE" || normalized == "PARAMETERS" {
				// Collect the identifiers on this line until semicolon
				for j := i + 1; j < len(tokens); j++ {
					t := tokens[j]
					if t.Type == lexer.TokenPunctuation && t.Text == ";" {
						break
					}
					if t.Type == lexer.TokenIdentifier {
						upper := strings.ToUpper(t.Text)
						if firstDecl, exists := currentScope.declared[upper]; exists {
							diagnostics = append(diagnostics, Diagnostic{
								Severity: SeverityHint,
								Range:    tokenToRange(t),
								Message:  fmt.Sprintf("Variable '%s' is already declared (first declared at line %d). Re-declaration is silently ignored at runtime.", t.Text, firstDecl.Line),
								Source:   "ssl-lsp",
								Code:     CodeRedeclareIsNoop,
							})
						} else {
							currentScope.declared[upper] = t
						}
					}
				}
			}
		}
	}

	return diagnostics
}

// checkNestedIIF detects nested IIF() calls which reduce readability.
// Schema: no_nested_ternaries: true
func checkNestedIIF(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic
	// An inner IIF is found once per enclosing IIF scan; report it once.
	reported := make(map[int]bool)

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier {
			continue
		}
		if !strings.EqualFold(token.Text, "IIF") {
			continue
		}
		// Check this is a function call (followed by `(`)
		nextIdx := nextSignificantTokenIndex(tokens, i+1)
		if nextIdx < 0 || tokens[nextIdx].Text != "(" {
			continue
		}

		// Scan inside the IIF(...) for nested IIF calls
		parenDepth := 0
		for j := nextIdx; j < len(tokens); j++ {
			if tokens[j].Text == "(" {
				parenDepth++
			} else if tokens[j].Text == ")" {
				parenDepth--
				if parenDepth == 0 {
					break
				}
			}
			if parenDepth > 0 && tokens[j].Type == lexer.TokenIdentifier && strings.EqualFold(tokens[j].Text, "IIF") {
				// Check it's a call
				nIdx := nextSignificantTokenIndex(tokens, j+1)
				if nIdx >= 0 && tokens[nIdx].Text == "(" && !reported[j] {
					reported[j] = true
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityInfo,
						Range:    tokenToRange(tokens[j]),
						Message:  "Nested IIF() reduces readability. Consider using :BEGINCASE/:CASE or :IF/:ELSE instead.",
						Source:   "ssl-lsp",
						Code:     CodeNestedIif,
					})
				}
			}
		}
	}

	return diagnostics
}

// checkNegativeLogic flags :IF blocks with negated conditions that have :ELSE blocks,
// suggesting the logic be inverted for readability.
// Schema: prefer_positive_logic: true
func checkNegativeLogic(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var diagnostics []Diagnostic
	conditions := conditionExprsByKeyword(stmts)

	for i, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}
		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		if normalized != "IF" {
			continue
		}

		// Check if the condition starts with a negation
		nextIdx := nextSignificantTokenIndex(tokens, i+1)
		if nextIdx < 0 {
			continue
		}

		isNegated := false
		negToken := tokens[nextIdx]
		if negToken.Type == lexer.TokenOperator {
			upper := strings.ToUpper(negToken.Text)
			if upper == ".NOT." || upper == "!" {
				isNegated = true
			}
		}

		if !isNegated {
			continue
		}
		// The negation must govern the whole condition: in
		// `.NOT. bA .AND. bB` it binds only bA, and swapping the branches
		// cannot remove it.
		if !negationGovernsCondition(conditions[i]) {
			continue
		}

		// Look for matching :ELSE — scan forward tracking IF/ENDIF depth
		depth := 1
		hasElse := false
		for j := nextIdx + 1; j < len(tokens); j++ {
			if tokens[j].Type != lexer.TokenKeyword {
				continue
			}
			kw := strings.ToUpper(strings.TrimPrefix(tokens[j].Text, ":"))
			if kw == "IF" {
				depth++
			} else if kw == "ENDIF" {
				depth--
				if depth == 0 {
					break
				}
			} else if kw == "ELSE" && depth == 1 {
				hasElse = true
				break
			}
		}

		if hasElse {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range:    tokenToRange(negToken),
				Message:  "Consider inverting this condition to use positive logic: swap the :IF and :ELSE branches and remove the negation.",
				Source:   "ssl-lsp",
				Code:     CodeNegativeLogic,
			})
		}
	}

	return diagnostics
}

// checkStepSpacing warns when :STEP has no space before it in FOR loops.
// Source of truth: ssl_agent_instructions.md gotcha #16.
func checkStepSpacing(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}
		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		if normalized != "STEP" {
			continue
		}
		// Check the preceding token — it should be whitespace
		if i > 0 && tokens[i-1].Type != lexer.TokenWhitespace {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(token),
				Message:  "':STEP' should have a space before it: ':FOR i := 1 :TO 10 :STEP 2;'",
				Source:   "ssl-lsp",
				Code:     CodeStepSpacing,
			})
		}
	}

	return diagnostics
}

// checkRegionEndMismatch flags a /* endregion; marker with no open
// /* region; to close. The canonical closer takes no name (trailing text
// before the ';' is prose); pairing is innermost-first, mirroring
// extractRegions (symbols.go). An orphan endregion closes nothing, so
// without this signal the broken region structure is silent.
func checkRegionEndMismatch(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	regionStartPattern := regexp.MustCompile(`(?i)^/\*\s*region\b`)
	regionEndPattern := regexp.MustCompile(`(?i)^/\*\s*endregion\b`)

	openRegions := 0
	for _, token := range tokens {
		if token.Type != lexer.TokenComment {
			continue
		}
		text := strings.TrimSpace(strings.TrimSuffix(token.Text, ";"))

		if regionEndPattern.MatchString(text) {
			if openRegions == 0 {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityHint,
					Range:    tokenToRange(token),
					Message:  "'endregion' has no open '/* region' to close",
					Source:   "ssl-lsp",
					Code:     CodeRegionEndMismatch,
				})
				continue
			}
			openRegions--
			continue
		}

		if regionStartPattern.MatchString(text) {
			openRegions++
		}
	}

	return diagnostics
}

// checkCodeBlockStructure validates code block literals {|params| expr}.
// Source of truth: ssl_agent_instructions.md — code blocks require at least one bound variable.
func checkCodeBlockStructure(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenCodeBlock {
			continue
		}
		text := token.Text
		// Check for empty parameter list: {|| expr} or {| | expr} (with whitespace)
		// Source of truth: ssl-ebnf-grammar.md — at least one parameter required between pipes.
		if len(text) >= 3 && text[0] == '{' && text[1] == '|' {
			// Find closing pipe and check if anything non-whitespace exists between pipes
			hasParam := false
			for ci := 2; ci < len(text); ci++ {
				if text[ci] == '|' {
					break
				}
				if text[ci] != ' ' && text[ci] != '\t' {
					hasParam = true
					break
				}
			}
			if !hasParam {
				// A compile-time error (reference: special-forms/code-block),
				// so error severity (issue #255).
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "Code blocks require at least one bound variable between the pipes: {|x| expr} - '{|| ...}' does not compile",
					Source:   "ssl-lsp",
					Code:     CodeCodeBlockStructure,
				})
			}
		}
	}

	return diagnostics
}

// checkKeywordFormsDataSource is the data-source variant of checkKeywordForms.
// It accepts builder directives (:DSN, :TABLENAME, :NULLASBLANK, :INVARIANTDATECOLUMNS)
// as valid colon-prefixed forms instead of flagging them as unknown keywords.
func checkKeywordFormsDataSource(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}

		text := token.Text
		normalized := strings.ToUpper(strings.TrimPrefix(text, ":"))

		if strings.HasPrefix(text, ":") {
			if isLegacyLabelKeywordForm(text) {
				if !strings.HasPrefix(text, ":LABEL") {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  "SSL label keyword forms are case-sensitive: use ':LABEL Name;' or ':LABELName;'",
						Source:   "ssl-lsp",
						Code:     CodeLabelKeywordForm,
					})
				}
				continue
			}

			// Builder directives are valid in data source files
			if constants.IsBuilderDirective(normalized) {
				canonical := ":" + normalized
				if text != canonical {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Builder directives must be uppercase: use '%s'", canonical),
						Source:   "ssl-lsp",
						Code:     CodeBuilderDirectiveCase,
					})
				}
				continue
			}

			if !constants.IsKeyword(normalized) {
				if normalized == "ENDFOR" {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  "':ENDFOR' is not valid — FOR loops must be terminated with ':NEXT'",
						Source:   "ssl-lsp",
						Code:     CodeEndForInvalid,
					})
				} else if d, ok := knownInvalidKeyword(token, normalized, true); ok {
					diagnostics = append(diagnostics, d)
				} else {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityWarning,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Unknown SSL keyword: '%s'", text),
						Source:   "ssl-lsp",
						Code:     CodeUnknownKeyword,
					})
				}
				continue
			}

			canonical := ":" + normalized
			if text != canonical {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  fmt.Sprintf("SSL keywords are case-sensitive and must be uppercase: use '%s'", canonical),
					Source:   "ssl-lsp",
					Code:     CodeKeywordUppercase,
				})
			}
		}
	}

	return diagnostics
}

// parametersEntry is one name in a :PARAMETERS statement and whether it
// carries an inline := default.
type parametersEntry struct {
	name       lexer.Token
	assign     lexer.Token // the := token, when hasDefault
	hasDefault bool
}

// parametersStatements returns, for each :PARAMETERS statement, its entries
// and whether the statement sits inside a :PROCEDURE body. After a name, a
// := default is consumed through the next top-level , or ; so identifiers
// inside a default value (including in parentheses, braces or brackets)
// are never read as parameter names.
func parametersStatements(tokens []lexer.Token) (stmts [][]parametersEntry, inProc []bool) {
	procDepth := 0
	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenKeyword {
			continue
		}
		switch strings.ToUpper(strings.TrimPrefix(token.Text, ":")) {
		case "PROCEDURE":
			procDepth++
			continue
		case "ENDPROC":
			if procDepth > 0 {
				procDepth--
			}
			continue
		case "PARAMETERS":
		default:
			continue
		}

		var entries []parametersEntry
		expectName := true
		depth := 0
		j := i + 1
		for ; j < len(tokens); j++ {
			t := tokens[j]
			if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
				continue
			}
			if t.Type == lexer.TokenPunctuation {
				switch t.Text {
				case "(", "{", "[":
					depth++
					continue
				case ")", "}", "]":
					if depth > 0 {
						depth--
					}
					continue
				case ",":
					if depth == 0 {
						expectName = true
					}
					continue
				case ";":
					if depth == 0 {
						goto done
					}
					continue
				}
			}
			if expectName && depth == 0 && t.Type == lexer.TokenIdentifier {
				entry := parametersEntry{name: t}
				k := j + 1
				for k < len(tokens) && (tokens[k].Type == lexer.TokenWhitespace || tokens[k].Type == lexer.TokenComment) {
					k++
				}
				if k < len(tokens) && tokens[k].Type == lexer.TokenOperator && tokens[k].Text == ":=" {
					entry.hasDefault = true
					entry.assign = tokens[k]
					j = k
				}
				entries = append(entries, entry)
				expectName = false
			}
		}
	done:
		stmts = append(stmts, entries)
		inProc = append(inProc, procDepth > 0)
		i = j
	}
	return stmts, inProc
}

// checkDataSourceParameterDefaults reports each script-level data-source
// parameter without an inline := default (issues #263, #269). A data
// source, in SSL or SQL mode, with such a parameter fails when it runs:
// "No default value for one or more parameters.", whatever the caller
// passes. A :PARAMETERS inside a :PROCEDURE body is an ordinary procedure
// signature, not the data source's interface, and is not checked.
func checkDataSourceParameterDefaults(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic
	stmts, inProc := parametersStatements(tokens)
	for s, entries := range stmts {
		if inProc[s] {
			continue
		}
		for _, e := range entries {
			if e.hasDefault {
				continue
			}
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(e.name),
				Message:  fmt.Sprintf("Data source parameter '%s' needs an inline ':=' default; without one the data source fails when it runs (No default value for one or more parameters)", e.name.Text),
				Source:   "ssl-lsp",
				Code:     CodeDatasourceDefaultRequired,
			})
		}
	}
	return diagnostics
}

// checkInlineDefaultOutsideDataSource reports an inline := default on a
// :PARAMETERS name in a file that is not a data source (issue #264). Only
// data sources accept the inline form; in a script or procedure the
// statement takes names only and does not compile. The range runs from the
// name through the := so a client fix can rewrite it as a :DEFAULT line.
func checkInlineDefaultOutsideDataSource(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic
	stmts, _ := parametersStatements(tokens)
	for _, entries := range stmts {
		for _, e := range entries {
			if !e.hasDefault {
				continue
			}
			r := tokenToRange(e.name)
			r.End = tokenToRange(e.assign).End
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    r,
				Message:  fmt.Sprintf("Inline ':=' defaults on ':PARAMETERS' are valid only in data source files; move the default for '%s' to a ':DEFAULT' line after ':PARAMETERS'", e.name.Text),
				Source:   "ssl-lsp",
				Code:     CodeInlineDefaultOutsideDatasource,
			})
		}
	}
	return diagnostics
}

// checkDataSourceDefaultUsage flags :DEFAULT statements in data source files.
// Data sources use inline := defaults in :PARAMETERS, not separate :DEFAULT statements.
func checkDataSourceDefaultUsage(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}
		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		if normalized == "DEFAULT" {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "Data source files use inline ':=' defaults in ':PARAMETERS', not separate ':DEFAULT' statements",
				Source:   "ssl-lsp",
				Code:     CodeNoDefaultStatementsInDatasource,
			})
		}
	}

	return diagnostics
}

// checkSQLConcatenationInjection detects string concatenation in the first
// argument of a SQL function call, which may indicate a SQL injection
// vulnerability (diag.sql_injection).
// Source of truth: ssl-style-guide.schema.yaml lints.security.prevent_sql_injection.
//
// The argument is judged on its expression tree (issue #255): the `+`
// chain is flattened into operands, and the call is flagged when at least
// one operand is a string literal (the concatenation builds SQL text) and
// at least one is a runtime value — anything but a string or number
// literal, a logical/NIL literal, or a call to a known-safe SQL builder.
// Constant-only concatenation stays silent, a safe builder no longer hides
// a later spliced value, and identifier + identifier (no literal SQL text)
// stays out of scope. The range is the `+` adjoining the first runtime
// operand; one diagnostic per call.
func checkSQLConcatenationInjection(tokens []lexer.Token, calls []parser.CallSite) []Diagnostic {
	var diagnostics []Diagnostic

	for _, call := range calls {
		if !constants.IsSQLFunction(strings.ToUpper(call.Name)) {
			continue
		}
		args := call.EffectiveArgs()
		if len(args) == 0 {
			continue
		}
		operands, ops := flattenConcatenation(tokens, args[0])
		if len(operands) < 2 {
			continue
		}
		hasSQLText := false
		firstRuntime := -1
		for i, operand := range operands {
			switch {
			case isStringLiteralExpr(operand):
				hasSQLText = true
			case isConstantLiteralExpr(operand), isSafeSQLBuilderExpr(operand):
			default:
				if firstRuntime < 0 {
					firstRuntime = i
				}
			}
		}
		if !hasSQLText || firstRuntime < 0 {
			continue
		}
		// ops[i] is the `+` between operands[i] and operands[i+1]: use the
		// one before the runtime operand, or after it when it leads.
		opIdx := ops[0]
		if firstRuntime > 0 {
			opIdx = ops[firstRuntime-1]
		}
		if opIdx < 0 {
			continue
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range:    tokenToRange(tokens[opIdx]),
			Message:  fmt.Sprintf("String concatenation in '%s' argument may cause SQL injection. Use parameterized queries instead.", call.Name),
			Source:   "ssl-lsp",
			Code:     CodeSqlInjection,
		})
	}

	return diagnostics
}

// safeSQLBuilderFunctions lists functions that produce safe SQL fragments
// (e.g. properly escaped IN-clause value lists). Concatenating their return
// value into a SQL string is not an injection risk.
var safeSQLBuilderFunctions = map[string]bool{
	"BUILDSTRINGFORIN": true,
}

// flattenConcatenation splits a `+` chain (parentheses unwrapped) into its
// operands in source order, plus the token index of each `+` between
// consecutive operands (-1 when it cannot be located). A non-`+`
// expression is a single operand.
func flattenConcatenation(tokens []lexer.Token, e *parser.Expr) ([]*parser.Expr, []int) {
	var operands []*parser.Expr
	var ops []int
	var walk func(e *parser.Expr)
	walk = func(e *parser.Expr) {
		for e != nil && e.Kind == parser.ExprGroup && len(e.Children) == 1 {
			e = e.Children[0]
		}
		if e != nil && e.Kind == parser.ExprBinary && e.Op == "+" && len(e.Children) == 2 {
			walk(e.Children[0])
			ops = append(ops, findOperatorToken(tokens, e.Children[0].End+1, e.Children[1].Start-1, "+"))
			walk(e.Children[1])
			return
		}
		operands = append(operands, e)
	}
	walk(e)
	return operands, ops
}

// findOperatorToken returns the index of the first token in [from, to]
// whose text is op, or -1.
func findOperatorToken(tokens []lexer.Token, from, to int, op string) int {
	for i := from; i <= to && i < len(tokens); i++ {
		if i >= 0 && tokens[i].Text == op {
			return i
		}
	}
	return -1
}

func isStringLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Kind != parser.ExprLiteral || e.Name == "" {
		return false
	}
	switch e.Name[0] {
	case '"', '\'', '[':
		return true
	}
	return false
}

// isConstantLiteralExpr reports a non-string literal: a number, .T./.F.,
// or NIL. Concatenating one cannot carry injected SQL.
func isConstantLiteralExpr(e *parser.Expr) bool {
	return e != nil && e.Kind == parser.ExprLiteral && !isStringLiteralExpr(e)
}

// isSafeSQLBuilderExpr reports a call to a known-safe SQL builder
// (BuildStringForIn(...)), whose escaped output may be concatenated.
func isSafeSQLBuilderExpr(e *parser.Expr) bool {
	if e == nil || e.Kind != parser.ExprCall || len(e.Children) == 0 {
		return false
	}
	callee := e.Children[0]
	return callee.Kind == parser.ExprIdentifier && safeSQLBuilderFunctions[strings.ToUpper(callee.Name)]
}

// checkClassNameCollision warns when a `:CLASS Foo;` declaration uses a
// name that collides with one of the published built-in SSL classes. Such
// declarations are confusing because the user-defined class shadows the
// built-in only in the file's local scope, and `Foo{}` instantiation may
// resolve to the built-in elsewhere.
func checkClassNameCollision(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}
		if strings.ToUpper(strings.TrimPrefix(token.Text, ":")) != "CLASS" {
			continue
		}

		// Find the next non-whitespace, non-comment token — the class name.
		for j := i + 1; j < len(tokens); j++ {
			next := tokens[j]
			if next.Type == lexer.TokenWhitespace || next.Type == lexer.TokenComment {
				continue
			}
			if next.Type != lexer.TokenIdentifier {
				break
			}
			if constants.IsSSLClass(next.Text) {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range:    tokenToRange(next),
					Message: fmt.Sprintf(
						"':CLASS %s' shadows the built-in SSL class %q. "+
							"Pick a different name to avoid confusion when readers reach for the built-in.",
						next.Text, next.Text),
					Source: "ssl-lsp",
					Code:   CodeClassNameCollision,
				})
			}
			break
		}
	}

	return diagnostics
}

// checkStepZeroLiteral flags a :FOR loop whose :STEP is a literal zero
// (diag.step_zero_literal, issue #199): a zero step never advances the loop
// variable, so the loop cannot terminate once entered. Only provable
// literals flag: the whole step expression, read from the expression AST,
// must be a numeric literal whose value is zero, optionally negated. A
// variable or a larger expression (`:STEP 0 + nInc`) is left alone.
func checkStepZeroLiteral(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var diagnostics []Diagnostic

	for _, se := range stmts {
		if se.Kind != parser.StmtFor || len(se.Exprs) < 4 {
			continue
		}
		step := se.Exprs[3]
		for step != nil && step.Kind == parser.ExprUnary && step.Op == "-" && len(step.Children) == 1 {
			step = step.Children[0]
		}
		if step == nil || step.Kind != parser.ExprLiteral || step.Start != step.End ||
			step.Start < 0 || step.Start >= len(tokens) {
			continue
		}
		num := tokens[step.Start]
		if num.Type != lexer.TokenNumber {
			continue
		}
		if strings.Trim(strings.ReplaceAll(num.Text, ".", ""), "0") != "" {
			continue
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range:    tokenToRange(num),
			Message:  "':STEP 0' never advances the loop variable - the ':FOR' loop cannot terminate once entered",
			Source:   "ssl-lsp",
			Code:     CodeStepZeroLiteral,
		})
	}

	return diagnostics
}

// checkExitCaseAfterReturn flags an :EXITCASE that immediately follows a
// branch-level :RETURN statement inside a :BEGINCASE structure
// (diag.exitcase_after_return, issue #190): the :RETURN already leaves the
// procedure, so the :EXITCASE is unreachable. The pair is a common
// generated/refactored pattern because general guidance says to end every
// :CASE with :EXITCASE.
func checkExitCaseAfterReturn(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	caseDepth := 0
	for i, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}
		switch strings.ToUpper(strings.TrimPrefix(token.Text, ":")) {
		case "BEGINCASE":
			caseDepth++
		case "ENDCASE":
			if caseDepth > 0 {
				caseDepth--
			}
		case "RETURN":
			if caseDepth == 0 {
				continue
			}
			// Find the end of the :RETURN statement — the terminating
			// semicolon at top nesting level.
			parenDepth, braceDepth, bracketDepth := 0, 0, 0
			end := -1
			for j := i + 1; j < len(tokens); j++ {
				t := tokens[j]
				if t.Type != lexer.TokenPunctuation {
					continue
				}
				switch t.Text {
				case "(":
					parenDepth++
				case ")":
					parenDepth--
				case "{":
					braceDepth++
				case "}":
					braceDepth--
				case "[":
					bracketDepth++
				case "]":
					bracketDepth--
				case ";":
					if parenDepth == 0 && braceDepth == 0 && bracketDepth == 0 {
						end = j
					}
				}
				if end >= 0 {
					break
				}
			}
			if end < 0 {
				continue
			}
			nextIdx := nextSignificantTokenIndex(tokens, end+1)
			if nextIdx < 0 || tokens[nextIdx].Type != lexer.TokenKeyword {
				continue
			}
			if !strings.EqualFold(strings.TrimPrefix(tokens[nextIdx].Text, ":"), "EXITCASE") {
				continue
			}
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityHint,
				Range:    tokenToRange(tokens[nextIdx]),
				Message:  "Unreachable ':EXITCASE' - the preceding ':RETURN' already leaves the procedure",
				Source:   "ssl-lsp",
				Code:     CodeExitCaseAfterReturn,
			})
		}
	}

	return diagnostics
}

// checkMixedErrorHandlingFamilies flags a procedure that combines the legacy
// :ERROR/:RESUME handler family with structured :TRY/:CATCH handling
// (diag.mixed_error_handling_families, issue #191): the legacy handler can
// intercept a raised error before the :CATCH sees it, producing confusing
// control flow that is rarely intentional. One diagnostic per span, ranged
// on the first token of whichever family appears later.
//
// Each :PROCEDURE ... :ENDPROC is its own span. All top-level script code
// is one further span, however many procedures sit between its pieces:
// the grammar puts a script's legacy handler after the statements it
// protects (Program ::= {Statement} [ErrorHandler]), so a top-level :TRY
// before the procedures and a top-level :ERROR after them still meet.
func checkMixedErrorHandlingFamilies(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// span tracks the first occurrence of each family; -1 means not seen.
	type span struct{ legacy, structured int }
	newSpan := func() span { return span{-1, -1} }
	flush := func(sp span, where, advice string) {
		if sp.legacy < 0 || sp.structured < 0 {
			return
		}
		later := sp.legacy
		if sp.structured > later {
			later = sp.structured
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range:    tokenToRange(tokens[later]),
			Message:  "Legacy ':ERROR'/':RESUME' and structured ':TRY'/':CATCH' are mixed in " + where + " - the legacy handler can intercept errors before ':CATCH' sees them; use one family " + advice,
			Source:   "ssl-lsp",
			Code:     CodeMixedErrorHandlingFamilies,
		})
	}

	topLevel, proc := newSpan(), newSpan()
	inProc := false
	current := func() *span {
		if inProc {
			return &proc
		}
		return &topLevel
	}

	for i, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}
		switch strings.ToUpper(strings.TrimPrefix(token.Text, ":")) {
		case "PROCEDURE":
			if inProc {
				flush(proc, "the same procedure", "per procedure")
			}
			proc = newSpan()
			inProc = true
		case "ENDPROC":
			if inProc {
				flush(proc, "the same procedure", "per procedure")
				proc = newSpan()
			}
			inProc = false
		case "ERROR", "RESUME":
			// Only the marker-statement forms `:ERROR;` / `:RESUME;` are
			// legacy handlers. `:ERROR` also appears in expression position
			// (e.g. `LimsString(:ERROR)` inside a :CATCH, corpus-observed) —
			// that is not the legacy family.
			sp := current()
			if sp.legacy >= 0 {
				continue
			}
			if n := nextSignificantTokenIndex(tokens, i+1); n >= 0 &&
				tokens[n].Type == lexer.TokenPunctuation && tokens[n].Text == ";" {
				sp.legacy = i
			}
		case "TRY", "CATCH":
			if sp := current(); sp.structured < 0 {
				sp.structured = i
			}
		}
	}
	if inProc {
		flush(proc, "the same procedure", "per procedure")
	}
	// The top-level span is the script body, not a procedure (issue #328).
	flush(topLevel, "the top-level script code", "in the script body")

	return diagnostics
}

// validLimsTypeExResults is the complete result set of LimsTypeEx.
var validLimsTypeExResults = map[string]bool{
	"NIL": true, "STRING": true, "NUMERIC": true, "LOGIC": true,
	"DATE": true, "ARRAY": true, "CODEBLOCK": true, "OBJECT": true,
	"SSLVALUE": true,
}

// checkInvalidLimsTypeExComparison flags a comparison between a
// LimsTypeEx(...) call and a string literal outside the function's fixed
// result set (diag.invalid_limstypeex_comparison, issue #187): LimsTypeEx
// returns exactly one of NIL, STRING, NUMERIC, LOGIC, DATE, ARRAY,
// CODEBLOCK, OBJECT, SSLVALUE, so a guard against any other literal (the
// chronic one is "NUMBER") can never pass. Both operand orders are checked.
func checkInvalidLimsTypeExComparison(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	isComparison := func(idx int) bool {
		if idx < 0 || tokens[idx].Type != lexer.TokenOperator {
			return false
		}
		switch tokens[idx].Text {
		case "=", "==", "!=", "<>", "#":
			return true
		}
		return false
	}
	// String comparison is case-sensitive and `=` is a prefix match
	// (reference: operators/equals), so the literal is judged exactly as
	// written and `=` is judged by its prefix semantics (issue #255).
	flagIfInvalid := func(strIdx, opIdx int, literalOnLeft bool) {
		literal := unquoteSSLString(tokens[strIdx].Text)
		op := tokens[opIdx].Text
		possible := false
		if op == "=" {
			for result := range validLimsTypeExResults {
				if literalOnLeft && strings.HasPrefix(literal, result) {
					possible = true
				}
				// `LimsTypeEx(x) = ""` is always true; equals_empty_string
				// owns that shape.
				if !literalOnLeft && (literal == "" || strings.HasPrefix(result, literal)) {
					possible = true
				}
			}
		} else {
			possible = validLimsTypeExResults[literal]
		}
		if possible {
			return
		}
		verdict := "this comparison can never be true"
		if op == "!=" || op == "<>" || op == "#" {
			verdict = "this comparison is always true"
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(tokens[strIdx]),
			Message:  fmt.Sprintf("LimsTypeEx never returns %q - %s. Valid results (uppercase, compared case-sensitively): NIL, STRING, NUMERIC, LOGIC, DATE, ARRAY, CODEBLOCK, OBJECT, SSLVALUE", literal, verdict),
			Source:   "ssl-lsp",
			Code:     CodeInvalidLimsTypeExComparison,
		})
	}

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier || !strings.EqualFold(token.Text, "LimsTypeEx") {
			continue
		}
		openIdx := nextSignificantTokenIndex(tokens, i+1)
		if openIdx < 0 || tokens[openIdx].Type != lexer.TokenPunctuation || tokens[openIdx].Text != "(" {
			continue
		}

		// Forward order: LimsTypeEx(...) <op> "literal".
		depth := 0
		closeIdx := -1
		for j := openIdx; j < len(tokens); j++ {
			if tokens[j].Type != lexer.TokenPunctuation {
				continue
			}
			switch tokens[j].Text {
			case "(":
				depth++
			case ")":
				depth--
				if depth == 0 {
					closeIdx = j
				}
			}
			if closeIdx >= 0 {
				break
			}
		}
		if closeIdx >= 0 {
			opIdx := nextSignificantTokenIndex(tokens, closeIdx+1)
			if isComparison(opIdx) {
				strIdx := nextSignificantTokenIndex(tokens, opIdx+1)
				if strIdx >= 0 && tokens[strIdx].Type == lexer.TokenString {
					flagIfInvalid(strIdx, opIdx, false)
					continue
				}
			}
		}

		// Reversed order: "literal" <op> LimsTypeEx(...).
		opIdx := previousSignificantTokenIndex(tokens, i-1)
		if isComparison(opIdx) {
			strIdx := previousSignificantTokenIndex(tokens, opIdx-1)
			if strIdx >= 0 && tokens[strIdx].Type == lexer.TokenString {
				flagIfInvalid(strIdx, opIdx, true)
			}
		}
	}

	return diagnostics
}

// sqlCallFirstArgStrings iterates call sites of the recognized embedded-SQL
// functions (SQLExecute plus the positional family) and yields the string
// tokens that make up each call's first argument — including the pieces of a
// concatenated SQL string. yield receives the call's function-name token
// index and the string token index.
func sqlCallFirstArgStrings(tokens []lexer.Token, funcFilter func(string) bool, yield func(callIdx, strIdx int)) {
	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier || !funcFilter(strings.ToUpper(token.Text)) {
			continue
		}
		openIdx := nextSignificantTokenIndex(tokens, i+1)
		if openIdx < 0 || tokens[openIdx].Type != lexer.TokenPunctuation || tokens[openIdx].Text != "(" {
			continue
		}
		argStarts, argEnds, _ := parseTopLevelCallArguments(tokens, openIdx)
		if len(argStarts) == 0 || argStarts[0] < 0 {
			continue
		}
		for j := argStarts[0]; j <= argEnds[0]; j++ {
			if tokens[j].Type == lexer.TokenString {
				yield(i, j)
			}
		}
	}
}

// stripLeadingSQLComments removes leading whitespace, `--` line comments,
// and `/* */` block comments from a SQL string so the first real keyword
// can be inspected.
func stripLeadingSQLComments(s string) string {
	for {
		s = strings.TrimLeft(s, " \t\r\n")
		switch {
		case strings.HasPrefix(s, "--"):
			nl := strings.IndexByte(s, '\n')
			if nl < 0 {
				return ""
			}
			s = s[nl+1:]
		case strings.HasPrefix(s, "/*"):
			end := strings.Index(s, "*/")
			if end < 0 {
				return ""
			}
			s = s[end+2:]
		default:
			return s
		}
	}
}

var (
	sqlDMLVerbPattern       = regexp.MustCompile(`(?i)\b(insert|update|delete|merge)\b`)
	sqlSelectIntoPattern    = regexp.MustCompile(`(?i)\binto\b`)
	leadingSQLWordExtractor = regexp.MustCompile(`^[A-Za-z]+`)
)

// checkRunSQLNonDML flags RunSQL calls whose SQL string is a result-returning
// statement (diag.runsql_non_dml, issue #195): RunSQL is for DML; a query
// whose first keyword is SELECT or WITH should use a result-returning API
// (LSearch/LSelect/GetDataSet/...) instead. SELECT ... INTO and WITH-wrapped
// DML are left alone — those write. Call sites come from the expression
// AST: the first argument must begin with a string literal (its leading
// keyword decides), every string piece of that argument feeds the write
// guards, and a qualified `recv:RunSQL(...)` is a member call — never the
// built-in — and is not reported (D10).
func checkRunSQLNonDML(tokens []lexer.Token, calls []parser.CallSite) []Diagnostic {
	var diagnostics []Diagnostic

	for _, site := range calls {
		if site.Qualified() || !strings.EqualFold(site.Name, "RunSQL") || len(site.Args) == 0 {
			continue
		}
		arg := site.Args[0]
		// The leftmost operand of the argument holds the statement's
		// leading keyword; only a string literal there is provable.
		lead := arg
		for lead != nil && (lead.Kind == parser.ExprBinary || lead.Kind == parser.ExprGroup) && len(lead.Children) > 0 {
			lead = lead.Children[0]
		}
		if lead == nil || lead.Kind != parser.ExprLiteral || lead.Start < 0 || lead.Start >= len(tokens) ||
			tokens[lead.Start].Type != lexer.TokenString {
			continue
		}
		strIdx := lead.Start
		content := stripLeadingSQLComments(unquoteSSLString(tokens[strIdx].Text))
		word := strings.ToUpper(leadingSQLWordExtractor.FindString(content))
		if word != "SELECT" && word != "WITH" {
			continue
		}
		// A SELECT ... INTO writes; a WITH wrapping INSERT/UPDATE/DELETE/
		// MERGE writes. Both are legitimate RunSQL statements. Every string
		// piece of the argument is consulted for the guard.
		full := content
		for j := strIdx + 1; j <= arg.End && j < len(tokens); j++ {
			if tokens[j].Type == lexer.TokenString {
				full += " " + unquoteSSLString(tokens[j].Text)
			}
		}
		if word == "SELECT" && sqlSelectIntoPattern.MatchString(full) {
			continue
		}
		if word == "WITH" && sqlDMLVerbPattern.MatchString(full) {
			continue
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range:    tokenToRange(tokens[strIdx]),
			Message:  fmt.Sprintf("RunSQL is for DML statements - this '%s' query returns a result RunSQL discards. Use a result-returning API (LSearch, LSelect, GetDataSet, ...) instead", word),
			Source:   "ssl-lsp",
			Code:     CodeRunSQLNonDML,
		})
	}

	return diagnostics
}

// checkUnicodeLiteralPrefix flags N'...' Unicode literal prefixes in
// embedded SQL (diag.unicode_literal_prefix, issue #196). Info-tier style
// note: most schemas don't need the prefix and it creeps in via
// copy-paste. Detection runs over the SQL lexer's token stream — an `N`
// identifier immediately followed by a '…' literal — so a 'N' flag
// literal, a ?'N'? placeholder, or N'…' text in a SQL comment never
// matches (issue #255). One diagnostic per string token.
func checkUnicodeLiteralPrefix(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	sqlCallFirstArgPieces(tokens, func(p sqlArgPiece) {
		strIdx := p.strIdx
		if !hasUnicodeLiteralPrefix(p.toks) {
			return
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityInfo,
			Range:    tokenToRange(tokens[strIdx]),
			Message:  "Unicode literal prefix N'...' in embedded SQL - drop the prefix unless the target column genuinely requires it",
			Source:   "ssl-lsp",
			Code:     CodeUnicodeLiteralPrefix,
		})
	})

	return diagnostics
}

// checkCollateJustification flags COLLATE in embedded SQL when no comment
// directly precedes the containing statement (diag.unjustified_collate,
// issue #197). COLLATE is matched as a lexed SQL word, never inside a SQL
// character literal or comment (issue #255). Info-tier style note: forcing collation is occasionally
// necessary but should carry a documented reason; an unexplained COLLATE
// is usually cargo-culted.
func checkCollateJustification(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	sqlCallFirstArgPieces(tokens, func(p sqlArgPiece) {
		callIdx, strIdx := p.callIdx, p.strIdx
		if !hasSQLWord(p.toks, "COLLATE") {
			return
		}
		// Justified when a comment sits between the previous statement's
		// terminator and this statement's first token.
		justified := false
		for j := callIdx - 1; j >= 0; j-- {
			t := tokens[j]
			if t.Type == lexer.TokenComment {
				justified = true
				break
			}
			if t.Type == lexer.TokenPunctuation && t.Text == ";" {
				break
			}
		}
		if justified {
			return
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityInfo,
			Range:    tokenToRange(tokens[strIdx]),
			Message:  "COLLATE in embedded SQL without a justification comment - document why the forced collation is needed in a comment directly above this statement",
			Source:   "ssl-lsp",
			Code:     CodeUnjustifiedCollate,
		})
	})

	return diagnostics
}

// skipCommaRun is an inclusive token-index span from the first to the last
// comma of a run of skip-commas.
type skipCommaRun struct{ first, last int }

// trailingSkipCommaRuns returns, in source order, the run of commas that
// follows the last real argument of each call whose argument list ends in
// skipped slots (`Foo(a,,)`, `Foo(a,)`). Call sites come from the
// expression AST; qualified `recv:Name(...)` member calls are excluded —
// the runtime's NIL padding of missing trailing arguments is a property of
// SSL calls, not of whatever a member call dispatches to (D10).
func trailingSkipCommaRuns(tokens []lexer.Token, calls []parser.CallSite) []skipCommaRun {
	var runs []skipCommaRun
	for _, site := range calls {
		n := len(site.Args)
		if site.Qualified() || n == 0 || site.Args[n-1].Kind != parser.ExprSkipped {
			continue
		}
		from := site.Call.Children[0].End + 1
		for k := n - 1; k >= 0; k-- {
			if site.Args[k].Kind != parser.ExprSkipped {
				from = site.Args[k].End + 1
				break
			}
		}
		run := skipCommaRun{first: -1, last: -1}
		for j := from; j < site.Call.End && j < len(tokens); j++ {
			if tokens[j].Type == lexer.TokenPunctuation && tokens[j].Text == "," {
				if run.first < 0 {
					run.first = j
				}
				run.last = j
			}
		}
		if run.first >= 0 {
			runs = append(runs, run)
		}
	}
	sort.Slice(runs, func(a, b int) bool { return runs[a].first < runs[b].first })
	return runs
}

// hasUnicodeLiteralPrefix reports whether a lexed SQL fragment holds an
// N'…' literal: one string token starting `N'`, or an `N` word token
// immediately followed (no whitespace) by a single-quoted string token.
func hasUnicodeLiteralPrefix(toks []SQLToken) bool {
	for i := 0; i < len(toks); i++ {
		t := toks[i]
		// The SQL lexer reads a prefixed literal as one string token
		// (`N'x'`, issue #258).
		if t.Type == SQLTokenString && len(t.Text) > 1 && (t.Text[0] == 'N' || t.Text[0] == 'n') && t.Text[1] == '\'' {
			return true
		}
		if i+1 >= len(toks) {
			break
		}
		if (t.Type == SQLTokenIdentifier || t.Type == SQLTokenKeyword) && strings.EqualFold(t.Text, "N") &&
			toks[i+1].Type == SQLTokenString && strings.HasPrefix(toks[i+1].Text, "'") {
			return true
		}
	}
	return false
}

// hasSQLWord reports whether a lexed SQL fragment holds the word (keyword,
// function, or identifier — case-insensitive) outside literals and
// comments.
func hasSQLWord(toks []SQLToken, word string) bool {
	for _, t := range toks {
		switch t.Type {
		case SQLTokenIdentifier, SQLTokenKeyword, SQLTokenFunction:
			if strings.EqualFold(t.Text, word) {
				return true
			}
		}
	}
	return false
}

// checkTrailingSkipCommas flags skip-commas immediately preceding a call's
// closing parenthesis (diag.trailing_skip_commas, issue #193): trailing
// skipped arguments are unnecessary — the runtime pads missing trailing
// arguments with NIL, so `Foo(a,,)` is `Foo(a)`. The range covers the
// comma run. Info tier: it is the comma form of the style guide's
// "omit trailing optional parameters" clause, whose `{}` form is
// diag.empty_optional_param_array.
func checkTrailingSkipCommas(tokens []lexer.Token, runs []skipCommaRun) []Diagnostic {
	var diagnostics []Diagnostic

	for _, run := range runs {
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityInfo,
			Range: Range{
				Start: tokenToRange(tokens[run.first]).Start,
				End:   tokenToRange(tokens[run.last]).End,
			},
			Message: "Trailing skipped arguments before ')' are unnecessary - the runtime pads missing trailing arguments with NIL; omit them",
			Source:  "ssl-lsp",
			Code:    CodeTrailingSkipCommas,
		})
	}

	return diagnostics
}

// checkSpacedSkipCommas flags skip-comma pairs written with whitespace
// between them (diag.spaced_skip_commas, issue #193). Info-tier style
// note: `, ,` is valid syntax, but the adjacent form `,,` makes the
// skipped argument visually deliberate. One diagnostic per run. A run that
// diag.trailing_skip_commas already reports is left to it: deleting the
// run fixes both, so one finding per cause.
func checkSpacedSkipCommas(tokens []lexer.Token, trailing []skipCommaRun) []Diagnostic {
	var diagnostics []Diagnostic

	ownedByTrailing := make(map[int]bool, len(trailing))
	for _, run := range trailing {
		ownedByTrailing[run.last] = true
	}

	for i := 0; i < len(tokens); i++ {
		if tokens[i].Type != lexer.TokenPunctuation || tokens[i].Text != "," {
			continue
		}
		// Extend a run of commas where at least one adjacent pair has
		// whitespace (and nothing else) between the commas.
		last := i
		spaced := false
		for j := last + 1; j < len(tokens); j++ {
			if tokens[j].Type == lexer.TokenWhitespace {
				continue
			}
			if tokens[j].Type == lexer.TokenPunctuation && tokens[j].Text == "," {
				if j > last+1 {
					spaced = true
				}
				last = j
				continue
			}
			break
		}
		if !spaced || ownedByTrailing[last] {
			i = last
			continue
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityInfo,
			Range: Range{
				Start: tokenToRange(tokens[i]).Start,
				End:   tokenToRange(tokens[last]).End,
			},
			Message: "Spaced skip-commas - write the skip form with adjacent commas (',,') so the skipped argument reads as deliberate",
			Source:  "ssl-lsp",
			Code:    CodeSpacedSkipCommas,
		})
		i = last
	}

	return diagnostics
}

// checkCStyleCommentClosers flags SSL comments whose text ends with a
// C-style `*/` immediately before the terminating `;`
// (diag.c_style_comment_closer, issue #208 discussion). The construct is
// valid — SSL reads the `*/` as literal comment text and the `;` as the
// real terminator — so this is a pure info-tier style observation: the
// `*/` suggests a mental model where it closes the comment, which in SSL
// it never does.
// codeAfterCStyleCloser reports the first statement-looking fragment that
// follows a `*/` inside a comment token, if any. It is what the comment
// swallowed past the point the author thought it ended.
func codeAfterCStyleCloser(commentText string) (string, bool) {
	idx := strings.Index(commentText, "*/")
	if idx < 0 {
		return "", false
	}
	rest := strings.TrimRight(commentText[idx+2:], "; \t\r\n")
	for _, line := range strings.Split(rest, "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		// Banner decoration is not code.
		if strings.Trim(line, "*=-_+#~ \t") == "" {
			continue
		}
		// Only report when it actually looks like a statement: an
		// assignment, a colon keyword, or a call.
		if !strings.Contains(line, ":=") &&
			!(strings.HasPrefix(line, ":") && len(line) > 2) &&
			!strings.Contains(line, "(") {
			continue
		}
		if len(line) > 48 {
			line = line[:45] + "..."
		}
		return "'" + line + "'", true
	}
	return "", false
}

func checkCStyleCommentClosers(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenComment {
			continue
		}
		text := strings.TrimRight(strings.TrimSpace(token.Text), ";")
		if strings.HasSuffix(strings.TrimRight(text, " \t"), "*/") {
			// `/* ... */;` — the `*/` is the last thing before the real
			// terminator, so it is inert text and nothing is hidden.
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range:    tokenToRange(token),
				Message:  "SSL comments end at ';' - the '*/' before it is literal comment text, not a closer (valid; stylistic)",
				Source:   "ssl-lsp",
				Code:     CodeCStyleCommentCloser,
			})
			continue
		}
		// `/* ... */` with no `;` after it: the comment did NOT end at the
		// `*/`, it ran on to the next `;` and took whatever was in between
		// with it. The author believed they closed the comment, so the
		// swallowed statement looks like live code in the editor while
		// never executing. This is the dangerous half of the C-style
		// confusion and the one the validator used to miss entirely —
		// a swallowed `:PROCEDURE` surfaced only as a puzzling
		// `unmatched_block_end` elsewhere, and a swallowed assignment
		// surfaced as nothing at all (issue #249).
		if swallowed, ok := codeAfterCStyleCloser(token.Text); ok {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(token),
				Message: fmt.Sprintf(
					"This comment does not end at its '*/' - SSL comments end at ';', so it runs on and swallows %s. That code never executes. End the comment with ';'",
					swallowed),
				Source: "ssl-lsp",
				Code:   CodeCommentSwallowsCode,
			})
		}
	}

	return diagnostics
}
