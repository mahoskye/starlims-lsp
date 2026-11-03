package providers

// Semantic tokens: the names a TextMate grammar cannot tell apart, classified
// from what the parser and the undeclared-variable check already know.
// Normative behavior: catalog/features/semantic_tokens.md.

import (
	"sort"
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// Semantic token types, as indices into SemanticTokenTypes.
const (
	SemanticFunction = iota
	SemanticMethod
	SemanticParameter
	SemanticVariable
	SemanticProperty
	SemanticClass
)

// SemanticTokenTypes names the token types, in the legend's order.
var SemanticTokenTypes = []string{"function", "method", "parameter", "variable", "property", "class"}

// Semantic token modifiers, as bits; bit n is SemanticTokenModifiers[n].
const (
	SemanticDeclaration = 1 << iota
	SemanticDefaultLibrary
	SemanticReadonly
	SemanticPublic
	SemanticUnknown
)

// SemanticTokenModifiers names the modifiers, in the legend's order. `public`
// and `unknown` are this server's own (D20); the others are standard LSP.
var SemanticTokenModifiers = []string{"declaration", "defaultLibrary", "readonly", "public", "unknown"}

// SemanticToken is one classified name. Its range lies on one line.
type SemanticToken struct {
	Range     Range
	Type      int
	Modifiers int
}

// SemanticTokenOptions carries what the classification needs from outside
// the document, the way DiagnosticOptions does for the undeclared check.
type SemanticTokenOptions struct {
	IsDataSourceFile bool
	IsEndpointFile   bool
	GlobalVariables  []string
	// IncludeDeclaredVariables are the names resolved :INCLUDE targets
	// declare; IncludesResolved says every target was read.
	IncludeDeclaredVariables []string
	IncludesResolved         bool
	// Resolver resolves cross-file dispatch targets; nil resolves none.
	Resolver WorkspaceResolver
}

// GetSemanticTokens classifies the document's names, sorted by position.
// tokens and ast are the document's cached parse; ast may be nil.
func GetSemanticTokens(text string, tokens []lexer.Token, ast *parser.Node, opts SemanticTokenOptions) []SemanticToken {
	if opts.IsDataSourceFile {
		// An SQL body is not SSL; only a directive header above one is
		// (feature.diagnostics_pipeline A10). The header is a
		// position-preserving prefix, so ranges line up unchanged.
		header, body := SplitDataSourceHeader(text)
		if IsSQLModeDataSourceSplit(text, header, body) {
			if strings.TrimSpace(header) == "" {
				return nil
			}
			tokens, ast = lexer.NewLexer(header).Tokenize(), nil
		}
	}
	if len(tokens) == 0 {
		return nil
	}

	p := parser.NewParser(tokens)
	if ast == nil {
		ast = p.Parse()
	}
	procedures := p.ExtractProcedures(ast)
	stmts := parser.ExtractStatementExpressions(tokens)
	unparsed := unparsedTokens(tokens, stmts)
	names := newSemanticNames(tokens, procedures, opts)
	unknown := unknownUses(tokens, p, unparsed, opts)
	roles := parser.IdentifierRoles(tokens)

	procedureType := SemanticFunction
	if isFirstSignificantStatementKeyword(tokens, "CLASS") {
		procedureType = SemanticMethod
	}

	var out []SemanticToken
	add := func(token lexer.Token, tokenType, modifiers int) {
		out = append(out, SemanticToken{Range: tokenToRange(token), Type: tokenType, Modifiers: modifiers})
	}
	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier || unparsed[i] {
			continue
		}
		if unknown[i] {
			add(token, SemanticVariable, SemanticUnknown)
			continue
		}
		switch roles[i] {
		case parser.RoleProcedureName:
			add(token, procedureType, SemanticDeclaration)
		case parser.RoleDeclaredName:
			if tokenType, modifiers, ok := names.classify(token); ok {
				add(token, tokenType, modifiers|SemanticDeclaration)
			}
		case parser.RoleVariable:
			if tokenType, modifiers, ok := names.classify(token); ok {
				add(token, tokenType, modifiers)
			}
		case parser.RoleCall:
			add(token, SemanticFunction, libraryModifier(constants.IsSSLFunction(token.Text)))
		case parser.RoleInstantiation:
			add(token, SemanticClass, libraryModifier(constants.IsSSLClass(token.Text)))
		case parser.RoleMember:
			memberType := SemanticProperty
			if next := nextSignificantTokenIndex(tokens, i+1); next >= 0 && tokens[next].Text == "(" {
				memberType = SemanticMethod
			}
			add(token, memberType, 0)
		}
	}

	for _, site := range resolvedDispatchSites(tokens, procedures, opts.Resolver) {
		out = append(out, SemanticToken{Range: site.Range, Type: SemanticFunction})
	}

	sort.Slice(out, func(a, b int) bool {
		return positionBefore(out[a].Range.Start, out[b].Range.Start)
	})
	return out
}

func libraryModifier(inLibrary bool) int {
	if inLibrary {
		return SemanticDefaultLibrary
	}
	return 0
}

// unparsedTokens marks the tokens of the statements whose words are not
// code: those diag.undeclared_variable defers to the comment-termination
// check and those that fail to parse on their first line.
func unparsedTokens(tokens []lexer.Token, stmts []parser.StatementExprs) []bool {
	marked := make([]bool, len(tokens))
	_, stranded := checkCommentTermination(tokens, stmts)
	for _, sp := range append(stranded, unparsedLineSpans(tokens, stmts)...) {
		if sp.start < 0 || sp.end >= len(tokens) {
			continue
		}
		for i := sp.start; i <= sp.end; i++ {
			marked[i] = true
		}
	}
	return marked
}

// unknownUses marks the uses of the names diag.undeclared_variable reports,
// under the same gates: never in a data source, nor in a file with an
// :INCLUDE that was not read. The check reports a name at its first
// undeclared use, and a finding in unparsed text is dropped, so such a name
// is not reported and is not unknown anywhere.
func unknownUses(tokens []lexer.Token, p *parser.Parser, unparsed []bool, opts SemanticTokenOptions) map[int]bool {
	if opts.IsDataSourceFile || (!opts.IncludesResolved && len(ExtractIncludeTargets(tokens)) > 0) {
		return nil
	}
	unknown := make(map[int]bool)
	reported := make(map[string]bool)
	for _, i := range undeclaredUses(tokens, p, opts.GlobalVariables, opts.IncludeDeclaredVariables, opts.IsEndpointFile) {
		name := strings.ToUpper(tokens[i].Text)
		if _, seen := reported[name]; !seen {
			reported[name] = !unparsed[i]
		}
		if reported[name] {
			unknown[i] = true
		}
	}
	return unknown
}

// resolvedDispatchSites returns the DoProc and ExecFunction targets that
// resolve, the way go to definition resolves them: a one-part target names a
// procedure of this file, any other goes through the workspace.
func resolvedDispatchSites(tokens []lexer.Token, procedures []parser.ProcedureInfo, resolver WorkspaceResolver) []CallSite {
	var out []CallSite
	verdicts := make(map[string]bool)
	for _, site := range ExtractCallSites(tokens) {
		if site.Kind != CallDispatch {
			continue
		}
		key := strings.ToLower(site.Raw)
		resolves, known := verdicts[key]
		if !known {
			if strings.Contains(site.Raw, ".") {
				resolves = resolver != nil && len(resolver.ResolveDispatch(site.Raw)) > 0
			} else {
				resolves = procedureLocation(tokens, site.Raw, "", procedures) != nil
			}
			verdicts[key] = resolves
		}
		if resolves {
			out = append(out, site)
		}
	}
	return out
}

// semanticScope is a declaration's place: a procedure, or nil for the top
// level of the file.
type semanticScope struct {
	procedure *parser.ProcedureInfo
	name      string
}

// semanticNames answers what a bare name is at a position.
type semanticNames struct {
	procedures []parser.ProcedureInfo
	// declared holds each :DECLARE and :PARAMETERS by its scope, the first
	// declaration winning.
	declared map[semanticScope]parser.VariableScope
	// public holds the :PUBLIC names and the configured globals.
	public map[string]bool
	// anywhere holds the names declared in any scope of the file or by an
	// included script: SSL's caller-scope lookup lets a procedure read
	// what its caller declares (diag.undeclared_variable).
	anywhere map[string]bool
	// isDataSource: a data source's undeclared names are directive values
	// or SQL, not variables.
	isDataSource bool
}

func newSemanticNames(tokens []lexer.Token, procedures []parser.ProcedureInfo, opts SemanticTokenOptions) *semanticNames {
	names := &semanticNames{
		procedures:   procedures,
		declared:     make(map[semanticScope]parser.VariableScope),
		public:       make(map[string]bool),
		anywhere:     make(map[string]bool),
		isDataSource: opts.IsDataSourceFile,
	}
	for _, decl := range parser.CollectDeclarations(tokens) {
		for _, v := range decl.Names {
			name := strings.ToUpper(v.Name)
			names.anywhere[name] = true
			if decl.Scope == parser.ScopePublic {
				names.public[name] = true
				continue
			}
			scope := semanticScope{parser.FindProcedureAtLine(procedures, v.Line), name}
			if _, taken := names.declared[scope]; !taken {
				names.declared[scope] = decl.Scope
			}
		}
	}
	for _, g := range opts.GlobalVariables {
		names.public[strings.ToUpper(g)] = true
	}
	for _, v := range opts.IncludeDeclaredVariables {
		names.anywhere[strings.ToUpper(v)] = true
	}
	return names
}

// classify resolves a bare name the way go to definition does, a procedure's
// own declaration over a file-wide one (feature.definition A4). ok is false
// for a name that gets no token.
func (n *semanticNames) classify(token lexer.Token) (tokenType, modifiers int, ok bool) {
	name := strings.ToUpper(token.Text)
	proc := parser.FindProcedureAtLine(n.procedures, token.Line)

	if scope, found := n.declared[semanticScope{proc, name}]; found {
		return declaredType(scope), 0, true
	}
	if n.public[name] {
		return SemanticVariable, SemanticPublic, true
	}
	if scope, found := n.declared[semanticScope{nil, name}]; found {
		return declaredType(scope), 0, true
	}
	if n.anywhere[name] {
		return SemanticVariable, 0, true
	}

	switch {
	case constants.IsPredefinedGlobal(name), constants.IsStatusKeyword(name):
		return SemanticVariable, SemanticDefaultLibrary | SemanticReadonly, true
	case name == "ME", name == "BASE":
		return SemanticVariable, SemanticDefaultLibrary, true
	case n.isDataSource, semanticBuiltins[name]:
		return 0, 0, false
	}
	// Assigned without a declaration, which declares it from there on, or
	// in a file whose :INCLUDE could not be read.
	return SemanticVariable, 0, true
}

func declaredType(scope parser.VariableScope) int {
	if scope == parser.ScopeParameter {
		return SemanticParameter
	}
	return SemanticVariable
}

// semanticBuiltins are the names the grammar colours: a literal, a logical
// operator's text, and a function or class named without its call.
var semanticBuiltins = func() map[string]bool {
	set := buildBuiltinSet()
	set["AND"], set["OR"], set["NOT"] = true, true, true
	return set
}()
