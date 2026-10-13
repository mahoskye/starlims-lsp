// Package constants defines SSL language keywords, operators, functions, and classes.
package constants

import (
	"slices"
	"strings"
)

// SSLKeywords contains all SSL language keywords (38 total).
var SSLKeywords = []string{
	"BEGINCASE", "BEGININLINECODE",
	"CASE", "CATCH", "CLASS",
	"DECLARE", "DEFAULT",
	"ELSE", "ENDCASE", "ENDIF", "ENDINLINECODE", "ENDPROC", "ENDREGION", "ENDTRY", "ENDWHILE", "ERROR", "EXITCASE", "EXITFOR", "EXITWHILE",
	"FINALLY", "FOR",
	"IF", "INCLUDE", "INHERIT",
	"LABEL", "LOOP",
	"NEXT",
	"OTHERWISE",
	"PARAMETERS", "PROCEDURE", "PUBLIC",
	"REGION", "RESUME", "RETURN",
	"STEP",
	"TO", "TRY",
	"WHILE",
}

// BlockStartKeywords are keywords that start a block (increase indent after).
// Includes middle keywords like ELSE, CASE, CATCH that start new indented content.
// Note: CLASS is excluded because there is no :ENDCLASS - classes extend to end of file.
var BlockStartKeywords = []string{
	"IF", "ELSE", "WHILE", "FOR", "BEGINCASE", "CASE", "OTHERWISE", "TRY", "CATCH", "FINALLY", "PROCEDURE", "REGION", "BEGININLINECODE",
}

// BlockEndKeywords are keywords that end a block.
var BlockEndKeywords = []string{
	"ENDIF", "ENDWHILE", "NEXT", "ENDCASE", "ENDTRY", "ENDPROC", "ENDREGION", "ENDINLINECODE",
}

// BlockMiddleKeywords are keywords that appear in the middle of blocks.
// These dedent before themselves, then indent after (dedent-then-indent pattern).
var BlockMiddleKeywords = []string{"ELSE", "CATCH", "FINALLY", "CASE", "OTHERWISE"}

// CaseKeywords are keywords used in CASE statements.
var CaseKeywords = []string{"CASE", "OTHERWISE"}

// ProcedureLevelKeywords are keywords valid at procedure level.
var ProcedureLevelKeywords = []string{"PARAMETERS", "DEFAULT", "PUBLIC", "DECLARE"}

// DataSourceBuilderDirectives are colon-prefixed directives used in SQL data source
// files. These are preprocessed by SqlDataSourceBuilder before SSL compilation and
// are NOT part of the SSL grammar. They must not be flagged as unknown keywords.
var DataSourceBuilderDirectives = []string{"DSN", "TABLENAME", "NULLASBLANK", "INVARIANTDATECOLUMNS"}

// IsBuilderDirective checks if a normalized keyword name (without colon prefix)
// is a SQL data source builder directive.
func IsBuilderDirective(s string) bool {
	return slices.Contains(DataSourceBuilderDirectives, s)
}

// SSLOperators contains all SSL operators (32 total in this list).
// Note: the source of truth counts 36 total operators including `:` (member access),
// `/*` (comment delimiter), and bitwise function calls (_AND, _OR, _XOR, _NOT).
// Those additional four are handled as punctuation/comments/functions, not operators.
var SSLOperators = []string{
	// Logical operators
	".AND.", ".OR.", ".NOT.", "!",
	// Assignment
	":=",
	// Compound assignment
	"+=", "-=", "*=", "/=", "%=", "^=",
	// Comparison
	"=", "==", "!=", "<>", ">", "<", ">=", "<=",
	// Arithmetic
	"+", "-", "*", "/", "%", "^", "**", "++", "--", "<<", ">>",
	// Special
	"$", "#",
}

// SSLLogicalOperators are the logical operators.
var SSLLogicalOperators = []string{".AND.", ".OR.", ".NOT."}

// SSLCompoundOperators are compound assignment operators (operate-and-assign).
// Note: := is the simple assignment operator and is NOT included here.
var SSLCompoundOperators = []string{"+=", "-=", "*=", "/=", "%=", "^="}

// SSLMultiCharOperators are all operators that span multiple characters.
// Used by the lexer to correctly tokenize multi-character operators as single tokens.
var SSLMultiCharOperators = []string{
	// Comparison operators
	"<=", ">=", "==", "!=", "<>",
	// Assignment operators
	":=", "+=", "-=", "*=", "/=", "%=", "^=",
	"**", "++", "--", "<<", ">>",
	// Invalid but recognized for diagnostics (C-style logical operators)
	"&&", "||",
}

// SSLPredefinedGlobals is loaded from the vendored reference guide; see
// predefined_globals.go.

// SSLStatusKeywords are predefined global constants provided by the STARLIMS runtime.
// Each keyword is a read-only identifier whose value equals its own name as a string
// (e.g. Pending == "Pending"). They are used for workflow status comparisons.
// Source: status_keywords.txt from the vs-code-ssl-formatter project.
var SSLStatusKeywords = []string{
	"Accepted", "Active", "Aliquote", "All", "Approved", "Archived", "Assembled",
	"Cancel", "Canceled", "Cancelled", "Completed", "Created",
	"DBDE", "DCUTEMP", "Deleted", "Dependent", "Dismissed", "Disposed", "Done",
	"Draft", "DraftReport",
	"Edit",
	"Fail",
	"Hold",
	"IS", "InProcess", "Inactive", "Invited",
	"Locked", "Logged",
	"NIS", "NeedPrep", "New", "NoEquipment",
	"OKTOSTART", "OODL", "OOP", "OOS", "OOSPECS", "OOS_A", "OOS_B", "OOS_P",
	"OktoSubmit", "Open", "OutForCalib", "OutForRepair", "OutInUse",
	"Pending", "PendingReport", "Planned", "Prelogged", "Prepped", "Pulled",
	"Quarantine",
	"RTR", "RTROOS", "Received", "Rejected", "Released", "Reported", "Requested",
	"Reserved", "Resulted", "Retired", "Reviewed",
	"Shipped", "Started", "Submitted",
}

// IsStatusKeyword checks if a string matches a status keyword (case-insensitive).
func IsStatusKeyword(s string) bool {
	upper := strings.ToUpper(s)
	for _, kw := range SSLStatusKeywords {
		if strings.ToUpper(kw) == upper {
			return true
		}
	}
	return false
}

// SSLClassContextForms are case-insensitive identifiers used in class definitions.
// Me: self-reference, Base: parent class reference, Constructor: reserved constructor name.
var SSLClassContextForms = []string{"Me", "Base", "Constructor"}

// SSLClassContextDescriptions maps class-context forms to their descriptions.
var SSLClassContextDescriptions = map[string]string{
	"Me":          "Self-reference to the current class instance; use Me:PropertyName or Me:MethodName(args). Only valid inside :CLASS definitions.",
	"Base":        "Parent class reference used in colon-chained access; use Base:MethodName(args) to call overridden parent methods. Only valid inside :CLASS with :INHERIT.",
	"Constructor": "Reserved constructor declaration name inside :CLASS. Define with :PROCEDURE Constructor;. Cannot return a value.",
}

// IsSSLClassContextForm checks if a string is a class-context form (case-insensitive).
func IsSSLClassContextForm(s string) bool {
	lower := strings.ToLower(s)
	for _, form := range SSLClassContextForms {
		if strings.ToLower(form) == lower {
			return true
		}
	}
	return false
}

// SSLLiterals contains boolean and null literal values (3 total).
var SSLLiterals = []string{".T.", ".F.", "NIL"}

// SSLLiteralAliases maps alternative forms to canonical forms.
var SSLLiteralAliases = map[string]string{
	".t.": ".T.",
	".f.": ".F.",
	"nil": "NIL",
}

// InlineSQLFunctions are functions that support named ?param? placeholders.
// Per the SSL style guide, only SQLExecute supports named substitution.
var InlineSQLFunctions = []string{
	"SQLExecute",
}

// ParameterizedSQLFunctions are SQL-related functions that require positional
// '?' placeholders and separate value arrays or equivalent positional arguments.
var ParameterizedSQLFunctions = []string{
	"GetDataSet",
	"GetDataSetEx",
	"GetDataSetWithSchemaFromSelect",
	"GetDataSetXMLFromSelect",
	"GetNETDataSet",
	"GetTables",
	"RunSQL",
	"LSearch",
	"LSelect",
	"LSelect1",
	"LSelectC",
	"XmlExportSql",
}

// IsSQLFunction checks if a function name (uppercase) is any SQL-related function.
func IsSQLFunction(upper string) bool {
	for _, f := range InlineSQLFunctions {
		if strings.EqualFold(upper, f) {
			return true
		}
	}
	for _, f := range ParameterizedSQLFunctions {
		if strings.EqualFold(upper, f) {
			return true
		}
	}
	return false
}

// SSLKeywordDescriptions maps each keyword (uppercase, no colon) to its
// one-line description: the published reference summary (issue #252). The
// hand-written one-liners this replaced had drifted (`:PUBLIC` described as
// call-stack scoped, `:ERROR` as a marker); hover adds the reference syntax
// block, caveats, and don't-list on top.
var SSLKeywordDescriptions = buildKeywordDescriptions()

func buildKeywordDescriptions() map[string]string {
	out := make(map[string]string, len(GeneratedKeywordDetails))
	for kw, det := range GeneratedKeywordDetails {
		out[strings.ToUpper(kw)] = det.Summary
	}
	return out
}

// DataSourceBuilderDirectiveDescriptions maps builder directives to their
// descriptions. The directives are not SSL elements, so the reference
// covers them in its data-sources guide rather than in the element data;
// these follow that guide.
var DataSourceBuilderDirectiveDescriptions = map[string]string{
	"DSN":       "Names the database connection the data source runs against: `:DSN := name;`.",
	"TABLENAME": "Names the table in the resulting dataset: `:TABLENAME := name;`.",
	"NULLASBLANK": "Controls null-to-blank conversion: `:NULLASBLANK := true;` or `false`, matched case-insensitively. " +
		"With `true` (also the behavior when the directive is absent), a NULL column value comes back as a blank value " +
		"of the column's type; with `false`, as NIL. The SSL literals `.T.` and `.F.` are ignored here without an error.",
	"INVARIANTDATECOLUMNS": "Lists columns treated as invariant (culture-neutral) dates, as bare comma-separated names: " +
		"`:INVARIANTDATECOLUMNS := logdate, releasedate;`. A quoted name raises no error but keeps its quotes, so it matches no column.",
}

// SSLOperatorDescriptions maps each operator symbol to its published
// reference summary (issue #252). Hover adds the type-behavior table,
// caveats, and don't-list, which carry the `=` / `==` / `!=` string
// semantics in full.
var SSLOperatorDescriptions = buildOperatorDescriptions()

func buildOperatorDescriptions() map[string]string {
	out := make(map[string]string, len(GeneratedOperatorDetails))
	for _, det := range GeneratedOperatorDetails {
		if det.Symbol != "" {
			out[strings.ToUpper(det.Symbol)] = det.Summary
		}
	}
	return out
}

// OperatorSlug returns the reference slug ("equals", "not-equals", ...) for
// an operator symbol, case-insensitively, or "" when the symbol is unknown.
func OperatorSlug(symbol string) string {
	for slug, det := range GeneratedOperatorDetails {
		if det.Symbol != "" && strings.EqualFold(det.Symbol, symbol) {
			return slug
		}
	}
	return ""
}

// literalSlugs maps each canonical literal to its reference slug.
var literalSlugs = map[string]string{".T.": "true", ".F.": "false", "NIL": "nil"}

// LiteralSlug returns the reference slug for a canonical literal (".T." ->
// "true"), or "" when it is not one.
func LiteralSlug(canonical string) string {
	return literalSlugs[canonical]
}

// SSLLiteralDescriptions maps each canonical literal to its published
// reference summary (issue #252).
var SSLLiteralDescriptions = buildLiteralDescriptions()

func buildLiteralDescriptions() map[string]string {
	out := make(map[string]string, len(literalSlugs))
	for lit, slug := range literalSlugs {
		if det, ok := GeneratedLiteralDetails[slug]; ok {
			out[lit] = det.Summary
		}
	}
	return out
}

// Helper functions for checking membership

// IsKeyword checks if a string is an SSL keyword.
func IsKeyword(s string) bool {
	return slices.Contains(SSLKeywords, s)
}

// IsBlockStartKeyword checks if a string is a block start keyword.
func IsBlockStartKeyword(s string) bool {
	return slices.Contains(BlockStartKeywords, s)
}

// IsBlockEndKeyword checks if a string is a block end keyword.
func IsBlockEndKeyword(s string) bool {
	return slices.Contains(BlockEndKeywords, s)
}

// IsBlockMiddleKeyword checks if a string is a block middle keyword.
func IsBlockMiddleKeyword(s string) bool {
	return slices.Contains(BlockMiddleKeywords, s)
}

// IsCaseKeyword checks if a string is a case keyword.
func IsCaseKeyword(s string) bool {
	return slices.Contains(CaseKeywords, s)
}

// IsSSLOperator checks if a string is an SSL operator.
func IsSSLOperator(s string) bool {
	return slices.Contains(SSLOperators, s)
}

// IsSSLLogicalOperator checks if a string is a logical operator.
func IsSSLLogicalOperator(s string) bool {
	return slices.Contains(SSLLogicalOperators, s)
}

// IsSSLCompoundOperator checks if a string is a compound operator.
func IsSSLCompoundOperator(s string) bool {
	return slices.Contains(SSLCompoundOperators, s)
}

// IsSSLMultiCharOperator checks if a string is a multi-character operator.
func IsSSLMultiCharOperator(s string) bool {
	return slices.Contains(SSLMultiCharOperators, s)
}

// CanonicalSSLLiteral returns the canonical literal form for an SSL literal or alias.
func CanonicalSSLLiteral(s string) (string, bool) {
	if slices.Contains(SSLLiterals, s) {
		return s, true
	}

	if alias, ok := SSLLiteralAliases[strings.ToLower(s)]; ok {
		return alias, true
	}

	return "", false
}

// IsSSLLiteral checks if a string is an SSL literal or recognized alias.
func IsSSLLiteral(s string) bool {
	_, ok := CanonicalSSLLiteral(s)
	return ok
}

// IsSSLFunction checks if a string is an SSL function name.
func IsSSLFunction(s string) bool {
	_, ok := sslFunctionLookup[strings.ToLower(s)]
	return ok
}

// IsSSLClass checks if a string is an SSL class name.
func IsSSLClass(s string) bool {
	_, ok := sslClassLookup[strings.ToLower(s)]
	return ok
}

// IsReturnsObject checks if a string names a returns-category object
// (an object only obtainable as a built-in method's return value, or
// ambient in endpoint scripts).
func IsReturnsObject(s string) bool {
	_, ok := GeneratedReturnsObjectDetails[strings.ToLower(s)]
	return ok
}
