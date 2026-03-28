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
}

// SSLPredefinedGlobals contains runtime-provided read-only global variables.
// These are always recognized as pre-declared identifiers and must never be assigned to.
// Source: ssl_agent_instructions.md — "Predefined Global Variables" section.
var SSLPredefinedGlobals = []string{
	"MYUSERNAME", // The currently logged-in user's login name. Read-only.
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

// legacySSLFunctionNames contains the historical built-in function inventory.
// The public SSLFunctionNames slice is the canonical inventory in canonical.go.
var legacySSLFunctionNames = []string{
	"aadd", "Abs", "AddColDelimiters", "AddNameDelimiters", "AddProperty", "AddToApplication", "AddToSession",
	"aeval", "aevala", "afill", "alen", "AllTrim", "arraycalc", "arraynew", "ArrayToTVP", "Asc", "ascan", "ascanexact", "At",
	"BeginLimsTransaction", "Break", "buildarray", "buildarray2", "buildstring", "buildstring2", "BuildStringForIn",
	"CallBuiltInFunction", "CheckOnFtp", "ChkNewPassword", "ChkPassword", "Chr", "ClearLastSSLError", "ClearSession",
	"ClientEndOfDay", "ClientStartOfDay", "CMonth", "CombineFiles", "comparray", "Compress", "ConvertReport", "CopyToFtp",
	"CreateGUID", "CreateLocal", "CreateORMSession", "CreatePublic", "CreateUdObject", "CreateZip", "CToD",
	"DateAdd", "DateDiff", "DateDiffEx", "DateFormat", "DateFromNumbers", "DateFromString", "DateToString", "Day",
	"Decompress", "DecryptData", "delarray", "DeleteDirOnFtp", "DeleteFromFtp", "deleteinlinecode", "DetectSqlInjections", "Directory",
	"DocAcquireWorkitem", "DocAddUsersToGroup", "DocCancelCheckout", "DocCheckinDocument", "DocCheckoutDocument",
	"DocCommandFailed", "DocCompleteWorkitem", "DocCreateACL", "DocCreateCabinet", "DocCreateFolder", "DocCreateGroup",
	"DocCreateUser", "DocDelegateWorkitem", "DocDelete", "DocDeleteCabinet", "DocDeleteFolder", "DocDeleteUser",
	"DocEndDocumentumInterface", "DocExists", "DocExistsUser", "DocExportDocument", "DocGetCabinets", "DocGetDocuments",
	"DocGetErrorMessage", "DocGetFolders", "DocGetMetadata", "DocGetTasks", "DocGetTasksCount", "DocGetTypeAttributes",
	"DocGetTypeAttributesAsDataset", "DocGetWorkflowStatus", "DocGetWorkitemProperties", "DocImportDocument",
	"DocInitDocumentumInterface", "DocLoginToDocumentum", "DocPauseWorkflow", "DocRemoveAllUsersFromGroup",
	"DocRemoveUsersFromGroup", "DocRepeatWorkitem", "DocResumeWorkflow", "DocSearchAsDataset", "DocSearchFullText",
	"DocSearchUsingDql", "DocSetMetadata", "DocStartWorkflow", "DocStopWorkflow", "DocUpdateUser",
	"DoProc", "DosSupport", "DOW", "DOY", "DToC", "DToS",
	"Empty", "EncryptData", "endlimsoleconnect", "EndLimsTransaction", "ErrorMes", "ExecFunction", "ExecInternal",
	"ExecUdf", "ExecuteDataSource", "extractcol", "ExtractZip",
	"FileSupport", "FormatErrorMessage", "FormatSqlErrorMessage", "FromJson", "FromXml",
	"GetAllClientScripts", "GetAppBaseFolder", "GetAppWorkPathFolder", "GetByName", "GetClientScriptReferences",
	"GetConnectionByName", "GetConnectionStrings", "GetDataSet", "GetDataSetEx", "GetDataSetFromArray",
	"GetDataSetFromArrayEx", "GetDataSetWithSchemaFromSelect", "GetDataSetXMLFromArray", "GetDataSetXMLFromSelect",
	"GetDBMSName", "GetDBMSProviderName", "GetDecimalSep", "GetDecimalSeparator", "GetDefaultConnection",
	"GetDirFromFtp", "GetDSParameters", "GetExecutionTrace", "GetFeaturesAndNumbers", "GetFileVersion",
	"GetForbiddenAppIDs", "GetForbiddenDesignerAppIDs", "GetFormReferences", "GetFromApplication", "GetFromFtp",
	"GetFromSession", "GetGroupSeparator", "getinlinecode", "GetInstallationKey", "GetInternal", "GetInternalC",
	"GetLastSQLError", "GetLastSSLError", "GetLicenseInfoAsText", "GetLogsFolder", "GetNETDataSet", "GetNoLock",
	"GetNumberOfInstrumentConnections", "GetNumberOfNamedConcurrentUsers", "GetNumberOfNamedUsers", "GetPrinters",
	"GetRdbmsDelimiter", "getregion", "getregionex", "GetSetting", "GetSettings", "GetSSLDataset", "GetTables",
	"GetTransactionsCount", "GetUserData", "GetWebFolder",
	"HashData", "HasProperty", "Hour", "HtmlDecode", "HtmlEncode",
	"IgnoreSqlErrors", "IIf", "In64BitMode", "InBatchProcess", "InfoMes", "Integer", "IsDBConnected", "IsDefined",
	"IsDemoLicense", "IsFeatureAuthorized", "IsFeatureBasedLicense", "IsGuid", "IsHex", "IsInTransaction",
	"IsInvariantDate", "IsNumeric", "IsProductionModeOn", "IsTable", "IsTableFld",
	"JDay",
	"LCase", "LDAPAuth", "LDAPAuthEX", "lDir", "Left", "Len", "LFromHex", "LHex2Dec", "LimsAt", "LimsCleanup",
	"LIMSDate", "LimsExec", "LimsGetDateFormat", "LimsNETCast", "LimsNETConnect", "LimsNETTypeOf", "limsoleconnect",
	"LimsRecordsAffected", "LimsSetCounter", "LimsSqlConnect", "LimsSqlDisconnect", "LimsString", "LimsTime",
	"LimsType", "LimsTypeEx", "LimsXOr", "LKill", "LLower", "Lower", "LPrint", "LSearch", "LSelect", "LSelect1",
	"LSelectC", "LStr", "LToHex", "LTransform", "LTrim", "lWait",
	"MakeDateInvariant", "MakeDateLocal", "MakeDirOnFtp", "MakeNETObject", "MatFunc", "Max", "MergeGlobalResources",
	"MergeHtmlForm", "MergeXfd", "MimeDecode", "MimeEncode", "Min", "Minute", "Month", "MoveInFtp",
	"NetFrameworkVersion", "NoOfDays", "Nothing", "Now",
	"PrepareArrayForIn", "PrepareForm", "PrepareFormClientScript", "PrmCount", "ProcessXfdFormForImport",
	"RaiseError", "Rand", "Rat", "ReadBytesBase64", "ReadFromFtp", "ReadText", "RenameOnFtp", "Replace", "Replicate",
	"ResetApplication", "ResetFeatures", "RetrieveLong", "ReturnLastSQLError", "Right", "Round", "RoundPoint5",
	"RunApp", "RunDS", "RunSQL",
	"Scient", "SearchLDAPUser", "Second", "Seconds", "SendFromOutbox", "SendLimsEmail", "SendOutlookReminder",
	"SendToFtp", "SendToOutbox", "ServerEndOfDay", "ServerStartOfDay", "ServerTimeZone", "SetAmPm", "SetByName",
	"SetDecimalSeparator", "SetDefaultConnection", "SetGroupSeparator", "SetInternal", "SetInternalC",
	"SetLocationOracle", "SetLocationSQLServer", "SetSqlTimeout", "SetUserData", "SetUserPassword", "ShowSqlErrors",
	"SigFig", "SortArray", "SQLExecute", "SQLRemoveComments", "SqlTraceOff", "SqlTraceOn", "Sqrt", "StationName",
	"StdRound", "Str", "StringToDate", "StrSrch", "StrTran", "StrZero", "SubmitToBatch", "SubmitToBatchEx", "SubStr",
	"SyncDesignResources", "SyncProgramaticResources",
	"TableFldLst", "Time", "Today", "ToJson", "ToNumeric", "ToScientific", "ToXml", "TraceOff", "TraceOn", "Trim",
	"TryConnect",
	"UndeclaredVars", "UpdLong", "Upper", "UrlDecode", "UrlEncode", "UserTimeZone", "usrmes",
	"Val", "ValidateDate", "ValidateNumeric", "VerifySignature",
	"WriteBytesBase64", "WriteText", "WriteToFtp",
	"XmlDomToUdObject", "XmlExportSql",
	"Year",
}

// legacySSLClassNames contains the historical built-in class inventory.
// The public SSLClassNames slice is the canonical inventory in canonical.go.
var legacySSLClassNames = []string{
	"AzureStorage",
	"BatchSupport",
	"CDataColumn", "CDataColumns", "CDataField", "CDataRow", "CDataTable",
	"Email",
	"EnterpriseExporter", "EnterpriseImpExBase",
	"FtpsClient",
	"HtmlConverter",
	"PatcherSupport", "PdfSupport",
	"RegSetup",
	"SDMS", "SDMSDocUploader", "Sequence", "SQLConnection",
	"SSLBaseDictionary", "SSLCodeProvider", "SSLCompilerError", "SSLCompilerErrorList",
	"SSLDataset", "SSLExpando", "SSLIntDictionary", "SSLRegex", "SSLStringDictionary",
	"TablesImport",
	"WebServices",
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

// SSLKeywordDescriptions maps keywords to their descriptions.
var SSLKeywordDescriptions = map[string]string{
	"IF":              "Conditional statement - executes code block if condition is true",
	"ELSE":            "Alternative code path when IF condition is false",
	"ENDIF":           "Marks the end of an IF conditional block",
	"WHILE":           "Loop that executes while condition is true",
	"ENDWHILE":        "Marks the end of a WHILE loop",
	"FOR":             "Loop with counter variable",
	"TO":              "Specifies the upper bound of a FOR loop",
	"STEP":            "Specifies the increment for a FOR loop",
	"NEXT":            "Marks the end of a FOR loop",
	"BEGINCASE":       "Starts a CASE block where each :CASE evaluates an independent boolean expression",
	"CASE":            "Evaluates a CASE condition within a BEGINCASE block",
	"OTHERWISE":       "Runs when no earlier CASE body has executed",
	"ENDCASE":         "Marks the end of a CASE statement",
	"TRY":             "Begins a structured error handling block; requires at least one CATCH or FINALLY",
	"CATCH":           "Handles an error raised inside a TRY block; use GetLastSSLError() for details",
	"FINALLY":         "Runs cleanup code after TRY/CATCH; :RETURN, :EXITFOR, :EXITWHILE, and :LOOP are compile-time errors inside :FINALLY",
	"ENDTRY":          "Marks the end of a TRY/CATCH/FINALLY block",
	"PROCEDURE":       "Defines a reusable code procedure/function",
	"ENDPROC":         "Marks the end of a PROCEDURE",
	"PARAMETERS":      "Declares procedure parameters",
	"DEFAULT":         "Sets default value for a parameter",
	"RETURN":          "Returns a value from a procedure",
	"DECLARE":         "Declares local variables",
	"PUBLIC":          "Declares public/global variables",
	"INCLUDE":         "Includes external SSL file",
	"REGION":          "Begins a legacy functional text-capture region used with GetRegion()",
	"ENDREGION":       "Ends a legacy functional text-capture region",
	"CLASS":           "Defines a class",
	"INHERIT":         "Specifies base class for inheritance",
	"EXITFOR":         "Exits a FOR loop immediately",
	"EXITWHILE":       "Exits a WHILE loop immediately",
	"EXITCASE":        "Stops evaluating further CASE blocks in the current BEGINCASE; without it, later :CASE expressions are still evaluated and additional matching bodies may execute",
	"LOOP":            "Jump back to start of loop",
	"BEGININLINECODE": "Begins a legacy named inline-code storage block",
	"ENDINLINECODE":   "Ends a legacy named inline-code storage block",
	"ERROR":           "Legacy error handling marker; prefer TRY/CATCH/FINALLY",
	"RESUME":          "Legacy resume-mode error handling keyword; prefer TRY/CATCH/FINALLY",
	"LABEL":           "Defines a legacy Branch() target label",
}

// SSLOperatorDescriptions maps operators to their descriptions.
var SSLOperatorDescriptions = map[string]string{
	".AND.": "Logical AND operator",
	".OR.":  "Logical OR operator",
	".NOT.": "Logical NOT operator",
	":=":    "Assignment operator",
	"+=":    "Add and assign operator",
	"-=":    "Subtract and assign operator",
	"*=":    "Multiply and assign operator",
	"/=":    "Divide and assign operator",
	"%=":    "Modulo and assign operator",
	"^=":    "Power and assign operator",
	"=":     "Equality comparison operator; for strings this is loose (prefix) matching — .T. if right is empty or left starts with right",
	"==":    "Strict equality comparison operator; use for exact string equality",
	"!=":    "Not equal operator (negates ==, not =); for strings, = and != are NOT logical opposites",
	"<>":    "Not equal operator (equivalent to !=, but != is preferred)",
	">":     "Greater than comparison operator",
	"<":     "Less than comparison operator",
	">=":    "Greater than or equal comparison operator",
	"<=":    "Less than or equal comparison operator",
	"+":     "Addition operator",
	"-":     "Subtraction operator",
	"*":     "Multiplication operator",
	"/":     "Division operator",
	"%":     "Modulo operator",
	"^":     "Power/exponentiation operator",
	"**":    "Power/exponentiation operator (alias for ^)",
	"++":    "Increment operator",
	"--":    "Decrement operator",
	"<<":    "Bitwise left shift operator",
	">>":    "Bitwise right shift operator",
	"$":     "String containment operator; left $ right is .T. if left is found inside right",
	"#":     "Not equal operator (equivalent to !=, but != is preferred)",
	"!":     "Logical NOT operator (alternative to .NOT.)",
}

// SSLLiteralDescriptions maps literals to their descriptions.
var SSLLiteralDescriptions = map[string]string{
	".T.": "Boolean true literal",
	".F.": "Boolean false literal",
	"NIL": "Null/nothing literal",
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
