// Package constants defines SSL language keywords, operators, functions, and classes.
package constants

import "slices"

// SSLKeywords contains all SSL language keywords (37 total).
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
	"REGION", "RETURN",
	"STEP",
	"TO", "TRY",
	"WHILE",
}

// BlockStartKeywords are keywords that start a block.
var BlockStartKeywords = []string{
	"IF", "WHILE", "FOR", "BEGINCASE", "TRY", "PROCEDURE", "CLASS", "REGION", "BEGININLINECODE",
}

// BlockEndKeywords are keywords that end a block.
var BlockEndKeywords = []string{
	"ENDIF", "ENDWHILE", "NEXT", "ENDCASE", "ENDTRY", "ENDPROC", "ENDREGION", "ENDINLINECODE",
}

// BlockMiddleKeywords are keywords that appear in the middle of blocks.
var BlockMiddleKeywords = []string{"ELSE", "CATCH", "FINALLY", "CASE", "OTHERWISE"}

// CaseKeywords are keywords used in CASE statements.
var CaseKeywords = []string{"CASE", "OTHERWISE"}

// ProcedureLevelKeywords are keywords valid at procedure level.
var ProcedureLevelKeywords = []string{"PARAMETERS", "DEFAULT", "PUBLIC", "DECLARE"}

// SSLOperators contains all SSL operators (27 total).
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
	"+", "-", "*", "/", "%", "^",
	// Special
	"$", "#",
}

// SSLLogicalOperators are the logical operators.
var SSLLogicalOperators = []string{".AND.", ".OR.", ".NOT."}

// SSLCompoundOperators are compound assignment operators.
var SSLCompoundOperators = []string{":=", "+=", "-=", "*=", "/=", "%=", "^="}

// SSLLiterals contains boolean and null literal values (3 total).
var SSLLiterals = []string{".T.", ".F.", "NIL"}

// SSLLiteralAliases maps alternative forms to canonical forms.
var SSLLiteralAliases = map[string]string{
	".t.":   ".T.",
	".f.":   ".F.",
	"nil":   "NIL",
	"true":  ".T.",
	"false": ".F.",
}

// SSLFunctionNames contains all 367 SSL function names.
var SSLFunctionNames = []string{
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

// SSLClassNames contains all 30 SSL class names.
var SSLClassNames = []string{
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

// InlineSQLFunctions are functions that take inline SQL.
var InlineSQLFunctions = []string{
	"SQLExecute",
	"GetDataSet",
	"GetDataSetWithSchemaFromSelect",
	"GetDataSetXMLFromSelect",
	"GetNETDataSet",
}

// ParameterizedSQLFunctions are functions that take parameterized SQL.
var ParameterizedSQLFunctions = []string{
	"RunSQL",
	"LSearch",
	"LSelect",
	"LSelect1",
	"LSelectC",
	"GetDataSetEx",
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
	"BEGINCASE":       "Start of a CASE statement for multiple conditions",
	"CASE":            "Individual condition in a CASE statement",
	"OTHERWISE":       "Default case when no other CASE conditions match",
	"ENDCASE":         "Marks the end of a CASE statement",
	"TRY":             "Begin error handling block",
	"CATCH":           "Handle errors from TRY block",
	"FINALLY":         "Code that always executes after TRY/CATCH",
	"ENDTRY":          "Marks the end of TRY/CATCH block",
	"PROCEDURE":       "Defines a reusable code procedure/function",
	"ENDPROC":         "Marks the end of a PROCEDURE",
	"PARAMETERS":      "Declares procedure parameters",
	"DEFAULT":         "Sets default value for a parameter",
	"RETURN":          "Returns a value from a procedure",
	"DECLARE":         "Declares local variables",
	"PUBLIC":          "Declares public/global variables",
	"INCLUDE":         "Includes external SSL file",
	"REGION":          "Marks the beginning of a code region",
	"ENDREGION":       "Marks the end of a code region",
	"CLASS":           "Defines a class",
	"INHERIT":         "Specifies base class for inheritance",
	"EXITFOR":         "Exits a FOR loop immediately",
	"EXITWHILE":       "Exits a WHILE loop immediately",
	"EXITCASE":        "Exits a CASE statement immediately",
	"LOOP":            "Jump back to start of loop",
	"BEGININLINECODE": "Start of inline code block",
	"ENDINLINECODE":   "End of inline code block",
	"ERROR":           "Error handling keyword",
	"LABEL":           "Defines a label for GOTO",
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
	"=":     "Equality comparison operator",
	"==":    "Strict equality comparison operator",
	"!=":    "Not equal comparison operator",
	"<>":    "Not equal comparison operator (legacy)",
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
	"$":     "String containment operator",
	"#":     "Not equal operator (alternative to <>)",
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

// IsSSLLiteral checks if a string is an SSL literal.
func IsSSLLiteral(s string) bool {
	return slices.Contains(SSLLiterals, s)
}

// IsSSLFunction checks if a string is an SSL function name.
func IsSSLFunction(s string) bool {
	return slices.Contains(SSLFunctionNames, s)
}

// IsSSLClass checks if a string is an SSL class name.
func IsSSLClass(s string) bool {
	return slices.Contains(SSLClassNames, s)
}
