package providers

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/parser"
)

// CompletionItemKind represents the kind of a completion item.
type CompletionItemKind int

const (
	CompletionKindText          CompletionItemKind = 1
	CompletionKindMethod        CompletionItemKind = 2
	CompletionKindFunction      CompletionItemKind = 3
	CompletionKindConstructor   CompletionItemKind = 4
	CompletionKindField         CompletionItemKind = 5
	CompletionKindVariable      CompletionItemKind = 6
	CompletionKindClass         CompletionItemKind = 7
	CompletionKindInterface     CompletionItemKind = 8
	CompletionKindModule        CompletionItemKind = 9
	CompletionKindProperty      CompletionItemKind = 10
	CompletionKindUnit          CompletionItemKind = 11
	CompletionKindValue         CompletionItemKind = 12
	CompletionKindEnum          CompletionItemKind = 13
	CompletionKindKeyword       CompletionItemKind = 14
	CompletionKindSnippet       CompletionItemKind = 15
	CompletionKindColor         CompletionItemKind = 16
	CompletionKindFile          CompletionItemKind = 17
	CompletionKindReference     CompletionItemKind = 18
	CompletionKindFolder        CompletionItemKind = 19
	CompletionKindEnumMember    CompletionItemKind = 20
	CompletionKindConstant      CompletionItemKind = 21
	CompletionKindStruct        CompletionItemKind = 22
	CompletionKindEvent         CompletionItemKind = 23
	CompletionKindOperator      CompletionItemKind = 24
	CompletionKindTypeParameter CompletionItemKind = 25
)

// InsertTextFormat represents the format of inserted text.
type InsertTextFormat int

const (
	InsertTextFormatPlainText InsertTextFormat = 1
	InsertTextFormatSnippet   InsertTextFormat = 2
)

// CompletionItem represents a completion item.
type CompletionItem struct {
	Label            string
	Kind             CompletionItemKind
	Detail           string
	Documentation    string
	InsertText       string
	InsertTextFormat InsertTextFormat
}

// GetKeywordCompletions returns keyword completions.
func GetKeywordCompletions() []CompletionItem {
	var items []CompletionItem
	for _, keyword := range constants.SSLKeywords {
		desc := constants.SSLKeywordDescriptions[keyword]
		if desc == "" {
			desc = fmt.Sprintf("SSL keyword: %s", keyword)
		}
		items = append(items, CompletionItem{
			Label:            ":" + keyword,
			Kind:             CompletionKindKeyword,
			Detail:           "SSL Keyword",
			Documentation:    desc,
			InsertText:       ":" + keyword,
			InsertTextFormat: InsertTextFormatPlainText,
		})
	}
	return items
}

// GetFunctionCompletions returns function completions.
func GetFunctionCompletions() []CompletionItem {
	var items []CompletionItem
	for _, fnName := range constants.SSLFunctionNames {
		items = append(items, buildFunctionCompletion(fnName, false))
	}
	return items
}

// GetFunctionSnippetCompletions returns function snippet completions with parameter placeholders.
func GetFunctionSnippetCompletions() []CompletionItem {
	var items []CompletionItem
	for _, fnName := range constants.SSLFunctionNames {
		items = append(items, buildFunctionCompletion(fnName, true))
	}
	return items
}

func buildFunctionCompletion(fnName string, useSnippet bool) CompletionItem {
	detail := "SSL Function"
	doc := fmt.Sprintf("Built-in SSL function: %s", fnName)
	insertText := fnName
	label := fnName
	kind := CompletionKindFunction
	format := InsertTextFormatPlainText

	if useSnippet {
		label = fnName + "()"
		kind = CompletionKindSnippet
		format = InsertTextFormatSnippet
		insertText = fmt.Sprintf("%s($0)", fnName)
	}

	// Try to get richer info from signatures
	if sig, ok := constants.GetFunctionSignature(fnName); ok {
		docInfo := buildFunctionDoc(sig)
		detail = docInfo.Detail
		doc = docInfo.Documentation
		if useSnippet {
			insertText = buildFunctionSnippet(fnName, sig)
		}
	}

	return CompletionItem{
		Label:            label,
		Kind:             kind,
		Detail:           detail,
		Documentation:    doc,
		InsertText:       insertText,
		InsertTextFormat: format,
	}
}

// GetClassCompletions returns class completions.
func GetClassCompletions() []CompletionItem {
	var items []CompletionItem
	for _, className := range constants.SSLClassNames {
		items = append(items, CompletionItem{
			Label:            className,
			Kind:             CompletionKindClass,
			Detail:           "SSL Class",
			Documentation:    fmt.Sprintf("Built-in SSL class: %s", className),
			InsertText:       className,
			InsertTextFormat: InsertTextFormatPlainText,
		})
	}
	return items
}

// GetLiteralCompletions returns literal completions.
func GetLiteralCompletions() []CompletionItem {
	var items []CompletionItem
	for _, lit := range constants.SSLLiterals {
		desc := constants.SSLLiteralDescriptions[lit]
		if desc == "" {
			desc = fmt.Sprintf("SSL literal: %s", lit)
		}
		items = append(items, CompletionItem{
			Label:            lit,
			Kind:             CompletionKindConstant,
			Detail:           "SSL Literal",
			Documentation:    desc,
			InsertText:       lit,
			InsertTextFormat: InsertTextFormatPlainText,
		})
	}
	return items
}

// GetOperatorCompletions returns operator completions.
func GetOperatorCompletions() []CompletionItem {
	var items []CompletionItem
	for _, op := range constants.SSLLogicalOperators {
		desc := constants.SSLOperatorDescriptions[op]
		if desc == "" {
			desc = fmt.Sprintf("SSL operator: %s", op)
		}
		items = append(items, CompletionItem{
			Label:            op,
			Kind:             CompletionKindOperator,
			Detail:           "Logical Operator",
			Documentation:    desc,
			InsertText:       op,
			InsertTextFormat: InsertTextFormatPlainText,
		})
	}
	return items
}

// GetProcedureCompletions returns procedure completions from the current document.
func GetProcedureCompletions(procedures []parser.ProcedureInfo, classMethodContext bool) []CompletionItem {
	var items []CompletionItem
	for _, proc := range procedures {
		paramsDoc := "*No parameters*"
		if len(proc.Parameters) > 0 {
			paramsDoc = fmt.Sprintf("**Parameters:** %s", strings.Join(proc.Parameters, ", "))
		}

		dispatchDoc := "Use `DoProc(...)` for same-file script procedures, or `ExecFunction(...)` for external script procedures."
		if classMethodContext {
			dispatchDoc = "Inside `:CLASS` methods, call sibling or inherited members with `Me:MethodName(...)` or `Base:MethodName(...)`."
		}

		doc := fmt.Sprintf("**Procedure:** %s\n\n%s\n\n%s\n\n**Location:** Line %d-%d",
			proc.Name, paramsDoc, dispatchDoc, proc.StartLine, proc.EndLine)

		items = append(items, CompletionItem{
			Label:            proc.Name,
			Kind:             CompletionKindFunction,
			Detail:           fmt.Sprintf("Procedure (line %d)", proc.StartLine),
			Documentation:    doc,
			InsertText:       buildProcedureDispatchSnippet(proc, classMethodContext),
			InsertTextFormat: InsertTextFormatSnippet,
		})
	}
	return items
}

func buildProcedureDispatchSnippet(proc parser.ProcedureInfo, classMethodContext bool) string {
	callTarget := "DoProc"
	if classMethodContext {
		callTarget = "Me:" + proc.Name
	}

	if len(proc.Parameters) == 0 {
		if classMethodContext {
			return fmt.Sprintf(`%s()`, callTarget)
		}
		return fmt.Sprintf(`DoProc("%s")`, proc.Name)
	}

	placeholders := make([]string, 0, len(proc.Parameters))
	for i, param := range proc.Parameters {
		placeholders = append(placeholders, fmt.Sprintf("${%d:%s}", i+1, param))
	}

	if classMethodContext {
		return fmt.Sprintf(`%s(%s)`, callTarget, strings.Join(placeholders, ", "))
	}

	return fmt.Sprintf(`DoProc("%s", {%s})`, proc.Name, strings.Join(placeholders, ", "))
}

// GetVariableCompletions returns variable completions from the current document.
func GetVariableCompletions(variables []parser.VariableInfo) []CompletionItem {
	var items []CompletionItem
	for _, v := range variables {
		items = append(items, CompletionItem{
			Label:            v.Name,
			Kind:             CompletionKindVariable,
			Detail:           fmt.Sprintf("%s variable", v.Scope),
			Documentation:    fmt.Sprintf("Declared at line %d", v.Line),
			InsertText:       v.Name,
			InsertTextFormat: InsertTextFormatPlainText,
		})
	}
	return items
}

// GetAllCompletions returns all completions.
func GetAllCompletions(procedures []parser.ProcedureInfo, variables []parser.VariableInfo, classMethodContext bool) []CompletionItem {
	var items []CompletionItem
	items = append(items, GetKeywordCompletions()...)
	items = append(items, GetFunctionCompletions()...)
	items = append(items, GetClassCompletions()...)
	items = append(items, GetLiteralCompletions()...)
	if classMethodContext {
		items = append(items,
			CompletionItem{
				Label:            "Me",
				Kind:             CompletionKindKeyword,
				Detail:           "Class Self Reference",
				Documentation:    "Refers to the current object instance inside a `:CLASS` method.",
				InsertText:       "Me",
				InsertTextFormat: InsertTextFormatPlainText,
			},
			CompletionItem{
				Label:            "Base",
				Kind:             CompletionKindKeyword,
				Detail:           "Parent Class Reference",
				Documentation:    "Refers to inherited members inside a `:CLASS` method and must be used as `Base:Member`.",
				InsertText:       "Base",
				InsertTextFormat: InsertTextFormatPlainText,
			},
			CompletionItem{
				Label:            "Constructor",
				Kind:             CompletionKindConstructor,
				Detail:           "Reserved Constructor Name",
				Documentation:    "Reserved method name used for class constructors inside `:CLASS`.",
				InsertText:       "Constructor",
				InsertTextFormat: InsertTextFormatPlainText,
			},
		)
	}
	items = append(items, GetOperatorCompletions()...)
	items = append(items, GetProcedureCompletions(procedures, classMethodContext)...)
	items = append(items, GetVariableCompletions(variables)...)
	return items
}

// GetSnippetCompletions returns common SSL code snippets.
func GetSnippetCompletions() []CompletionItem {
	return []CompletionItem{
		{
			Label:         "proc",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL Procedure",
			Documentation: "Create a new procedure",
			InsertText: `/*
 * Procedure: ${1:ProcedureName}
 * Description: ${2:Brief description}
 * Parameters:
 * Returns: -
;
:PROCEDURE ${1:ProcedureName};
	${0}
:ENDPROC;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "procparams",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL Procedure with Parameters",
			Documentation: "Create a new procedure with parameters",
			InsertText: `/*
 * Procedure: ${1:ProcedureName}
 * Description: ${2:Brief description}
 * Parameters:
 *   ${3:sParam1} - ${4:Description}
 * Returns: ${5:sResult} - ${6:Description}
;
:PROCEDURE ${1:ProcedureName};
	:PARAMETERS ${3:sParam1};
	:DEFAULT ${3:sParam1}, "";
	:DECLARE ${5:sResult};
	${5:sResult} := "";
	${0}
	:RETURN ${5:sResult};
:ENDPROC;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "if",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL If Statement",
			Documentation: "Create an if statement",
			InsertText: `:IF ${1:condition};
	${0}
:ENDIF;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "ifelse",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL If-Else Statement",
			Documentation: "Create an if-else statement",
			InsertText: `:IF ${1:condition};
	${2}
:ELSE;
	${0}
:ENDIF;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "while",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL While Loop",
			Documentation: "Create a while loop",
			InsertText: `:WHILE ${1:condition};
	${0}
:ENDWHILE;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "for",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL For Loop",
			Documentation: "Create a for loop",
			InsertText: `:FOR ${1:i} := ${2:1} :TO ${3:10};
	${0}
:NEXT;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "forstep",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL For Loop with Step",
			Documentation: "Create a for loop with custom step",
			InsertText: `:FOR ${1:i} := ${2:1} :TO ${3:10} :STEP ${4:2};
	${0}
:NEXT;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "try",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL Try-Catch",
			Documentation: "Create a try-catch block",
			InsertText: `:TRY;
	${1}
:CATCH;
	${0}
:ENDTRY;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "tryfinally",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL Try-Catch-Finally",
			Documentation: "Create a try-catch-finally block",
			InsertText: `:TRY;
	${1}
:CATCH;
	${2}
:FINALLY;
	${0}
:ENDTRY;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "case",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL Case Statement",
			Documentation: "Create a case statement",
			InsertText: `:BEGINCASE;
:CASE ${1:condition1};
	${2}
	:EXITCASE;
:CASE ${3:condition2};
	${4}
	:EXITCASE;
:OTHERWISE;
	${0}
	:EXITCASE;
:ENDCASE;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "sql",
			Kind:          CompletionKindSnippet,
			Detail:        "SQL Execute",
			Documentation: "Execute a SQL query",
			InsertText: `aResults := SQLExecute("
    SELECT ${1:*}
    FROM ${2:table_name}
    WHERE ${3:column_name} = ?${4:sValue}?
");`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:            "doproc",
			Kind:             CompletionKindSnippet,
			Detail:           "DoProc Call",
			Documentation:    "Call a procedure",
			InsertText:       `DoProc("${1:ProcedureName}", {${0}});`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:            "declare",
			Kind:             CompletionKindSnippet,
			Detail:           "Declare Variable",
			Documentation:    "Declare a local variable",
			InsertText:       `:DECLARE ${0:sValue};`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:            "public",
			Kind:             CompletionKindSnippet,
			Detail:           "Public Variable",
			Documentation:    "Declare a public variable",
			InsertText:       `:PUBLIC ${0:sGlobalValue};`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "catchssl",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL Error Handling",
			Documentation: "Create an SSL error handling block",
			InsertText: `:TRY;
	${1}
:CATCH;
	:DECLARE ${2:oSslErr};
	${2:oSslErr} := GetLastSSLError();
	:IF ${2:oSslErr} != NIL;
		${0}
		ClearLastSSLError();
	:ENDIF;
:ENDTRY;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "catchsql",
			Kind:          CompletionKindSnippet,
			Detail:        "SQL Error Handling",
			Documentation: "Create a SQL error handling block",
			InsertText: `:TRY;
	${1}
:CATCH;
	:DECLARE ${2:oSqlErr};
	${2:oSqlErr} := GetLastSQLError();
	:IF ${2:oSqlErr} != NIL;
		${0}
	:ENDIF;
:ENDTRY;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "region",
			Kind:          CompletionKindSnippet,
			Detail:        "Comment Region",
			Documentation: "Create a comment-based editor region",
			InsertText: `/* region ${1:RegionName};
	${0}
/* endregion;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "inlinecode",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL Inline Code",
			Documentation: "Create an inline code block",
			InsertText: `:BEGININLINECODE "${1:BlockName}";
	${0}
:ENDINLINECODE;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "class",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL Class Definition",
			Documentation: "Define a new class",
			InsertText: `:CLASS ${1:ClassName};
	:INHERIT ${2:BaseClass};

	:DECLARE ${3:sValue};

	:PROCEDURE ${4:MethodName};
	${5}
:ENDPROC;

:PROCEDURE Constructor;
	${0}
:ENDPROC;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "classctor",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL Class with Constructor",
			Documentation: "Define a new class with a constructor",
			InsertText: `:CLASS ${1:ClassName};
	:INHERIT ${2:BaseClass};

	:DECLARE ${3:sValue};

	:PROCEDURE ${4:MethodName};
	Me:${3:sValue} := ${5:""};
:ENDPROC;

:PROCEDURE Constructor;
	${0}
:ENDPROC;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "constructor",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL Constructor",
			Documentation: "Create a constructor inside a class",
			InsertText: `:PROCEDURE Constructor;
	${0}
:ENDPROC;`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "expando",
			Kind:          CompletionKindSnippet,
			Detail:        "SSL Expando Object",
			Documentation: "Create a new Expando object",
			InsertText: `:DECLARE ${1:oObj};
	${1:oObj} := CreateUdObject();
	${1:oObj}:AddProperty("${2:PropertyName}");
	${0}`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:            "inherit",
			Kind:             CompletionKindSnippet,
			Detail:           "Inherit Class",
			Documentation:    "Inherit from a parent class",
			InsertText:       `:INHERIT ${1:ParentClass};`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:            "include",
			Kind:             CompletionKindSnippet,
			Detail:           "Include Script",
			Documentation:    "Include another script",
			InsertText:       `:INCLUDE ${1:Category.ScriptName};`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:            "raiseerror",
			Kind:             CompletionKindSnippet,
			Detail:           "Raise Error",
			Documentation:    "Raise a custom error",
			InsertText:       `RaiseError("${1:Description}", "${2:Location}", ${3:10001});`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:         "limsole",
			Kind:          CompletionKindSnippet,
			Detail:        "LIMS OLE Connection",
			Documentation: "Connect to an OLE object",
			InsertText: `:DECLARE ${1:oOle};
	${1:oOle} := LimsOleConnect("${2:ProgID}");
	${0}
	EndLimsOleConnect(${1:oOle});`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
		{
			Label:            "rsql",
			Kind:             CompletionKindSnippet,
			Detail:           "RunSQL",
			Documentation:    "Execute a SQL command",
			InsertText:       `bSuccess := RunSQL("${1:UPDATE table SET column = ? WHERE id = ?}",, {${2:values}});`,
			InsertTextFormat: InsertTextFormatSnippet,
		},
	}
}
