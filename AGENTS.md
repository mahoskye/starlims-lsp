# AGENTS.md - SSL Coding Conventions for AI Agents

This document provides essential SSL (STARLIMS Scripting Language) coding conventions for AI agents working on this codebase. Following these conventions ensures generated code is syntactically correct and stylistically consistent.

When this file conflicts with the upstream sources, they win: [`ssl-style-guide`](https://github.com/mahoskye/ssl-style-guide) for language and style rules, and the published [`starlims-ssl-reference`](https://github.com/mahoskye/starlims-ssl-reference) for element behavior (functions, classes, keywords, operators). Read them upstream, not from a local `dev/` copy, which goes stale.

**Target version:** these conventions describe STARLIMS 12 SSL as verified by the SSL reference (written for STARLIMS 11, verified on STARLIMS 12). Where the two versions are known to differ, write code that works on both; the starlims-lsp diagnostics report what fails on either ([D17](https://github.com/mahoskye/starlims-lsp/blob/main/catalog/DECISIONS.md)).

---

## Quick Reference Card

| Aspect | Rule |
|--------|------|
| Statement terminator | Every statement ends with `;` (including comments) |
| Keywords | Colon-prefixed, UPPERCASE: `:IF`, `:WHILE`, `:PROCEDURE` |
| Logical operators | Period-wrapped: `.AND.`, `.OR.`, `.NOT.` |
| Assignment | `:=` (not `=`) |
| Property access | Colon notation: `object:Property` (not `object.Property`) |
| Array indexing | **1-based** (first element is `[1]`, not `[0]`) |
| Comments | `/* comment text;` (ends with semicolon; avoid extra `;` inside comment text) |
| Procedure calls | Use `DoProc()` / `ExecFunction()` for script procedures; use `Me:Method()` / `Base:Method()` inside `:CLASS` methods |
| Parameters | `:PARAMETERS` must follow `:PROCEDURE`; `:DEFAULT` must follow `:PARAMETERS` |

---

## 1. SSL Syntax Essentials

### Keywords (Colon-Prefixed, UPPERCASE)

All SSL keywords must be prefixed with `:` and written in UPPERCASE:

```ssl
/* Control Flow;
:IF condition;
:ELSE;
:ENDIF;

:WHILE condition;
:ENDWHILE;

:FOR i := 1 :TO 10;
:NEXT;

:FOR i := 1 :TO 10 :STEP 2;
:NEXT;

:BEGINCASE;
:CASE condition;
	:EXITCASE;
:OTHERWISE;
	:EXITCASE;
:ENDCASE;

/* Error Handling;
:TRY;
:CATCH;
:FINALLY;
:ENDTRY;

/* Declarations;
:DECLARE var1, var2;
:PARAMETERS param1, param2;
:DEFAULT param1, "value";
:PUBLIC globalVar;

/* Procedures;
:PROCEDURE Name;
:ENDPROC;
:RETURN value;

/* Classes - ONE per file, NO :ENDCLASS, extends to EOF;
:CLASS ClassName;
:INHERIT Category.BaseClass;  /* Parent must be category-qualified (a bare name does not compile);

/* Stored text blocks, read at run time with GetRegion() / GetInlineCode();
:REGION RegionName;
:ENDREGION;
:BEGININLINECODE BlockName;
:ENDINLINECODE;
```

### Statement Termination

**Every statement must end with a semicolon `;`** — including comments:

```ssl
:DECLARE sName, nCount;
sName := "Test";
nCount := 42;
/* This comment also ends with semicolon;
```

### Comments

SSL uses block comment syntax that terminates with semicolon:

```ssl
/* Single-line comment;

/*
Multi-line comment: the opening marker alone on the first line,
then the text, then the semicolon alone on the last line
;

/*
 * Documentation header style
 * Procedure: DoSomething
 * Parameters: sInput - the input string
 * Returns: processed result
;
```

The first semicolon ends the comment. Do not place an extra `;` inside comment text or the remaining text becomes executable code.

### Operators

#### Assignment Operators
```ssl
x := 10;       /* Assignment;
x += 5;        /* Add and assign;
x -= 3;        /* Subtract and assign;
x *= 2;        /* Multiply and assign;
x /= 4;        /* Divide and assign;
x ^= 2;        /* Power and assign;
x %= 3;        /* Modulo and assign;
```

#### Comparison Operators
```ssl
x = y          /* Equality (loose for strings: .T. if right is empty OR left starts with right, case-sensitive);
x == y         /* Strict equality (exact match);
x != y         /* Not equal (negates ==, NOT =);
x <> y         /* Not equal (same as !=, less preferred);
x # y          /* Not equal (same as !=, less preferred);
x < y          /* Less than;
x > y          /* Greater than;
x <= y         /* Less than or equal;
x >= y         /* Greater than or equal;
```

**String equality gotcha:** `=` and `!=` are **NOT logical opposites** for strings. `=` does prefix matching, but `!=` negates `==` (exact match). Example: `"Logged" = "Log"` is `.T.` AND `"Logged" != "Log"` is also `.T.` — both true simultaneously. Always use `==` for exact string comparisons.

#### Arithmetic Operators
```ssl
x + y          /* Addition / string concatenation;
x - y          /* Subtraction / trim-trailing-spaces-then-concat for strings;
x * y          /* Multiplication;
x / y          /* Division (always floating-point: 5 / 2 = 2.5);
x % y          /* Modulo;
x ^ y          /* Exponentiation (power);
x ** y         /* Exponentiation (alias for ^);
x++;           /* Increment (prefix and postfix);
x--;           /* Decrement (prefix and postfix);
```

#### Bitwise and Shift Operators
```ssl
nResult := _AND(nA, nB);   /* Bitwise AND (function syntax, integer operands only);
nResult := _OR(nA, nB);    /* Bitwise OR;
nResult := _XOR(nA, nB);   /* Bitwise XOR;
nResult := _NOT(nA);       /* Bitwise NOT;
nResult := nA << 2;        /* Left shift;
nResult := nA >> 2;        /* Right shift;
```

#### Logical Operators (MUST include periods)
```ssl
/* CORRECT;
:IF bCondA .AND. bCondB;
:IF bCondA .OR. bCondB;
:IF .NOT. bCondA;

/* WRONG - missing periods;
:IF bCondA AND bCondB;      /* Will not work!;
```

#### String Operators
```ssl
sResult := sFirst + sSecond;     /* Concatenation;
sResult := sFirst - sSecond;     /* Trim trailing spaces from left, then concatenate;
bFound := "needle" $ "haystack"; /* Contains: .T. if left string is found within right string;
```

### Literals

```ssl
/* Boolean;
bTrue := .T.;
bFalse := .F.;

/* Null;
vValue := NIL;

/* Strings (three quote styles);
s1 := "double quotes";
s2 := 'single quotes';
s3 := [bracket quotes];  /* Useful for SQL with quotes inside;

/* Arrays (1-based!);
aItems := {"first", "second", "third"};
sFirst := aItems[1];  /* Gets "first", NOT aItems[0];

/* Dates (use functions, NOT brace syntax - {2024,12,25} is an array, not a date);
dDate := DateFromNumbers(2024, 12, 25, 14, 30, 0);
dToday := Today();
dNow := Now();
dParsed := CToD("12/25/2024");  /* Parsed with the session DateFormat (default M/d/yyyy). CToD("2024-12-25") returns an empty date under the default;
```

---

## 2. Hungarian Notation

All variables must use Hungarian notation prefixes to indicate type:

| Prefix | Type | Examples |
|--------|------|----------|
| `s` | String | `sUserName`, `sMessage`, `sSQL` |
| `n` | Numeric | `nCount`, `nTotal`, `nIndex` |
| `b` | Boolean | `bIsValid`, `bExists`, `bSuccess` |
| `d` | Date | `dStartDate`, `dCreatedOn`, `dExpiry` |
| `a` | Array | `aResults`, `aItems`, `aRows` |
| `o` | Object | `oDataset`, `oEmail`, `oConnection` |
| `fn` | Code block | `fnTransform`, `fnPredicate` |
| `v` | Variant / unknown | `vResult`, `vValue` |

### Exceptions to Hungarian Notation

1. **Loop counters**: Single letters `i`, `j`, `k`, `x`, `y`, `z` are allowed when declared (in a script that calls other scripts, prefer a specific name such as `nSampleIdx`)
2. **Constants**: Use `UPPER_SNAKE_CASE` (e.g., `MAX_RETRIES`, `DEFAULT_PATH`)
3. **Literals**: `NIL`, `.T.`, `.F.`. Acronyms inside a name (`sSQL`, `sURL`) keep their casing only where the surrounding codebase already does so

### Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Procedures | PascalCase | `CalculateTotal`, `ProcessOrder` |
| Classes | PascalCase | `InvoiceManager`, `DataHandler` |
| Variables | camelCase with prefix | `sCustomerName`, `nOrderCount` |
| Constants | UPPER_SNAKE_CASE | `MAX_RETRY_COUNT` |
| UDO properties | lowerCamelCase (no prefix) | `oOrder:orderNo`, `oState:isValid` |
| Built-in properties | PascalCase (defined by STARLIMS) | `oError:Description`, `oSeq:SequenceName` |

---

## 3. Common Anti-Patterns

### ❌ Direct Procedure Calls — NEVER DO THIS

```ssl
/* WRONG - SSL does not support direct procedure calls;
CalculateTotal(5, 10);
MyProcedure();
```

### ✅ Correct Procedure Calling

```ssl
/* Same file - use DoProc;
vResult := DoProc("CalculateTotal", {5, 10});

/* Different file entry point - use ExecFunction;
vResult := ExecFunction("Category.Script", {5, 10});

/* Different file specific procedure - use ExecFunction;
vResult := ExecFunction("Category.Script.CalculateTotal", {5, 10});

/* SSL source held in a string - use ExecUdf (arguments arrive through its own :PARAMETERS);
vResult := ExecUdf(":PARAMETERS a, b; :RETURN a + b;", {2, 3});

/* Code block value - use Eval (it runs code blocks, not source text);
vResult := Eval({|n| n * 2}, 5);

/* Skip parameters with adjacent commas (style rule, a space between them also compiles);
vResult := DoProc("MyProc", {vArg1,, vArg3});  /* Skips the second argument;

/* Omit trailing optional args entirely;
vResult := DoProc("NoArgProc");
```

### ❌ Omitting `:EXITCASE` Unintentionally

```ssl
/* WRONG - missing :EXITCASE can let later matching CASE blocks run;
:BEGINCASE;
:CASE nVal == 1;
	sLabel := "one";
:CASE nVal == 2;
	sLabel := "two";
:ENDCASE;
```

### ✅ Correct CASE Structure

```ssl
/* CORRECT - prefer :EXITCASE unless multi-match behavior is intentional;
:BEGINCASE;
:CASE nVal == 1;
	sLabel := "one";
	:EXITCASE;
:CASE nVal == 2;
	sLabel := "two";
	:EXITCASE;
:OTHERWISE;
	sLabel := "other";
	:EXITCASE;
:ENDCASE;
```

### ❌ Zero-Based Array Indexing

```ssl
/* WRONG - SSL arrays are 1-based;
sFirst := aItems[0];
```

### ✅ Correct Array Indexing

```ssl
/* CORRECT - first element is at index 1;
sFirst := aItems[1];
sLast := aItems[Len(aItems)];

/* Looping through array;
:FOR i := 1 :TO Len(aItems);
	DoProc("Process", {aItems[i]});
:NEXT;
```

### ❌ Dot Notation for Properties

```ssl
/* WRONG - SSL uses colon, not dot;
oEmail.Subject := "Test";
sXml := oDataset.ToXml();
```

### ✅ Correct Property Access

```ssl
/* CORRECT - use colon notation;
oEmail:Subject := "Test";
sXml := oDataset:ToXml();
aRows := oDataset:ToArray();
```

### ❌ Wrong Logical Operators

```ssl
/* WRONG - missing periods;
:IF x > 5 AND y < 10;
:IF NOT bFlag;
```

### ✅ Correct Logical Operators

```ssl
/* CORRECT - periods required;
:IF x > 5 .AND. y < 10;
:IF .NOT. bFlag;
```

### ❌ Using DEFAULT with DECLARE

```ssl
/* WRONG - DEFAULT only works with PARAMETERS;
:DECLARE sName;
:DEFAULT sName, "Unknown";
```

### ✅ Correct Declaration Patterns

```ssl
/* For parameters with defaults;
:PARAMETERS sName;
:DEFAULT sName, "Unknown";

/* For local variables - assign after declare;
:DECLARE sName;
sName := "Unknown";
```

### ❌ Lowercase Keywords

```ssl
/* WRONG - keywords must be uppercase;
:if condition;
:while x < 10;
:procedure MyProc;
```

### ✅ Correct Keyword Casing

```ssl
/* CORRECT - always UPPERCASE;
:IF condition;
:WHILE x < 10;
:PROCEDURE MyProc;
```

---

## 4. Database Patterns

### SQLExecute — Named Parameters (`?varName?`)

`SQLExecute` is the **only** function that supports `?varName?` syntax for automatic variable substitution:

```ssl
:DECLARE sStatus, sSampleID, aResults;
sStatus := "A";
sSampleID := "12345";

/* Variable substitution - variables must be in scope;
sSQL := "SELECT * FROM Samples WHERE Status = ?sStatus? AND SampleID = ?sSampleID?";
aResults := SQLExecute(sSQL);

/* Array expansion for IN clauses;
:DECLARE aStatusCodes;
aStatusCodes := {"A", "P", "C"};
sSQL := "SELECT * FROM Samples WHERE Status IN (?aStatusCodes?)";
/* ?aStatusCodes? becomes ?,?,? automatically (use a local array, a UDO array property raises a run-time error);

/* Object property access;
sSQL := "SELECT * FROM Users WHERE UserID = ?oUser:ID?";

/* Parameterless function calls;
sSQL := "SELECT * FROM Samples WHERE CreateDate < ?Today()?";
```

### RunSQL, LSearch, LSelect, GetDataSet — Positional Parameters (`?`)

`RunSQL`, `LSearch`, the `LSelect` family, and `GetDataSet` use positional `?` placeholders with an explicit value array (`GetSSLDataset` uses `@name` markers with parallel name and value arrays instead). Omit the connection-name argument for the default database:

```ssl
:DECLARE sSQL, bSuccess, sResult, aResults, sXml;

/* RunSQL - non-query SQL (INSERT/UPDATE/DELETE/DDL), returns boolean;
sSQL := "UPDATE Samples SET Status = ? WHERE SampleID = ?";
bSuccess := RunSQL(sSQL,, {sStatus, sSampleID});

/* LSearch - single value lookup with default;
sResult := LSearch("SELECT Name FROM Samples WHERE ID = ?", "Unknown",, {sSampleID});

/* LSelect1 - multi-row SELECT returning 2D array (values array is the 3rd argument);
aResults := LSelect1("SELECT * FROM Samples WHERE Status = ?",, {sStatus});

/* LSelect / LSelectC - same result, but the values array is the 4th argument;
aResults := LSelect("SELECT * FROM Samples WHERE Status = ?",,, {sStatus});

/* GetDataSet - SELECT returning XML dataset;
sXml := GetDataSet("SELECT * FROM Samples WHERE Status = ?", {sStatus});
```

### Database Function Summary

| Function | Parameter Style | Call shape | Use Case | Returns |
|----------|----------------|------------|----------|---------|
| `SQLExecute` | `?varName?` (named) | `SQLExecute(sSQL)` | Universal - auto-routes | Array, XML, Dataset, or Bool |
| `RunSQL` | `?` (positional) | `RunSQL(sSQL,, aValues)` | **Non-query only** (INSERT/UPDATE/DELETE/DDL; never SELECT) | Boolean |
| `LSearch` | `?` (positional) | `LSearch(sSQL, vDefault,, aValues)` | Single value lookup | Value with default |
| `LSelect` | `?` (positional) | `LSelect(sSQL,,, aValues)` | Multi-row SELECT | 2D Array |
| `LSelect1` | `?` (positional) | `LSelect1(sSQL,, aValues)` | Multi-row SELECT | 2D Array |
| `LSelectC` | `?` (positional) | `LSelectC(sSQL,,, aValues)` | Multi-row SELECT (delegates to LSelect) | 2D Array |
| `GetDataSet` | `?` (positional) | `GetDataSet(sSQL, aValues)` | SELECT to XML | XML String |

The skipped arguments are the connection name (and, for `LSelect`/`LSelectC`, an unimplemented field list that must be omitted). Writing `LSelect(sSQL,, aValues)` passes the array as the connection name.

### ❌ Wrong: Using Named Params with RunSQL

```ssl
/* WRONG - RunSQL doesn't support ?varName? syntax;
RunSQL("INSERT INTO Log VALUES(?sMessage?)");
```

### ✅ Correct: Positional Params with RunSQL

```ssl
/* CORRECT - use positional ? with value array;
RunSQL("INSERT INTO Log(Message) VALUES(?)",, {sMessage});
```

---

## 5. Procedure Structure

### Standard Procedure Template

```ssl
/*
 * Procedure: ProcedureName
 * Description: Brief description of what it does
 * Parameters:
 *   sInput - description
 *   nValue - description
 * Returns: description of return value
;
:PROCEDURE ProcedureName;
	:PARAMETERS sInput, nValue;
	:DEFAULT sInput, "";
	:DEFAULT nValue, 0;
	:DECLARE sResult, nCount, aItems, i;

	/* Initialization;
	sResult := "";
	nCount := 0;

	/* Main logic;
	:TRY;
		aItems := SQLExecute("SELECT ItemID FROM Items WHERE Category = ?sInput?");

		:IF Len(aItems) > 0;
			:FOR i := 1 :TO Len(aItems);
				nCount += 1;
			:NEXT;
		:ENDIF;

		sResult := "Processed " + LimsString(nCount) + " items";
	:CATCH;
		sResult := "Error: " + GetLastSSLError():Description;
	:ENDTRY;

	:RETURN sResult;
:ENDPROC;
```

### Order of Statements

1. `:PARAMETERS` (if any)
2. `:DEFAULT` values (if any)
3. `:DECLARE` local variables
4. Initialization
5. Main logic
6. `:RETURN` (if applicable)
7. `:ENDPROC;`

Placement rules:
- Script-level `:PARAMETERS` must be the first statement of the script (an `:INCLUDE` may precede it). The LSP tolerates leading `:PROCEDURE` blocks before it, but the style guide treats that layout as wrong
- Inside a procedure, `:PARAMETERS` must immediately follow `:PROCEDURE`
- `:DEFAULT` must immediately follow `:PARAMETERS`

---

## 6. Object Creation

### Built-in Classes (Curly Braces)

```ssl
/* Built-in SSL classes use curly braces;
oEmail := Email{};     /* Failures return .F. and set oEmail:Exception;
oEmail := Email{.F.};  /* Failures raise, handle them with :TRY;
oRegex := SSLRegex{'\d+'};
oDataset := SSLDataset{};
oDict := SSLStringDictionary{};
```

### User-Defined Objects (CreateUdObject)

```ssl
/* Custom classes use CreateUdObject with the category-qualified class name;
oCustom := CreateUdObject("Category.MyClass");
oCustom := CreateUdObject("Category.MyClass", {vArg1, vArg2});

/* Empty dynamic object;
oAnon := CreateUdObject();
oAnon:property1 := "value";
oAnon:property2 := 123;

/* Anonymous object with named properties;
oSeeded := CreateUdObject({{"property1", "value"}, {"property2", 123}});
```

---

## 7. Function Casing Reference

SSL functions are case-insensitive but should use documented casing for consistency:

### Array Functions (canonical PascalCase)
```ssl
AAdd(aArray, value);
ALen(aArray);
AScan(aArray, value);
AScanExact(aArray, value);
AEval(aArray, codeBlock);
```

### String Functions (PascalCase)
```ssl
AllTrim(sString);
SubStr(sString, nStart, nLen);
StrTran(sString, sFind, sReplace);
Left(sString, nCount);
Right(sString, nCount);
Upper(sString);
Lower(sString);
```

### Type/Conversion Functions (PascalCase)
```ssl
LimsString(value);     /* General value-to-string conversion;
Str(nValue, 6, 2);    /* Numeric formatting with width/decimals;
LimsTypeEx(value);
Empty(value);
Val(sString);
Chr(nAscii);
```

### Database Functions (PascalCase)
```ssl
SQLExecute(sSQL);
RunSQL(sSQL, sFriendlyName, aParams);
LSearch(sSQL, defaultValue, sFriendlyName, aParams);
GetDataSet(sSQL, aParams);
```

### Date Functions (PascalCase)
```ssl
Today();
Now();
DateAdd(dDate, nNumber, sDatePart);
DateDiff(dStart, dEnd, sDatePart);
DateToString(dDate, sFormat);
LIMSDate(vDate, sFormat);     /* Note: LIMSDate, not LimsDate;
DOW(dDate);                   /* Day of week (1=Sunday to 7=Saturday);
DOY(dDate);                   /* Day of year;
```

### Canonical Casing Exceptions
Most functions use PascalCase, but these exceptions must be preserved exactly:
```ssl
_AND(nA, nB);     /* Bitwise AND (not "And");
_OR(nA, nB);      /* Bitwise OR (not "Or");
_XOR(nA, nB);     /* Bitwise XOR (not "Xor");
_NOT(nA);         /* Bitwise NOT (not "Not");
DOW(dDate);       /* Day of week (not "Dow");
DOY(dDate);       /* Day of year (not "Doy");
LIMSDate(vDate);  /* Date formatting (not "LimsDate");
```

---

## 8. Error Handling

### Preferred: TRY/CATCH/FINALLY

```ssl
:TRY;
	/* Risky operations;
	aResults := SQLExecute(sSQL);
	:IF Len(aResults) = 0;
		RaiseError("No data found");
	:ENDIF;
:CATCH;
	oErr := GetLastSSLError();
	sErrMsg := "Error: " + oErr:Description;
	ErrorMes("Error", sErrMsg);  /* Caption first, then message. Writes the server log, not a UI message;
	:RETURN .F.;
:FINALLY;
	/* Cleanup - always runs;
	:IF .NOT. Empty(oConnection);
		oConnection := NIL;
	:ENDIF;
:ENDTRY;
```

Important restrictions:
- `:TRY` must include at least one statement before `:CATCH` or `:FINALLY`
- A `:FINALLY` body must also contain at least one statement (a `:CATCH` body may be empty)
- At least one of `:CATCH` or `:FINALLY` is required
- `:CATCH` does not bind an exception variable; call `GetLastSSLError()` inside the block
- `:RETURN`, `:EXITFOR`, `:EXITWHILE`, and `:LOOP` are compile-time errors inside `:FINALLY`

---

## Summary: Top 20 SSL Rules

1. **Semicolons everywhere** — Every statement, every comment
2. **Colon-prefix keywords** — `:IF`, `:WHILE`, `:PROCEDURE`, etc.
3. **UPPERCASE keywords** — Never `:if` or `:while`
4. **Period-wrapped logical ops** — `.AND.`, `.OR.`, `.NOT.`
5. **Hungarian notation** — `sName`, `nCount`, `bFlag`, `aItems`, `oObject`
6. **DoProc/ExecFunction** — Never call procedures directly
7. **Prefer `:EXITCASE;`** — Use it unless multi-match CASE behavior is intentional
8. **1-based arrays** — First element is `[1]`
9. **Colon property access** — `object:Property`, not `object.Property`
10. **SQLExecute vs others** — Only SQLExecute uses `?varName?` syntax
11. **`=` vs `==` for strings** — `=` does prefix matching; `!=` negates `==` not `=`; always use `==` for exact match
12. **`:NEXT` not `:ENDFOR`** — FOR loops end with `:NEXT;` (`:ENDFOR` is invalid)
13. **`:DEFAULT` only with `:PARAMETERS`** — Never on a `:DECLARE` line
14. **Declared variables start as `""`** — Empty string, not NIL
15. **Curly braces for built-ins** — `Email{}`, `SSLDataset{}`; never `CreateUdObject("Email")`. User-defined classes use `CreateUdObject("Category.ClassName")`
16. **Dates are not brace literals** — `{2024,12,25}` is an array; use `DateFromNumbers()` or `CToD()`
17. **No bare-name DoProc in classes** — `DoProc("Name")` inside a class method is a compile error; use `Me:Method()` / `Base:Method()` inside `:CLASS`
18. **Skipped params: adjacent commas** — write `DoProc("P", {a,,c})`, not `DoProc("P", {a, , c})` (both compile; adjacent commas are the style rule)
19. **Scientific notation needs decimal** — `7.0e2` not `7e2`
20. **`:FINALLY` restrictions** — No `:RETURN`, `:EXITFOR`, `:EXITWHILE`, `:LOOP` inside `:FINALLY`
