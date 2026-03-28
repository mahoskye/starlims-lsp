# AGENTS.md - SSL Coding Conventions for AI Agents

This document provides essential SSL (STARLIMS Scripting Language) coding conventions for AI agents working on this codebase. Following these conventions ensures generated code is syntactically correct and stylistically consistent.

When this file conflicts with anything under `dev/ssl-style-guide/`, treat `dev/ssl-style-guide/` as the source of truth.

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
:INHERIT BaseClass;

/* Legacy functional text blocks;
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

/* Multi-line comment
   spanning multiple lines
   still ends with semicolon;

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
x = y          /* Equality (loose for strings: .T. if right is empty OR left starts with right);
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
xValue := NIL;

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
dParsed := CToD("2024-12-25");
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

1. **Loop counters**: Single letters `i`, `j`, `k`, `x`, `y`, `z` are allowed
2. **Constants**: Use `UPPER_SNAKE_CASE` (e.g., `MAX_RETRIES`, `DEFAULT_PATH`)
3. **Special identifiers**: `NIL`, `.T.`, `.F.`, `ID`, `SQL`, `URL`, `XML`

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
result := DoProc("CalculateTotal", {5, 10});

/* Different file entry point - use ExecFunction;
result := ExecFunction("Category.Script", {5, 10});

/* Different file specific procedure - use ExecFunction;
result := ExecFunction("Category.Script.CalculateTotal", {5, 10});

/* User-defined functions - use ExecUdf;
result := ExecUdf("MyUdfName", {param1, param2});

/* Dynamic code execution - use Eval;
result := Eval("1 + 2");

/* Skip parameters with empty array positions;
result := DoProc("MyProc", {param1,, param3});  /* Skips param2;

/* Omit trailing optional args entirely;
result := DoProc("NoArgProc");
```

### ❌ Omitting `:EXITCASE` Unintentionally

```ssl
/* WRONG - missing :EXITCASE can let later matching CASE blocks run;
:BEGINCASE;
:CASE nVal == 1;
    DoSomething();
:CASE nVal == 2;
    DoOther();
:ENDCASE;
```

### ✅ Correct CASE Structure

```ssl
/* CORRECT - prefer :EXITCASE unless multi-match behavior is intentional;
:BEGINCASE;
:CASE nVal == 1;
    DoSomething();
    :EXITCASE;
:CASE nVal == 2;
    DoOther();
    :EXITCASE;
:OTHERWISE;
    DoDefault();
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
nCount := oDataset.RowCount;
```

### ✅ Correct Property Access

```ssl
/* CORRECT - use colon notation;
oEmail:Subject := "Test";
nCount := oDataset:RowCount;
sValue := oDataset:GetValue(1, "FieldName");
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
/* ?aStatusCodes? becomes ?,?,? automatically;

/* Object property access;
sSQL := "SELECT * FROM Users WHERE UserID = ?oUser:ID?";

/* Parameterless function calls;
sSQL := "SELECT * FROM Samples WHERE CreateDate < ?Today()?";
```

### RunSQL, LSearch, GetDataSet — Positional Parameters (`?`)

All other database functions use positional `?` placeholders with explicit value arrays:

```ssl
:DECLARE sSQL, bSuccess, sResult, aResults;

/* RunSQL - for INSERT/UPDATE/DELETE, returns boolean;
sSQL := "UPDATE Samples SET Status = ? WHERE SampleID = ?";
bSuccess := RunSQL(sSQL,, {sStatus, sSampleID});

/* LSearch - single value lookup with default;
sResult := LSearch("SELECT Name FROM Samples WHERE ID = ?", "Unknown",, {sSampleID});

/* LSelect1 - multi-row SELECT returning 2D array;
aResults := LSelect1("SELECT * FROM Samples WHERE Status = ?",, {sStatus});

/* GetDataSet - SELECT returning XML dataset;
sXml := GetDataSet("SELECT * FROM Samples WHERE Status = ?", {sStatus});
```

### Database Function Summary

| Function | Parameter Style | Use Case | Returns |
|----------|----------------|----------|---------|
| `SQLExecute` | `?varName?` (named) | Universal - auto-routes | Array, XML, Dataset, or Bool |
| `RunSQL` | `?` (positional) | **DML only** (INSERT/UPDATE/DELETE) | Boolean |
| `LSearch` | `?` (positional) | Single value lookup | Value with default |
| `LSelect` | `?` (positional) | Multi-row SELECT | 2D Array |
| `LSelect1` | `?` (positional) | Multi-row SELECT | 2D Array |
| `LSelectC` | `?` (positional) | Multi-row SELECT (delegates to LSelect) | 2D Array |
| `GetDataSet` | `?` (positional) | SELECT to XML | XML String |

### ❌ Wrong: Using Named Params with RunSQL

```ssl
/* WRONG - RunSQL doesn't support ?varName? syntax;
RunSQL("INSERT INTO Log VALUES(?sMessage?)", "", {});
```

### ✅ Correct: Positional Params with RunSQL

```ssl
/* CORRECT - use positional ? with value array;
RunSQL("INSERT INTO Log(Message) VALUES(?)", "", {sMessage});
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
:DECLARE sResult, nCount, aItems;

/* Initialization;
sResult := "";
nCount := 0;

/* Main logic;
:TRY;
    /* Your code here;
    aItems := SQLExecute("SELECT * FROM Table WHERE Field = ?sInput?");
    
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
- Script-level `:PARAMETERS` must appear before top-level executable statements, though leading `:PROCEDURE` blocks may come first
- Inside a procedure, `:PARAMETERS` must immediately follow `:PROCEDURE`
- `:DEFAULT` must immediately follow `:PARAMETERS`

---

## 6. Object Creation

### Built-in Classes (Curly Braces)

```ssl
/* Built-in SSL classes use curly braces;
oEmail := Email{};
oEmail := Email{.T.};  /* With parameter;
oRegex := SSLRegex{'\d+'};
oDataset := SSLDataset{};
oDict := SSLStringDictionary{};
```

### User-Defined Objects (CreateUdObject)

```ssl
/* Custom classes use CreateUdObject;
oCustom := CreateUdObject("MyClass");
oCustom := CreateUdObject("MyClass", {param1, param2});

/* Empty dynamic object;
oAnon := CreateUdObject();
oAnon:Property1 := "value";
oAnon:Property2 := 123;

/* Anonymous object with named properties;
oSeeded := CreateUdObject({{"Property1", "value"}, {"Property2", 123}});
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
    UsrMes(sErrMsg, "Error");
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
15. **Curly braces for built-ins** — `Email{}`, `SSLDataset{}`; never `CreateUdObject("Email")`
16. **Dates are not brace literals** — `{2024,12,25}` is an array; use `DateFromNumbers()` or `CToD()`
17. **DoProc is compile error in classes** — Use `Me:Method()` / `Base:Method()` inside `:CLASS`
18. **Skipped params: adjacent commas** — `DoProc("P", {a,,c})` not `DoProc("P", {a, , c})`
19. **Scientific notation needs decimal** — `7.0e2` not `7e2`
20. **`:FINALLY` restrictions** — No `:RETURN`, `:EXITFOR`, `:EXITWHILE`, `:LOOP` inside `:FINALLY`
