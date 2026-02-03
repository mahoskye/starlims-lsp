# AGENTS.md - SSL Coding Conventions for AI Agents

This document provides essential SSL (STARLIMS Scripting Language) coding conventions for AI agents working on this codebase. Following these conventions ensures generated code is syntactically correct and stylistically consistent.

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
| Comments | `/* comment text;` (ends with semicolon) |
| Procedure calls | Use `DoProc()` or `ExecFunction()`, never direct calls |

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

/* Classes;
:CLASS ClassName;
:INHERIT BaseClass;
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

### Operators

#### Assignment Operators
```ssl
x := 10;       /* Assignment;
x += 5;        /* Add and assign;
x -= 3;        /* Subtract and assign;
x *= 2;        /* Multiply and assign;
x /= 4;        /* Divide and assign;
```

#### Comparison Operators
```ssl
x = y          /* Equality (loose for strings);
x == y         /* Strict equality;
x != y         /* Not equal;
x <> y         /* Not equal (alternative);
x < y          /* Less than;
x > y          /* Greater than;
x <= y         /* Less than or equal;
x >= y         /* Greater than or equal;
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
sResult := sFirst + sSecond;    /* Concatenation;
bFound := "needle" $ "haystack"; /* Contains (returns .T./.F.);
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

/* Date literal;
dDate := {2024, 12, 25, 14, 30, 0};  /* year, month, day, hour, min, sec;
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

### Exceptions to Hungarian Notation

1. **Loop counters**: Single letters `i`, `j`, `k` are allowed
2. **Constants**: Use `UPPER_SNAKE_CASE` (e.g., `MAX_RETRIES`, `DEFAULT_PATH`)
3. **Special identifiers**: `NIL`, `.T.`, `.F.`, `ID`, `SQL`, `URL`, `XML`

### Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Procedures | PascalCase | `CalculateTotal`, `ProcessOrder` |
| Classes | PascalCase | `InvoiceManager`, `DataHandler` |
| Variables | camelCase with prefix | `sCustomerName`, `nOrderCount` |
| Constants | UPPER_SNAKE_CASE | `MAX_RETRY_COUNT` |

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

/* Different file - use ExecFunction;
result := ExecFunction("Module.CalculateTotal", {5, 10});

/* Skip parameters with empty array positions;
result := DoProc("MyProc", {param1,, param3});  /* Skips param2;
```

### ❌ Missing EXITCASE — ALWAYS REQUIRED

```ssl
/* WRONG - missing :EXITCASE will cause fall-through;
:BEGINCASE;
:CASE nVal == 1;
    DoSomething();
:CASE nVal == 2;
    DoOther();
:ENDCASE;
```

### ✅ Correct CASE Structure

```ssl
/* CORRECT - every case must have :EXITCASE;
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
| `LSelect1` | `?` (positional) | Multi-row SELECT | 2D Array |
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

---

## 6. Object Creation

### Built-in Classes (Curly Braces)

```ssl
/* Built-in SSL classes use curly braces;
oEmail := Email{};
oEmail := Email{.T.};  /* With parameter;
oRegex := SSLRegex{'\d+'};
oTable := CDataTable{};
oDict := SSLStringDictionary{};
```

### User-Defined Objects (CreateUdObject)

```ssl
/* Custom classes use CreateUdObject;
oCustom := CreateUdObject("MyClass");
oCustom := CreateUdObject("MyClass", {param1, param2});

/* Anonymous object;
oAnon := CreateUdObject();
oAnon:Property1 := "value";
oAnon:Property2 := 123;
```

---

## 7. Function Casing Reference

SSL functions are case-insensitive but should use documented casing for consistency:

### Array Functions (lowercase)
```ssl
aadd(aArray, value);
alen(aArray);
ascan(aArray, value);
ascanexact(aArray, value);
aeval(aArray, codeBlock);
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
LimsString(value);     /* NOT Str() - Str doesn't exist;
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

---

## Summary: Top 10 SSL Rules

1. **Semicolons everywhere** — Every statement, every comment
2. **Colon-prefix keywords** — `:IF`, `:WHILE`, `:PROCEDURE`, etc.
3. **UPPERCASE keywords** — Never `:if` or `:while`
4. **Period-wrapped logical ops** — `.AND.`, `.OR.`, `.NOT.`
5. **Hungarian notation** — `sName`, `nCount`, `bFlag`, `aItems`, `oObject`
6. **DoProc/ExecFunction** — Never call procedures directly
7. **:EXITCASE required** — Every CASE block needs it
8. **1-based arrays** — First element is `[1]`
9. **Colon property access** — `object:Property`, not `object.Property`
10. **SQLExecute vs others** — Only SQLExecute uses `?varName?` syntax
