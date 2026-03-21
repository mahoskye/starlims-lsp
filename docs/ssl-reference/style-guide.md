# SSL Style Guide

This document summarizes SSL coding conventions and best practices.

> **See also:** [AGENTS.md](../../AGENTS.md) for quick AI agent reference.
> **Authority:** `dev/ssl-style-guide/` is the source of truth when this public summary lags behind.

---

## Naming Conventions

### Hungarian Notation

STARLIMS uses Hungarian notation prefixes to indicate variable types:

| Prefix | Type | Example |
|--------|------|---------|
| `s` | String | `sCustomerName`, `sMessage`, `sSQL` |
| `n` | Numeric | `nOrderTotal`, `nCount`, `nIndex` |
| `b` | Boolean | `bIsActive`, `bExists`, `bSuccess` |
| `d` | Date | `dCreatedOn`, `dDueDate`, `dExpiry` |
| `o` | Object | `oConnection`, `oDataset`, `oEmail` |
| `a` | Array | `aItems`, `aResults`, `aRows` |
| `fn` | Code block | `fnTransform`, `fnPredicate` |
| `v` | Variant / unknown | `vResult`, `vValue` |

### Identifier Naming

| Element | Convention | Example |
|---------|------------|---------|
| Procedures | PascalCase | `CalculateTotal`, `ProcessOrder` |
| Classes | PascalCase | `InvoiceManager`, `DataHandler` |
| Variables | camelCase with prefix | `sUserName`, `nTotal`, `vResult` |
| Constants | UPPER_SNAKE_CASE | `MAX_RETRIES`, `DEFAULT_PATH` |

### Exceptions to Hungarian Notation

- **Loop counters:** Single letters `i`, `j`, `k`, `x`, `y`, `z` are acceptable
- **Acronyms:** Preserve existing acronym casing such as `ID`, `SQL`, `URL`, `XML`
- **SSL constants:** `NIL`, `.T.`, `.F.`

### Naming Guidelines

- Use abbreviations sparingly and only when obvious
- Avoid underscores in variable names for general use (discouraged, not prohibited)
- **Exception:** Underscore-prefixed class members (e.g. `_sInternal`) are a deliberate private-by-convention pattern. They are excluded from reflection-based access and should not be renamed or removed. Do not treat them as style violations.
- Keep class-context references in their canonical forms: `Me:Member`, `Base:Member`, `Constructor`

---

## Formatting Standards

### Indentation

- Use **tabs** (preferred) or consistent spaces
- Indent block contents by one level
- Align continuation lines for readability
- **Class files:** Do not add an extra indentation level for the class body itself. Because SSL has no `:ENDCLASS` and the class extends to the end of the file, the formatter does not indent the class body relative to `:CLASS`.

```ssl
:PROCEDURE ProcessData;
:PARAMETERS sInput;
:DECLARE nCount, aResults;

    nCount := 0;
    
    :IF .NOT. Empty(sInput);
        aResults := SQLExecute("SELECT * FROM Table WHERE Field = ?sInput?");
        
        :IF Len(aResults) > 0;
            :FOR i := 1 :TO Len(aResults);
                nCount += 1;
            :NEXT;
        :ENDIF;
    :ENDIF;

:RETURN nCount;
:ENDPROC;
```

### Spacing

| Context | Rule | Example |
|---------|------|---------|
| Assignment | Space around `:=` | `x := 1` |
| Comparison | Space around operators | `x > 0` |
| After commas | One space | `DoProc("MyProc", {sName, nCount, bFlag})` |
| Before semicolon | No space | `statement;` |
| Inside parentheses | No space | `DoProc("MyProc", {sName, nCount})` not `DoProc( "MyProc", { sName, nCount } )` |
| Member access | Prefer no spaces around `:` | `oEmail:Subject` not `oEmail : Subject` |

### Not-Preferred Operators

The following comparison operators are valid SSL but not preferred — use `!=` instead:

| Operator | Use Instead |
|----------|-------------|
| `<>` | `!=` |
| `#` | `!=` |

### Line Length

- Maximum **90 characters** recommended
- Break long lines at logical points (after comma, before operator)

```ssl
/* Long SQL - break at logical points;
sSQL := "SELECT CustomerID, CustomerName, OrderTotal " +
        "FROM Orders " +
        "WHERE Status = ?sStatus? " +
        "ORDER BY OrderDate";
```

### Blank Lines

- One blank line between procedures
- Blank line before major sections within procedures
- Blank line after declarations, before logic

---

## Code Structure

### Procedure Calls

Custom procedures are not called directly:

```ssl
vResult := DoProc("LocalProcedure", {sValue, nCount});
vResult := ExecFunction("Category.Script.ProcedureName", {sValue});
```

Inside `:CLASS` methods, use `Me:MethodName()` / `Base:MethodName()` for sibling and inherited methods instead of `DoProc(...)`.

When optional trailing arguments are omitted, leave them out entirely rather than passing empty arrays or empty strings:

```ssl
DoProc("NoArgProc");
GetDataSet(sSQL);
```

### Procedure Organization

```ssl
/*
 * Procedure: ProcedureName
 * Description: Brief description
 * Parameters:
 *   sInput - description
 *   nCount - description
 * Returns: description
;
:PROCEDURE ProcedureName;
:PARAMETERS sInput, nCount;
:DEFAULT sInput, "";
:DEFAULT nCount, 0;
:DECLARE sResult, aRows;

/* Initialization;
sResult := "";

/* Main logic;
:TRY;
    /* Process data;
:CATCH;
    /* Handle error;
:ENDTRY;

:RETURN sResult;
:ENDPROC;
```

**Recommended order:**
1. Header comment with documentation
2. `:PARAMETERS` (if any)
3. `:DEFAULT` values (if any)
4. `:DECLARE` local variables
5. Initialization
6. Main logic
7. `:RETURN` (if applicable)
8. `:ENDPROC;`

**Placement rules:**
- Script-level `:PARAMETERS` must appear before top-level script statements, though leading `:PROCEDURE` definitions may come first
- Inside a procedure, `:PARAMETERS` must immediately follow `:PROCEDURE`
- `:DEFAULT` must immediately follow `:PARAMETERS`

### Control Flow Best Practices

**Prefer `:EXITCASE;` at the end of every `:CASE` and `:OTHERWISE` block unless multi-match behavior is intentional:**
```ssl
:BEGINCASE;
:CASE condition;
    sResult := "matched";
    :EXITCASE;
:OTHERWISE;
    sResult := "default";
    :EXITCASE;
:ENDCASE;
```

**Keep nesting shallow:**
- Maximum 3-4 levels of nesting
- Extract complex logic to separate procedures

**Prefer positive logic:**
```ssl
/* Prefer this;
:IF bIsValid;
    sStatus := "valid";
:ELSE;
    sStatus := "invalid";
:ENDIF;

/* Over this;
:IF .NOT. bIsValid;
    sStatus := "invalid";
:ELSE;
    sStatus := "valid";
:ENDIF;
```

---

## Comments

### Block Comments

```ssl
/* Single-line comment;

/* Multi-line comment
   that spans multiple lines
   and ends with semicolon
;
```

### Documentation Headers

```ssl
/*
 * Procedure: CalculateTotal
 * Description: Calculates order total with applicable discounts
 * Parameters:
 *   nPrice    - Unit price
 *   nQuantity - Number of items
 *   bApplyDiscount - Whether to apply discount
 * Returns: Numeric total
 * Author: INITIALS
 * Date: YYYY-MM-DD
;
:PROCEDURE CalculateTotal;
```

### Comment Guidelines

- Explain "why" not "what"
- Keep comments concise
- Use sentence case
- Place comments above the code they describe
- All comments must end with semicolon
- Never place an extra semicolon inside comment text; the first semicolon terminates the comment and the remaining text becomes executable code

### Visibility Annotations (Scripts Only)

Place `/*@private;` or `/*@protected;` on its own line immediately before `:PROCEDURE` to restrict access from external callers:

```ssl
/*@private;
:PROCEDURE InternalHelper;
    /* Not accessible via DoProc/ExecFunction from outside this script;
:ENDPROC;
```

- Both annotations make the procedure inaccessible via `DoProc` / `ExecFunction` from external scripts.
- **These annotations have no effect on class methods.** Class methods are always public regardless.

### Code Organization

- Prefer comment regions (`/* region ...;` / `/* endregion;`) for editor grouping
- Treat `:REGION` / `:ENDREGION` as legacy functional text-capture constructs, not formatting aids

---

## SQL Standards

### Embedded SQL with SQLExecute

```ssl
sSQL := "
    SELECT customer_id, customer_name
    FROM customers
    WHERE active = 1
        AND region = ?sRegion?
    ORDER BY customer_name
";
aResults := SQLExecute(sSQL);
```

### SQL Formatting

- SQL keywords in UPPERCASE: `SELECT`, `FROM`, `WHERE`
- Preserve external object casing only when the schema requires it
- Place major clauses on new lines
- Indent continuation lines

```ssl
sSQL := "
    SELECT c.customer_id, c.customer_name, o.order_total
    FROM customers c
    INNER JOIN orders o
      ON o.customer_id = c.customer_id
    WHERE c.status = ?sStatus?
      AND o.order_date >= ?dStartDate?
    ORDER BY o.order_date DESC
";
```

### SQL Parameters

**SQLExecute (named parameters):**
```ssl
sSQL := "SELECT * FROM Orders WHERE CustomerID = ?sCustomerID?";
aResults := SQLExecute(sSQL);
```

**RunSQL/LSearch (positional parameters):**
```ssl
sSQL := "UPDATE Orders SET Status = ? WHERE OrderID = ?";
bSuccess := RunSQL(sSQL,, {sNewStatus, nOrderID});

sName := LSearch("SELECT Name FROM Customers WHERE ID = ?", "",, {nCustomerID});
```

`LSelect`, `LSelect1`, `LSelectC`, `GetDataSet`, `GetDataSetEx`, `GetDataSetWithSchemaFromSelect`, `GetDataSetXMLFromSelect`, `GetNETDataSet`, `XmlExportSql`, and `GetTables` follow the same positional-parameter convention.

`SQLExecute` also supports source-aligned array expansion (`?aValues?`), object-property access (`?oUser:ID?`), and parameterless function calls such as `?Today()?`.

### Database Function Selection

| Function | Use Case | Parameter Style |
|----------|----------|-----------------|
| `SQLExecute` | General queries | `?varName?` (named) |
| `RunSQL` | INSERT/UPDATE/DELETE | `?` (positional) |
| `LSearch` | Single value lookup | `?` (positional) |
| `LSelect` / `LSelect1` / `LSelectC` | Multi-row SELECT | `?` (positional) |
| `GetDataSet` | XML dataset output | `?` (positional) |
| `XmlExportSql` | Export SQL result as XML | `?` (positional) |
| `GetTables` | Get tables result | `?` (positional) |

---

## Error Handling

### TRY/CATCH Pattern

```ssl
:TRY;
    /* Risky operation;
    aResults := SQLExecute(sSQL);
    
    :IF Len(aResults) = 0;
        RaiseError("No results found");
    :ENDIF;
:CATCH;
    /* Handle error;
    oErr := GetLastSSLError();
    sMsg := "Error: " + oErr:Description;
    UsrMes(sMsg, "Error");
    :RETURN NIL;
:FINALLY;
    /* Cleanup (always runs);
    :IF .NOT. Empty(oConnection);
        oConnection := NIL;
    :ENDIF;
:ENDTRY;
```

### Error Handling Guidelines

- Use structured `:TRY`/`:CATCH` over legacy `:ERROR` / `:RESUME`
- A `:TRY` block must contain at least one statement before `:CATCH` or `:FINALLY`
- A `:TRY` block must include at least one `:CATCH` or `:FINALLY`
- Only one `:CATCH` block is allowed, and `:CATCH` does not name an exception variable
- Legacy `:ERROR` handlers must contain at least one statement before `:RESUME` or the next scope boundary
- Always log errors with contextual information
- Never swallow errors silently
- Clean up resources in `:FINALLY` blocks
- `:RETURN`, `:EXITFOR`, `:EXITWHILE`, and `:LOOP` are compile-time errors inside `:FINALLY`

---

## Performance Guidelines

### SQL Optimization

- Avoid `SELECT *` — specify needed columns
- Use `EXISTS` over `DISTINCT` when checking for existence
- Use `BETWEEN` for range queries instead of multiple conditions
- Prefer derived tables over correlated subqueries
- Minimize database round-trips

### Array Operations

- Preallocate arrays when size is known
- Use appropriate array functions (`AScan`, `AScanExact`)
- Prefer `SQLExecute` array expansion (`?aValues?`) for dynamic `IN` clauses over manual string building

### General

- Pre-compute complex expressions before SQL
- Avoid complex expressions in SQL parameter placeholders
- Use `LSearch` for single-value lookups (faster than full query)

---

## Security Best Practices

### SQL Injection Prevention

- **Always use parameterized queries**
- Never concatenate user input directly into SQL strings
- Validate parameter counts match placeholders

```ssl
/* WRONG - SQL injection risk;
sSQL := "SELECT * FROM Users WHERE Name = '" + sUserInput + "'";

/* CORRECT - parameterized;
sSQL := "SELECT * FROM Users WHERE Name = ?sUserInput?";
aResults := SQLExecute(sSQL);
```

### Input Validation

- Validate all inputs before processing
- Use appropriate data types
- Check array bounds before access

---

## Function Casing Reference

### Array Functions (Canonical Casing)
```ssl
AAdd(aArray, value);
ALen(aArray);
AScan(aArray, value);
AScanExact(aArray, value);
```

### String Functions (PascalCase)
```ssl
AllTrim(sString);
SubStr(sString, nStart, nLen);
StrTran(sString, sFind, sReplace);
Upper(sString);
Lower(sString);
```

### Type Functions (PascalCase)
```ssl
LimsString(value);  /* General value-to-string conversion;
Str(nValue, 6, 2);  /* Numeric formatting with width/decimals;
Empty(value);       /* Check if empty/nil/zero;
Val(sString);       /* Convert to number;
LimsTypeEx(value);  /* Get type name;
```

### Database Functions (PascalCase)
```ssl
SQLExecute(sSQL);
RunSQL(sSQL, sFriendlyName, aParams);
LSearch(sSQL, default, sFriendlyName, aParams);
GetDataSet(sSQL, aParams);
```

---

## Predefined Global Variables

SSL provides read-only predefined globals available in all scripts:

| Variable | Type | Description |
|----------|------|-------------|
| `MYUSERNAME` | String | Current user's username |

Do not assign to `MYUSERNAME`; it is a runtime-provided read-only value.

---

## Summary Checklist

- [ ] Use Hungarian notation for all variables
- [ ] Use PascalCase for procedures and classes
- [ ] Terminate all statements with semicolon
- [ ] Use colon-prefixed UPPERCASE keywords
- [ ] Use `.AND.`, `.OR.`, `.NOT.` (with periods)
- [ ] Use `DoProc`/`ExecFunction` for procedure calls
- [ ] Include `:EXITCASE` in every CASE block
- [ ] Use 1-based array indexing
- [ ] Use parameterized queries for all SQL
- [ ] Add documentation headers to procedures
