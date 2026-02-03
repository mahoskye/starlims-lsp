# SSL Style Guide

This document summarizes SSL coding conventions and best practices.

> **See also:** [AGENTS.md](../../AGENTS.md) for quick AI agent reference.

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

### Identifier Naming

| Element | Convention | Example |
|---------|------------|---------|
| Procedures | PascalCase | `CalculateTotal`, `ProcessOrder` |
| Classes | PascalCase | `InvoiceManager`, `DataHandler` |
| Local Variables | camelCase with prefix | `sUserName`, `nTotal` |
| Parameters | camelCase with prefix | `sInput`, `nValue` |
| Constants | UPPER_SNAKE_CASE | `MAX_RETRIES`, `DEFAULT_PATH` |
| Global Variables | `g` prefix | `gCurrentUser` |

### Exceptions to Hungarian Notation

- **Loop counters:** Single letters `i`, `j`, `k` are acceptable
- **Common abbreviations:** `ID`, `SQL`, `URL`, `XML`, `HTML`, `API`, `UID`, `GUID`
- **SSL constants:** `NIL`, `.T.`, `.F.`

### Naming Guidelines

- Keep variable names under 20 characters
- Keep function names under 30 characters
- Use abbreviations sparingly and only when obvious
- Avoid underscores in variable names (discouraged, not prohibited)

---

## Formatting Standards

### Indentation

- Use **tabs** (preferred) or consistent spaces
- Indent block contents by one level
- Align continuation lines for readability

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
| After commas | One space | `DoProc(a, b, c)` |
| Before semicolon | No space | `statement;` |
| Inside parentheses | No space | `DoProc(a, b)` not `DoProc( a, b )` |

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

### Procedure Organization

```ssl
/*
 * Procedure: ProcedureName
 * Description: Brief description
 * Parameters:
 *   param1 - description
 *   param2 - description
 * Returns: description
;
:PROCEDURE ProcedureName;
:PARAMETERS param1, param2;
:DEFAULT param1, "";
:DEFAULT param2, 0;
:DECLARE localVar1, localVar2;

/* Initialization;
localVar1 := "";

/* Main logic;
:TRY;
    /* Process data;
:CATCH;
    /* Handle error;
:ENDTRY;

:RETURN result;
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

### Control Flow Best Practices

**Always use EXITCASE:**
```ssl
:BEGINCASE;
:CASE condition;
    DoSomething();
    :EXITCASE;  /* Required!;
:OTHERWISE;
    DoDefault();
    :EXITCASE;  /* Required!;
:ENDCASE;
```

**Keep nesting shallow:**
- Maximum 3-4 levels of nesting
- Extract complex logic to separate procedures

**Prefer positive logic:**
```ssl
/* Prefer this;
:IF bIsValid;
    ProcessValid();
:ELSE;
    HandleInvalid();
:ENDIF;

/* Over this;
:IF .NOT. bIsValid;
    HandleInvalid();
:ELSE;
    ProcessValid();
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

- SQL keywords in lowercase: `select`, `from`, `where`
- Table/column names in UPPERCASE: `CUSTOMERS`, `CUSTOMER_ID`
- Place major clauses on new lines
- Indent continuation lines

```ssl
sSQL := "select C.CUSTOMER_ID, C.CUSTOMER_NAME, O.ORDER_TOTAL
         from CUSTOMERS C
         inner join ORDERS O on O.CUSTOMER_ID = C.CUSTOMER_ID
         where C.STATUS = ?sStatus?
             and O.ORDER_DATE >= ?dStartDate?
         order by O.ORDER_DATE desc";
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

### Database Function Selection

| Function | Use Case | Parameter Style |
|----------|----------|-----------------|
| `SQLExecute` | General queries | `?varName?` (named) |
| `RunSQL` | INSERT/UPDATE/DELETE | `?` (positional) |
| `LSearch` | Single value lookup | `?` (positional) |
| `GetDataSet` | XML dataset output | `?` (positional) |

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

- Use structured `:TRY`/`:CATCH` over legacy `:ERROR` markers
- Always log errors with contextual information
- Never swallow errors silently
- Clean up resources in `:FINALLY` blocks

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
- Use appropriate array functions (`ascan`, `ascanexact`)
- Consider `BuildStringForIn()` for SQL `IN` clauses

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

### Array Functions (lowercase)
```ssl
aadd(aArray, value);
alen(aArray);
ascan(aArray, value);
ascanexact(aArray, value);
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
LimsString(value);  /* Convert to string (NOT Str());
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
