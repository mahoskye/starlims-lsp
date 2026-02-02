# SSL Style Guide

This document summarizes SSL coding conventions. For the complete official style guide, see:

**Primary Source:** [`ssl-style-guide/reference/abbot-starlims-style-guide.md`](../../ssl-style-guide/reference/abbot-starlims-style-guide.md)

---

## Naming Conventions

### Hungarian Notation

STARLIMS conventionally uses Hungarian notation prefixes:

| Prefix | Type | Example |
|--------|------|---------|
| `s` | String | `sCustomerName` |
| `n` | Numeric | `nOrderTotal` |
| `b` | Boolean | `bIsActive` |
| `d` | Date | `dCreatedOn` |
| `o` | Object | `oConnection` |
| `a` | Array | `aItems` |

### Identifier Naming

| Element | Convention | Example |
|---------|------------|---------|
| Procedures | PascalCase | `CalculateTotal` |
| Local Variables | camelCase or Hungarian | `nTotal`, `customerName` |
| Parameters | camelCase or Hungarian | `sInput`, `nValue` |
| Constants | UPPER_CASE | `MAX_RETRIES` |
| Global Variables | `g` prefix | `gCurrentUser` |

---

## Formatting Standards

### Indentation

- Use tabs (preferred) or consistent spaces
- Indent block contents by one level
- Align continuation lines

### Spacing

- Space around assignment operator: `x := 1`
- Space around comparison operators: `x > 0`
- Space after commas: `DoProc(a, b, c)`
- No space before semicolon: `statement;`

### Line Length

- Maximum 90 characters recommended
- Break long lines at logical points

### Blank Lines

- One blank line between procedures
- Blank line before major sections within procedures

---

## Code Structure

### Procedure Organization

```ssl
:PROCEDURE ProcedureName;
:PARAMETERS param1, param2 :DEFAULT "";
:DECLARE localVar1, localVar2;

/* Main logic here ;

:RETURN result;
:ENDPROC;
```

Recommended order:
1. `:PARAMETERS` (if any)
2. `:DECLARE` (all local variables)
3. Initialization
4. Main logic
5. `:RETURN` (if applicable)

### Control Flow Best Practices

**Always use EXITCASE:**
```ssl
:BEGINCASE;
:CASE condition;
    DoSomething();
    :EXITCASE;  /* Required! ;
:OTHERWISE;
    DoDefault();
    :EXITCASE;  /* Required! ;
:ENDCASE;
```

**Keep nesting shallow:**
- Maximum 3-4 levels of nesting
- Extract complex logic to separate procedures

---

## Comments

### Block Comments

```ssl
/* This is a properly formatted
   SSL block comment that spans
   multiple lines
;
```

### Documentation Comments

```ssl
/*
 * Procedure: CalculateTotal
 * Purpose:   Calculates the total with discount
 * Parameters:
 *   nPrice    - Unit price
 *   nQuantity - Number of items
 * Returns:   Numeric total
 ;
:PROCEDURE CalculateTotal;
```

---

## SQL Standards

### Embedded SQL

```ssl
sSQL := "
    SELECT customer_id, customer_name
    FROM customers
    WHERE active = 1
        AND region = ?sRegion?
    ORDER BY customer_name
";
ds := SQLExecute(sSQL, "dsCustomers");
```

### SQL Placeholders

- Named parameters: `?variableName?`
- Use declared variables only
- Match case for clarity

---

## Error Handling

### TRY/CATCH Pattern

```ssl
:TRY;
    /* Risky operation ;
    result := PerformOperation();
:CATCH;
    /* Handle error ;
    LogError(Error():Message);
    result := NIL;
:FINALLY;
    /* Cleanup ;
    CloseResources();
:ENDTRY;
```

---

## Machine-Readable Style Rules

For automated style checking, see:

**Schema:** [`ssl-style-guide/ssl-style-guide/ssl-style-guide.schema.yaml`](../../ssl-style-guide/ssl-style-guide/ssl-style-guide.schema.yaml)

---

## Complete Reference

See: [`ssl-style-guide/reference/abbot-starlims-style-guide.md`](../../ssl-style-guide/reference/abbot-starlims-style-guide.md)
