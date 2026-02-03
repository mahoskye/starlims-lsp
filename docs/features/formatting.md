# Formatting

**Status:** IMPLEMENTED  
**LSP Methods:** `textDocument/formatting`, `textDocument/rangeFormatting`  
**Source Files:** `internal/providers/formatting.go`, `internal/providers/sql_formatter.go`

---

## 1. Overview

The SSL formatter provides consistent code formatting for STARLIMS Scripting Language files. It handles both SSL code and embedded SQL strings, operating on tokens to preserve comments and string content while applying consistent style.

---

## 2. Capabilities

### 2.1 Formatting Methods

| Method | Description |
|--------|-------------|
| `textDocument/formatting` | Format entire document |
| `textDocument/rangeFormatting` | Format selected range |

### 2.2 What Gets Formatted

| Element | Formatted | Notes |
|---------|-----------|-------|
| Indentation | Yes | Block-based indentation |
| Operator spacing | Yes | `x := 1` not `x:=1` |
| Comma spacing | Yes | `a, b, c` not `a,b,c` |
| Semicolons | Yes | Added if missing |
| Newlines | Yes | After statements |
| SQL strings | Yes | Optional, configurable |
| Comment content | No | Preserved exactly |
| String content | No | Preserved (except SQL) |

---

## 3. Configuration

### 3.1 SSL Formatting Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `ssl.format.indentStyle` | string | `"tab"` | `"tab"` or `"space"` |
| `ssl.format.indentSize` | int | `4` | Spaces per indent level |
| `ssl.format.maxLineLength` | int | `90` | Maximum line length (0 = unlimited) |
| `ssl.format.operatorSpacing` | bool | `true` | Space around operators |
| `ssl.format.commaSpacing` | bool | `true` | Space after commas |
| `ssl.format.semicolonEnforcement` | bool | `true` | Ensure semicolons at statement end |
| `ssl.format.blankLinesBetweenProcs` | int | `1` | Blank lines between procedures |

### 3.2 SQL Formatting Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `ssl.format.sql.enabled` | bool | `true` | Enable SQL formatting |
| `ssl.format.sql.style` | string | `"standard"` | Formatting style |
| `ssl.format.sql.keywordCase` | string | `"upper"` | Keyword case transformation |
| `ssl.format.sql.indentSize` | int | `4` | Spaces per SQL indent |
| `ssl.format.sql.maxLineLength` | int | `90` | Max line length for SQL |

### 3.3 SQL Styles

| Style | Description |
|-------|-------------|
| `standard` | Simple clause breaks (default) |
| `canonicalCompact` | Balanced with indented AND/OR |
| `compact` | Minimal breaks |
| `expanded` | Each element on own line |

---

## 4. Indentation Rules

### 4.1 Block Keywords

**Increase indent after:**
`:IF`, `:ELSE`, `:WHILE`, `:FOR`, `:BEGINCASE`, `:CASE`, `:OTHERWISE`, `:TRY`, `:CATCH`, `:FINALLY`, `:PROCEDURE`, `:CLASS`, `:REGION`

**Decrease indent before:**
`:ENDIF`, `:ENDWHILE`, `:NEXT`, `:ENDCASE`, `:ENDTRY`, `:ENDPROC`, `:ENDREGION`

**Same level (dedent then indent):**
`:ELSE`, `:CASE`, `:OTHERWISE`, `:CATCH`, `:FINALLY`

**Note:** `:EXITCASE` is a regular statement (like `break`) that stays at content indentation level.

### 4.2 Parenthesis Alignment

Content within parentheses maintains alignment:

```ssl
result := CallFunction(param1,
                       param2,
                       param3);
```

Or with line wrapping:

```ssl
result := CallFunction(
    param1,
    param2,
    param3
);
```

---

## 5. Edge Cases & Special Handling

### 5.1 End-of-Line Comments

End-of-line comments are preserved on the same line as their code:

```ssl
x := 5;  /* set x to 5;
/* Stays on same line after formatting;
```

The formatter detects comments that appear on the same line as code and ensures they remain attached to that line.

### 5.2 Multi-Line Structure Preservation

Intentionally structured multi-line code is preserved with proper indentation:

```ssl
result := OuterFunction(
    InnerFunction(
        arg1,
        arg2
    ),
    arg3
);
/* Multi-line structure is preserved with continuation indentation;
```

The formatter tracks parenthesis depth and applies appropriate indentation for each nesting level.

### 5.3 Comment Content Preservation

Comment content should NEVER be modified:

```ssl
/*
    This is a nicely
    formatted comment
    block
;

/* Should remain exactly as written, not collapsed;
```

### 5.4 SQL Function Casing

SQL built-in functions (COUNT, SUM, AVG, etc.) follow the same casing rules as SQL keywords:

```sql
/* With keywordCase = "upper":;
SELECT COUNT(*), SUM(amount), AVG(price) FROM orders

/* With keywordCase = "lower":;
select count(*), sum(amount), avg(price) from orders
```

Functions are distinguished from identifiers and formatted appropriately, with no space before the opening parenthesis.

### 5.5 Multi-Line Logical Expressions

Lines ending with `.AND.`, `.OR.`, `.NOT.` are continuations:

```ssl
:IF condition1 .AND.
    condition2 .AND.
    condition3;
```

Should not receive semicolons on intermediate lines.

### 5.6 Array Literals in Function Calls

Lines within multi-line array literals should not require semicolons:

```ssl
DoProc("MyProc", {
    arg1,
    arg2,
    arg3
});
```

### 5.7 SQL String Auto-Detection

SQL strings are automatically detected and formatted in any string literal, not just those passed to SQL functions. Detection is based on structural patterns:

| SQL Command | Required Structure |
|-------------|-------------------|
| `SELECT` | Content after SELECT (expression or FROM with columns) |
| `INSERT` | Must contain `INTO` |
| `UPDATE` | Must contain `SET` |
| `DELETE` | Must contain `FROM` |
| `CREATE/ALTER/DROP` | Must contain object type (TABLE, VIEW, etc.) |

**Example:**
```ssl
/* Before formatting:;
sSQL := "select * from users where status = 'active'";

/* After formatting (auto-detected as SQL):;
sSQL := "
    SELECT *
    FROM users
    WHERE status = 'active'
";
```

This feature is controlled by `ssl.format.sql.detectSQLStrings` (default: `true`).

---

## 6. Known Limitations

| Limitation | Notes |
|------------|-------|
| Continuation indentation | #31 - Needs improvement |

*Most previously known limitations (end-of-line comments, multi-line structure, SQL function casing) have been resolved.*

---

## 7. Test Specifications

### 7.1 Basic Indentation

```ssl
/* Test: Block indentation;
/* Before:;
:PROCEDURE Test;
:IF .T.;
x := 1;
:ENDIF;
:ENDPROC;

/* After:;
:PROCEDURE Test;
    :IF .T.;
        x := 1;
    :ENDIF;
:ENDPROC;
```

### 7.2 Operator Spacing

```ssl
/* Test: Spacing around operators;
/* Before:;
x:=1;
y:=x+y*z;
:IF a>b.AND.c<d;

/* After:;
x := 1;
y := x + y * z;
:IF a > b .AND. c < d;
```

### 7.3 Comma Spacing

```ssl
/* Test: Spacing after commas;
/* Before:;
DoSomething(a,b,c);

/* After:;
DoSomething(a, b, c);
```

### 7.4 CASE Statement Indentation

```ssl
/* Test: CASE block formatting;
/* Before:;
:BEGINCASE;
:CASE x=1;
DoOne();
:EXITCASE;
:OTHERWISE;
DoDefault();
:EXITCASE;
:ENDCASE;

/* After:;
:BEGINCASE;
:CASE x = 1;
    DoOne();
    :EXITCASE;
:OTHERWISE;
    DoDefault();
    :EXITCASE;
:ENDCASE;
```

### 7.5 TRY/CATCH Formatting

```ssl
/* Test: TRY block formatting;
/* Before:;
:TRY;
DoRisky();
:CATCH;
HandleError();
:FINALLY;
Cleanup();
:ENDTRY;

/* After:;
:TRY;
    DoRisky();
:CATCH;
    HandleError();
:FINALLY;
    Cleanup();
:ENDTRY;
```

### 7.6 SQL Formatting (standard)

```ssl
/* Test: SQL string formatting;
/* Before:;
ds := GetDataSet("select id,name from users where active=1 and status='open'", "ds");

/* After:;
ds := GetDataSet("
    SELECT id, name
    FROM users
    WHERE active = 1 AND status = 'open'
", "ds");
```

### 7.7 SQL Formatting (canonicalCompact)

```ssl
/* Test: SQL with indented AND/OR;
/* Config: sql.style = "canonicalCompact";
/* Before:;
ds := GetDataSet("select * from users where a=1 and b=2 and c=3", "ds");

/* After:;
ds := GetDataSet("
    SELECT *
    FROM users
    WHERE a = 1
        AND b = 2
        AND c = 3
", "ds");
```

### 7.8 Range Formatting

```ssl
/* Test: Format selected range preserves context;
/* Document:;
:PROCEDURE Outer;
    :IF condition;
        x:=1;
        y:=2;
    :ENDIF;
:ENDPROC;

/* Select and format lines 3-4 only;
/* Expected: Only those lines formatted, indentation preserved;
        x := 1;
        y := 2;
```

### 7.9 End-of-Line Comment Preservation (Expected)

```ssl
/* Test: End-of-line comments stay on same line;
/* Before:;
x := 5;  /* initialize x;

/* Expected After:;
x := 5;  /* initialize x;

/* Should NOT become:;
x := 5;
/* initialize x;
```

### 7.10 Blank Lines Between Procedures

```ssl
/* Test: Configurable blank lines;
/* Config: blankLinesBetweenProcs = 1;
/* Before:;
:PROCEDURE First;
:ENDPROC;
:PROCEDURE Second;
:ENDPROC;

/* After:;
:PROCEDURE First;
:ENDPROC;

:PROCEDURE Second;
:ENDPROC;
```

### 7.11 SQL String Auto-Detection

```ssl
/* Test: SQL in variable assignment is detected and formatted;
/* Config: sql.detectSQLStrings = true (default);
/* Before:;
sSQL := "select id, name from users where active = 1";

/* After:;
sSQL := "
    SELECT id, name
    FROM users
    WHERE active = 1
";
```

### 7.12 Non-SQL String Not Formatted

```ssl
/* Test: English sentences are not mistakenly formatted as SQL;
/* Before:;
msg := "Update your settings in the configuration";

/* After (unchanged - no SET keyword means not SQL):;
msg := "Update your settings in the configuration";
```

---

## 8. Related Issues

| Issue | Description | Status |
|-------|-------------|--------|
| #9 | End-of-line comments moved | Fixed |
| #10 | Multi-line structure collapsed | Fixed |
| #28 | SQL function casing incorrect | Fixed |
| #31 | Continuation indentation | Planned v1.3 |
| #8 | Comment content modified | To Verify |
| #43 | SQL schema layout | Fixed |
| #44 | Formatter silently fails | To Investigate |

---

## 9. Implementation Notes

### 9.1 Token-Based Formatting

The formatter operates on tokens, not raw text:
1. Tokenize document
2. Process tokens maintaining state
3. Reconstruct formatted output

### 9.2 SQL Detection

SQL is detected in two ways:

1. **Function Arguments:** First argument of known SQL functions:
   `SQLExecute`, `GetDataSet`, `RunSQL`, `LSearch`, `LSelect`, etc.

2. **Auto-Detection:** Any string literal that matches SQL patterns (when `detectSQLStrings` is enabled):
   - Starts with SQL command keyword (SELECT, INSERT, UPDATE, DELETE, etc.)
   - Contains required structural elements (e.g., SELECT with FROM, INSERT with INTO)
   - Uses `IsSQLString()` function for validation

Auto-detection distinguishes SQL from English sentences by requiring structural patterns, not just keyword presence.

### 9.3 Performance

Formatting should complete within 500ms for typical files. Very large files may take longer.
