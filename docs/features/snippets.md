# Snippets

**Status:** IMPLEMENTED  
**LSP Method:** Via `textDocument/completion`  
**Source Files:** `internal/providers/completion.go`

---

## 1. Overview

Snippets are code templates that expand into multi-line code structures with placeholder positions. They accelerate writing common SSL patterns like procedures, control flow blocks, and error handling.

---

## 2. Capabilities

### 2.1 Snippet Completion

Snippets are delivered through the completion provider with:
- `kind`: Snippet (15)
- `insertTextFormat`: Snippet (2)
- `insertText`: Contains `$1`, `$2`, `${1:placeholder}` markers

### 2.2 Placeholder Syntax

| Syntax | Description |
|--------|-------------|
| `$1`, `$2` | Tab stops in order |
| `${1:default}` | Tab stop with default text |
| `$0` | Final cursor position |

### 2.3 Available Snippets

#### Procedure Templates

| Trigger | Expands To |
|---------|------------|
| `proc` | Procedure with header comment and body |
| `procparams` | Procedure with parameters and defaults scaffold |

#### Control Flow

| Trigger | Expands To |
|---------|------------|
| `if` | IF/ENDIF block |
| `ifelse` | IF/ELSE/ENDIF block |
| `while` | WHILE/ENDWHILE loop |
| `for` | FOR/NEXT loop |
| `forstep` | FOR loop with STEP |

#### Case Statements

| Trigger | Expands To |
|---------|------------|
| `case` | BEGINCASE with CASE/ENDCASE |

#### Error Handling

| Trigger | Expands To |
|---------|------------|
| `try` | TRY/CATCH/ENDTRY block |
| `tryfinally` | TRY/CATCH/FINALLY/ENDTRY block |
| `catchssl` | TRY/CATCH with `GetLastSSLError()` |
| `catchsql` | TRY/CATCH with `GetLastSQLError()` |

#### Declarations

| Trigger | Expands To |
|---------|------------|
| `declare` | Variable declaration |
| `public` | Public variable declaration |
| `include` | Include directive |

#### SQL Patterns

| Trigger | Expands To |
|---------|------------|
| `sql` | SQLExecute with named placeholders |
| `doproc` | DoProc call with argument array |

---

## 3. Configuration

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| (None currently) | - | - | No snippet-specific configuration |

### Future Configuration

| Setting | Purpose |
|---------|---------|
| `ssl.snippets.enabled` | Enable/disable snippets |
| `ssl.snippets.custom` | User-defined snippets |

---

## 4. Edge Cases & Special Handling

### 4.1 Context Sensitivity

The server does not apply extra semantic snippet filtering. Snippets are returned alongside regular completions everywhere outside strings and comments.

### 4.2 Indentation

Snippet bodies contain their own baseline indentation and are inserted at the current cursor position. Final alignment depends on the editor client and any follow-up formatting pass.

### 4.3 Conflict with Keywords

When a snippet trigger overlaps a common keyword stem, the final ordering is client-side. The server returns both regular completions and snippet completions together.

---

## 5. Known Limitations

| Limitation | Notes |
|------------|-------|
| No custom snippets | User-defined snippets not supported |
| No project snippets | No per-project snippet files |
| No nested snippets | Cannot trigger snippet inside snippet |

---

## 6. Test Specifications

### 6.1 Procedure Snippet

```ssl
/* Test: Procedure snippet expands correctly;
/* Type: proc<Tab>;
/* Expected expansion:;
:/*
 * Procedure: |ProcedureName|
 * Description: |Brief description|
 * Parameters:
 * Returns: |NIL|
;
:PROCEDURE |ProcedureName|;
    :DECLARE |sResult|;
    |/* body;|
:ENDPROC;
/* Where |...| are tab stop positions;
```

### 6.2 IF/ELSE Snippet

```ssl
/* Test: IF/ELSE snippet;
/* Type: ifelse<Tab>;
/* Expected expansion:;
:IF |condition|;
    |/* then body;|
:ELSE;
    |/* else body;|
:ENDIF;
```

### 6.3 FOR Loop Snippet

```ssl
/* Test: FOR loop snippet;
/* Type: for<Tab>;
/* Expected expansion:;
:FOR |i| := |1| :TO |10|;
    |/* body;|
:NEXT;
```

### 6.4 TRY/CATCH Snippet

```ssl
/* Test: TRY/CATCH snippet;
/* Type: try<Tab>;
/* Expected expansion:;
:TRY;
    |/* code;|
:CATCH;
    |/* handle error;|
:ENDTRY;
```

### 6.5 BEGINCASE Snippet

```ssl
/* Test: BEGINCASE snippet;
/* Type: case<Tab>;
/* Expected expansion:;
:BEGINCASE;
:CASE |condition1|;
    |/* action;|
    :EXITCASE;
:OTHERWISE;
    |/* default;|
    :EXITCASE;
:ENDCASE;
```

### 6.6 SQL Snippet

```ssl
/* Test: SQLExecute snippet;
/* Type: sql<Tab>;
/* Expected expansion:;
SQLExecute("
    SELECT |*|
    FROM |table_name|
    WHERE |column_name| = ?|sValue|?
", "|dsName|");
```

### 6.7 Indentation Preservation

```ssl
/* Test: Snippet respects current indentation;
:PROCEDURE Test;
    :IF x > 0;
        for<Tab>  /* Trigger snippet here;
/* Expected: FOR loop indented to match context;
        :FOR i := 1 :TO 10;
            /* body;
        :NEXT;
```

---

## 7. Related Issues

| Issue | Description | Status |
|-------|-------------|--------|
| (None) | - | - |

---

## 8. Implementation Notes

### 8.1 Snippet Definitions

Snippets are defined in `internal/providers/completion.go` with:

```go
type Snippet struct {
    Prefix       string   // Trigger text
    Label        string   // Display label
    Body         string   // Snippet body with placeholders
    Description  string   // Documentation
}
```

### 8.2 Placeholder Processing

The `insertText` uses LSP snippet syntax:
- `$1` for first tab stop
- `${1:default}` for tab stop with default value
- `$0` for final cursor position

### 8.3 Available Snippet List

| Label | Prefix | Description |
|-------|--------|-------------|
| `:PROCEDURE...:ENDPROC` | `proc` | Complete procedure |
| `:PROCEDURE with params` | `procparams` | Procedure with parameters |
| `:IF...:ENDIF` | `if` | IF block |
| `:IF...:ELSE...:ENDIF` | `ifelse` | IF/ELSE block |
| `:WHILE...:ENDWHILE` | `while` | WHILE loop |
| `:FOR...:NEXT` | `for` | FOR loop |
| `:FOR...:STEP...:NEXT` | `forstep` | FOR loop with STEP |
| `:BEGINCASE...:ENDCASE` | `case` | Case statement |
| `:TRY...:ENDTRY` | `try` | Try/Catch block |
| `:TRY...:CATCH...:FINALLY...:ENDTRY` | `tryfinally` | Try/Catch/Finally block |
| `:TRY...:CATCH (GetLastSSLError)` | `catchssl` | Try/Catch with SSL error retrieval |
| `:TRY...:CATCH (GetLastSQLError)` | `catchsql` | Try/Catch with SQL error retrieval |
| `:DECLARE` | `declare` | Variable declaration |
| `:PUBLIC` | `public` | Public variable declaration |
| `:INCLUDE` | `include` | Include directive |
| `SQLExecute` | `sql` | SQL execution pattern |
| `DoProc` | `doproc` | Same-file procedure call |
