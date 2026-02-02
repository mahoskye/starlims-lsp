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
| `:PROCEDURE` | Procedure with name and body |
| `:proc` | Same as above |
| `:procparams` | Procedure with parameters |

#### Control Flow

| Trigger | Expands To |
|---------|------------|
| `:IF` | IF/ENDIF block |
| `:IFELSE` | IF/ELSE/ENDIF block |
| `:WHILE` | WHILE/ENDWHILE loop |
| `:FOR` | FOR/NEXT loop |
| `:FOREACH` | FOR loop over array |

#### Case Statements

| Trigger | Expands To |
|---------|------------|
| `:BEGINCASE` | BEGINCASE with CASE/ENDCASE |
| `:CASE` | Single CASE with EXITCASE |

#### Error Handling

| Trigger | Expands To |
|---------|------------|
| `:TRY` | TRY/CATCH/ENDTRY block |
| `:TRYFINALLY` | TRY/CATCH/FINALLY/ENDTRY block |

#### Declarations

| Trigger | Expands To |
|---------|------------|
| `:DECLARE` | Variable declaration |
| `:PARAMETERS` | Parameter declaration with DEFAULT |
| `:PUBLIC` | Public variable declaration |

#### SQL Patterns

| Trigger | Expands To |
|---------|------------|
| `sqlexec` | SQLExecute with placeholders |
| `getds` | GetDataSet pattern |
| `runsql` | RunSQL with parameters |

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

Snippets are filtered based on context:
- Block snippets offered at statement positions
- SQL snippets offered in expression positions

### 4.2 Indentation

Snippets should inherit the current line's indentation. Each line in the snippet template is indented relative to the trigger position.

### 4.3 Conflict with Keywords

When a snippet trigger matches a keyword, both appear in completion:
- `:IF` shows both the keyword and the snippet
- Snippet has additional documentation explaining the expansion

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
/* Type: :proc<Tab>;
/* Expected expansion:;
:PROCEDURE |ProcedureName|;
    |/* body;|
:ENDPROC;
/* Where |...| are tab stop positions;
```

### 6.2 IF/ELSE Snippet

```ssl
/* Test: IF/ELSE snippet;
/* Type: :IFELSE<Tab>;
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
/* Type: :FOR<Tab>;
/* Expected expansion:;
:FOR |i| := |1| :TO |10|;
    |/* body;|
:NEXT;
```

### 6.4 TRY/CATCH Snippet

```ssl
/* Test: TRY/CATCH snippet;
/* Type: :TRY<Tab>;
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
/* Type: :BEGINCASE<Tab>;
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
/* Type: sqlexec<Tab>;
/* Expected expansion:;
|dsResult| := SQLExecute("|SELECT * FROM table|", "|dsName|");
```

### 6.7 Indentation Preservation

```ssl
/* Test: Snippet respects current indentation;
:PROCEDURE Test;
    :IF x > 0;
        :FOR<Tab>  /* Trigger snippet here;
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
| `:PROCEDURE...:ENDPROC` | `:proc` | Complete procedure |
| `:PROCEDURE with params` | `:procparams` | Procedure with parameters |
| `:IF...:ENDIF` | `:IF` | IF block |
| `:IF...:ELSE...:ENDIF` | `:IFELSE` | IF/ELSE block |
| `:WHILE...:ENDWHILE` | `:WHILE` | WHILE loop |
| `:FOR...:NEXT` | `:FOR` | FOR loop |
| `:BEGINCASE...:ENDCASE` | `:BEGINCASE` | Case statement |
| `:TRY...:ENDTRY` | `:TRY` | Try/Catch block |
| `:DECLARE` | `:DECLARE` | Variable declaration |
| `:PARAMETERS` | `:PARAMETERS` | Parameter with default |
| `SQLExecute` | `sqlexec` | SQL execution pattern |
| `GetDataSet` | `getds` | Dataset retrieval |
