# Go to Definition

> **Normative source:** [`feature.definition`](../../catalog/features/definition.md) in the behavior catalog. This page is a guide; when it disagrees with the catalog entry, the entry wins.

**Status:** IMPLEMENTED  
**LSP Method:** `textDocument/definition`  
**Source Files:** `internal/providers/definition.go`, `internal/server/handler.go`

---

## 1. Overview

The definition provider enables navigation from a usage of an identifier to its definition. Users can Ctrl+Click (or use Go to Definition) to jump to where a procedure or variable is declared.

---

## 2. Capabilities

### 2.1 Navigable Elements

| Element | Navigates To |
|---------|--------------|
| Procedure calls | `:PROCEDURE` declaration line |
| Variable usages | `:DECLARE`, `:PARAMETERS`, or `:PUBLIC` declaration |
| Parameter usages | `:PARAMETERS` line where declared |

### 2.2 Response Format

```json
{
  "uri": "file:///path/to/file.ssl",
  "range": {
    "start": { "line": 10, "character": 0 },
    "end": { "line": 10, "character": 20 }
  }
}
```

### 2.3 Definition Detection

**Procedure Definitions:**
- Line starting with `:PROCEDURE ProcName`
- Range covers the procedure name

**Variable Definitions:**
1. `:DECLARE varName` - explicit declaration
2. `:PARAMETERS varName` - procedure parameter
3. `:PUBLIC varName` - public variable

---

## 3. Configuration

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| (None currently) | - | - | Definition has no specific configuration |

---

## 4. Edge Cases & Special Handling

### 4.1 Case Insensitivity

Definition lookup is case-insensitive:
- Clicking on `myproc` finds `:PROCEDURE MyProc`

### 4.2 Built-in Functions

Built-in functions (like `SQLExecute`) have no definition in user code. Return null for these.

### 4.3 Keywords

Keywords (`:IF`, `:WHILE`) have no definition. Return null.

### 4.4 Multiple Matches

If the same name appears in multiple scopes, return the most relevant:
1. Local scope (current procedure) first
2. Then global scope (`:PUBLIC`)

### 4.5 Cross-File Definition

Dotted `DoProc`/`ExecFunction` targets, `RunDS` data-source targets, and
`:INCLUDE` paths resolve across the workspace through the workspace index
(normative contract: `catalog/features/cross_file_resolution.md`).
Resolution is layout-flexible — canonical `Category.Script[.Proc]` anchors
first, flat-layout basename degradation, and a workspace-unique-procedure
fallback with a uniqueness gate. Open documents overlay live-buffer
positions so unsaved edits never produce stale jump targets.

### 4.6 DoProc/ExecFunction String Targets

Go-to-definition works for procedure names inside `DoProc()` and
`ExecFunction()` string arguments, same-file and cross-file:

```ssl
:PROCEDURE Main;
    DoProc("CalculateTotal", {10, 5});
    /*      ^--- 1-part target: navigates within this file;
    ExecFunction("Helpers.CalculateTotal", {10, 5});
    /*           ^--- dotted target: resolves across the workspace;
:ENDPROC;
```

Bare 1-part targets keep same-file semantics by design (the resolver's
scoping rule); multi-line dispatch calls resolve too (token-based
extraction, issue #125).

---

## 5. Known Limitations

| Limitation | Notes |
|------------|-------|
| Class-method channel | `obj:Method()` / `Base:Method()` calls into class scripts are not resolved (bare-identifier channel unmodeled) |
| Concatenated dispatch strings | `DoProc("CAT." + sName)` is not extracted as a target |
| No built-in source | Built-in functions have no navigable source |

---

## 6. Test Specifications

### 6.1 Procedure Definition

```ssl
/* Test: Navigate to procedure definition;
:PROCEDURE HelperProc;
:ENDPROC;

:PROCEDURE Main;
    DoProc("HelperProc");
/*  ^ Go to definition here;
:ENDPROC;
/* Expected: Location of line 1, character 11-21 (HelperProc);
```

### 6.2 Variable Definition (DECLARE)

```ssl
/* Test: Navigate to declared variable;
:PROCEDURE Test;
:DECLARE counter;
x := counter + 1;
/*   ^ Go to definition here;
:ENDPROC;
/* Expected: Location of line 2 (DECLARE line);
```

### 6.3 Variable Definition (PARAMETERS)

```ssl
/* Test: Navigate to parameter;
:PROCEDURE Calculate;
:PARAMETERS nValue, sType;
result := nValue * 2;
/*        ^ Go to definition here;
:ENDPROC;
/* Expected: Location of line 2 (PARAMETERS line);
```

### 6.4 Variable Definition (PUBLIC)

```ssl
/* Test: Navigate to public variable;
:PUBLIC gGlobalCounter;

:PROCEDURE Test;
x := gGlobalCounter;
/*   ^ Go to definition here;
:ENDPROC;
/* Expected: Location of line 1 (PUBLIC line);
```

### 6.5 Case Insensitivity

```ssl
/* Test: Case-insensitive matching;
:PROCEDURE MyProcedure;
:ENDPROC;

:PROCEDURE Main;
    DoProc("myprocedure");
/*  ^ Go to definition here;
:ENDPROC;
/* Expected: Location of line 1 (MyProcedure, despite case difference);
```

### 6.6 Built-in Function (No Definition)

```ssl
/* Test: Built-in function returns null;
result := SQLExecute(query, "ds");
/*        ^ Go to definition here;
/* Expected: null (no definition available);
```

### 6.7 Keyword (No Definition)

```ssl
/* Test: Keyword returns null;
:IF .T.;
/* ^ Go to definition here;
/* Expected: null;
```

### 6.8 Local vs Global Scope

```ssl
/* Test: Local scope takes precedence;
:DECLARE globalVar;

:PROCEDURE Test;
:DECLARE globalVar;  /* Local shadows global;
x := globalVar;
/*   ^ Go to definition here;
:ENDPROC;
/* Expected: Location of line 4 (local DECLARE), not line 1;
```

### 6.9 DoProc String Target (Same File)

```ssl
/* Test: Navigate to procedure from DoProc string;
:PROCEDURE HelperProc;
:PARAMETERS nValue;
:ENDPROC;

:PROCEDURE Main;
    DoProc("HelperProc", {10});
/*         ^ Go to definition here (inside string);
:ENDPROC;
/* Expected: Location of line 1 (HelperProc definition);
```

### 6.10 ExecFunction String Target (Same File)

```ssl
/* Test: Navigate to procedure from ExecFunction string;
:PROCEDURE Calculate;
:ENDPROC;

:PROCEDURE Main;
    result := ExecFunction("Calculate");
/*                          ^ Go to definition here;
:ENDPROC;
/* Expected: Location of line 1 (Calculate definition);
```

---

## 7. Related Issues

| Issue | Description | Status |
|-------|-------------|--------|
| #11 | DoProc/ExecFunction same-file definition | Fixed |
| #16 | Namespace-based file linking for ExecFunction/DoProc | Future (v2.0) |

---

## 8. Implementation Notes

### 8.1 Algorithm

1. Get identifier at cursor position
2. Check if it's a built-in function/keyword → return null
3. Search for procedure definitions matching the name
4. If not found, search for variable declarations in scope order:
   - Current procedure's `:DECLARE`, `:PARAMETERS`
   - Global `:PUBLIC` declarations
5. Return the first match as a Location

### 8.2 Scope Handling

Procedures are scoped globally within the file. Variables are scoped to their declaring procedure, except `:PUBLIC` which is file-global.

### 8.3 Performance

Definition lookup should complete within 50ms. The document is parsed once and cached.
