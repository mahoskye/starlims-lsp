# Find References

**Status:** IMPLEMENTED  
**LSP Method:** `textDocument/references`  
**Source Files:** `internal/providers/definition.go`, `internal/server/handler.go`

---

## 1. Overview

The references provider finds all occurrences of an identifier within the document. This includes the definition (optionally) and all usages.

---

## 2. Capabilities

### 2.1 Reference Types

| Element | References Found |
|---------|------------------|
| Procedures | `:PROCEDURE` definition + all call sites |
| Variables | Declaration + all usages |
| Parameters | `:PARAMETERS` declaration + all usages |

### 2.2 Request Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `includeDeclaration` | boolean | true | Include the definition in results |

### 2.3 Response Format

```json
[
  {
    "uri": "file:///path/to/file.ssl",
    "range": { "start": { "line": 5, "character": 0 }, "end": { "line": 5, "character": 10 } }
  },
  {
    "uri": "file:///path/to/file.ssl",
    "range": { "start": { "line": 12, "character": 4 }, "end": { "line": 12, "character": 14 } }
  }
]
```

---

## 3. Configuration

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| (None currently) | - | - | References has no specific configuration |

---

## 4. Edge Cases & Special Handling

### 4.1 Case Insensitivity

Reference matching is case-insensitive:
- Searching for `myProc` finds `MyProc`, `MYPROC`, `myproc`

### 4.2 Inside Strings - PARTIAL

**Current Behavior:** May find matches inside strings.

**Expected Behavior:** Should distinguish:
- References in code context → include
- References in comments → exclude
- References in arbitrary strings → exclude
- Procedure names in `DoProc("ProcName")` → include

### 4.3 Inside Comments

**Expected Behavior:** References inside comments (`/* ... ;`) should be excluded.

### 4.4 Whole Word Matching

Only match complete identifiers:
- Searching for `Count` should NOT match `CountAll` or `RecountItems`

### 4.5 Built-in Functions

Built-in functions return references (call sites) but no definition.

### 4.6 Cross-File References - NOT IMPLEMENTED

Currently, references are found only within the same file.

---

## 5. Known Limitations

| Limitation | Notes |
|------------|-------|
| Single-file only | Cannot find references in other files |
| May match in strings | Context detection incomplete |
| No comment exclusion | Comments may be included |

---

## 6. Test Specifications

### 6.1 Procedure References

```ssl
/* Test: Find all references to procedure;
:PROCEDURE HelperProc;
:ENDPROC;

:PROCEDURE Main;
    HelperProc();
    x := HelperProc();
:ENDPROC;
/* Find references on: HelperProc (line 1);
/* Expected (includeDeclaration: true):
   - Line 1, "HelperProc" (definition)
   - Line 5, "HelperProc" (call)
   - Line 6, "HelperProc" (call)
;
```

### 6.2 Variable References

```ssl
/* Test: Find all references to variable;
:PROCEDURE Test;
:DECLARE counter;
counter := 0;
counter := counter + 1;
x := counter;
:ENDPROC;
/* Find references on: counter (line 2);
/* Expected (includeDeclaration: true):
   - Line 2, "counter" (declaration)
   - Line 3, "counter" (assignment target)
   - Line 4, "counter" (twice - target and usage)
   - Line 5, "counter" (usage)
;
```

### 6.3 Exclude Declaration

```ssl
/* Test: Find references without declaration;
:PROCEDURE Test;
:DECLARE myVar;
x := myVar;
:ENDPROC;
/* Find references on: myVar, includeDeclaration: false;
/* Expected:
   - Line 3, "myVar" (usage only)
   NOT line 2 (declaration excluded)
;
```

### 6.4 Case Insensitivity

```ssl
/* Test: Case-insensitive matching;
:PROCEDURE Test;
:DECLARE MyVariable;
x := myvariable;
y := MYVARIABLE;
:ENDPROC;
/* Find references on: MyVariable (line 2);
/* Expected:
   - Line 2, "MyVariable"
   - Line 3, "myvariable"
   - Line 4, "MYVARIABLE"
;
```

### 6.5 Whole Word Matching

```ssl
/* Test: Only whole word matches;
:PROCEDURE Test;
:DECLARE count;
x := count;
y := countAll;
z := recount;
:ENDPROC;
/* Find references on: count (line 2);
/* Expected:
   - Line 2, "count"
   - Line 3, "count"
   NOT line 4 (countAll is different identifier)
   NOT line 5 (recount is different identifier)
;
```

### 6.6 Scoped References

```ssl
/* Test: References respect scope;
:PROCEDURE ProcA;
:DECLARE localVar;
x := localVar;
:ENDPROC;

:PROCEDURE ProcB;
:DECLARE localVar;
y := localVar;
:ENDPROC;
/* Find references on: localVar in ProcA (line 2);
/* Expected:
   - Line 2, "localVar" (ProcA's declaration)
   - Line 3, "localVar" (ProcA's usage)
   NOT line 7 or 8 (different scope)
;
```

### 6.7 Context Exclusion (Expected Behavior)

```ssl
/* Test: Exclude references in comments;
:PROCEDURE Test;
/* This mentions myVar but shouldn't count;
:DECLARE myVar;
x := myVar;
:ENDPROC;
/* Find references on: myVar (line 3);
/* Expected (ideally):
   - Line 3, "myVar" (declaration)
   - Line 4, "myVar" (usage)
   NOT line 2 (inside comment)
;
```

### 6.8 DoProc String Reference (PLANNED)

```ssl
/* Test: Find procedure reference in DoProc string;
:PROCEDURE TargetProc;
:ENDPROC;

:PROCEDURE Main;
    DoProc("TargetProc", {});
:ENDPROC;
/* Find references on: TargetProc (line 1);
/* Expected (when implemented):
   - Line 1, "TargetProc" (definition)
   - Line 5, "TargetProc" inside string (procedure call reference)
;
```

---

## 7. Related Issues

| Issue | Description | Status |
|-------|-------------|--------|
| #36 | Procedure references matched in comments/strings | To Fix |

---

## 8. Implementation Notes

### 8.1 Algorithm

1. Get identifier at cursor position
2. Determine if it's a procedure or variable name
3. Scan all tokens in document for matches
4. Filter based on `includeDeclaration` option
5. Return list of Locations

### 8.2 Context Detection (Future)

To properly exclude comments and non-procedure strings:
1. Track whether each token is inside a comment or string
2. Only include code-context matches
3. Special handling for `DoProc`/`ExecFunction` first parameter

### 8.3 Performance

Reference search should complete within 100ms for typical files. Large files may take longer due to full document scan.
