# Diagnostics

> **Normative source:** [`feature.diagnostics_pipeline`](../../catalog/features/diagnostics_pipeline.md) in the behavior catalog. This page is a guide; when it disagrees with the catalog entry, the entry wins.

**Status:** IMPLEMENTED  
**LSP Method:** `textDocument/publishDiagnostics` (server to client)  
**Source Files:** `internal/providers/diagnostics.go`

---

## 1. Overview

The diagnostics provider analyzes SSL code and reports potential issues as squiggly underlines in the editor. Diagnostics are computed on document open, change, and save.

---

## 2. Capabilities

### 2.1 Diagnostic Severity Levels

| Level | LSP Code | Visual | Use Case |
|-------|----------|--------|----------|
| Error | 1 | Red underline | Definite problems causing runtime errors |
| Warning | 2 | Yellow underline | Potential issues or style violations |
| Info | 3 | Blue underline | **Opt-in advisory tier** — see below |
| Hint | 4 | Faint dots | Minor suggestions with a one-keystroke fix |

**The info tier is opt-in** (catalog decision D13): info-severity
diagnostics are advisory detail — style observations and SQL idiom notes
aimed at assistant/LLM consumers and teams that want the full picture —
and are dropped unless `ssl.diagnostics.infoDiagnostics` is enabled
(`--validate --info` on the CLI). A rule listed explicitly in
`ssl.diagnostics.rules` always shows regardless of the gate, which is
also the per-rule promotion path. Errors, warnings, and hints are never
gated. The tier's membership is enumerated in
[CONFIGURATION.md §5.8](../configuration/CONFIGURATION.md).

### 2.2 Implemented Diagnostics

The normative rule list lives in the behavior catalog — one entry per code
slug with expected behavior, executable examples, and decision history:

- Generated table: [docs/reference/DIAGNOSTICS.md](../reference/DIAGNOSTICS.md)
- Entries: [`catalog/diagnostics/`](../../catalog/diagnostics/)

Each row of the generated table gives the rule's default severity; each
entry says whether the rule is always on or opt-in, and why.

### 2.3 Opt-in Diagnostics

Checks with inherent false-positive exposure are off by default
([D5](../../catalog/DECISIONS.md)):

| Diagnostic | Default severity | How to enable |
|------------|------------------|---------------|
| `hungarian_notation` | Warning | `ssl.diagnostics.hungarianNotation`; `--validate --hungarian` (which also enables `hungarian_type_mismatch`) |
| `hungarian_type_mismatch` | Warning | `ssl.diagnostics.hungarianTypes`; `--validate --hungarian-types` |
| `unused_variable` | Hint | `ssl.diagnostics.unusedVariables`; `--validate --strict` |
| `undeclared_variable` | Warning | `--validate --strict` (no editor setting) |
| `invalid_sql_param` | Warning | `--validate --strict` (no editor setting) |
| The entire info tier | Info | `ssl.diagnostics.infoDiagnostics`; `--validate --info` |

### 2.4 Planned Diagnostics

Planned behavior is specified as `status: planned` catalog entries (listed
in [STATUS.md](../STATUS.md)); proposed rules are tracked as
[open issues](https://github.com/mahoskye/starlims-lsp/issues).

---

## 3. Configuration

### 3.1 Diagnostic Options

All diagnostic settings — the opt-in switches above, `hungarianPrefixes`,
`globals`, `maxBlockDepth`, per-rule overrides (`rules`), and
`endpointPatterns` — are documented with their defaults in
[CONFIGURATION.md §5](../configuration/CONFIGURATION.md#5-diagnostic-options).

### 3.2 Configuration via LSP

```json
{
  "ssl": {
    "diagnostics": {
      "hungarianNotation": false,
      "hungarianPrefixes": ["a", "b", "d", "fn", "n", "o", "s", "v"],
      "globals": ["gCurrentUser", "gAppName", "gLimsDate"],
      "maxBlockDepth": 4
    }
  }
}
```

---

## 4. Edge Cases & Special Handling

### 4.1 `:INCLUDE` Paths

**Issue #56:** `:INCLUDE` path components flagged as undeclared.

**Current Behavior:** `:INCLUDE` statements are skipped when undeclared-variable checking is enabled:
```ssl
:INCLUDE File_Helpers.FileWork;
```

**Expected Behavior:** Skip `:INCLUDE` lines entirely for undeclared variable checking. Path components are module references, not variables.

**Implementation:**
```go
normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
if normalized == "INCLUDE" {
    // Skip rest of statement
    continue
}
```

### 4.2 The `Me` Keyword

**Issue #2:** `Me` flagged as undeclared.

**Current Behavior:** `Me` is treated as a built-in identifier and is not flagged.

**Expected Behavior:** `Me` is a built-in class self-reference (like `this` in other languages) and should NEVER be flagged as undeclared.

**Implementation:** `Me`, `Base`, `Constructor`, case-insensitive literal forms (`.T.`, `.F.`, `NIL`), classes, and built-in functions are included in the built-in identifier set.

### 4.3 Function Calls

**Issue #53:** Built-in function calls flagged as undeclared variables.

**Current Behavior:** Built-in function calls are excluded from undeclared-variable checking.

**Expected Behavior:** Any identifier immediately followed by `(` is a function call and should NOT be validated as a variable.

**Implementation:**
```go
// Skip if followed by '('
if nextToken.Text == "(" {
    continue // Function call, not variable
}
```

### 4.4 Configured Globals

**Issue #55:** Configured globals still flagged as undeclared.

**Current Behavior:** Configured globals are recognized as declared names by provider-level undeclared-variable and SQL-parameter checks.

**Expected Behavior:** Variables in `ssl.diagnostics.globals` should be treated as pre-declared and never flagged as undeclared.

**Implementation:**
```go
if isInGlobalsList(varName, opts.GlobalVariables) {
    continue // Configured global, not undeclared
}
```

### 4.5 Property Access

**Issue #22:** Property access confused with variable reference.

**Current Behavior:** May flag `bLog` in `oLogging:bLog := bLog;` incorrectly.

**Expected Behavior:** Identifiers preceded by `:` are property access and should NOT be validated as variables.

**Implementation:**
```go
if previousChar == ':' {
    continue // Property access, not variable
}
```

### 4.6 SQL Parameter Case Sensitivity (Resolved)

**Issues #47, #25:** SQL parameters not matching declared variables due to case.

**Status:** ✅ RESOLVED

SQL parameter matching is now case-insensitive. `?sRunno?` correctly matches `sRUNNO` declared in `:PARAMETERS`.

### 4.7 Comment Block Edge Cases

**Issue #52:** Semicolon inside comment ends comment early.

**Current Behavior:** The diagnostics provider now warns when a block comment ends and more tokens continue on the same line, which usually means an embedded semicolon terminated the comment early:
```ssl
/* This is a comment; this text is CODE not comment
```

**Limit:** This is a heuristic. It catches the common accidental case but does not try to infer author intent for every same-line comment/code pattern.

### 4.8 Additional Style-Guide Enforcement

The diagnostics provider also enforces newer guide rules that are always on:

- `:BEGININLINECODE` must include a block name, either `:BEGININLINECODE Name;` or `:BEGININLINECODE "Name";`
- `:PARAMETERS` must appear immediately after `:PROCEDURE`, and script-level `:PARAMETERS` must appear before top-level statements (leading `:PROCEDURE` blocks are allowed)
- `:DEFAULT` must appear immediately after `:PARAMETERS`
- `:TRY` must contain at least one statement before `:CATCH` or `:FINALLY`
- `:TRY` must have at least one `:CATCH` or `:FINALLY` block, and only one `:CATCH` is allowed
- `:CATCH` must appear before `:FINALLY`
- `:CATCH` never names an exception variable; use `GetLastSSLError()` inside the catch block
- `:FINALLY` must contain at least one statement
- `:ERROR` handlers must contain at least one statement before `:RESUME` or the next scope boundary
- `:RETURN`, `:EXITFOR`, `:EXITWHILE`, and `:LOOP` inside `:FINALLY` are compile-time errors
- Comments that terminate before the line ends are flagged as likely embedded-semicolon mistakes
- Non-numeric `:FOR` loop variables, start values, limits, and `:STEP` values are flagged when the type can be inferred from declarations, assignments, constructors, or built-in function returns
- String `=` comparisons are flagged because `=` does prefix matching for strings (`!=` negates `==`, not `=`, so `=` and `!=` are not logical opposites for strings)
- `NIL` vs `""`, `0`, or `.F.` comparisons are flagged because declared variables start as empty string, not `NIL`
- Non-string operands inferred from literals, declarations, assignments, constructors, or built-in function returns are flagged for `$`
- `NIL` arithmetic/string operations are flagged
- Code blocks inferred from `{|...|}` literals, `fn...` identifiers, or simple assignments are flagged when compared with `=` or `==`
- `:INCLUDE` statements are expected at the top of the file
- `:PUBLIC` declarations are warned because they introduce persistent shared state
- Procedures with more than 20 parameters generate a warning
- Class files are checked for the preferred member order `:INHERIT`, `:DECLARE`, regular methods, then `Constructor`
- `Me` is class-only, and `Base` must be used as `Base:MemberName` inside a class that declares `:INHERIT`
- Inside class methods, `DoProc(...)` is flagged in favor of `Me:MethodName()` / `Base:MethodName()` (because `DoProc` is a compile-time error inside class methods — all forms are rejected)
- `:RETURN` inside a `Constructor` cannot return a value; bare `:RETURN;` is allowed
- Literal `Branch("...")` targets are checked for the required `LABEL` token text

---

## 5. Known Limitations

| Limitation | Notes |
|------------|-------|
| `:INCLUDE` paths flagged | #56 - Handled when undeclared checking is enabled |
| Globals not recognized | #55 - Handled |
| `Me` flagged as undeclared | #2 - Handled |
| Functions flagged as variables | #53 - Handled |
| SQL parameter case | #47, #25 - ✅ Resolved |
| Property access confusion | #22 - Handled |
| Undeclared variable checking | ✅ Implemented (opt-in) |
| Unused variable checking | ✅ Implemented (opt-in) |

The remaining limit is whole-program semantic analysis. Diagnostics stay conservative when types cannot be inferred from local declarations, assignments, constructors, or known built-in function return shapes.

---

## 6. Test Specifications

### 6.1 Unclosed Blocks

```ssl
/* Test: Unclosed IF block;
:PROCEDURE Test;
:IF .T.;
    x := 1;
:ENDPROC;
/* Expected: Error on line 2 - "Unclosed ':IF' - expected ':ENDIF'";

/* Test: Mismatched blocks;
:IF .T.;
:ENDWHILE;
/* Expected: Error on line 2 - "':ENDWHILE' does not match ':IF'";
```

### 6.2 Unmatched Delimiters

```ssl
/* Test: Unmatched parenthesis;
x := (a + b;
/* Expected: Error on line 1 - "Unclosed '('";

/* Test: Mismatched delimiters;
x := (a + b];
/* Expected: Error - "Expected ')' but found ']'";
```

### 6.3 Missing EXITCASE

```ssl
/* Test: Case without EXITCASE;
:BEGINCASE;
:CASE x = 1;
    sResult := "one";
:ENDCASE;
/* Expected: Warning on line 2 - "':CASE' block should end with ':EXITCASE;'";
```

### 6.4 Bare Logical Operators

```ssl
/* Test: AND without periods;
:IF a AND b;
/* Expected: Error on 'AND' - "Use '.AND.' instead of 'AND' for logical operations in SSL";
```

### 6.5 DEFAULT on DECLARE

```ssl
/* Test: DEFAULT with DECLARE;
:DECLARE x :DEFAULT 10;
/* Expected: Warning - "':DEFAULT' cannot be used with ':DECLARE' - use ':PARAMETERS' with ':DEFAULT' instead";
```

### 6.6 Global Assignment Prevention

```ssl
/* Test: Assignment to configured global;
/* Config: globals = ["gCurrentUser"];
gCurrentUser := "hacker";
/* Expected: Error - "Cannot assign to global variable 'gCurrentUser'";

/* Test: Using global is OK;
sUser := gCurrentUser;
/* Expected: No error;
```

### 6.7 Hungarian Notation Detection

```ssl
/* Test: Missing Hungarian prefix;
/* Config: hungarianNotation = true;
:DECLARE goodName, sValue;
/* Expected: Warning on 'goodName' - "Variable 'goodName' should use a Hungarian notation prefix";
/* Expected: No warning on 'sValue';

/* Test: Loop counters are exempt;
:DECLARE i, j;
/* Expected: No warning;
```

### 6.8 `:INCLUDE` Path Handling (Expected)

```ssl
/* Test: Include paths not flagged;
:INCLUDE File_Helpers.FileWork;
/* Expected: No warnings on 'File_Helpers' or 'FileWork';
```

### 6.9 `Me` Recognition (Expected)

```ssl
/* Test: Me not flagged as undeclared;
:CLASS MyClass;
    :PROCEDURE Initialize;
        Me:bActive := .T.;
        Me:nCounter := 0;
    :ENDPROC;
/* Note: No :ENDCLASS - class extends to end of file;
/* Expected: No warnings on 'Me';
```

### 6.10 Function Call Recognition (Expected)

```ssl
/* Test: Function calls not flagged as variables;
:PROCEDURE Test;
    infomes("Hello");
    result := SQLExecute(sql, "ds");
    result := Len(sValue);
:ENDPROC;
/* Expected: No warnings on 'infomes', 'SQLExecute', 'Len';
```

### 6.11 Configured Global Recognition (Expected)

```ssl
/* Test: Configured globals recognized as declared;
/* Config: globals = ["gCurrentUser", "gAppName"];
:PROCEDURE Test;
    sUser := gCurrentUser;
    sApp := gAppName;
:ENDPROC;
/* Expected: No warnings on 'gCurrentUser' or 'gAppName';
```

### 6.12 Block Depth Warning

```ssl
/* Test: Excessive nesting warning;
/* Config: maxBlockDepth = 3;
:IF a;
    :IF b;
        :IF c;
            :IF d;  /* Depth 4 - exceeds limit;
            :ENDIF;
        :ENDIF;
    :ENDIF;
:ENDIF;
/* Expected: Warning on line 4 - "Block nesting depth (4) exceeds maximum (3)";
```

### 6.13 SQL Parameter Validation

```ssl
/* Test: Undeclared SQL parameter warning;
/* Config: CheckSQLParams = true;
:PROCEDURE Test;
:DECLARE sName;
sSQL := "SELECT * FROM users WHERE name = ?sName? AND id = ?nId?";
/*                                                        ^^^^;
/* Expected: Warning on '?nId?' - "SQL parameter 'nId' is not declared";
:ENDPROC;

/* Test: Declared SQL parameter - no warning;
:PROCEDURE Test2;
:DECLARE sStatus;
sSQL := "SELECT * FROM orders WHERE status = ?sStatus?";
/* Expected: No warning - sStatus is declared;
:ENDPROC;
```

---

## 7. Related Issues

| Issue | Description | Priority | Status |
|-------|-------------|----------|--------|
| #56 | `:INCLUDE` paths flagged | High | Handled |
| #55 | Globals not recognized | High | Handled |
| #2 | `Me` flagged as undeclared | High | Handled |
| #53 | Functions flagged as variables | High | Handled |
| #8 | SQL parameter validation | Medium | ✅ Implemented |
| #47 | SQL param case sensitivity | Medium | ✅ Resolved |
| #25 | SQL param case (duplicate) | Medium | ✅ Resolved |
| #22 | Property access confusion | Medium | Handled |
| #52 | Comment semicolon edge case | Medium | Handled heuristically |
| #4 | Duplicate diagnostics | Low | Closed |
| #3 | Multi-line expression semicolons | Low | Closed |

---

## 8. Implementation Notes

### 8.1 Diagnostic Flow

1. Document opened/changed → tokenize
2. Run enabled diagnostic checks
3. Collect all diagnostics
4. Send `publishDiagnostics` to client

### 8.2 Scope Tracking

Variables are scoped to their declarations:
- `:DECLARE` → procedure scope
- `:PARAMETERS` → procedure scope
- `:PUBLIC` → file scope
- Assignment targets are ignored by undeclared-variable checking on the defining line, but assignments are not treated as declarations

### 8.3 Case Sensitivity

- Keywords: case-sensitive and must use canonical colon-prefixed uppercase forms
- Variables: case-insensitive for matching
- Literals (`.T.`, `.F.`, `NIL`) and class-context forms (`Me`, `Base`, `Constructor`): case-insensitive

### 8.4 Error Recovery

Diagnostics continue after errors to provide complete feedback. One error should not cascade into many false positives.

---

## 9. Skip List for Undeclared Variable Checking

When implementing undeclared variable checking, skip these:

| Category | Examples | Reason |
|----------|----------|--------|
| SSL Keywords | `IF`, `WHILE`, `FOR` | Language constructs |
| Built-in Functions | `SQLExecute`, `Len`, `Upper` | Runtime functions |
| Built-in Classes | `SSLExpando`, `SSLDataset` | Runtime classes |
| Literals | `.T.`, `.F.`, `NIL` | Language literals |
| Operators | `.AND.`, `.OR.`, `.NOT.` | Language operators |
| `Me` identifier | `Me:Property` | Class self-reference |
| Property access | `:PropertyName` | Object member |
| Function calls | `Func(...)` | Identifier + `(` |
| Assignment targets | `x := 1` | Being defined |
| Configured globals | `gCurrentUser` | User-configured |
| `:INCLUDE` paths | `Namespace.Script` | Module references |
| Declaration lines | `:DECLARE x` | Being declared |
