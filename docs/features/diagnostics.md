# Diagnostics

**Status:** PARTIAL  
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
| Info | 3 | Blue underline | Informational hints |
| Hint | 4 | Faint dots | Minor suggestions |

### 2.2 Implemented Diagnostics

| Diagnostic | Severity | Description |
|------------|----------|-------------|
| Unclosed blocks | Error | `:IF` without `:ENDIF`, etc. |
| Unmatched delimiters | Error | `(`, `[`, `{` without matching closer |
| Block depth exceeded | Warning | Nesting exceeds configured maximum |
| Missing EXITCASE | Warning | `:CASE` block without `:EXITCASE` |
| Bare logical operators | Error | `AND` instead of `.AND.` |
| DEFAULT on DECLARE | Warning | `:DEFAULT` only works with `:PARAMETERS` |
| Global assignment | Error | Assignment to configured global variable |
| Hungarian notation | Warning | Optional style check |

### 2.3 Opt-in Diagnostics

These diagnostics are implemented but disabled by default:

| Diagnostic | Severity | Description |
|------------|----------|-------------|
| Undeclared variables | Warning | Variable used without declaration (enable via `CheckUndeclaredVars`) |

### 2.4 Planned Diagnostics

| Diagnostic | Severity | Description |
|------------|----------|-------------|
| Unused variables | Hint | Variable declared but never used |
| SQL parameter validation | Warning | SQL `?param?` not matching declared variable |

---

## 3. Configuration

### 3.1 Diagnostic Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `ssl.diagnostics.enabled` | bool | `true` | Enable diagnostics |
| `ssl.diagnostics.hungarianNotation` | bool | `false` | Check for Hungarian prefixes |
| `ssl.diagnostics.hungarianPrefixes` | string[] | `["a","b","d","n","o","s"]` | Prefixes to detect |
| `ssl.diagnostics.globals` | string[] | `[]` | Pre-declared global variables |
| `ssl.diagnostics.maxBlockDepth` | int | `10` | Maximum block nesting depth |

### 3.2 Configuration via LSP

```json
{
  "ssl": {
    "diagnostics": {
      "hungarianNotation": false,
      "hungarianPrefixes": ["a", "b", "d", "n", "o", "s"],
      "globals": ["gCurrentUser", "gAppName", "gLimsDate"]
    }
  }
}
```

---

## 4. Edge Cases & Special Handling

### 4.1 `:INCLUDE` Paths

**Issue #56:** `:INCLUDE` path components flagged as undeclared.

**Current Behavior:** May flag `File_Helpers` and `FileWork` as undeclared in:
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

**Current Behavior:** May report `Me` as an undeclared variable.

**Expected Behavior:** `Me` is a built-in class self-reference (like `this` in other languages) and should NEVER be flagged as undeclared.

**Implementation:** Add `Me` to the built-in identifier list.

### 4.3 Function Calls

**Issue #53:** Built-in function calls flagged as undeclared variables.

**Current Behavior:** May flag `infomes` in `infomes("test");` as undeclared.

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

**Current Behavior:** Globals are checked for assignment prevention but NOT recognized as declared variables.

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

### 4.6 SQL Parameter Case Sensitivity

**Issues #47, #25:** SQL parameters not matching declared variables due to case.

**Current Behavior:** `?sRunno?` may not match `sRUNNO` declared in `:PARAMETERS`.

**Expected Behavior:** SQL parameter matching should be case-insensitive (SSL is case-insensitive).

### 4.7 Comment Block Edge Cases

**Issue #52:** Semicolon inside comment ends comment early.

**Current Behavior:** May not detect that text after `;` in a comment is actually code:
```ssl
/* This is a comment; this text is CODE not comment
```

**Expected Behavior:** Warn when comment appears to terminate prematurely with text following.

---

## 5. Known Limitations

| Limitation | Notes |
|------------|-------|
| `:INCLUDE` paths flagged | #56 - Priority fix |
| Globals not recognized | #55 - Priority fix |
| `Me` flagged as undeclared | #2 - Priority fix |
| Functions flagged as variables | #53 - Priority fix |
| SQL parameter case | #47, #25 - Medium priority |
| Property access confusion | #22 - Medium priority |
| Undeclared variable checking | Not yet implemented |
| Unused variable checking | Not yet implemented |

---

## 6. Test Specifications

### 6.1 Unclosed Blocks

```ssl
/* Test: Unclosed IF block */
:PROCEDURE Test;
:IF .T.;
    x := 1;
:ENDPROC;
/* Expected: Error on line 2 - "Unclosed ':IF' - expected ':ENDIF'" */

/* Test: Mismatched blocks */
:IF .T.;
:ENDWHILE;
/* Expected: Error on line 2 - "':ENDWHILE' does not match ':IF'" */
```

### 6.2 Unmatched Delimiters

```ssl
/* Test: Unmatched parenthesis */
x := (a + b;
/* Expected: Error on line 1 - "Unclosed '('" */

/* Test: Mismatched delimiters */
x := (a + b];
/* Expected: Error - "Expected ')' but found ']'" */
```

### 6.3 Missing EXITCASE

```ssl
/* Test: Case without EXITCASE */
:BEGINCASE;
:CASE x = 1;
    DoSomething();
:ENDCASE;
/* Expected: Warning on line 2 - "':CASE' block should end with ':EXITCASE;'" */
```

### 6.4 Bare Logical Operators

```ssl
/* Test: AND without periods */
:IF a AND b;
/* Expected: Error on 'AND' - "Use '.AND.' instead of 'AND' for logical operations in SSL" */
```

### 6.5 DEFAULT on DECLARE

```ssl
/* Test: DEFAULT with DECLARE */
:DECLARE x :DEFAULT 10;
/* Expected: Warning - "':DEFAULT' cannot be used with ':DECLARE' - use ':PARAMETERS' with ':DEFAULT' instead" */
```

### 6.6 Global Assignment Prevention

```ssl
/* Test: Assignment to configured global */
/* Config: globals = ["gCurrentUser"] */
gCurrentUser := "hacker";
/* Expected: Error - "Cannot assign to global variable 'gCurrentUser'" */

/* Test: Using global is OK */
sUser := gCurrentUser;
/* Expected: No error */
```

### 6.7 Hungarian Notation Detection

```ssl
/* Test: Hungarian prefix detection */
/* Config: hungarianNotation = true */
:DECLARE sName, nCount;
/* Expected: Warning on 'sName' - "Hungarian notation prefix 's' detected in 'sName'" */
/* Expected: Warning on 'nCount' - "Hungarian notation prefix 'n' detected in 'nCount'" */

/* Test: Not Hungarian (lowercase after prefix) */
:DECLARE sample, number;
/* Expected: No warning */
```

### 6.8 `:INCLUDE` Path Handling (Expected)

```ssl
/* Test: Include paths not flagged */
:INCLUDE File_Helpers.FileWork;
/* Expected: No warnings on 'File_Helpers' or 'FileWork' */
```

### 6.9 `Me` Recognition (Expected)

```ssl
/* Test: Me not flagged as undeclared */
:CLASS MyClass;
:PROCEDURE Initialize;
    Me:bActive := .T.;
    Me:nCounter := 0;
:ENDPROC;
:ENDCLASS;
/* Expected: No warnings on 'Me' */
```

### 6.10 Function Call Recognition (Expected)

```ssl
/* Test: Function calls not flagged as variables */
:PROCEDURE Test;
    infomes("Hello");
    result := SQLExecute(sql, "ds");
    MyCustomProc(x, y);
:ENDPROC;
/* Expected: No warnings on 'infomes', 'SQLExecute', 'MyCustomProc' */
```

### 6.11 Configured Global Recognition (Expected)

```ssl
/* Test: Configured globals recognized as declared */
/* Config: globals = ["gCurrentUser", "gAppName"] */
:PROCEDURE Test;
    sUser := gCurrentUser;
    sApp := gAppName;
:ENDPROC;
/* Expected: No warnings on 'gCurrentUser' or 'gAppName' */
```

### 6.12 Block Depth Warning

```ssl
/* Test: Excessive nesting warning */
/* Config: maxBlockDepth = 3 */
:IF a;
    :IF b;
        :IF c;
            :IF d;  /* Depth 4 - exceeds limit */
            :ENDIF;
        :ENDIF;
    :ENDIF;
:ENDIF;
/* Expected: Warning on line 4 - "Block nesting depth (4) exceeds maximum (3)" */
```

---

## 7. Related Issues

| Issue | Description | Priority | Status |
|-------|-------------|----------|--------|
| #56 | `:INCLUDE` paths flagged | High | Open |
| #55 | Globals not recognized | High | Open |
| #2 | `Me` flagged as undeclared | High | Open |
| #53 | Functions flagged as variables | High | Open |
| #47 | SQL param case sensitivity | Medium | Closed |
| #25 | SQL param case (duplicate) | Medium | Closed |
| #22 | Property access confusion | Medium | Closed |
| #52 | Comment semicolon edge case | Medium | Closed |
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

Variables are scoped to their declaring procedure:
- `:DECLARE` → procedure scope
- `:PARAMETERS` → procedure scope
- `:PUBLIC` → file scope
- Assignment → procedure scope (dynamic declaration)

### 8.3 Case Sensitivity

- Keywords: case-insensitive
- Variables: case-insensitive for matching
- Literals (`.T.`, `.F.`, `NIL`): case-sensitive

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
