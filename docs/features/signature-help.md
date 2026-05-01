# Signature Help

**Status:** IMPLEMENTED  
**LSP Method:** `textDocument/signatureHelp`  
**Source Files:** `internal/providers/signaturehelp.go`, `internal/server/handler.go`

---

## 1. Overview

The signature help provider displays function parameter information while the user is typing a function call. It shows the full signature and highlights the currently active parameter based on cursor position.

---

## 2. Capabilities

### 2.1 Trigger Characters

| Character | Behavior |
|-----------|----------|
| `(` | Opens signature help for function before `(` |
| `,` | Updates active parameter to next position |

### 2.2 Retrigger Characters

| Character | Behavior |
|-----------|----------|
| `,` | Re-evaluates active parameter position |

### 2.3 Signature Information

| Property | Description |
|----------|-------------|
| `label` | Full function signature string |
| `documentation` | Function description (Markdown) |
| `parameters` | Array of parameter info |
| `activeParameter` | Index of current parameter (0-based) |

### 2.4 Parameter Information

Each parameter includes:

| Property | Description |
|----------|-------------|
| `label` | Parameter portion of signature (e.g., `commandString: variant`) |
| `documentation` | Parameter description |

### 2.5 Coverage

- **330 canonical built-in functions** with parameter documentation
- Built-in dispatch helpers such as `DoProc` and `ExecFunction`
- No direct user-procedure signature resolution beyond those built-in dispatch helpers

---

## 3. Configuration

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| (None currently) | - | - | Signature help has no specific configuration |

---

## 4. Edge Cases & Special Handling

### 4.1 Nested Function Calls

When functions are nested, signature help shows the innermost function:

```ssl
Upper(AllTrim(|))
/*               ^ Cursor here shows AllTrim signature;
```

### 4.2 Optional Parameters

Optional parameters are indicated in the signature:

```ssl
DoProc(name: string, [args: array]) → variant
/*                  ^--- Optional bracketed parameter;
```

### 4.3 No Signature Found

If the function is not recognized, return null (no signature help).

### 4.4 Direct User Procedure Calls

Direct custom procedure calls are invalid SSL, so signature help intentionally does not surface signatures for `MyProc(...)` patterns. Runtime dispatch goes through `DoProc(...)` or `ExecFunction(...)`, and the current provider only shows the built-in helper signatures for those calls.

---

## 5. Known Limitations

| Limitation | Notes |
|------------|-------|
| Single signature per function | No overload support |
| No DoProc/ExecFunction resolution | Cannot look up called procedure's signature |

---

## 6. Test Specifications

### 6.1 Basic Signature Help

```ssl
/* Test: Opening parenthesis triggers signature;
SQLExecute(|
/* Position: after '(';
/* Expected:
   signatures: [{
     label: "SQLExecute(commandString: variant, [friendlyName: variant], [rollbackExistingTransaction: variant], [nullAsBlank: variant], [invariantDateColumns: variant], [returnType: variant], [tableName: variant], [includeSchema: variant], [includeHeader: variant]) → variant",
     parameters: [
       { label: "commandString: variant" },
       { label: "friendlyName: variant" }
     ]
   }],
   activeParameter: 0
;
```

### 6.2 Active Parameter Tracking

```ssl
/* Test: First parameter active;
SQLExecute(query|
/* Expected: activeParameter: 0;

/* Test: Second parameter active after comma;
SQLExecute(query, |
/* Expected: activeParameter: 1;

/* Test: Second parameter still active mid-argument;
SQLExecute(query, dsName|
/* Expected: activeParameter: 1;
```

### 6.3 Nested Function Calls

```ssl
/* Test: Innermost function signature shown;
Upper(AllTrim(|))
/* Expected: Signature for AllTrim, not Upper;
/* activeParameter: 0 for AllTrim;

/* Test: Outer function after closing inner;
Upper(AllTrim(sValue)|)
/* Expected: Signature for Upper;
/* activeParameter: 0 (the AllTrim(sValue) result);
```

### 6.4 Multiple Commas

```ssl
/* Test: Third parameter;
SQLExecute(sSQL, sFriendlyName, .T., |)
/* Expected: activeParameter: 3;
```

### 6.5 Direct User Procedure Call

```ssl
/* Test: Direct user procedure call should not produce signature help;
:PROCEDURE Calculate;
:PARAMETERS nValue, sType, bFlag;
:ENDPROC;

Calculate(|
/* Expected: null because custom procedures are invoked via DoProc/ExecFunction;
```

### 6.6 Unknown Function

```ssl
/* Test: Unknown function returns null;
UnknownFunc(|
/* Expected: null (no signature help);
```

### 6.7 Outside Function Call

```ssl
/* Test: No signature help outside parens;
x := 5;|
/* Expected: null;
```

---

## 7. Related Issues

| Issue | Description | Status |
|-------|-------------|--------|
| (None) | - | - |

---

## 8. Implementation Notes

### 8.1 Active Parameter Calculation

The active parameter is determined by counting commas between the opening `(` and the cursor position, accounting for:
- Nested parentheses (commas inside inner calls don't count)
- String literals (commas inside strings don't count)

### 8.2 Function Detection

Walk backward from cursor to find the function name:
1. Find the matching `(` for current context
2. Extract identifier immediately before `(`
3. Look up in signature database

### 8.3 Performance

Signature help should respond within 50ms. The signature database is pre-indexed for O(1) lookup.
