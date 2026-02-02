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
| `label` | Parameter portion of signature (e.g., `cSQL: String`) |
| `documentation` | Parameter description |

### 2.5 Coverage

- **367 built-in functions** with full parameter documentation
- User-defined procedures with parameters from `:PARAMETERS` declarations

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
OuterFunc(InnerFunc(|))
/*                  ^ Cursor here shows InnerFunc signature;
```

### 4.2 Optional Parameters

Optional parameters are indicated in the signature:

```ssl
function DoProc(cProcName: String, aArgs?: Array): Any
/*                                 ^--- Optional indicator;
```

### 4.3 No Signature Found

If the function is not recognized, return null (no signature help).

### 4.4 User Procedures

For user-defined procedures, parameters are extracted from `:PARAMETERS` declarations but type information is not available:

```ssl
procedure MyProc(param1, param2)
/* No type info - SSL is dynamically typed;
```

---

## 5. Known Limitations

| Limitation | Notes |
|------------|-------|
| No type info for user procedures | SSL is dynamically typed |
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
     label: "SQLExecute(cSQL: String, cDSName: String): Dataset",
     parameters: [
       { label: "cSQL: String" },
       { label: "cDSName: String" }
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
Upper(Trim(|))
/* Expected: Signature for Trim, not Upper;
/* activeParameter: 0 for Trim;

/* Test: Outer function after closing inner;
Upper(Trim(x)|)
/* Expected: Signature for Upper;
/* activeParameter: 0 (the Trim(x) result);
```

### 6.4 Multiple Commas

```ssl
/* Test: Third parameter;
SomeFunc(a, b, |)
/* Expected: activeParameter: 2;
```

### 6.5 User Procedure Signature

```ssl
/* Test: User procedure with parameters;
:PROCEDURE Calculate;
:PARAMETERS nValue, sType, bFlag;
:ENDPROC;

:PROCEDURE Main;
Calculate(|
/* Expected:
   signatures: [{
     label: "Calculate(nValue, sType, bFlag)",
     parameters: [
       { label: "nValue" },
       { label: "sType" },
       { label: "bFlag" }
     ]
   }],
   activeParameter: 0
;
:ENDPROC;
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
