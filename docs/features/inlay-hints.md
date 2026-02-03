# Inlay Hints

**Status:** IMPLEMENTED  
**LSP Method:** `textDocument/inlayHint`  
**Source Files:** `internal/providers/inlayhints.go`, `internal/server/wrapper_handler.go`

---

## 1. Overview

Inlay hints display inline parameter names before function arguments, making code more readable without modifying the source. This feature shows parameter names for built-in SSL functions and user-defined procedures called via `DoProc` or `ExecFunction`.

**Example:**

```ssl
/* Without inlay hints;
result := Substr("Hello World", 1, 5);

/* With inlay hints, the editor displays parameter names inline:;
/*   result := Substr(source: "Hello World", startPos: 1, length: 5);
/* Note: The hints are visual only - the actual code is unchanged;
```

---

## 2. Capabilities

### 2.1 Supported Function Types

| Type | Hints Shown | Notes |
|------|-------------|-------|
| Built-in functions | Yes | 367 functions with parameter names |
| DoProc calls | Yes | Shows `sProcName:` and `aParams:` |
| ExecFunction calls | Yes | Same as DoProc |
| DoProc with known procedure | Yes | Also shows inner parameter names |
| User procedure direct calls | No | SSL procedures can't be called directly |

### 2.2 Hint Display

| Property | Value |
|----------|-------|
| Kind | Parameter (2) |
| Format | `paramName:` |
| Position | Before the argument value |
| Padding | Right padding for visual separation |

### 2.3 Built-in Function Hints

For the 367 built-in SSL functions, parameter names come from the signature database:

```ssl
/* Actual code;
x := SQLExecuteScalar(query, "DSN1");

/* Editor displays with hints: SQLExecuteScalar(cSQL: query, cDSName: "DSN1");
```

### 2.4 DoProc/ExecFunction Hints

SSL procedures must be called via `DoProc` or `ExecFunction`. The server provides two levels of hints:

**Level 1: DoProc parameters (always shown)**
```ssl
/* Actual code;
DoProc("Calculate", {100, 25.50});

/* Editor displays: DoProc(sProcName: "Calculate", aParams: {100, 25.50});
```

**Level 2: Inner array parameters (when procedure is known)**

If the procedure name matches a `:PROCEDURE` in the current document:

```ssl
:PROCEDURE Calculate;
:PARAMETERS nQty, nPrice;
:ENDPROC;

/* Actual code;
DoProc("Calculate", {100, 25.50});

/* Editor displays: DoProc(sProcName: "Calculate", aParams: {nQty: 100, nPrice: 25.50});
```

---

## 3. Configuration

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `ssl.inlayHints.enabled` | boolean | `true` | Enable/disable inlay hints |
| `ssl.inlayHints.minParameterCount` | number | `2` | Minimum parameters to show hints |

### 3.1 Minimum Parameter Count

To reduce visual noise, hints are only shown for functions with a minimum number of parameters:

| Setting Value | Effect |
|---------------|--------|
| 1 | Show hints for all functions with parameters |
| 2 | Show hints only for 2+ parameter functions (default) |
| 3 | Show hints only for 3+ parameter functions |

**Example with `minParameterCount: 2`:**
```ssl
Len(sValue);              /* No hints - only 1 parameter;
Substr(s, 1, 5);          /* Shows hints - 3 parameters;
```

### 3.2 Disabling Hints

To disable inlay hints entirely:

```json
{
  "ssl.inlayHints.enabled": false
}
```

---

## 4. Edge Cases & Special Handling

### 4.1 Nested Function Calls

Hints are shown for all function calls in an expression:

```ssl
/* Actual code;
x := Trim(Substr(s, 1, 10));

/* Editor displays hints for both Trim and Substr parameters;
```

### 4.2 Partial Arguments

Hints are only shown for arguments that exist:

```ssl
/* Two arguments provided to function with 3 parameters;
Substr(s, 1);

/* Only shows hints for provided arguments;
```

### 4.3 Inside Strings and Comments

No hints are generated for function-like patterns inside strings or comments:

```ssl
/* No hints generated for function calls inside strings;
sCode := "Substr(s, 1, 5)";

/* Comments are also ignored;
```

### 4.4 Unknown Functions

If a function is not in the built-in signature database and is not a user procedure, no hints are shown.

### 4.5 DoProc with Variable Procedure Name

When the procedure name is a variable (not a string literal), only DoProc parameter hints are shown:

```ssl
/* Only DoProc hints shown - procedure unknown;
DoProc(sProcName, aParams);
```

---

## 5. Known Limitations

| Limitation | Description |
|------------|-------------|
| Single-file only | Cannot resolve procedures from other files |
| No `:INCLUDE` resolution | Included procedures not available |
| Array literal hints only | DoProc array arguments must be literal `{...}` |
| No overload support | Single signature per function |
| LSP 3.17 required | Older clients may not support inlay hints |

---

## 6. Test Specifications

### 6.1 Built-in Function with 2+ Parameters

```ssl
/* Test: Built-in function hints;
x := Substr("Hello", 1, 5);
/* Expected hints:
   - Position before "Hello": source:
   - Position before 1: startPos:
   - Position before 5: length:;
```

### 6.2 Built-in Function with 1 Parameter (Below Threshold)

```ssl
/* Test: Single parameter function - no hints when minParameterCount=2;
x := Len(sValue);
/* Expected: No hints generated;
```

### 6.3 DoProc with Known Procedure

```ssl
/* Test: DoProc with resolvable procedure;
:PROCEDURE Calculate;
:PARAMETERS nQty, nPrice;
:ENDPROC;

DoProc("Calculate", {100, 25.50});
/* Expected hints:
   - Position before "Calculate": sProcName:
   - Position before {: aParams:
   - Position before 100: nQty:
   - Position before 25.50: nPrice:;
```

### 6.4 DoProc with Unknown Procedure

```ssl
/* Test: DoProc with unknown procedure;
DoProc("UnknownProc", {x, y, z});
/* Expected hints:
   - Position before "UnknownProc": sProcName:
   - Position before {: aParams:
   - No hints for x, y, z (procedure unknown);
```

### 6.5 ExecFunction (Same as DoProc)

```ssl
/* Test: ExecFunction behavior;
ExecFunction("MyProc", {arg1});
/* Expected hints:
   - Position before "MyProc": sProcName:
   - Position before {: aParams:;
```

### 6.6 Nested Function Calls

```ssl
/* Test: Nested calls get hints;
x := Upper(Trim(Substr(s, 1, 5)));
/* Expected: Hints for all three functions;
```

### 6.7 Function Inside String (Ignored)

```ssl
/* Test: No hints for strings;
s := "Substr(s, 1, 5)";
/* Expected: No hints generated;
```

### 6.8 Disabled Hints

```ssl
/* Test: Hints disabled via config;
/* Config: ssl.inlayHints.enabled = false;
x := Substr("Hello", 1, 5);
/* Expected: No hints generated;
```

### 6.9 Range Filtering

```ssl
/* Test: Only hints in requested range;
/* Request range: lines 5-10;
x := Substr(a, 1, 2);  /* Line 3 - No hints, outside range;
y := Substr(b, 3, 4);  /* Line 7 - Hints generated, in range;
```

---

## 7. Related Issues

| Issue | Description | Status |
|-------|-------------|--------|
| (None) | - | - |

---

## 8. Implementation Notes

### 8.1 LSP 3.17 Support

The glsp library only supports LSP 3.16, so we implement a wrapper handler that:
1. Intercepts `textDocument/inlayHint` requests
2. Parses the params and generates hints
3. Delegates all other methods to the standard handler

### 8.2 Function Call Detection

Function calls are detected by scanning tokens for the pattern:
- `IDENTIFIER` followed by `(` 
- Extract arguments by tracking parenthesis depth
- Handle nested calls correctly

### 8.3 Performance

- Only processes the requested line range (from client)
- Token-based scanning is efficient
- Signature lookup is O(1) via pre-indexed map

---

## 9. Related Features

- [Signature Help](./signature-help.md) - Shows parameter info while typing
- [Hover](./hover.md) - Shows function documentation on hover
- [Completion](./completion.md) - Suggests functions with signatures
