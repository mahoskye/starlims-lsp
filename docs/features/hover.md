# Hover

> **Normative source:** [`feature.hover`](../../catalog/features/hover.md) in the behavior catalog. This page is a guide; when it disagrees with the catalog entry, the entry wins.

**Status:** IMPLEMENTED  
**LSP Method:** `textDocument/hover`  
**Source Files:** `internal/providers/hover.go`, `internal/server/handler.go`

---

## 1. Overview

The hover provider displays contextual information when the user hovers over identifiers in the code. It shows function signatures, parameter documentation, keyword descriptions, and variable declarations.

---

## 2. Capabilities

### 2.1 Hover Content by Element Type

| Element | Hover Shows |
|---------|-------------|
| Keywords | Description |
| Built-in Functions | Signature, parameters, return type, description |
| Built-in Classes | Summary, base class, constructors with parameter lists, properties table, methods table |
| Core SSL Types (`array`, `boolean`, `codeblock`, `date`, `netobject`, `number`, `object`, `string`) | Runtime type, supported operators, members |
| Special Forms (`access-modifiers`, `base`, `code-block`, `code-organization`, `constructor`, `me`, `request`, `response`) | Summary and canonical syntax block (`request`/`response` render only through the endpoint ambient hover below) |
| Returns Objects (`HttpClient`, `HttpResponse`, `SoapClient`, `SSLRequest`, `SSLResponse`, …) | Summary, properties table, methods table, element meta |
| Endpoint Ambients (`Request` / `Response`, endpoint files only) | Special-form summary/syntax plus the backing `SSLRequest`/`SSLResponse` member tables |
| Typed-Receiver Members (`oResp:ContentType` where `oResp` came from a producer chain) | The member's type/access/description, attributed to its class or returns object |
| Literals (`.T.`, `.F.`, `NIL`) | Description of the literal value |
| Operators (`.AND.`, `.OR.`, `+=`, `==`, `$`, …) | Description, usage, and type-behavior table for binary operators |
| User Procedures | Signature with parameters from `:PARAMETERS` |
| Variables | Declaration location and scope |

### 2.2 Hover Format

Hover content is rendered as Markdown text:

```markdown
**SQLExecute(commandString: variant, [friendlyName: variant], ...)**

Built-in SSL function

**Parameters:**
- `commandString`: The SQL command to execute
- `friendlyName`: Optional logging/friendly name
- `...`: Additional optional execution controls

**Returns:** Variant result routed by SQLExecute
```

### 2.3 Hover Response Structure

```json
{
  "contents": {
    "kind": "markdown",
    "value": "..."
  }
}
```

---

## 3. Configuration

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| (None currently) | - | - | Hover has no specific configuration |

---

## 4. Edge Cases & Special Handling

### 4.1 Case Insensitivity

Hover lookup normalizes built-ins for convenience:
- Hovering over `sqlexecute` shows info for `SQLExecute`

Valid SSL source still requires colon-prefixed uppercase keywords, while identifiers/functions remain case-insensitive and `.T.`, `.F.`, `NIL`, `Me`, `Base`, and `Constructor` are case-insensitive.

### 4.2 Inside Strings

**Current Behavior:** General symbol hover is suppressed inside strings. The only supported string-context hover is SQL placeholder hover (`?varName?` or positional `?`).

### 4.3 Inside Comments

**Expected Behavior:** Hover should NOT activate for content inside comments (`/* ... ;`).

### 4.4 Property Access

When hovering over `object:property`:
- Hovering over `object` shows the variable's declaration
- Hovering over `property` should show nothing (property info not available)

### 4.5 Class-Context Keywords

Hovering over `Me` should show:
```markdown
`Me` - Self-reference to the current class instance.

Used within `:CLASS` blocks to access instance properties and methods.
```

Hovering over `Base` shows that it refers to inherited members, must be used as `Base:Member` (cannot stand alone), and is only meaningful inside a `:CLASS` that declares `:INHERIT`.

Hovering over `Constructor` shows that it is the reserved constructor name inside `:CLASS`.

### 4.6 SQL Placeholders

**Named Parameters (`?varName?`, `?oObj:Prop?`, `?aArr[i]?`, `?Func()?`):**

Named `?varName?` placeholders are exclusive to `SQLExecute`. Other database functions (`RunSQL`, `LSearch`, `LSelect`, `LSelect1`, `LSelectC`, `GetDataSet`) use positional `?` with explicit value arrays.

Hovering over `?myVar?` inside a SQL string shows:
- Parameter name and placeholder kind
- Runtime substitution note

**Positional Parameters (`?`):**

Hovering over `?` inside a SQL string shows:
- Parameter position (1st, 2nd, etc.)
- Note about corresponding array element

---

## 5. Known Limitations

| Limitation | Notes |
|------------|-------|
| Object method hover | Typed-receiver tracking covers class instances and returns objects from producer chains (issue #123); ad-hoc object methods outside those patterns are untracked |
| `:INCLUDE` definitions | Cannot show info from included files |
| Custom function hover | No support for project-defined functions |

---

## 6. Test Specifications

### 6.1 Keyword Hover

```ssl
/* Test: Hover over keyword;
:IF .T.;
/* Hover position: line 1, character 1 (over "IF");
/* Expected: Markdown content describing :IF usage;
```

### 6.2 Built-in Function Hover

```ssl
/* Test: Hover over built-in function;
result := SQLExecute(query, "ds");
/* Hover position: line 1, character 12 (over "SQLExecute");
/* Expected:
   Markdown text beginning with the function label/signature for SQLExecute
   followed by the built-in function description and parameter list
   
   **Parameters:**
   - `commandString`: ...
   - `friendlyName`: ...
   
   **Returns:** variant
;
```

### 6.3 Built-in Class Hover

```ssl
/* Test: Hover over built-in class;
obj := SSLExpando{};
/* Hover position: over "SSLExpando";
/* Expected: Description of SSLExpando class;
```

### 6.4 Literal Hover

```ssl
/* Test: Hover over boolean literal;
bFlag := .T.;
/* Hover position: over ".T.";
/* Expected: ".T. - Boolean true value";

/* Test: Hover over NIL;
x := NIL;
/* Hover position: over "NIL";
/* Expected: "NIL - Null/empty value";
```

### 6.5 Operator Hover

```ssl
/* Test: Hover over logical operator;
:IF a .AND. b;
/* Hover position: over ".AND.";
/* Expected: ".AND. - Logical AND operator";
```

### 6.6 User Procedure Hover

```ssl
/* Test: Hover over user-defined procedure;
:PROCEDURE CalculateTotal;
:PARAMETERS nPrice, nQuantity;
:ENDPROC;
/* Hover position: over "CalculateTotal" on line 2;
/* Expected:
   Markdown text showing the procedure name, parameter list, and declaration location
;
```

### 6.7 Variable Hover

```ssl
/* Test: Hover over declared variable;
:PROCEDURE Test;
:DECLARE nCounter;
x := nCounter + 1;
/* Hover position: over "nCounter" on line 4;
/* Expected: "nCounter - Declared at line 3";
:ENDPROC;
```

### 6.8 DoProc Signature Hover

```ssl
/* Test: Hover over DoProc shows correct signature;
DoProc("MyProcedure", {arg1, arg2});
/* Hover position: over "DoProc";
/* Expected:
   Markdown text showing the DoProc signature and noting that the second argument
   is optional and should be omitted entirely when there are no parameters
;
```

### 6.9 Context Exclusion

```ssl
/* Test: No hover inside strings (for non-SQL content);
x := "SQLExecute is a function";
/* Hover position: over "SQLExecute" inside the string;
/* Expected: No hover (or null response);

/* Test: No hover inside comments;
/* SQLExecute would be here;
/* Hover position: over "SQLExecute" inside comment;
/* Expected: No hover (or null response);
```

### 6.10 Constructor Hover

```ssl
/* Test: Hover over Constructor in class;
:CLASS MyClass;
:PROCEDURE Constructor;
:ENDPROC;
/* Hover position: over "Constructor";
/* Expected:
   Constructor — reserved constructor name inside :CLASS
   :RETURN cannot return a value from a constructor
;
```

### 6.11 SQL Placeholder Hover

```ssl
/* Test: Hover over named SQL parameter;
:DECLARE sCustomer;
sCustomer := "ACME";
sSQL := "SELECT * FROM customers WHERE name = ?sCustomer?";
/* Hover position: over "sCustomer" inside the ?...?;
/* Expected:
   **SQL Parameter: sCustomer**
   Named parameter placeholder
   This placeholder will be replaced with the value of `sCustomer` at runtime.
;

/* Test: Hover over positional SQL placeholder;
RunSQL("SELECT * FROM t WHERE a = ? AND b = ?", {val1, val2});
/* Hover position: over second "?";
/* Expected:
   **SQL Parameter #2**
   Positional parameter placeholder
;
```

---

## 7. Related Issues

| Issue | Description | Status |
|-------|-------------|--------|
| #30 | DoProc hover shows incorrect signature | Fixed |
| #27 | SQL functions inside strings trigger hover | Fixed |
| #15 | Hover for named SQL parameters | Fixed |
| #13 | Hover for positional SQL placeholders | Fixed |
| #37 | Unnecessary "Usage Frequency" line | Fixed |

---

## 8. Implementation Notes

### 8.1 Performance

Hover should respond within 50ms. Token lookup is O(n) where n is document length, but short-circuits on match.

### 8.2 Range Calculation

The current server returns hover contents without an explicit hover range.

### 8.3 Function Signature Database

All 330 canonical built-in functions are exposed through the canonical inventory in `internal/constants/canonical.go`, backed by the legacy signature corpus in `internal/constants/signatures.go`, with:
- Function name
- Parameter list (name, type, optional flag)
- Return type
- Description
