# Hover

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
| Keywords | Description and usage example |
| Built-in Functions | Signature, parameters, return type, description |
| Built-in Classes | Class name and description |
| Literals (`.T.`, `.F.`, `NIL`) | Description of the literal value |
| Operators (`.AND.`, `.OR.`, `.NOT.`) | Description and usage |
| User Procedures | Signature with parameters from `:PARAMETERS` |
| Variables | Declaration location and type (if determinable) |

### 2.2 Hover Format

Hover content is rendered as Markdown with code blocks:

```markdown
```ssl
function SQLExecute(cSQL: String, cDSName: String): Dataset
```

Executes a SQL statement and returns a dataset.

**Parameters:**
- `cSQL`: The SQL statement to execute
- `cDSName`: The dataset name

**Returns:** Dataset containing query results
```

### 2.3 Hover Response Structure

```json
{
  "contents": {
    "kind": "markdown",
    "value": "..."
  },
  "range": {
    "start": { "line": 5, "character": 4 },
    "end": { "line": 5, "character": 14 }
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

Hover lookup is case-insensitive:
- Hovering over `sqlexecute` shows info for `SQLExecute`

### 4.2 Inside Strings - PARTIAL

**Current Behavior:** Hover may activate for identifiers inside strings.

**Expected Behavior:** Hover should NOT show function info inside strings, EXCEPT for:
- SQL placeholders (`?varName?`) - show variable info
- Procedure names in `DoProc`/`ExecFunction` - show procedure info

### 4.3 Inside Comments

**Expected Behavior:** Hover should NOT activate for content inside comments (`/* ... ;`).

### 4.4 Property Access

When hovering over `object:property`:
- Hovering over `object` shows the variable's declaration
- Hovering over `property` should show nothing (property info not available)

### 4.5 The `Me` Keyword

Hovering over `Me` should show:
```markdown
`Me` - Self-reference to the current class instance.

Used within `:CLASS` blocks to access instance properties and methods.
```

### 4.6 SQL Placeholders - PLANNED

**Named Parameters (`?varName?`):**

Hovering over `?myVar?` should show:
- Variable name
- Declaration location
- Value (if statically determinable)

**Positional Parameters (`?`):**

Hovering over `?` should show:
- Parameter position (1st, 2nd, etc.)
- Corresponding array element (if determinable from context)

---

## 5. Known Limitations

| Limitation | Notes |
|------------|-------|
| SQL placeholder hover | Not yet implemented |
| Object method hover | No type tracking for object methods |
| `:INCLUDE` definitions | Cannot show info from included files |
| Custom function hover | No support for project-defined functions |

---

## 6. Test Specifications

### 6.1 Keyword Hover

```ssl
/* Test: Hover over keyword */
:IF .T.;
/* Hover position: line 1, character 1 (over "IF") */
/* Expected: Markdown content describing :IF usage */
```

### 6.2 Built-in Function Hover

```ssl
/* Test: Hover over built-in function */
result := SQLExecute(query, "ds");
/* Hover position: line 1, character 12 (over "SQLExecute") */
/* Expected:
   ```ssl
   function SQLExecute(cSQL: String, cDSName: String): Dataset
   ```
   Executes a SQL statement...
   
   **Parameters:**
   - `cSQL`: ...
   - `cDSName`: ...
   
   **Returns:** Dataset
*/
```

### 6.3 Built-in Class Hover

```ssl
/* Test: Hover over built-in class */
obj := CreateUDObject("SSLExpando");
/* Hover position: over "SSLExpando" */
/* Expected: Description of SSLExpando class */
```

### 6.4 Literal Hover

```ssl
/* Test: Hover over boolean literal */
bFlag := .T.;
/* Hover position: over ".T." */
/* Expected: ".T. - Boolean true value" */

/* Test: Hover over NIL */
x := NIL;
/* Hover position: over "NIL" */
/* Expected: "NIL - Null/empty value" */
```

### 6.5 Operator Hover

```ssl
/* Test: Hover over logical operator */
:IF a .AND. b;
/* Hover position: over ".AND." */
/* Expected: ".AND. - Logical AND operator" */
```

### 6.6 User Procedure Hover

```ssl
/* Test: Hover over user-defined procedure */
:PROCEDURE CalculateTotal;
:PARAMETERS nPrice, nQuantity;
:ENDPROC;

:PROCEDURE Main;
    CalculateTotal(10, 5);
/* Hover position: over "CalculateTotal" on line 6 */
/* Expected:
   ```ssl
   procedure CalculateTotal(nPrice, nQuantity)
   ```
   Defined at line 1
*/
:ENDPROC;
```

### 6.7 Variable Hover

```ssl
/* Test: Hover over declared variable */
:PROCEDURE Test;
:DECLARE myCounter;
x := myCounter + 1;
/* Hover position: over "myCounter" on line 4 */
/* Expected: "myCounter - Declared at line 3" */
:ENDPROC;
```

### 6.8 DoProc Signature Hover

```ssl
/* Test: Hover over DoProc shows correct signature */
DoProc("MyProcedure", {arg1, arg2});
/* Hover position: over "DoProc" */
/* Expected:
   ```ssl
   function DoProc(cProcedureName: String, aArgs: Array): Any
   ```
   (First parameter is procedure name, not just "any[] args")
*/
```

### 6.9 Context Exclusion

```ssl
/* Test: No hover inside strings (for non-SQL content) */
x := "SQLExecute is a function";
/* Hover position: over "SQLExecute" inside the string */
/* Expected: No hover (or null response) */

/* Test: No hover inside comments */
/* SQLExecute would be here ;
/* Hover position: over "SQLExecute" inside comment */
/* Expected: No hover (or null response) */
```

### 6.10 SQL Placeholder Hover (PLANNED)

```ssl
/* Test: Hover over named SQL parameter */
:DECLARE sCustomer;
sCustomer := "ACME";
sSQL := "SELECT * FROM customers WHERE name = ?sCustomer?";
/* Hover position: over "sCustomer" inside the ?...? */
/* Expected:
   **SQL Parameter:** `sCustomer`
   **Value:** "ACME"
   Declared at line 2
*/

/* Test: Hover over positional SQL placeholder */
RunSQL("SELECT * FROM t WHERE a = ? AND b = ?", {val1, val2});
/* Hover position: over second "?" */
/* Expected:
   **Positional SQL Placeholder**
   Parameter 2
*/
```

---

## 7. Related Issues

| Issue | Description | Status |
|-------|-------------|--------|
| #30 | DoProc hover shows incorrect signature | To Verify |
| #27 | SQL functions inside strings trigger hover | To Fix |
| #15 | Hover for named SQL parameters | Planned |
| #13 | Hover for positional SQL placeholders | Planned |
| #37 | Unnecessary "Usage Frequency" line | Fixed |

---

## 8. Implementation Notes

### 8.1 Performance

Hover should respond within 50ms. Token lookup is O(n) where n is document length, but short-circuits on match.

### 8.2 Range Calculation

The hover response includes a `range` indicating exactly which characters the hover applies to. This helps the editor highlight the hovered element.

### 8.3 Function Signature Database

All 367 built-in functions have pre-defined signatures in `internal/constants/signatures.go` with:
- Function name
- Parameter list (name, type, optional flag)
- Return type
- Description
