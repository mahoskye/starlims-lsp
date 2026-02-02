# Folding Ranges

**Status:** IMPLEMENTED  
**LSP Method:** `textDocument/foldingRange`  
**Source Files:** `internal/providers/symbols.go`, `internal/server/handler.go`

---

## 1. Overview

The folding ranges provider enables code folding in the editor. Users can collapse procedures, regions, and multi-line comments to focus on specific sections of code.

---

## 2. Capabilities

### 2.1 Foldable Elements

| Element | Fold Kind | Start | End |
|---------|-----------|-------|-----|
| Procedure | region | `:PROCEDURE` | `:ENDPROC` |
| Region | region | `/* region` | `/* endregion` |
| Multi-line Comment | comment | `/*` | `;` |
| IF Block | region | `:IF` | `:ENDIF` |
| WHILE Block | region | `:WHILE` | `:ENDWHILE` |
| FOR Block | region | `:FOR` | `:NEXT` |
| CASE Block | region | `:BEGINCASE` | `:ENDCASE` |
| TRY Block | region | `:TRY` | `:ENDTRY` |

### 2.2 Response Format

```json
[
  {
    "startLine": 0,
    "startCharacter": 0,
    "endLine": 10,
    "endCharacter": 8,
    "kind": "region"
  }
]
```

### 2.3 Fold Kinds

| Kind | Description |
|------|-------------|
| `region` | Code regions (procedures, blocks) |
| `comment` | Multi-line comments |
| `imports` | Import sections (not used in SSL) |

---

## 3. Configuration

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| (None currently) | - | - | Folding ranges has no specific configuration |

---

## 4. Edge Cases & Special Handling

### 4.1 Nested Blocks

Nested blocks each get their own folding range:

```ssl
:PROCEDURE Outer;
    :IF condition;      /* Foldable */
        :WHILE x > 0;   /* Foldable */
        :ENDWHILE;
    :ENDIF;
:ENDPROC;              /* Foldable */
```

### 4.2 Unclosed Blocks

If a block is opened but not closed, the folding range extends to the end of the file.

### 4.3 Region Comments

Region markers are case-insensitive:
- `/* region Name ;`
- `/* Region Name ;`
- `/* REGION Name ;`

### 4.4 Multi-line Comments

SSL comments start with `/*` and end with `;`. A multi-line comment is foldable:

```ssl
/* This is a long comment
   that spans multiple lines
   and should be foldable
;
```

### 4.5 Single-line Elements

Elements that fit on a single line are not foldable:

```ssl
:IF x > 0; :RETURN x; :ENDIF;  /* Not foldable - single line */
```

---

## 5. Known Limitations

| Limitation | Notes |
|------------|-------|
| No import folding | SSL uses `:INCLUDE` but not grouped |
| No class folding | `:CLASS` blocks not specifically handled |

---

## 6. Test Specifications

### 6.1 Procedure Folding

```ssl
/* Test: Procedure is foldable */
:PROCEDURE MyProc;       /* Line 0 */
:DECLARE x;              /* Line 1 */
x := 1;                  /* Line 2 */
:ENDPROC;                /* Line 3 */
/* Expected folding range:
   { startLine: 0, endLine: 3, kind: "region" }
*/
```

### 6.2 Region Folding

```ssl
/* Test: Region is foldable */
/* region Helpers ;      /* Line 0 */
:PROCEDURE Helper;       /* Line 1 */
:ENDPROC;                /* Line 2 */
/* endregion ;           /* Line 3 */
/* Expected folding ranges:
   { startLine: 0, endLine: 3, kind: "region" },  // Region
   { startLine: 1, endLine: 2, kind: "region" }   // Procedure
*/
```

### 6.3 Comment Folding

```ssl
/* Test: Multi-line comment is foldable */
/* This is a                /* Line 0 */
   multi-line               /* Line 1 */
   comment                  /* Line 2 */
;                           /* Line 3 */
/* Expected folding range:
   { startLine: 0, endLine: 3, kind: "comment" }
*/
```

### 6.4 Control Flow Folding

```ssl
/* Test: IF block is foldable */
:IF condition;              /* Line 0 */
    DoSomething();          /* Line 1 */
    DoMore();               /* Line 2 */
:ENDIF;                     /* Line 3 */
/* Expected folding range:
   { startLine: 0, endLine: 3, kind: "region" }
*/
```

### 6.5 Nested Folding

```ssl
/* Test: Nested blocks have separate ranges */
:PROCEDURE Test;            /* Line 0 */
    :IF x > 0;              /* Line 1 */
        :WHILE y < 10;      /* Line 2 */
            y := y + 1;     /* Line 3 */
        :ENDWHILE;          /* Line 4 */
    :ENDIF;                 /* Line 5 */
:ENDPROC;                   /* Line 6 */
/* Expected folding ranges:
   { startLine: 0, endLine: 6, kind: "region" },  // Procedure
   { startLine: 1, endLine: 5, kind: "region" },  // IF
   { startLine: 2, endLine: 4, kind: "region" }   // WHILE
*/
```

### 6.6 Single-Line Block (Not Foldable)

```ssl
/* Test: Single-line block not foldable */
:IF x > 0; :RETURN x; :ENDIF;
/* Expected: No folding range for this line */
```

### 6.7 Unclosed Block

```ssl
/* Test: Unclosed block extends to end */
:PROCEDURE Test;            /* Line 0 */
    :IF x > 0;              /* Line 1 */
        DoSomething();      /* Line 2 */
/* Missing :ENDIF and :ENDPROC */
/* Expected folding ranges:
   { startLine: 0, endLine: 2, kind: "region" },  // Procedure to EOF
   { startLine: 1, endLine: 2, kind: "region" }   // IF to EOF
*/
```

---

## 7. Related Issues

| Issue | Description | Status |
|-------|-------------|--------|
| #19 | Region support for minimap highlighting | Fixed (regions work) |

---

## 8. Implementation Notes

### 8.1 Algorithm

1. Parse document for block keywords
2. Match start/end keyword pairs
3. Calculate line ranges for each pair
4. Filter out single-line blocks
5. Return folding ranges

### 8.2 Keyword Pairs

| Start | End |
|-------|-----|
| `:PROCEDURE` | `:ENDPROC` |
| `:IF` | `:ENDIF` |
| `:WHILE` | `:ENDWHILE` |
| `:FOR` | `:NEXT` |
| `:BEGINCASE` | `:ENDCASE` |
| `:TRY` | `:ENDTRY` |
| `/* region` | `/* endregion` |
| `/*` (multi-line) | `;` |

### 8.3 Performance

Folding range calculation should complete within 50ms. Uses same parsing as document symbols for efficiency.
