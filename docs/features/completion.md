# Completion

**Status:** IMPLEMENTED  
**LSP Method:** `textDocument/completion`  
**Source Files:** `internal/providers/completion.go`, `internal/server/handler.go`

---

## 1. Overview

The completion provider offers context-aware suggestions as the user types. It provides completions for SSL keywords, built-in functions, classes, literals, operators, user-defined procedures, variables, and code snippets.

---

## 2. Capabilities

### 2.1 Trigger Characters

Completion is triggered automatically on these characters:

| Character | Context |
|-----------|---------|
| `:` | Triggers keyword completions (`:IF`, `:WHILE`, etc.) |
| `.` | Triggers literal (`.T.`, `.F.`) and operator (`.AND.`) completions |
| `(` | Triggers parameter-related completions |
| `,` | Triggers parameter-related completions |

### 2.2 Completion Types

| Type | Count | Example | CompletionItemKind |
|------|-------|---------|-------------------|
| Keywords | 37 | `:IF`, `:WHILE`, `:DECLARE` | Keyword (14) |
| Built-in Functions | 367 | `SQLExecute`, `Len`, `Upper` | Function (3) |
| Built-in Classes | 30 | `SSLExpando`, `SSLDataset` | Class (7) |
| Literals | 3 | `.T.`, `.F.`, `NIL` | Constant (21) |
| Operators | 3 | `.AND.`, `.OR.`, `.NOT.` | Operator (24) |
| Procedures | Dynamic | User-defined procedures | Function (3) |
| Variables | Dynamic | Declared variables | Variable (6) |
| Snippets | 25+ | Code templates | Snippet (15) |

### 2.3 Context-Aware Filtering

Completions are filtered based on context:

| Context | Completions Offered |
|---------|---------------------|
| After `:` at line start | Keywords |
| After `.` | Literals and operators |
| Start of identifier | Functions, classes, procedures, variables |
| Inside parentheses | Variables, literals |

### 2.4 Completion Item Details

Each completion item includes:

| Property | Content |
|----------|---------|
| `label` | The completion text |
| `kind` | LSP CompletionItemKind |
| `detail` | Brief description or signature |
| `documentation` | Full documentation (Markdown) |
| `insertText` | Text to insert (may include snippet placeholders) |
| `insertTextFormat` | PlainText (1) or Snippet (2) |

---

## 3. Configuration

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| (None currently) | - | - | Completion has no specific configuration |

### Future Configuration Options

| Setting | Purpose |
|---------|---------|
| `ssl.completion.customFunctions` | User-defined function signatures |
| `ssl.completion.customClasses` | User-defined class definitions |
| `ssl.completion.snippets.enabled` | Enable/disable snippets |

---

## 4. Edge Cases & Special Handling

### 4.1 Case Insensitivity

SSL is case-insensitive. Completions match regardless of case:
- Typing `sql` matches `SQLExecute`
- Typing `IF` matches `:IF`

### 4.2 Colon-Prefix Keywords

Keywords should be suggested with the `:` prefix. When the user types `:`, the prefix is included in filtering but the completion inserts the full keyword.

### 4.3 Period-Wrapped Operators

Operators like `.AND.` must include both periods. The completion should insert the full operator.

### 4.4 Inside Strings

Completions should NOT be offered inside string literals (content between `"` or `'`).

### 4.5 Inside Comments

Completions should NOT be offered inside comments (`/* ... ;`).

---

## 5. Known Limitations

| Limitation | Notes |
|------------|-------|
| No custom functions | Cannot define project-specific function signatures |
| No custom classes | Cannot define project-specific classes |
| Single-file scope | Variables from `:INCLUDE` files not available |
| No type inference | Cannot suggest methods based on object type |

---

## 6. Test Specifications

### 6.1 Keyword Completion

```ssl
/* Test: Keyword completion after colon;
:I|
/* Expected: CompletionList containing :IF, :INCLUDE, :INHERIT;

/* Test: Keyword completion mid-word;
:WHIL|
/* Expected: CompletionList containing :WHILE;
```

### 6.2 Function Completion

```ssl
/* Test: Built-in function completion;
SQLEx|
/* Expected: CompletionList containing SQLExecute, SQLExecDirect, etc.;
/* Each item should have:
   - kind: Function (3)
   - detail: Signature
   - documentation: Full description
;
```

### 6.3 Literal Completion

```ssl
/* Test: Boolean literal completion;
x := .|
/* Expected: CompletionList containing .T., .F.;

/* Test: NIL literal;
x := NI|
/* Expected: CompletionList containing NIL;
```

### 6.4 Operator Completion

```ssl
/* Test: Logical operator completion;
:IF x .A|
/* Expected: CompletionList containing .AND.;
```

### 6.5 Variable Completion

```ssl
/* Test: Declared variable completion;
:PROCEDURE Test;
:DECLARE myVariable;
myV|
/* Expected: CompletionList containing myVariable;
:ENDPROC;
```

### 6.6 Procedure Completion

```ssl
/* Test: User procedure completion;
:PROCEDURE HelperFunction;
:ENDPROC;

:PROCEDURE Main;
Hel|
/* Expected: CompletionList containing HelperFunction;
:ENDPROC;
```

### 6.7 Context Exclusion

```ssl
/* Test: No completion in strings;
x := "some text SQL|";
/* Expected: No completions;

/* Test: No completion in comments;
/* This is a comment SQL|;
/* Expected: No completions;
```

### 6.8 Snippet Completion

```ssl
/* Test: Snippet for IF block;
:IF|
/* Expected: CompletionList containing snippet:
   - label: ":IF...:ENDIF"
   - insertText: ":IF ${1:condition};\n\t$0\n:ENDIF;"
   - insertTextFormat: Snippet (2)
;
```

---

## 7. Related Issues

| Issue | Description | Status |
|-------|-------------|--------|
| #46 | Missing configuration for customizing auto-complete | Open |
| #18 | CreateUDObject member tracking | Future Enhancement |

---

## 8. Implementation Notes

### 8.1 Completion Resolution

The `resolveProvider` capability is set to `false`. All completion item details are provided upfront to avoid a second round-trip.

### 8.2 Performance

Completion should return within 100ms. The built-in function list is pre-loaded at startup for fast access.

### 8.3 Sorting

Completions are sorted by:
1. Exact prefix matches first
2. Then by relevance (context-appropriate items)
3. Then alphabetically
