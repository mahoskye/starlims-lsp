# Completion

**Status:** IMPLEMENTED  
**LSP Method:** `textDocument/completion`  
**Source Files:** `internal/providers/completion.go`, `internal/server/handler.go`

---

## 1. Overview

The completion provider offers SSL symbol and snippet suggestions as the user types. It provides completions for SSL keywords, built-in functions, classes, literals, operators, user-defined procedures, variables, and code snippets. Class-only forms such as `Me`, `Base`, and `Constructor` are only suggested when completion is requested inside a `:CLASS` method.

---

## 2. Capabilities

### 2.1 Trigger Characters

Completion is triggered automatically only on `:` — both the SSL keyword
prefix (`:DECLARE`, `:IF`) and the member-access operator (`obj:prop`).
`.`, `,`, and `(` are intentionally **not** advertised as triggers because
they fire too aggressively during list/decimal/expression entry; the popup
they produced often Enter-selected the wrong token (e.g. `.AND.` while
typing a list literal). Use `Ctrl+Space` to invoke completion explicitly
in those contexts.

| Character | Context |
|-----------|---------|
| `:` | After `Me`/`Base`/built-in class/shaped variable: focused member list. Otherwise: keyword completions only. |

When `:` fires with no context-aware match, the server returns **only**
keyword completions — not procedures, variables, or snippets. The full
inventory is reserved for explicit `Ctrl+Space` invocation. See issue #8.

### 2.2 Completion Types

| Type | Count | Example | CompletionItemKind |
|------|-------|---------|-------------------|
| Keywords | 38 | `:IF`, `:WHILE`, `:DECLARE` | Keyword (14) |
| Built-in Functions | 330 | `SQLExecute`, `Len`, `Upper` | Function (3) |
| Built-in Classes | 29 | `SSLExpando`, `SSLDataset`, `Email` | Class (7) |
| Class-context forms | Contextual | `Me`, `Base`, `Constructor` | Keyword/Constructor |
| Literals | 3 | `.T.`, `.F.`, `NIL` | Constant (21) |
| Operators | 3 | `.AND.`, `.OR.`, `.NOT.` | Operator (24) |
| Procedures | Dynamic | User-defined procedures | Function (3) |
| Variables | Dynamic | Declared variables | Variable (6) |
| Snippets | 25+ | Code templates | Snippet (15) |

### 2.3 Current Filtering Rules

The server currently applies only lightweight filtering:

| Context | Behavior |
|---------|----------|
| Inside strings/comments | No completions returned |
| `:` auto-trigger, no context match | Keyword completions only |
| `Me:` / `Base:` (in `:CLASS` file) | Methods/properties of the enclosing class |
| `<BuiltInClass>:` | Methods/properties of that class |
| `<shapedVar>:` | Inferred UDObject properties (see [UDObject Shape Inference](#26-udobject-shape-inference)) |
| Inside `:CLASS` methods (explicit invocation) | Adds class-context forms; inserts procedure completions as `Me:MethodName(...)` |
| Explicit invocation (Ctrl+Space) | Built-in plus document-local completions and snippets |

### 2.6 UDObject Shape Inference

When a variable is initialized with `CreateUDObject({{"key", val}, ...})`,
the server infers a property shape from the dict literal and binds it to
the LHS variable. Subsequent `clone()` calls inherit the same shape:

```ssl
oTemplate := CreateUDObject({
    {"tableName", ""},
    {"exists", .F.}
});

oMetadata := oTemplate:clone();
oMetadata:tableName    /* completion lists tableName, exists */
```

Coarse value types (`string`, `boolean`, `number`, `array`) are extracted
from the initializer values and surfaced in the completion `detail`. The
analysis is file-global with last-write-wins semantics — there is no
per-procedure scoping yet. Variables assigned from procedure-call returns,
parameters, or anywhere outside a recognized `CreateUDObject`/`clone()`
chain do not get a shape; member access on them shows no focused list. See
issue #7.

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

Identifier and function matching are case-insensitive, but keyword completions insert canonical colon-prefixed uppercase forms:
- Typing `sql` matches `SQLExecute`
- Typing `IF` matches `:IF`
- Typing `me` matches `Me`

### 4.2 Colon-Prefix Keywords

Keywords should be suggested with the `:` prefix. When the user types `:`, the prefix is included in filtering but the completion inserts the full keyword.

### 4.3 Period-Wrapped Operators

Operators like `.AND.` must include both periods. The completion should insert the full operator.

### 4.4 Inside Strings

Completions should NOT be offered inside string literals (content between `"` or `'`).

### 4.5 Inside Comments

Completions should NOT be offered inside comments (`/* ... ;`).

### 4.6 Source-Aligned Snippets

Snippet templates follow the bundled style-guide defaults:

- Procedure snippets place `:DEFAULT` immediately after `:PARAMETERS`
- SQL snippets use `SQLExecute` named-parameter style and `RunSQL` positional-parameter style
- Class snippets avoid an extra class-body indent because `:CLASS` extends to EOF and has no `:ENDCLASS`
- Region snippets use comment-based `/* region ...;` / `/* endregion;` rather than legacy functional `:REGION`
- Procedure completions insert `DoProc(...)` dispatch snippets in script files, and `Me:MethodName(...)` snippets when completion is requested inside a `:CLASS` method (because `DoProc` is a compile-time error inside class methods — all forms are rejected)
- Class-context forms (`Me`, `Base`, `Constructor`) are only offered in class-method context

---

## 5. Known Limitations

| Limitation | Notes |
|------------|-------|
| No custom functions | Cannot define project-specific function signatures |
| No custom classes | Cannot define project-specific classes |
| Single-file scope | Variables from `:INCLUDE` files not available |
| No scope filtering | Variables are document-local, not narrowed to the current procedure |
| No semantic ranking | The server does not reorder items by context or prefix relevance |
| No type inference | Cannot suggest methods based on object type |

> **Future work — .NET method dispatch on built-in types.** When the LSP
> grows type-aware member completion or an "unknown member" diagnostic on
> `:` access, suppress warnings for receivers typed `string`, `number`,
> `date`, `array`, `boolean`, or `netobject`: the SSL runtime forwards
> unmatched `:` access on these to the underlying .NET value (e.g.
> `sName:Length`, `sName:ToUpper()`, `dDate:AddDays(1)`, `aList:Count`).
> A reasonable surface: treat unmatched members on these receivers as
> "unresolved .NET member" rather than an error, and offer completions
> only for SSL-side members (the .NET surface is too broad to enumerate).
> See `dev/ssl-style-guide/agent-guides/ssl_agent_instructions.md`
> ("`.NET Method Dispatch on Built-in Types`") for the canonical rule.
> Tracked in [issue #22](https://github.com/mahoskye/starlims-lsp/issues/22).

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
/* Expected: CompletionList containing SQLExecute, SQLRemoveComments, etc.;
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
:DECLARE sMyValue;
sMy|
/* Expected: CompletionList containing sMyValue;
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
| #18 | CreateUdObject member tracking | Future Enhancement |

---

## 8. Implementation Notes

### 8.1 Completion Resolution

The `resolveProvider` capability is set to `false`. All completion item details are provided upfront to avoid a second round-trip.

### 8.2 Performance

Completion should return within 100ms. The built-in function list is pre-loaded at startup for fast access.

### 8.3 Ordering

The server returns completions in provider order. Editors may apply their own filtering and sorting on top of that list.
