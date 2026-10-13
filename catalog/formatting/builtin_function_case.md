---
id: fmt.builtin_function_case
title: Built-in function name casing
kind: formatter
status: active
authority: style_only
schema_ref: null
config:
  - ssl.format.builtinFunctionCase
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-04-30
    ref: "PR #4 (v0.5.0)"
    note: >-
      Added with "preserve" as default — rewriting user casing is opt-in;
      "PascalCase" rewrites call sites to the canonical inventory casing.
  - date: 2026-07-02
    ref: "issue #34"
    note: >-
      The pass now re-lexes the formatted text and only rewrites identifier
      tokens, so built-in names inside string literals and comments are
      never re-cased.
  - date: 2026-07-23
    ref: "issue #92"
    note: >-
      Default flipped to "PascalCase": the style guide mandates the exact
      documented casing (schema naming rule R45), making canonicalization
      authoritative rather than opt-in. "preserve" remains available.
  - date: 2026-10-13
    ref: "issue #255 (catalog audit)"
    note: >-
      Member calls (`oVar:upper(...)`) are no longer re-cased: a `:` member
      is not a built-in function, and members of built-in value types are
      case-sensitive native .NET members at runtime, so the rewrite could
      change what runs. Built-in calls inside code-block bodies are now
      re-cased (they were skipped). Authority set to style_only to mirror
      the style-guide record; the default stays "PascalCase" (D12).
issues: ["#34", "#92"]
---

## Behavior

With `ssl.format.builtinFunctionCase: "PascalCase"` (default — the style
guide's exact documented casing is authoritative, schema R45; issue #92),
a post-format pass rewrites every call site — an identifier immediately followed by `(`,
optionally with intervening spaces/tabs — whose lowercased name matches the
published built-in inventory to the inventory's canonical casing (e.g.
`len(` → `Len(`, `sqlexecute(` → `SQLExecute(`). Setting `"preserve"`
keeps the author's casing.

Identifiers not in the inventory (user procedures, variables) are never
re-cased, and neither are non-call uses of a built-in name (no following
`(`). String literals and comments are literal text and are never rewritten:
the pass re-lexes the formatted output and only touches identifier tokens.
Member calls (`oVar:upper(...)`) are never re-cased: a `:` member is not a
built-in function, and the members of built-in value types are
case-sensitive native .NET members at runtime (issue #255). Calls inside a
code-block body are re-cased like any other call site (fmt.code_block_literals).

## Examples

### Before

```ssl
sName := upper(sInput);
nSize := len(sName);
sKept := MyCustomHelper(sName);
oObj:upper(sName);
fnLen := {|s| len(s)};
```

### After

```ssl
sName := Upper(sInput);
nSize := Len(sName);
sKept := MyCustomHelper(sName);
oObj:upper(sName);
fnLen := {|s| Len(s)};
```

A built-in name inside a string literal stays as written:

### Before

```ssl
sMsg := "please call upper(sInput) first";
```

### After

```ssl
sMsg := "please call upper(sInput) first";
```

## Rationale

SSL built-ins are case-insensitive at runtime, so casing is style
(style_only, mirroring the style-guide record); the style guide names the
documented casing (R45), so it is the default (issue #92, D12) and
`"preserve"` remains available. Member names are outside that rule: a
member is not a built-in, and native .NET members are case-sensitive, so
re-casing one could change runtime behavior (issue #255). The pass
originally ran on raw text and re-cased matches inside strings and comments;
issue #34 moved it onto the lexer's token stream so literal text is
untouched.
