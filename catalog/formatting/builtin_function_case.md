---
id: fmt.builtin_function_case
title: Built-in function name casing
kind: formatter
status: active
authority: style_only
schema_ref: null
config:
  - ssl.format.builtinFunctionCase
spec_options:
  builtin_function_case: "PascalCase"
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
issues: ["#34"]
---

## Behavior

With `ssl.format.builtinFunctionCase: "preserve"` (default), built-in
function names keep the author's casing. With `"PascalCase"` (the setting
exercised by the fences below, via `spec_options`), a post-format pass
rewrites every call site — an identifier immediately followed by `(`,
optionally with intervening spaces/tabs — whose lowercased name matches the
published built-in inventory to the inventory's canonical casing (e.g.
`len(` → `Len(`, `sqlexecute(` → `SQLExecute(`).

Identifiers not in the inventory (user procedures, variables) are never
re-cased, and neither are non-call uses of a built-in name (no following
`(`). String literals and comments are literal text and are never rewritten:
the pass re-lexes the formatted output and only touches identifier tokens.
It does not know about receivers, so a member call like `oVar:upper(...)`
is also rewritten.

## Examples

### Before

```ssl
sName := upper(sInput);
nSize := len(sName);
sKept := MyCustomHelper(sName);
oObj:upper(sName);
```

### After

```ssl
sName := Upper(sInput);
nSize := Len(sName);
sKept := MyCustomHelper(sName);
oObj:Upper(sName);
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

SSL built-ins are case-insensitive at runtime, so casing is pure style —
default-preserve respects the author; opt-in PascalCase serves teams
standardizing on the inventory's canonical names (PR #4, v0.5.0). The pass
originally ran on raw text and re-cased matches inside strings and comments;
issue #34 moved it onto the lexer's token stream so literal text is
untouched.
