---
id: fmt.builtin_function_case
title: Built-in function name casing
kind: formatter
status: draft
authority: style_only
schema_ref: null
config:
  - ssl.format.builtinFunctionCase
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-05-01
    ref: "PR #4 (v0.5.0)"
    note: >-
      Added with "preserve" as default — rewriting user casing is opt-in;
      "PascalCase" rewrites call sites to the canonical inventory casing.
issues: []
---

## Behavior

With `ssl.format.builtinFunctionCase: "preserve"` (default), built-in
function call sites keep the author's casing. With `"PascalCase"`, each
call site of a recognized built-in is rewritten to the canonical casing
from the element inventory (e.g. `sqlexecute(` → `SQLExecute(`). Unrecognized
identifiers and user procedure names are never re-cased. Occurrences inside
strings and comments are untouched.

## Examples

### Before

```ssl
sName := upper(sInput);
```

### After

```ssl
sName := upper(sInput);
```

## Rationale

SSL built-ins are case-insensitive at runtime, so casing is pure style —
default-preserve respects the author, opt-in PascalCase serves teams
standardizing on the inventory's canonical names (PR #4).
