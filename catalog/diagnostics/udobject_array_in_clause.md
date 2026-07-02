---
id: diag.udobject_array_in_clause
title: UDObject property expanded inside a SQL IN clause
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-04-03
    ref: "ssl-style-guide 635ff9f; commit d23fca8"
    note: >-
      Runtime discovery: expanding ?obj:prop? inside a SQL IN clause raises
      "The current array has more than 1 dimmension." — the value must be
      copied to a local first. Documented in the style guide
      (sql.array_expansion_caveat) and enforced here.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-05-01
    ref: "vs-code-ssl-formatter PR #58"
    note: Extension quick-fix keyed on this slug (copy-to-local rewrite).
issues: []
---

## Behavior

Flags a named SQL placeholder that performs UDObject property access
(`?name?` where the name contains `:`) when the placeholder sits directly
inside an `IN (...)` clause: the string text immediately before the
placeholder, ignoring spaces/tabs, must be `(` preceded by the word `IN`
(case-insensitive, at a word boundary). Every string literal in the file
is scanned — the rule is not limited to `SQLExecute` arguments. The range
covers the placeholder inside the string; the message quotes it and names
the runtime error verbatim ("The current array has more than 1
dimmension.", sic).

It must NOT flag:

- a property-access placeholder **outside** an IN clause
  (`WHERE name = ?oObj:sName?`) — scalar property expansion is fine;
- a plain local-variable placeholder inside an IN clause (`IN (?aIds?)`)
  — that is exactly the prescribed fix;
- `IN` that is merely the tail of a longer word (`JOIN (?oObj:aIds?)`).

## Examples

### Flags

```ssl
SQLExecute("SELECT ID FROM samples WHERE ID IN (?oFilter:aIds?)");
```

### Does not flag

```ssl
SQLExecute("SELECT ID FROM samples WHERE Name = ?oFilter:sName?");
```

### Does not flag

```ssl
:DECLARE aIds, oFilter;
aIds := oFilter:aIds;
SQLExecute("SELECT ID FROM samples WHERE ID IN (?aIds?)");
```

## Rationale

This encodes a verified runtime failure, not a style preference: IN-clause
array expansion of a UDObject property dies with the (misspelled) engine
error recorded in history, and the fix — copy to a local, expand the local
— is mechanical enough that the extension ships it as a quick-fix keyed on
this slug (PR #58). Warning rather than error because the placeholder
could hold a scalar property, which expands fine even inside IN; only
array-valued properties fail, and the tool cannot see the property's type.
The third fence pins the prescribed rewrite as permanently clean.
