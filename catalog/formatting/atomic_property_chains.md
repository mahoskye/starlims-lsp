---
id: fmt.atomic_property_chains
title: Property chains are atomic when wrapping
kind: formatter
status: active
authority: tool
schema_ref: null
config:
  - ssl.format.maxLineLength
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-05-13
    ref: "PR #20 (v0.7.6), issue #16, vs-code-ssl-formatter#76"
    note: >-
      Line wrapping split oVar:property and a:b:c chains across lines,
      producing code that reads as separate statements. canWrapBefore now
      refuses to break immediately before or after a member-access ':'.
issues: []
---

## Behavior

When a line exceeds `ssl.format.maxLineLength` and must wrap, a member-access
chain (`oVar:property`, `a:b:c`, including call forms `oVar:Method(...)`) is
never split at a `:` boundary: the wrapper refuses to break immediately
before or immediately after a member-access `:` (the `:` of `:=` and of
keywords like `:IF` is unaffected — those are different token kinds). The
wrap point moves to an earlier legal boundary (after a comma, after `:=`,
at an argument start), carrying the whole chain to the continuation line.
If the only way to satisfy the limit would be splitting a chain, the line is
left over-long instead.

## Examples

The second argument would overflow column 90, so it wraps after the comma —
the `oConfiguration:database:timeout` chain moves as one unit:

### Before

```ssl
sValue := SomeFunctionWithLongName(oConfiguration:database:connectionString, oConfiguration:database:timeout);
```

### After

```ssl
sValue := SomeFunctionWithLongName(oConfiguration:database:connectionString,
	oConfiguration:database:timeout);
```

A chain whose only possible break points are its own `:` boundaries is left
long rather than split (this line is 94 columns and stays that way):

### Idempotent

```ssl
DoSomething(oConfigurationManagerInstance:databaseConnectionSettings:primaryConnectionString);
```

## Rationale

In SSL, `:` is both the keyword prefix and the member-access operator; a
chain fragment at the start of a wrapped line (`:connectionString`) is
visually indistinguishable from a keyword and misreads as a new statement,
while a receiver stranded before a break (`oVar:`) reads as an incomplete
keyword. Issue #16 (vs-code-ssl-formatter#76) settled that readability of
the chain beats strict line-length compliance.
