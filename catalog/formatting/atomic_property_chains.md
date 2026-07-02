---
id: fmt.atomic_property_chains
title: Property chains are atomic when wrapping
kind: formatter
status: draft
authority: tool
schema_ref: null
config:
  - ssl.format.maxLineLength
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-05-14
    ref: "PR #20 (v0.7.6), issue #16"
    note: >-
      Line wrapping split oVar:property and a:b:c chains across lines,
      producing code that reads as separate statements. Chains declared
      atomic.
issues: []
---

## Behavior

When a line exceeds `ssl.format.maxLineLength` and must wrap, a member-access
chain (`oVar:property`, `a:b:c`, including call forms `oVar:Method(...)`) is
never split at a `:` boundary. The chain moves to the next line as a unit;
if a chain alone exceeds the limit, the line is left long rather than split.

## Examples

### Before

```ssl
sValue := SomeFunctionWithLongName(oConfiguration:database:connectionString, oConfiguration:database:timeout);
```

### After

```ssl
sValue := SomeFunctionWithLongName(oConfiguration:database:connectionString,
	oConfiguration:database:timeout);
```

## Rationale

In SSL, `:` is both the keyword prefix and the member-access operator; a
chain fragment at the start of a wrapped line (`:connectionString`) is
visually indistinguishable from a keyword and misreads as a new statement.
Issue #16 settled that readability of the chain beats strict line-length
compliance.
