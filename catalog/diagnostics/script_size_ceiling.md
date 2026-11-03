---
id: diag.script_size_ceiling
title: File over the size limit
kind: diagnostic
status: active
authority: style_only
schema_ref: lints.style_rules.script_size_ceiling
default_severity: warning
config:
  - ssl.diagnostics.maxScriptBytes
severity_overridable: true
suppressible: true
spec_options:
  max_script_bytes: 64
tests:
  - internal/providers/size_review_test.go
history:
  - date: 2026-11-03
    ref: "issue #358"
    note: >-
      Introduced from ssl-style-guide's script_size_ceiling
      (mahoskye/ssl-style-guide#140), which sets it at warning because it
      is a limit, unlike the info-tier review triggers of issue #344. The
      default of 50 KB (51,200 bytes) is the maintainer's.
issues: []
---

## Behavior

Reports a file larger than `ssl.diagnostics.maxScriptBytes` bytes (default
51,200, which is 50 KB), once per file, ranged on line 1. The size is the
file's length in bytes as the server receives it, line endings included,
so a CRLF file counts one byte more per line than the same file with LF.
Every file is measured: server scripts, class files and data sources (SSL
or SQL), test scripts included. A data source whose body is SQL is
measured whole, header and body.

The message gives the file's size and the limit in KB, the size rounded
up, so a file just over the limit never reads as the limit itself. A limit
that is no whole number of KB puts both in bytes. Setting the limit to `0`
disables the rule. The spec fences here run with the limit at 64 bytes.

It must NOT flag:

- a file at or under the limit;
- any file when the limit is `0`.

## Examples

### Flags

```ssl
:DECLARE nTotal, nCount;
nTotal := 0;
nCount := 10;
nTotal := nTotal + nCount;
:RETURN nTotal;
```

### Does not flag

```ssl
:DECLARE nA;
nA := 1;
:RETURN nA;
```

## Rationale

STARLIMS compiles a script as one unit, with its `:INCLUDE`d code, even
when a caller needs a single procedure. The cost comes back whenever the
script has to load again: on first use after a start, for every script
after a check-in in production mode (which resets the application), and
on every save during development. The agents that write and review SSL
also read at most about 50 KB at a time, so a larger file is never seen
whole. The fix is to split the file by responsibility until each part
fits. `script_size_review` is the earlier, advisory prompt to look at a
script's responsibilities.
