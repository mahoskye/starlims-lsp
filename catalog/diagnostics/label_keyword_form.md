---
id: diag.label_keyword_form
title: Label keyword written in the wrong case
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced with the style-guide alignment pass: label forms are the one
      keyword family where the compact no-space form (:LABELName;) is valid,
      so they get a dedicated case check instead of keyword_uppercase.
  - date: 2026-03-30
    ref: "commit f6e78ef"
    note: >-
      Data source files run a keyword-form variant that accepts builder
      directives; it carries the identical label-form check.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags a colon-prefixed keyword token that reads as a label form — the text
after the `:` starts with `label` in any casing and continues with more
characters — but does not start with the exact uppercase prefix `:LABEL`.
SSL label keywords are case-sensitive: the valid forms are `:LABEL Name;`
(spaced) and `:LABELName;` (compact). A wrong-case prefix such as
`:labelName;` is not recognized as a label at runtime, hence error severity.
The diagnostic ranges over the whole keyword token. The check runs in both
ordinary SSL files and data source files (the data-source keyword-form
variant carries the same rule).

It must NOT flag:

- the compact form with an uppercase prefix — `:LABELRETRY;` or
  `:LABELRetry;` — only the `LABEL` prefix is case-checked, the appended
  name may use any casing;
- the spaced form `:LABEL Name;`;
- a bare `:label`/`:Label` token with nothing appended — that is the plain
  keyword in the wrong case and belongs to `keyword_uppercase`, not this
  rule;
- identifiers that merely contain "label" without a leading colon
  (`sLabelText := "caption";`).

## Examples

### Flags

```ssl
:labelRetry;
Branch("LABELRETRY");
```

### Flags

```ssl
:LabelRetry;
Branch("LABELRETRY");
```

### Does not flag

```ssl
:LABELRETRY;
Branch("LABELRETRY");
```

### Does not flag

```ssl
:LABEL RETRY;
Branch("LABEL RETRY");
```

### Does not flag

```ssl
:DECLARE sLabelText;
sLabelText := "caption";
```

## Rationale

The style guide's label_behavior section documents that label token text is
significant (`:LABEL SKIP;` produces the token "LABEL SKIP" that `Branch()`
must match), and SSL keywords are uppercase-only. Because the compact
`:LABELName;` form glues the keyword to the name, the general
keyword_uppercase check cannot handle it — normalizing the whole token would
mangle the name — so label-shaped tokens get this dedicated case check
(cdbfee6). providers_test.go
(TestGetDiagnostics_LegacyCompactLabelAccepted) pins the compact-uppercase
false-positive fence.
