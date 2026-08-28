---
id: diag.c_style_comment_closer
title: C-style */ before a comment's terminating semicolon
kind: diagnostic
status: active
authority: style_only
schema_ref: null
default_severity: info
config:
  - ssl.diagnostics.infoDiagnostics
severity_overridable: true
suppressible: true
spec_options:
  include_info_diagnostics: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-08-27
    ref: "issue #208 discussion"
    note: >-
      Born from the production-corpus banner idiom `*/;`: the corpus
      owner ruled the form valid and purely stylistic (SSL never sees
      the `*/`) and suggested an info-tier note — the first rule designed
      for the opt-in info tier.
issues: []
---

## Behavior

Flags a comment whose text ends with a C-style `*/` immediately before
the terminating `;` (whitespace between `*/` and `;` allowed). The
construct is valid: SSL reads the `*/` as literal comment text and the
`;` as the real terminator — but the `*/` suggests a mental model where
it closes the comment, which in SSL it never does. Info severity: pure
style observation, visible only with the info tier enabled
(`ssl.diagnostics.infoDiagnostics`) or an explicit rule entry. The range
covers the comment.

It must NOT flag:

- comments without the `*/` tail (`/* plain comment;`) — the idiomatic
  form;
- `*/` sequences elsewhere in the comment text (`/* a*/b more text;`) —
  only the closing position implies the wrong mental model;
- anything at all under default options — info tier off.

## Examples

### Flags

```ssl
/********************************
 Description: banner comment
 ********************************/;
:DECLARE nCount;
```

### Flags

```ssl
/* short note */;
:DECLARE nCount;
```

### Does not flag

```ssl
/* short note;
:DECLARE nCount;
```

### Does not flag

```ssl
/* the sequence * / appears mid-text a*/b and is just text;
:DECLARE nCount;
```

## Rationale

Production banners widely close `*/;` — a C-style reflex that is
harmless in SSL but encodes a wrong belief about where comments end,
the same belief that produces genuinely broken comments elsewhere
(`/* ... */` with no `;` at all, which `comment_termination` handles).
The corpus owner's ruling (issue #208 discussion): valid, stylistic,
info at most. As an info-tier rule it costs nothing by default and
gives assistant/LLM consumers the language model they need to write
idiomatic comments.
