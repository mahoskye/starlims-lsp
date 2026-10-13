---
id: diag.keyword_uppercase
title: SSL keyword not written in uppercase
kind: diagnostic
status: active
authority: authoritative
schema_ref: keywords.case
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6 (v0.3.0)"
    note: Introduced in checkKeywordForms during style-guide alignment.
  - date: 2026-03-30
    ref: "commit f6e78ef"
    note: >-
      Data-source variant added: builder directives get their own code
      (builder_directive_case) instead of this one.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0), commit d744511"
    note: Stable code keyword_uppercase assigned.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      authority corrected to authoritative: the style-guide record
      keyword_uppercase (level authoritative) names this rule in
      enforced_by, so the guide is not silent. The 'lints-only
      schema_ref' explanation is removed; schema_ref set to
      keywords.case.
issues: []
---

## Behavior

Always-on check: flags a colon-prefixed token that the lexer recognizes as
an SSL keyword but whose text is not exactly `:` + the uppercase keyword
name (`:declare`, `:Declare`, `:EndIf`, ...). SSL keywords are
case-sensitive, so the lowercase form is rejected by the compiler, not just
unconventional. The message names the canonical spelling.

Deliberate hand-offs — these forms must NOT produce this code:

- colon-prefixed tokens that are not recognized keywords
  (`unknown_keyword`);
- `:ENDFOR` in any casing (`endfor_invalid` — the fix is `:NEXT`, not
  casing);
- legacy `:LABELname` forms (`label_keyword_form`);
- in data-source files, builder directives such as `:dsn`
  (`builder_directive_case`).

It must NOT flag correctly-cased keywords, or keyword names appearing
without the leading colon (identifiers).

## Examples

### Flags

```ssl
:declare nCount;
```

### Flags

```ssl
:IF nTotal > 0;
	nTotal := 0;
:EndIf;
```

### Does not flag

```ssl
:DECLARE nCount;
:IF nCount > 0;
	nCount := 0;
:ENDIF;
```

## Rationale

Keyword casing is authoritative language behavior — the schema's core
principles state all block keywords are colon-prefixed and must be
UPPERCASE (`keywords.case`, the entry's `schema_ref`; style-guide record
keyword_uppercase), and the runtime is case-sensitive — hence error
severity. The rule stays high-precision by handing near-miss cases
(unknown keywords, `:ENDFOR`, label forms, builder directives) to their own
codes rather than mislabeling them as casing problems (cdbfee6, f6e78ef).
