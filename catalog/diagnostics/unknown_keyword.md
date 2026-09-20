---
id: diag.unknown_keyword
title: Unknown colon-prefixed keyword
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
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced in the full alignment pass with ssl-style-guide
      (checkKeywordForms): colon-prefixed tokens are validated against the
      keyword list; unrecognized names warn.
  - date: 2026-03-30
    ref: "commit f6e78ef"
    note: >-
      Data-source variant added (checkKeywordFormsDataSource): builder
      directives (:DSN, :TABLENAME, :NULLASBLANK, :INVARIANTDATECOLUMNS)
      are valid in data-source files and exempted there.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-09-20
    ref: "issue #240 follow-up"
    note: >-
      The lexer guard now looks past whitespace and comments to the
      previous significant token, so a call chain continued on the next
      line (`txt:ToString()` then `:Replace(...)`) reads as member access.
      Over 6,228 production files this rule fired 3 times, all that
      shape, all false; the formatter was worse, recasing `:Replace` to
      `:REPLACE` and inserting a `;` that split the chain. The
      continuation reading yields to a real keyword, so a lost `;` before
      a line-leading `:IF` still lexes the keyword.
issues: []
---

## Behavior

Flags a colon-prefixed keyword token (`:Name`) whose name, uppercased, is
not a recognized SSL keyword. The range covers the token; the message names
the offending text verbatim (`Unknown SSL keyword: ':Foobar'`).

The lexer only forms a keyword token when the `:` does **not** follow a
receiver — an identifier, `)`, or `]` — so member access (`oObj:Method()`,
`Me:field`) never reaches this rule. The receiver may end the previous
line: a call chain continued on the next line with the colon leading it
(`txt:ToString()` then `:Replace(...)`) is member access too, since the
look-back skips whitespace and comments. That continuation reading is
taken only when the word after the colon is not an SSL keyword, so a
statement that lost its `;` before a line-leading `:IF` still lexes the
keyword. That lexer guard is the primary false-positive fence for this
check; a method named like a keyword on a continuation line
(`oObj:Return(...)`) is the one shape it cannot tell apart.

It must NOT flag:

- a *known* keyword in the wrong case (`:declare`) — that is
  `keyword_uppercase` (error), not an unknown keyword;
- `:ENDFOR` — recognized-but-invalid, reported as `endfor_invalid` with a
  targeted ":NEXT" message;
- legacy label forms: `:LABELName;` is valid compact label syntax and emits
  nothing; a miscased legacy form (`:labelName;`) is `label_keyword_form`,
  not this rule;
- colon member access on an object (lexer guard above);
- in data-source files (`IsDataSourceFile`), the builder directives `:DSN`,
  `:TABLENAME`, `:NULLASBLANK`, `:INVARIANTDATECOLUMNS`. In ordinary
  scripts those directives are not exempt and do flag — they are only
  meaningful in data-source files.

## Examples

### Flags

```ssl
:FOOBAR;
```

### Does not flag

```ssl
:declare nCount;
```

### Does not flag

```ssl
:DECLARE oObj;
oObj:Refresh();
```

### Does not flag

```ssl
:LABELSkip;
```

### Does not flag

```ssl
:DECLARE txt, sTitle, sUser, sOut;
sOut := txt:ToString()
	:Replace("##TITLE##", sTitle)
	:Replace("##USER##", sUser);
```

## Rationale

An unrecognized colon form is almost always a typo (`:PROCEDRUE`) or a
keyword from another dialect, but SSL also accepts label text directly
after `:LABEL`, and object member access uses the same `:` character —
so this rule leans on the lexer's context rule (no keyword token after an
identifier/`)`/`]`) and on dedicated rules for every recognized-but-wrong
shape (`keyword_uppercase`, `endfor_invalid`, `label_keyword_form`).
Warning rather than error because the validator's keyword list, not the
runtime, is the authority here — an unknown name may be a gap in the list
(as the builder directives were until commit f6e78ef).
