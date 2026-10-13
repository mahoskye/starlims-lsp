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
  - date: 2026-10-13
    ref: "issue #269"
    note: >-
      :ENDCLASS and the builder directives outside a data source moved to
      their own error codes (endclass_invalid,
      builder_directive_outside_datasource). They are known invalid forms,
      so the "the keyword list may have a gap" reasoning behind this
      rule's warning severity does not apply to them.
  - date: 2026-10-13
    ref: "issue #291"
    note: >-
      Kept at warning when unknown_token and scientific_notation were
      raised to error: a gap in the keyword list must not fail validation.
      unexpected_token no longer defers to this rule, so an unknown colon
      form in a position the statement grammar cannot accept is also
      reported there, as the parse failure it is (0 such sites in 6,228
      production files). A statement that is only an unknown keyword
      (`:FOOBAR;`) is not a parse failure and stays this rule's alone.
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
- `:ENDCLASS` — known invalid, reported as `endclass_invalid` (error);
- legacy label forms: `:LABELName;` is valid compact label syntax and emits
  nothing; a miscased legacy form (`:labelName;`) is `label_keyword_form`,
  not this rule;
- colon member access on an object (lexer guard above);
- the builder directives `:DSN`, `:TABLENAME`, `:NULLASBLANK`,
  `:INVARIANTDATECOLUMNS`: valid in data-source files, and reported as
  `builder_directive_outside_datasource` (error) anywhere else.

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
(as the builder directives were until commit f6e78ef), and a gap in the
list must not fail `--validate`. That is the one deliberate exception
among the rules `unexpected_token` used to defer to (issue #291): where
the unknown colon form also stands where the statement grammar cannot
place any token, `unexpected_token` reports the parse failure as an error
alongside this warning, so the warning never stands in for a failure the
parser can prove. A statement consisting of the unknown keyword alone is
the residual case — the statement grammar does not model unknown keyword
statements, so it is reported here only, and validation passes.
