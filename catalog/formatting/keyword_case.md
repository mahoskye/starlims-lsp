---
id: fmt.keyword_case
title: Keyword and canonical-form casing
kind: formatter
status: active
authority: authoritative
schema_ref: keywords.case
config: []
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-01-10
    ref: "v0.1.0 initial release"
    note: >-
      Colon-keyword uppercasing and dot-literal canonicalization shipped
      with the original formatter; advertised in --format --help but never
      given a normative entry.
  - date: 2026-07-22
    ref: "issue #90"
    note: >-
      Dot logical operators (.and. -> .AND.) and Me/Base receivers joined
      the casing surface; both had been missed because they do not lex as
      keyword tokens. Entry created to document the full surface.
  - date: 2026-10-13
    ref: "issue #255 (catalog audit)"
    note: >-
      The surface now reaches code-block bodies: `.t.`, `nil` and `me:`
      inside `{|...|}` were left as written because the body is
      normalized by its own re-lex (fmt.code_block_literals).
  - date: 2026-10-13
    ref: "issue #304 (corpus triage), reference keywords/LABEL.md"
    note: >-
      A joined `:LABELName` is no longer split into `:LABEL Name`. The two
      store different label text and Branch() matches exactly, so the
      split changed which label a Branch() reached; on the production
      corpus, 15 files gained branch_target_label errors after
      formatting. The joined form is now written verbatim.
issues: ["#90"]
---

## Behavior

Casing normalization is always on (no config): the style-guide schema makes
keyword casing authoritative (`keywords.case: upper`, R25/R38/R41), so the
formatter may canonicalize mechanically. The surface:

- Colon-prefixed keywords uppercase: `:if` → `:IF`, `:endproc` → `:ENDPROC`.
- Dot-wrapped literals canonicalize: `.t.` → `.T.`, `.f.` → `.F.`.
- `nil` → `NIL` (bare literal keyword).
- Dot-wrapped logical operators uppercase: `.and.`/`.or.`/`.not.` →
  `.AND.`/`.OR.`/`.NOT.` (issue #90 — these lex as operator tokens, not
  keywords, and were previously left alone).
- `me`/`base` in member-access receiver position (immediately followed by
  `:`) canonicalize to `Me`/`Base`. An ordinary identifier that merely
  shares the name (`me := 1;`) is never recased — the rewrite applies only
  where the runtime reserves the word.
- A joined `:LABELName;` is written verbatim, never split into
  `:LABEL Name;`: the two store different label text, and `Branch()`
  matches it exactly (reference keywords/LABEL.md), so splitting would
  change which label a `Branch()` reaches. A miscased joined form is
  `diag.label_keyword_form`'s, and is not re-cased either.

The surface applies inside code-block bodies too (`{|o| o != nil}` →
`{|o| o != NIL}`), except where fmt.code_block_literals passes a literal
through verbatim (nested or multi-line). Content inside strings and
comments is never recased (D7).

## Examples

### Before

```ssl
:procedure Demo;
:declare bA;
bA := bX .and. bY .or. .not. bZ;
:return bA;
:endproc;
```

### After

```ssl
:PROCEDURE Demo;
	:DECLARE bA;
	bA := bX .AND. bY .OR. .NOT. bZ;
	:RETURN bA;
:ENDPROC;
```

Receivers canonicalize; a variable named `me` does not:

### Before

```ssl
:CLASS Demo;
:PROCEDURE M;
me:Helper();
nT := base:Compute(1);
:RETURN nT;
:ENDPROC;
```

### After

```ssl
:CLASS Demo;
:PROCEDURE M;
	Me:Helper();
	nT := Base:Compute(1);
	:RETURN nT;
:ENDPROC;
```

### Idempotent

```ssl
me := 1;
nX := me + 1;
```

Inside a code-block body, and never inside a string or comment:

### Before

```ssl
fnReady := {|o| me:Check(o) .or. o == nil .or. .f.};
sNote := ":if nil .t.";  /* :endif nil;
```

### After

```ssl
fnReady := {|o| Me:Check(o) .OR. o == NIL .OR. .F.};
sNote := ":if nil .t.";  /* :endif nil;
```

A joined label keeps its exact text:

### Idempotent

```ssl
:LABELRETRY;
Branch("LABELRETRY");
```

## Rationale

The schema is authoritative on keyword casing, so normalization needs no
option. The dot-operator and Me/Base gaps existed because the casing logic
keyed on token type (keyword) rather than on the canonical-form inventory
(schema R38/R41) — the operator and identifier paths never consulted it.
Receiver-position gating for Me/Base keeps the formatter from renaming
user variables that collide with the reserved words outside class context.
