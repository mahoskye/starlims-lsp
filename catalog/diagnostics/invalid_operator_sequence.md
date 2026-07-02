---
id: diag.invalid_operator_sequence
title: C-style operator that is invalid in SSL
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests: []
history:
  - date: 2026-03-30
    ref: "commit c6e1eb4"
    note: Introduced for adjacent-operator compounds !== and ===.
  - date: 2026-03-30
    ref: "commit f63f1ef"
    note: >-
      Same day: split single & | (invalid, no equivalent) from && || (which
      suggest .AND./.OR.), so each form gets an accurate message.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0), commit d744511"
    note: Stable code invalid_operator_sequence assigned.
issues: []
---

## Behavior

Always-on check for operators imported from C-family languages, in three
forms (all this one code):

- `&&` / `||` — flagged with the SSL replacement (`.AND.` / `.OR.`);
- bare `&` / `|` — flagged as simply invalid (no SSL equivalent exists);
- `!==` / `===` — detected as two *immediately adjacent* operator tokens
  (no whitespace between them) and flagged with the SSL replacement
  (`!=` / `==`), range spanning both tokens.

It must NOT flag:

- valid SSL operators, including `==`, `!=`, `<>`, `#`, and the word forms
  `.AND.`/`.OR.`/`.NOT.` (bare `AND`/`OR`/`NOT` words belong to
  `bare_logical_operator`);
- `==` followed by `=` with whitespace between them — only glued sequences
  form a compound;
- `&` or `|` characters inside string literals or comments.

## Examples

### Flags

```ssl
:DECLARE bOk, bA, bB;
bOk := bA && bB;
```

### Flags

```ssl
:DECLARE nCount;
:IF nCount === 1;
	nCount := 2;
:ENDIF;
```

### Does not flag

```ssl
:DECLARE nCount, bOk, bFlag;
:IF nCount == 1 .AND. bFlag;
	bOk := .T.;
:ENDIF;
```

### Does not flag

```ssl
:DECLARE sLabel;
sLabel := "black & white | grey";
```

## Rationale

`&&`, `||`, `===`, and `!==` are muscle-memory imports that SSL rejects;
severity is error because the code cannot compile as written. The rule
churned once on day one (c6e1eb4 then f63f1ef): lumping `&` in with `&&`
produced the misleading suggestion that `.AND.` replaces a single `&`, so
the single-character forms now carry a plain "not a valid SSL operator"
message while the doubled forms name their replacement.
