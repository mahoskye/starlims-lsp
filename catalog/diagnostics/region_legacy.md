---
id: diag.region_legacy
title: REGION keyword flagged as legacy (disputed)
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: info
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Introduced (checkRegionLegacyWarning) citing ssl_agent_instructions.md
      gotcha #22, framing :REGION/:ENDREGION as legacy.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
  - date: 2026-06-29
    ref: "starlims-ssl-reference #12"
    note: >-
      Reference repo confirmed :REGION/:ENDREGION are current, supported SSL
      (GetRegion() with $placeholder$ substitution is in active use); the
      construct removed from the style guide was C#-style #region, not this.
      Same finding recorded in feature.folding history.
  - date: 2026-07-02
    ref: "issue #28 (open)"
    note: >-
      Dispute filed: the rule's "legacy" premise is wrong. Resolution —
      removal or neutral rewording — to be settled in this entry first, then
      the code fixed citing it.
issues: ["#28"]
---

## Behavior

Flags every `:REGION` and `:ENDREGION` keyword token, each with its own
info-severity diagnostic (a matched pair produces two diagnostics). The
message calls the keyword "a legacy functional construct that captures body
text for GetRegion()" and recommends `/* region` / `/* endregion` comment
markers for IDE folding and grouping. The range covers the keyword token.

It must NOT flag:

- `/* region ...;` / `/* endregion ...;` comment markers — those are the
  form this rule recommends, and they drive folding (feature.folding);
- the word "region" appearing in identifiers, strings, or ordinary
  comments.

Note: this entry specs the rule as implemented. Its premise is disputed —
see Known gaps.

## Examples

### Flags

```ssl
:REGION Helpers;
:PROCEDURE Helper;
:RETURN .T.;
:ENDPROC;
:ENDREGION;
```

### Does not flag

```ssl
/* region Helpers;
:PROCEDURE Helper;
:RETURN .T.;
:ENDPROC;
/* endregion;
```

## Rationale

The rule was written when `:REGION`/`:ENDREGION` was believed to be a
legacy construct (be7a174, gotcha #22). The schema in fact records
functional regions as authoritative, supported syntax
(`regions.functional_regions`: ":REGION Name; ... :ENDREGION;", "legacy
text storage/retrieval via GetRegion()") while recommending comment regions
for editor organization — and starlims-ssl-reference #12 confirmed the
keyword form is current SSL in active production use. Info severity has
kept the damage low: the diagnostic is advice, not an error. The dispute in
issue #28 is recorded below rather than silently ignored.

## Known gaps

- Issue #28: calling `:REGION`/`:ENDREGION` "legacy" is factually wrong —
  they are current, supported SSL, distinct from the C#-style
  `#region`/`#endregion` that was removed from the style guide
  (ssl-style-guide 48b66f5). Intended resolution per the issue: either
  remove this diagnostic entirely, or reword it to a neutral note that
  comment-marker regions are what editor folding uses (without the legacy
  framing) — the choice is settled by editing this entry, then fixing the
  code in a follow-up PR citing it. Because the resolution may be removal
  rather than a behavior change expressible as a fence, no expect=fail
  fence is recorded; the Flags fence above pins current behavior until the
  dispute is settled.
