---
id: diag.region_legacy
title: REGION keyword flagged as legacy (removed — premise was wrong)
kind: diagnostic
status: removed
authority: tool
schema_ref: null
default_severity: info
severity_overridable: true
suppressible: true
tests: []
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
    ref: "issue #28"
    note: >-
      Removed. The rule's premise was wrong: :REGION/:ENDREGION are valid,
      current SSL with a runtime purpose (GetRegion body capture), so
      flagging every use — even at info severity — was pure noise on
      correct code. Constant, check, and test deleted.
issues: ["#28"]
---

## Behavior

Removed. The rule flagged every `:REGION` and `:ENDREGION` keyword as "a
legacy functional construct", recommending `/* region` comment markers
instead — but the keywords are current, supported SSL with a real runtime
purpose (`GetRegion()` body capture with `$placeholder$` substitution),
distinct from the folding-only comment markers. The thing the style guide
actually dropped was C#-style `#region`.

If a nudge is ever wanted for authors who write `:REGION` intending only
editor folding, it must be specced first as a new `planned` entry with a
flag condition that does not fire on legitimate `GetRegion()` use — a
condition nobody has defined. Blanket flagging of a valid construct is not
it.
