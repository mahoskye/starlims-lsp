---
id: diag.max_block_depth
title: Block nesting exceeds the configured maximum depth
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.style_rules.limit_block_depth
default_severity: info
config:
  - ssl.diagnostics.infoDiagnostics
severity_overridable: true
suppressible: true
spec_options:
  include_info_diagnostics: true
tests:
  - internal/providers/providers_test.go
  - internal/providers/edge_test.go
history:
  - date: 2026-01-10
    ref: "commit 442fa69 (initial commit)"
    note: >-
      Present since the first commit, with the MaxBlockDepth option
      defaulting to 4 to match the style guide's limit_block_depth: 4.
  - date: 2026-01-10
    ref: "commit f27f727"
    note: Guarded against negative line numbers in the reported range.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-08-27
    ref: "issue #208 discussion (info-tier expansion)"
    note: >-
      Moved warning -> info in the info-tier expansion: complexity
      observation with a configurable threshold — advisory, not a bug.
      Info is the opt-in advisory tier
      (ssl.diagnostics.infoDiagnostics); explicit ssl.diagnostics.rules
      entries still promote or disable per rule.
issues: []
---

## Behavior

Flags every parsed block whose nesting depth exceeds the configured maximum
(`ssl.diagnostics.maxBlockDepth`, default 4; the spec fences run at the
default). Depth counts every block-opening construct, including the
procedure body: `:PROCEDURE`, `:IF`, `:WHILE`, `:FOR`, `:BEGINCASE`,
`:TRY`, `:REGION`, and `:BEGININLINECODE` each add one level, so four
`:IF` levels inside a `:PROCEDURE` already sit at depth 5. Middle keywords
(`:ELSE`, `:CASE`, `:OTHERWISE`, `:CATCH`, `:FINALLY`) continue the current
block and add no depth. One diagnostic is emitted per offending block,
as a zero-width range at column 0 of the block's starting line — deeply
nested code therefore produces one diagnostic per level beyond the limit.

It must NOT flag:

- nesting exactly at the maximum (depth must be strictly greater);
- anything when `ssl.diagnostics.maxBlockDepth` is set to `0`, which
  disables the check entirely.

## Examples

### Flags

```ssl
:IF nA > 0;
	:IF nB > 0;
		:IF nC > 0;
			:IF nD > 0;
				:IF nE > 0;
					nTotal := 1;
				:ENDIF;
			:ENDIF;
		:ENDIF;
	:ENDIF;
:ENDIF;
```

### Flags

```ssl
:PROCEDURE Deep;
	:IF nA > 0;
		:IF nB > 0;
			:IF nC > 0;
				:IF nD > 0;
					nTotal := 1;
				:ENDIF;
			:ENDIF;
		:ENDIF;
	:ENDIF;
:ENDPROC;
```

### Does not flag

```ssl
:IF nA > 0;
	:IF nB > 0;
		:IF nC > 0;
			:IF nD > 0;
				nTotal := 1;
			:ENDIF;
		:ENDIF;
	:ENDIF;
:ENDIF;
```

### Does not flag

```ssl
:PROCEDURE Shallow;
	:IF nA > 0;
		:IF nB > 0;
			:IF nC > 0;
				nTotal := 1;
			:ENDIF;
		:ENDIF;
	:ENDIF;
:ENDPROC;
```

## Rationale

The style guide's lints (advisory level) set `limit_block_depth: 4`, and the
tool has enforced exactly that limit since the initial commit (442fa69) via
a user-configurable threshold. Warning severity fits an advisory
maintainability rule: deep nesting is legal SSL but hard to read and
refactor. The second Flags/Does-not-flag pair pins that the `:PROCEDURE`
body itself counts as a nesting level — a deliberate consequence of the
parser treating every block-opening construct uniformly.
