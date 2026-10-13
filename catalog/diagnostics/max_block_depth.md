---
id: diag.max_block_depth
title: Block nesting exceeds the configured maximum depth
kind: diagnostic
status: active
authority: style_only
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
  - internal/providers/diagnostics_audit_test.go
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
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Branch bodies stopped counting as nesting. A :CASE/:OTHERWISE arm
      had added a level of its own, so a :BEGINCASE cost one more level
      than an :IF whose body sits at the same indentation (2,000 of 3,786
      reports on the production corpus); and every :ELSE/:CATCH/:FINALLY
      body of an over-deep block was reported again on top of the block
      itself. Authority corrected advisory -> style_only to mirror the
      style guide record (limit_block_depth).
issues: []
---

## Behavior

Flags every block whose nesting depth exceeds the configured maximum
(`ssl.diagnostics.maxBlockDepth`, default 4; the spec fences run at the
default). Depth counts every block-opening construct, including the
procedure body: `:PROCEDURE`, `:IF`, `:WHILE`, `:FOR`, `:BEGINCASE`,
`:TRY`, `:REGION`, and `:BEGININLINECODE` each add one level, so four
`:IF` levels inside a `:PROCEDURE` already sit at depth 5. A `:REGION`
body is opaque text and is never inspected, so nothing inside one is
counted.

Branch bodies are not nesting. An `:ELSE`, `:CATCH`, or `:FINALLY` body
sits at the depth of the `:IF` or `:TRY` it continues, and a `:CASE` or
`:OTHERWISE` arm sits at the depth of its `:BEGINCASE` — an arm body is
indented one level inside `:BEGINCASE`, exactly as an `:IF` body is
inside `:IF`. Branch bodies are never reported themselves, so one
diagnostic is emitted per offending block, as a zero-width range at
column 0 of the block's opening line. Deeply nested code produces one
diagnostic per block-opening construct beyond the limit.

It must NOT flag:

- nesting exactly at the maximum (depth must be strictly greater);
- a `:BEGINCASE` at the maximum depth — its arms do not add a level;
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

### Does not flag

```ssl
:IF nA > 0;
	:IF nB > 0;
		:IF nC > 0;
			:BEGINCASE;
			:CASE nD > 0;
				nTotal := 1;
				:EXITCASE;
			:OTHERWISE;
				nTotal := 2;
				:EXITCASE;
			:ENDCASE;
		:ENDIF;
	:ENDIF;
:ENDIF;
```

### Flags

```ssl
:IF nA > 0;
	:IF nB > 0;
		:IF nC > 0;
			:BEGINCASE;
			:CASE nD > 0;
				:IF nE > 0;
					nTotal := 1;
				:ENDIF;
				:EXITCASE;
			:ENDCASE;
		:ENDIF;
	:ENDIF;
:ENDIF;
```

## Rationale

The style guide's `limit_block_depth` record (style_only) sets the limit
at 4, and the tool has enforced exactly that limit since the initial
commit (442fa69) via a user-configurable threshold. Info severity (D13's
opt-in advisory tier) fits a maintainability rule: deep nesting is legal
SSL but hard to read and refactor. The second Flags/Does-not-flag pair
pins that the `:PROCEDURE` body itself counts as a nesting level — a
deliberate consequence of treating every block-opening construct
uniformly. Depth follows the indentation a reader sees, which is why
branch bodies do not count (issue #255): the last two fences pin that a
`:BEGINCASE` costs one level, like an `:IF`.
