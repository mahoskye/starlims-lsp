---
id: fmt.trim_trailing_whitespace
title: Trim trailing whitespace
kind: formatter
status: active
authority: style_only
schema_ref: null
config:
  - ssl.format.trimTrailingWhitespace
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-04-30
    ref: "PR #4 (v0.5.0)"
    note: >-
      Added as a dedicated post-pass, default on; the streaming formatter
      had already been trimming line ends unconditionally since v0.1.0.
  - date: 2026-07-02
    ref: "issue #39"
    note: >-
      The streaming formatter's trim is now gated on the option too, so
      trimTrailingWhitespace: false actually preserves line-end whitespace
      (observable inside multi-line comment content).
  - date: 2026-08-28
    ref: "issue #216 (production-corpus formatting review residual)"
    note: >-
      Line ends inside a multi-line string, code block or :REGION body are
      no longer trimmed: they are literal bytes, and trimming them changed
      string values (343 corpus files).
  - date: 2026-10-13
    ref: "issue #255 (catalog audit)"
    note: >-
      Behavior text records the issue-#216 literal exemption, and the
      comment-interior trim is named as a deliberate D7 exception.
issues: ["#39"]
---

## Behavior

With `ssl.format.trimTrailingWhitespace` on (default), trailing space and
tab characters are removed from every formatted line except line ends that
fall inside a string literal, code-block literal or `:REGION` body — those
are user content and keep their bytes (issue #216). Whitespace-only lines
become empty lines. The interior lines of multi-line `/* ... ;` comments
*are* trimmed: comment content is otherwise preserved verbatim, and this
line-end trim is the one deliberate edit to it (named in D7 — trailing
whitespace is invisible and changes nothing the comment says).

With the option off, line-end whitespace is preserved. In practice the only
place the setting is observable is multi-line comment content: the
token-stream formatter never emits trailing whitespace on code lines of its
own (spaces before a line break are not re-emitted), and literal line ends
keep their whitespace under either setting. `spec_options` apply entry-wide and the fences below pin the
default-on behavior, so the off-mode is pinned by a Go test
(`TestFormat_TrimTrailingWhitespaceDisabled_PreservesCommentInterior` in
internal/providers/formatting_test.go).

## Examples

Line one ends with a tab, line two is whitespace-only, line three ends with
spaces:

### Before

```ssl
:DECLARE sName;	
   
sName := "x";   
```

### After

```ssl
:DECLARE sName;

sName := "x";
```

Interior lines of a multi-line comment are trimmed too (`first line` ends
with three spaces before formatting):

### Before

```ssl
/* first line   
second line;
nValue := 1;
```

### After

```ssl
/* first line
second line;
nValue := 1;
```

A line end inside a multi-line string literal keeps its trailing spaces
(`first` ends with three spaces, before and after):

### Idempotent

```ssl
sTxt := "first   
second";
```

## Rationale

Trailing whitespace is invisible diff noise. Post-pass placement (PR #4,
v0.5.0) means it applies uniformly after all other formatting decisions.
Until issue #39 the streaming formatter also trimmed unconditionally before
the post-passes ran, which made the off-setting a silent no-op; the trim is
now gated on the option everywhere so the setting delivers what it
documents.
