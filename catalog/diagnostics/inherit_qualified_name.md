---
id: diag.inherit_qualified_name
title: ":INHERIT parent written as a plain class name"
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.class_rules.inherit_qualified_name
default_severity: error
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/inherit_qualified_name_test.go
history:
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: >-
      Constant CodeInheritQualifiedName reserved alongside the stable code
      batch, but no check was ever implemented.
  - date: 2026-07-02
    ref: "issue #31"
    note: >-
      Constant deleted. The schema's own message said both bare and
      qualified base names are accepted, which left no flag condition.
      If the style guide later settles on one form, spec a new planned
      entry first.
  - date: 2026-10-13
    ref: "issue #269; ssl-style-guide#81, PR ssl-style-guide#104"
    note: >-
      Implemented at error. The style guide settled the form: :INHERIT
      requires the parent's full Category.ClassName, and a plain name is a
      compile-time error ("Specify the full name of the class in
      :INHERIT"); the reference (keywords/INHERIT.md) agrees. The earlier
      "both forms accepted" note was wrong. Zero findings on the
      production corpus, where all :INHERIT statements are qualified.
issues: []
---

## Behavior

Flags an `:INHERIT` whose parent is a plain class name with no category
(`:INHERIT BaseCounter;`), at error severity. The range covers the name.
A name containing a `.` (`:INHERIT Samples.BaseCounter;`) is qualified and
never flagged. The colon form `:INHERIT Samples:BaseCounter;` is a syntax
error too; it is flagged with a message that gives the dotted form.

It must NOT flag:

- a qualified parent, `Category.ClassName`;
- a quoted parent (`:INHERIT "Samples.BaseCounter";`): its compile
  behavior is not documented, so the rule is silent (DECISIONS.md D10);
- an `:INHERIT` with no name at all (a different, structural problem).

`base_requires_inherit` still counts a plain `:INHERIT` as present, so a
class that uses `Base:` with an unqualified parent is reported once, here.

## Examples

### Flags

```ssl
:CLASS Counter;
:INHERIT BaseCounter;
:DECLARE nCount;
```

### Flags

```ssl
:CLASS Counter;
:INHERIT Samples:BaseCounter;
:DECLARE nCount;
```

### Does not flag

```ssl
:CLASS Counter;
:INHERIT Samples.BaseCounter;
:DECLARE nCount;

:PROCEDURE Increment;
	Base:Increment();
	Me:nCount += 1;
:ENDPROC;
```

## Rationale

The parent class is found by its full script name, so a plain name fails
when the class compiles. The style guide makes this an authoritative
error (`inherit_qualified_name`, moved out of style-only in
ssl-style-guide#81), and the reference's `:INHERIT` page names the parent
as `Category.ClassName`. The rule reports it so the compile failure is
seen before check-in.
