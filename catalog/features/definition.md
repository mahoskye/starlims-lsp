---
id: feature.definition
title: Go to definition
kind: feature
status: active
authority: tool
schema_ref: null
config: []
tests:
  - internal/providers/providers_test.go
  - internal/server/handler_test.go
history:
  - date: 2026-01-10
    ref: "442fa69 / f27f727"
    note: Initial same-file definition for procedures and declared variables,
      case-insensitive.
  - date: 2026-02-02
    ref: "d56cbfe (v0.2.0)"
    note: Scope precedence added — local/parameter declarations in the
      cursor's procedure win over :PUBLIC declarations.
  - date: 2026-02-02
    ref: "15102d6 (v0.2.0)"
    note: Go-to-definition for DoProc/ExecFunction string targets (double- or
      single-quoted), same-file only.
  - date: 2026-07-02
    ref: "issue #41"
    note: Scope fallback restricted — with procedure info available, only
      file-level declarations qualify; another procedure's local resolves
      to null instead of a foreign location.
issues: ["#41"]
---

## Behavior

- Definition MUST resolve, within the current file only:
  - procedure names to their `:PROCEDURE` declaration line, with the range
    covering the procedure name;
  - variable and parameter uses to their `:DECLARE`, `:PARAMETERS`, or
    `:PUBLIC` declaration line.
- Procedure names inside the first string argument of `DoProc("Name", ...)`
  and `ExecFunction("Name", ...)` MUST resolve to the same-file
  `:PROCEDURE Name` declaration when one exists; single-quoted targets
  (`DoProc('Name')`) resolve the same way.
- Matching MUST be case-insensitive (`myproc` finds `:PROCEDURE MyProc`).
- Scope resolution MUST prefer the local declaration: a `:DECLARE` inside the
  current procedure shadows a file-global declaration of the same name; only
  when no local declaration exists may a `:PUBLIC` / file-level declaration
  be returned.
- Definition MUST return null for built-in functions, built-in classes, and
  keywords — they have no navigable source in user code.
- Definition MUST return null when no same-file declaration matches;
  it MUST NOT guess across files (`:INCLUDE` targets and namespace paths such
  as `DoProc("Helpers.CalculateTotal")` are unresolved — cross-file
  navigation is planned on top of the workspace index).

## Acceptance

- A1: Given a file with `:PROCEDURE HelperProc;` and a later `DoProc("HelperProc")` (double- or single-quoted), when go-to-definition is invoked on the name inside the string, then the location of the `HelperProc` declaration line is returned.
- A2: Given `:DECLARE counter;` inside a procedure and a later use of `counter`, when go-to-definition is invoked on the use, then the `:DECLARE` line is returned; the same holds for `:PARAMETERS` and `:PUBLIC` declarations.
- A3: Given `:PROCEDURE Helper;` and a dispatch string spelled `DoProc("helper")`, when go-to-definition is invoked, then the declaration is found despite the case difference.
- A4: Given a file-level `:PUBLIC globalVar;` and a procedure-local `:DECLARE globalVar;`, when go-to-definition is invoked on a use inside that procedure, then the local declaration is returned, not the file-level one.
- A5: Given `SQLExecute(...)` or a keyword such as `:IF`, when go-to-definition is invoked on it, then the response is null — built-ins and keywords must not navigate anywhere.
- A6: Given `DoProc("SomeProc")` where `SomeProc` is not defined in the current file, when go-to-definition is invoked, then the response is null — the provider must not return a location in another file or a spurious same-file match.
- A7: Given `ExecFunction("Calculate")` with a same-file `:PROCEDURE Calculate;`, when go-to-definition is invoked inside the string, then the `Calculate` declaration is returned.

## Rationale

SSL dispatch is string-based (`DoProc`/`ExecFunction`), so treating those
first-argument strings as navigable call sites (15102d6, v0.2.0) is what
makes definition useful at all in this language; restricting it to same-file
targets keeps the result trustworthy until namespace path resolution lands on
the workspace index (vs-code-ssl-formatter#16, planned as part of cross-file
navigation). Local-over-global preference (d56cbfe) mirrors SSL's actual
scoping (variables are procedure-scoped, `:PUBLIC` is file-global).
Built-ins and keywords return null rather than an error because "no
definition" is the truthful answer — their documentation belongs to hover,
not navigation.

## Known gaps

- Cross-file targets (`:INCLUDE`, namespace paths) are unresolved; the
  workspace index provides the foundation, resolution is the planned
  cross-file feature.
