---
id: feature.definition
title: Go to definition
kind: feature
status: active
authority: tool
schema_ref: null
config: []
tests:
  - internal/providers/identifier_roles_test.go
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
  - date: 2026-07-02
    ref: "feature.cross_file_resolution"
    note: >-
      Cross-file resolution added: dotted dispatch targets and :INCLUDE
      paths navigate across the workspace; A6 narrowed to 1-part targets
      and unresolvable dotted targets. Same-file behavior unchanged.
  - date: 2026-07-03
    ref: "RunDS navigation PR"
    note: >-
      RunDS string targets navigate to the resolved data-source file
      (A13); 1-part RunDS targets resolve by basename, unlike dispatch
      targets, per feature.cross_file_resolution A16.
  - date: 2026-07-03
    ref: "UDObject member navigation PR"
    note: >-
      Member go-to-definition for shape-inferred UDObject receivers
      (A14-A15): the member navigates to the property's definition (the
      CreateUDObject initializer key or the first `:prop :=`
      augmentation). Shaped receiver + unknown member is null; unshaped
      receivers keep prior behavior.
  - date: 2026-08-28
    ref: "issue #184 (expression AST consumers)"
    note: >-
      Identifier occurrences are now classified by role from the
      expression tree (parser.IdentifierRoles): a variable reference, a
      member name, a call callee, a class name, a declared name, or a
      procedure header. Word matching could not separate a variable
      `sName` from the property in `oRec:sName` or from a like-named
      procedure, so this behavior acted on occurrences of a different
      symbol. Positions the tree cannot resolve stay unclassified and
      keep the prior word-match behavior.
issues: ["#41"]
---

## Behavior

- Definition MUST resolve, within the current file:
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
- Dotted dispatch targets (`ExecFunction("Cat.Script")`,
  `ExecFunction("Cat.Script.Proc")`, `DoProc("Cat.Script.Proc")`,
  flat-layout `"Script.Proc"`) and `:INCLUDE` paths (bare, dotted, or
  quoted; cursor on the keyword or the path) resolve through the
  workspace resolver (`feature.cross_file_resolution`): 2-part
  category.script targets land on the target script's entry point
  (file-level `:PARAMETERS` line), procedure targets land on the
  procedure, includes land on the file. Ambiguous targets return
  multiple Locations, anchored-layout candidates first.
- Bare 1-part dispatch targets keep same-script semantics: they resolve
  same-file or return null, never cross-file.
- `RunDS` string targets (`RunDS("Category.Name")` or `RunDS("Name")`)
  resolve to the data-source file's entry through the workspace resolver's
  data-source rules (`feature.cross_file_resolution` A15-A17). Unlike
  dispatch targets, 1-part RunDS targets resolve cross-file — a data
  source is always a separate file.
- Definition MUST return null when nothing matches — a dotted target
  the workspace cannot resolve, or a non-dotted name with no same-file
  declaration. Resolution never guesses: a resolved script that lacks
  the named procedure is null, not a nearby location.
- The member in `<recv>:<member>` where `<recv>` has a
  CreateUDObject-inferred shape (feature.completion's shape inference)
  navigates to the property's definition: the initializer literal's key
  or the first `:prop :=` augmentation, wherever the shape learned it.
  A shaped receiver whose shape lacks the member is null — never a
  fallback to an unrelated same-named symbol. Unshaped receivers keep
  the prior word-based behavior.

## Acceptance

- A1: Given a file with `:PROCEDURE HelperProc;` and a later `DoProc("HelperProc")` (double- or single-quoted), when go-to-definition is invoked on the name inside the string, then the location of the `HelperProc` declaration line is returned.
- A2: Given `:DECLARE counter;` inside a procedure and a later use of `counter`, when go-to-definition is invoked on the use, then the `:DECLARE` line is returned; the same holds for `:PARAMETERS` and `:PUBLIC` declarations.
- A3: Given `:PROCEDURE Helper;` and a dispatch string spelled `DoProc("helper")`, when go-to-definition is invoked, then the declaration is found despite the case difference.
- A4: Given a file-level `:PUBLIC globalVar;` and a procedure-local `:DECLARE globalVar;`, when go-to-definition is invoked on a use inside that procedure, then the local declaration is returned, not the file-level one.
- A5: Given `SQLExecute(...)` or a keyword such as `:IF`, when go-to-definition is invoked on it, then the response is null — built-ins and keywords must not navigate anywhere.
- A6: Given `DoProc("SomeProc")` where `SomeProc` is not defined in the current file, when go-to-definition is invoked, then the response is null — 1-part targets are same-script by language semantics and never resolve cross-file; a dotted target that resolves nowhere in the workspace is null too.
- A7: Given `ExecFunction("Calculate")` with a same-file `:PROCEDURE Calculate;`, when go-to-definition is invoked inside the string, then the `Calculate` declaration is returned.
- A8: Given `ExecFunction("Cat.Script.Proc")` where the workspace contains that script with that procedure, when go-to-definition is invoked inside the string, then a Location in the target file at the procedure's line is returned.
- A9: Given `ExecFunction("Cat.Script")` resolving to a workspace script with a file-level `:PARAMETERS`, when go-to-definition is invoked, then the Location is that script's entry point at the `:PARAMETERS` line.
- A10: Given `:INCLUDE SharedLib;`, `:INCLUDE Cat.SharedLib;`, or `:INCLUDE "SharedLib";` with the cursor on the keyword or path, when go-to-definition is invoked, then the resolved file is returned at line 0.
- A11: Given a dispatch target matching two workspace files (one anchored, one flat), when go-to-definition is invoked, then multiple Locations are returned with the anchored candidate first.
- A12: Given a dotted target differing from the indexed names only by case, when go-to-definition is invoked, then it resolves identically to the exact-case form.
- A13: Given `RunDS("QUERIES.ORDERS")` with a workspace data source `Data Sources/QUERIES/ORDERS.ds`, or `RunDS("Orders")` with a flat `Orders.ds`, when go-to-definition is invoked inside the string, then a Location in the data-source file is returned; a RunDS target resolving nowhere is null.
- A14: Given `oObj := CreateUDObject({{"Name", "x"}});` followed by `oObj:Total := 5;` and later uses, when go-to-definition is invoked on `Name` or `Total` after `oObj:`, then the Location is the initializer key or the first augmenting assignment respectively.
- A15: Given the same shaped `oObj` and go-to-definition on the member in `oObj:Unknown`, when the member is not in the inferred shape, then the response is null — even if an unrelated variable named `Unknown` exists in the file.
- A16: Given the cursor on the member name of `oRec:sName` with a local variable `sName` declared in the same procedure, when definition is requested, then no location is returned — a property is not the like-named local, and no answer beats a wrong one.

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

- Diagnostics remain `:INCLUDE`-unaware (included symbols do not count
  as declared) — a later milestone; navigation resolves the include
  itself.
