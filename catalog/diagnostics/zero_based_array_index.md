---
id: diag.zero_based_array_index
title: Array indexed with literal 0
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: error
config:
  - ssl.diagnostics.infoDiagnostics
severity_overridable: true
suppressible: true
spec_options:
  include_info_diagnostics: true
tests:
  - internal/providers/providers_test.go
  - internal/providers/fp_triage_304_test.go
history:
  - date: 2026-02-02
    ref: "commit 7261172"
    note: >-
      Introduced in the gotcha-detection batch (gotcha #5): SSL arrays are
      1-based, so [0] is always a bug.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-08-08
    ref: "issue #152"
    note: >-
      Contextual severity added: when the subscripted identifier is
      reached through colon member access (`dataSet:Tables[0]`), the
      value may be a .NET collection with zero-based indexing, so the
      diagnostic downgrades to a warning with .NET-aware wording. Bare
      identifiers (`aValues[0]`) keep the error.
  - date: 2026-08-12
    ref: "issue #166"
    note: >-
      .NET-derivation tracking added: a bare identifier whose most recent
      assignment's RHS is a colon member call or a
      LimsNetConnect/LimsNetCast result (aBytes := oInt:ToByteArray();
      aBytes[0]) also downgrades to the warning — indexing such values
      with [0] is correct code on 0-based .NET collections. Unrelated
      bare identifiers keep the error; a later non-.NET reassignment
      restores it.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Catalog audit. The .NET path moved from warning to the info tier:
      the style-guide rule net_collections_zero_based (authoritative)
      prescribes `dataSet:Tables[0]`, and all 245 production findings were
      that path — 227 of them a `:Tables[0]` access, the rest `:Columns`,
      `:Rows` and similar — so a default-on warning was firing on the
      prescribed form. A "verify which one this is" note is
      advisory (D13). Detection moved from a token scan to the expression
      tree, which sees every dimension: `aRows[1][0]` and `aRows[0, 1]`
      were missed. Authority corrected to authoritative to mirror
      arrays_one_based. Production: default 245 -> 0, --info 245 -> 247
      (two multi-dimensional sites, both info).
  - date: 2026-10-13
    ref: "issue #304"
    note: >-
      A subscript whose receiver chain passes through a `:Tables` member
      (`ds:Tables[0]`, `ds:Tables[0]:Rows[0]`) no longer reports, even at
      the info tier. No SSL built-in class has a Tables member, so the
      chain is the .NET DataSet API, which the style guide prescribes,
      and index 0 there is known to be right rather than worth
      verifying. Every --info finding on unmodified product code was this
      shape. Other member receivers keep the info advisory.
issues: []
---

## Behavior

Flags a subscript that is exactly the literal `0`, in any dimension of an
array access — `aItems[0]`, `aRows[1][0]`, `aRows[1, 0]`, `aRows[0, 1]`.
SSL arrays and string positions are 1-based (ssl-style-guide
`arrays_one_based`), so index 0 can never address a native SSL element.
Subscripts are read from the expression tree; the range covers the `0`
literal.

Severity follows what the subscripted value is:

- **Error** when the receiver is a bare variable (`aValues[0]`) — a native
  SSL array access.
- **Info** (the opt-in advisory tier) when the value is plausibly a
  0-based .NET collection: the receiver is reached through `:` member
  access (`dataSet:Tables[0]`, `oObj:Prop:Items[0]`), or the variable's
  most recent assignment (file order) has a right-hand side containing a
  colon member call or a `LimsNetConnect`/`LimsNetCast` call
  (`aBytes := oInt:ToByteArray(); ... aBytes[0]`). A later reassignment
  from a non-.NET right-hand side restores the error. The message says
  zero-based indexing may be valid for .NET collections.

The diagnostic code is the same in both cases.

The rule is deliberately literal-only and must NOT flag:

- any other literal index (`aItems[1]`, `aItems[10]`);
- a variable or expression index (`aItems[nIndex]`, `aItems[0 + 1]`) —
  even when it could evaluate to 0, only the bare literal is provable;
- a subscript whose receiver is a call result or a parenthesized
  expression (`DoProc("GetRows")[0]`) — what it returns is unknown, and
  unknown is never evidence;
- the .NET-path sites when the info tier is off (the default);
- a subscript whose receiver chain passes through a `:Tables` member,
  at any tier (`ds:Tables[0]`, `ds:Tables[0]:Rows[0]`,
  `ds:Tables[0]:Columns[0]`): no SSL built-in class has a Tables
  member, so this is the .NET DataSet and the DataTables reached from
  it, which are 0-based (issue #304);
- brackets that are not subscripts — an array literal, a bracket string
  (`[0]` as a value).

## Examples

### Flags

```ssl
:DECLARE aItems, sFirst;
sFirst := aItems[0];
```

### Flags

```ssl
:DECLARE oList, vFirst;
oList := LimsNetConnect("mscorlib", "System.Collections.ArrayList");
vFirst := oList:Items[0];
```

### Flags

```ssl
:DECLARE oInt, aBytes, bZero;
oInt := LimsNetConnect("System", "System.Numerics.BigInteger");
aBytes := oInt:ToByteArray();
bZero := aBytes[0] == 0;
```

### Flags

```ssl
:DECLARE aRows, sValue;
aRows := {{1, 2}};
sValue := aRows[1][0];
sValue := aRows[1, 0];
```

### Does not flag

```ssl
:DECLARE aItems, sFirst;
sFirst := aItems[1];
```

### Does not flag

```ssl
:DECLARE sValue;
sValue := DoProc("GetRows")[0];
sValue := [0];
```

### Does not flag

```ssl
:DECLARE dataSet, oTable, sName, vCell;
dataSet := LimsNetConnect("System.Data", "System.Data.DataSet");
oTable := dataSet:Tables[0];
sName := dataSet:Tables[0]:Columns[0]:ColumnName;
vCell := dataSet:Tables[0]:Rows[0]:ItemArray[0];
```

### Does not flag

```ssl
:DECLARE aItems, sFirst, nIndex;
sFirst := aItems[nIndex];
```

### Does not flag

```ssl
:DECLARE aItems, sFirst;
sFirst := aItems[0 + 1];
```

## Rationale

The zero-index habit from 0-based languages is one of the most common SSL
porting mistakes (gotcha #5, commit 7261172), and a literal `[0]` has no
legitimate reading on a native SSL array — hence error severity, and the
style-guide rule `arrays_one_based` (authoritative, error) that the
authority mirrors. Values reached through colon member access are
different: SSL code routinely holds .NET objects whose collections are
0-based, and the style guide prescribes `dataSet:Tables[0]`
(`net_collections_zero_based`). Issue #152 moved those sites from error to
warning; issue #255 moved them to the info tier, because over 6,228
production files every one of the 245 findings was such a site and a
default-on warning was firing on the prescribed form. "Verify which one
this is" is advice, not a likely bug, so it belongs to the advisory tier
(DECISIONS.md D13), where an LLM consumer still gets the nudge. The
nudge is dropped where there is nothing to verify: a chain through
`:Tables` is the DataSet API itself, and every info finding on
unmodified product code in the corpus was that shape (issue #304). The
derived-variable extension (issue #166) follows the same reasoning one
assignment further: 0-vs-1 basing is not statically decidable without
type flow, so a cheap last-assignment heuristic marks exactly the values
that flow from the .NET surface. Reading subscripts from the expression
tree (issue #255) is what catches the multi-dimensional forms in the
third Flags fence; the token scan only looked at the first bracket after
an identifier. Restricting the match to the exact literal keeps the
guarantee airtight: everything short of provable (computed indices,
variables, call results) is left alone, which the Does-not-flag fences
pin.
