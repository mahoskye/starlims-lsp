---
id: diag.undeclared_variable
title: Use of an undeclared variable
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
config:
  - ssl.diagnostics.globals
  - ssl.diagnostics.endpointPatterns
spec_options:
  check_undeclared_vars: true
  is_endpoint_file: true
  global_variables: ["gSystemContext"]
tests:
  - internal/providers/predefined_globals_test.go
  - internal/providers/providers_test.go
  - internal/providers/undeclared_declarations_test.go
  - internal/providers/fp_triage_304_test.go
history:
  - date: 2025-11-XX
    ref: "commit a4df25b"
    note: Introduced opt-in (default off) per the noisy-checks policy (DECISIONS.md D5).
  - date: 2025-11-19
    ref: "vs-code-ssl-formatter #2/#53 (v1.1.0+)"
    note: "False positives fixed: Me access and built-in function names are not undeclared variables."
  - date: 2025-12-XX
    ref: "vs-code-ssl-formatter #55/#56"
    note: >-
      ssl.diagnostics.globals entries count as declared; identifiers inside
      :INCLUDE paths are not variable references.
  - date: 2026-05-14
    ref: "PR #23 (v0.7.7)"
    note: >-
      Endpoint scripts (ssl.diagnostics.endpointPatterns match or Endpoint:
      docblock) treat Request/Response as pre-injected ambients — declared
      for reads, never flagged. In non-endpoint files they still flag:
      using them there is a real bug (DECISIONS.md D4).
  - date: 2026-07-03
    ref: "include-aware diagnostics PR"
    note: >-
      Names declared by resolved :INCLUDE targets count as declared
      (feature.cross_file_resolution A18-A19) — :INCLUDE is a full-splice
      textual paste, so the included script's declarations belong to the
      including file. Single-file behavior (and every fence) unchanged.
  - date: 2026-08-08
    ref: "issue #149"
    note: >-
      Identifiers inside :INHERIT qualified base names are not variable
      references — same exemption mechanism as :INCLUDE paths. (The
      :CLASS name itself still false-flags; filed separately.)
  - date: 2026-08-08
    ref: "issue #155"
    note: >-
      The identifier following :CLASS is the class-name declaration, not a
      variable use — same skip-until-semicolon mechanism as :INCLUDE and
      :INHERIT. Closes the exemption noted in the #149 entry above.
  - date: 2026-08-28
    ref: "issue #184 (expression AST consumers)"
    note: >-
      Declaration resolution is now statement-based rather than
      line-based. Both halves of the check read declarations by line: the
      declared-name set came from an AST-node walk that grouped tokens by
      line, and the declaration-site exemption asked whether a declaring
      keyword sat on the same line. A declaration written as a bare
      `:DECLARE` (or `:PARAMETERS`) with its names on the following lines
      therefore registered none of its names AND exempted none of them,
      so every name flagged itself at its own declaration. Names and
      spans now come from parser.CollectDeclarations /
      parser.DeclarationSpans, which read the statement through its
      terminating `;`. Corpus: 241 of 2,060 hits (11.7%) were this
      false positive; after the fix, zero flagged name appears on a
      declaration line anywhere in its own file.
  - date: 2026-10-13
    ref: "issues #272, #270"
    note: >-
      Label and block names are not variable uses: the name after
      :LABEL, :REGION and :BEGININLINECODE is skipped like an :INCLUDE
      path (#272). The base-installation predefined globals are now
      declared — vendored from the reference guide (Session,
      PublicConsts, CRLF, UserLang, sCC, ...), where only MYUSERNAME
      was before. A name the guide lists as not declared on a base
      installation (CR, LF, TAB, sTab, sSQ, sDQ, MYUSERID, ...) still
      flags, and its message names the replacement (#270). Request and
      Response are declared in every script too: the reference says
      reading them outside an endpoint does not raise, so D4's
      endpoint-only scoping was amended. Corpus under --strict:
      1,811 -> 829 findings.
  - date: 2026-10-13
    ref: "issue #258"
    note: >-
      Behavior text corrected: the check was described as per-scope, but
      it is file-wide, and deliberately so. SSL's caller-scope lookup lets
      a procedure read its caller's variables, so per-procedure scoping
      would flag valid code.
  - date: 2026-10-13
    ref: "issue #304"
    note: >-
      Two false-positive classes from unmodified product code. A name
      followed by `{` is class instantiation, not a variable read (the
      product instantiates designer classes the published inventory does
      not list). And a file with an :INCLUDE the caller could not read
      gets no findings: the included script may declare any name, and
      134 of the 210 --strict findings in including product files named
      a variable the included script declares. With a workspace index
      that resolves every include, the closure applies as before
      (feature.cross_file_resolution A21). Corpus under --strict,
      unmodified product code: 328 -> 115 findings.
  - date: 2026-10-13
    ref: "issue #328"
    note: >-
      A fragment another rule already reports is no longer an undeclared
      variable too (one typo is one finding): a bare AND/OR/NOT
      (bare_logical_operator), an exponent glued to a number such as the
      e2 of 7e2 (scientific_notation), and the segments of an unquoted
      ExecFunction/DoProc path (execfunction_missing_quotes).
  - date: 2026-10-14
    ref: "issue #343"
    note: >-
      Silent inside a statement that fails to parse on its first line, and
      inside comment text stranded by a `;`: one undeclared_variable per
      prose word was noise on top of the parse error.
issues: []
---

## Behavior

Opt-in check (default off, per DECISIONS.md D5): flags an identifier used as
a variable that has no `:DECLARE`/`:PUBLIC`/`:PARAMETERS` declaration, and
is not created by assignment, anywhere in the file; each name is reported
once per file. The check is file-wide, not per procedure, on purpose: SSL
resolves a name through the scopes of the routines on the call stack
(reference guides/variable-scope.md), so a procedure can read a variable
its caller declared, and which procedure calls which is not known
statically (DECISIONS.md D10).

Declarations are read as statements — keyword through the terminating
`;` — so layout never hides one: `:DECLARE sA, sB;`, a comma list broken
across lines, and a bare `:DECLARE` with its names on the following lines
all declare the same names, and every one of those positions is a binding
site rather than a use.

Treated as declared, never flagged: built-in function and class names,
`Me`/`Base` forms, identifiers inside `:INCLUDE` paths, `:INHERIT`
qualified base names (issue #149), the `:CLASS` name itself
(issue #155), the name after `:LABEL` (spaced form; the joined
`:LABELName` form is a single keyword token), `:REGION` and
`:BEGININLINECODE` (issue #272), the predefined globals a base
installation declares (issue #270; the platform's `Session` and
`PublicConsts` and the application's publics such as `MYUSERNAME`,
`CRLF`, `UserLang`, `sCC`, read from the vendored reference guide
`internal/constants/data/predefined-globals.md`), STARLIMS status names
(`Released`, `Draft`, ...), names listed in
`ssl.diagnostics.globals`, names declared by the file's resolved
`:INCLUDE` targets (the include declaration closure,
`feature.cross_file_resolution` A18-A19 — `:INCLUDE` splices the included
script's text, so its declarations are in scope), and the platform objects
`Request`, `Response`, `Session` and `PublicConsts` in every script: the
reference says referencing `Request` or `Response` outside an endpoint
does not raise (DECISIONS.md D4, amended 2026-10-13).

A name the reference lists as **not** declared on a base installation
(`CR`, `LF`, `TAB`, `sTab`, `sSQ`, `sDQ`, `MYUSERID`, `MYLANGID`,
`MYDEPT`, `MYSITE`) is not treated as declared. When one is undeclared,
the message adds the replacement, for example "use Chr(13) instead".
An installation that does declare one adds it to `ssl.diagnostics.globals`.

A file with an `:INCLUDE` whose closure is incomplete — a target that
does not resolve, or no workspace index at all, as in `--validate` — is
not checked: the unread script may declare any of its names, and a
finding there would assert a knowledge gap as a defect (DECISIONS.md
D10, issue #304). A file without an `:INCLUDE` is checked single-file.

A name immediately followed by `{` on its line is a class instantiation
(`FormDto{}`), not a variable read, whether or not the published
inventory lists the class (issue #304).

A statement that fails to parse on its first line (`check the totals;`,
prose stranded as code) is not checked: its words are not variables, and
`unexpected_token` already reports the line once. Comment text stranded by
a `;` is likewise `comment_text_after_terminator`'s alone (issue #343).

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	nTotal := nMissing + 1;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE nCount;
	nCount := 1;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:PARAMETERS sFirst, sSecond
	, sThird;
	:DECLARE
		sFourth,
		sFifth;
	UsrMes(sFirst + sSecond + sThird + sFourth + sFifth);
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Handler;
	Response:Write(Request:QueryString("id"));
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	gSystemContext := "ready";
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE sName;
	sName := "abc";
	sName:SomeRandomDotNetMethod();
:ENDPROC;
```

### Does not flag

```ssl
:CLASS RestApiUsers;
:PROCEDURE GetUsers;
	:DECLARE aOut;
	aOut := {};
	:RETURN aOut;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE nX;
	nX := 1;
	:LABEL SKIP;
	:REGION Body;
	SELECT 1
	:ENDREGION;
	:RETURN nX;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE sOut;
	sOut := MYUSERNAME + CRLF + UserLang + sCC + GlbImpExpPath;
	sOut += Session:SessionID + LimsString(glb_In_Batch);
	:RETURN sOut;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE sOut;
	sOut := "a" + CR + LF;
	:RETURN sOut;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE oForm;
	oForm := FormDto{"frmMain", 0, 0, 200, 200};
	:RETURN oForm;
:ENDPROC;
```

### Does not flag

```ssl
:INCLUDE Shared.ErrorCodes;
:PROCEDURE Demo;
	RaiseError(aErrNotFound[2], "Demo", aErrNotFound[1]);
:ENDPROC;
```

## Rationale

Undeclared-variable detection is the highest-value and highest-noise check
in the pipeline — its history is a list of contexts that had to be exempted
one false positive at a time. Each Does-not-flag fence pins one of those
exemptions permanently. The endpoint-ambient fence runs with
`is_endpoint_file: true` (spec_options); that `Request` is also declared
outside an endpoint is pinned in providers_test.go
(TestEndpointAmbients_RequestNotUndeclaredOutsideEndpoint), since spec
fences share one option set per entry.
The member-access fence pins DECISIONS.md D10 (issue #22): the member of
a `:` access is never a variable reference, and built-in value types
forward unmatched members to .NET at runtime, so flagging them would
false-positive every legitimate passthrough.
The include fence pins the same principle one step out (issue #304):
the spec runner has no workspace index, so `Shared.ErrorCodes` is
unread, and a finding for a name it may declare would be a guess.
