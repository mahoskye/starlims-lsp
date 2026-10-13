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
      Response keep the D4 endpoint scoping. Corpus under --strict:
      1,811 -> 829 findings.
issues: []
---

## Behavior

Opt-in check (default off, per DECISIONS.md D5): flags an identifier used as
a variable that has no `:DECLARE`/`:PUBLIC`/`:PARAMETERS` declaration in
scope, reported once per scope.

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
script's text, so its declarations are in scope), and — in endpoint
scripts only — the runtime ambients `Request` and `Response`. In
non-endpoint files `Request` and `Response` flag like any other
undeclared name.

A name the reference lists as **not** declared on a base installation
(`CR`, `LF`, `TAB`, `sTab`, `sSQ`, `sDQ`, `MYUSERID`, `MYLANGID`,
`MYDEPT`, `MYSITE`) is not treated as declared. When one is undeclared,
the message adds the replacement, for example "use Chr(13) instead".
An installation that does declare one adds it to `ssl.diagnostics.globals`.

Without a workspace index (or when includes don't
resolve) the check is single-file, exactly as before.

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

## Rationale

Undeclared-variable detection is the highest-value and highest-noise check
in the pipeline — its history is a list of contexts that had to be exempted
one false positive at a time. Each Does-not-flag fence pins one of those
exemptions permanently. The endpoint-ambient fence runs with
`is_endpoint_file: true` (spec_options); the inverse case — `Request`
flagging in a non-endpoint file — is covered by PR #23's tests in
providers_test.go, since spec fences share one option set per entry.
The member-access fence pins DECISIONS.md D10 (issue #22): the member of
a `:` access is never a variable reference, and built-in value types
forward unmatched members to .NET at runtime, so flagging them would
false-positive every legitimate passthrough.
