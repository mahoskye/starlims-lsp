---
id: feature.snippets
title: Snippets
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
    ref: "442fa69 (v0.1.0)"
    note: Initial server-side snippet set (procedures, control flow, error
      handling, declarations, DoProc/SQL patterns) delivered as completion
      items.
  - date: 2026-03-30
    ref: "f6e78ef"
    note: Data-source files (.ds/.ds.txt) get their own snippet set
      (dsparams, sqlds, sslds); the standard script snippets are withheld
      there.
issues: []
---

## Behavior

Snippets are served by the LSP itself (not the VS Code extension), delivered
through `textDocument/completion`:

- Snippet items carry `kind: Snippet` (15) and `insertTextFormat: Snippet`
  (2), using LSP placeholder syntax (`$1`, `${1:default}`, `$0`).
- The standard set MUST cover the core SSL patterns — procedures (`proc`,
  `procparams`), control flow (`if`, `ifelse`, `while`, `for`, `forstep`,
  `case` with `:CASE`/`:OTHERWISE`/`:EXITCASE`), error handling (`try`,
  `tryfinally`, `catchssl` with `GetLastSSLError()`, `catchsql` with
  `GetLastSQLError()`), declarations (`declare`, `public`, `include`), and
  dispatch/SQL (`doproc`, `sql` with named `?var?` placeholders) — plus
  class, region, and LIMS-specific templates.
- Data-source files (`.ds`/`.ds.txt`) get a data-source snippet set
  (`dsparams`, `sqlds`, `sslds`) instead; the standard script snippets MUST
  NOT be offered there.
- Snippet bodies MUST expand to structurally valid SSL: colon-prefixed
  keyword lines end with `;`, and every block opener has its matching
  closer (`:PROCEDURE`/`:ENDPROC`, `:IF`/`:ENDIF`, `:WHILE`/`:ENDWHILE`,
  `:FOR`/`:NEXT`, `:BEGINCASE`/`:ENDCASE`, `:TRY`/`:ENDTRY`,
  `:BEGININLINECODE`/`:ENDINLINECODE`, `/* region;`/`/* endregion;`).
- Every snippet MUST carry tab stops for the user-supplied parts (names,
  conditions, bodies) so expansion never leaves the cursor stranded.
- Snippets are offered alongside regular completions on explicit invocation
  in code context. They MUST NOT be offered when the cursor is inside a
  string literal or a comment (the DoProc/ExecFunction procedure-name
  string exception offers bare procedure names only — never snippet
  bodies), and they are not part of the keyword-only `:` auto-trigger
  response (see feature.completion).
- No snippet-specific configuration exists; there are no user-defined or
  per-project snippets.

## Acceptance

- A1: Given the snippet inventory, when snippet completions are produced, then every item has `kind: Snippet`, `insertTextFormat: Snippet`, and at least one `$`-style tab stop in its body.
- A2: Given the `proc` snippet, when it is expanded, then the result is a complete `:PROCEDURE ...;` / `:ENDPROC;` block with a header comment and a tab stop on the procedure name.
- A3: Given the `case` snippet, when it is expanded, then the result contains `:BEGINCASE;`, at least one `:CASE` with `:EXITCASE;`, an `:OTHERWISE;` branch, and `:ENDCASE;`.
- A4: Given the `sql` snippet, when it is expanded, then the result is a `SQLExecute("...")` call whose SQL body uses named `?value?` placeholder syntax.
- A5: Given every snippet in both sets, when its body is inspected, then all block-opening keywords have their matching closers and every colon-keyword line ends with `;` — no snippet may expand to structurally broken SSL.
- A6: Given the cursor inside an ordinary string literal or a comment, when completion is requested, then no snippet items are returned.
- A7: Given a data-source file, when snippet completions are requested, then the data-source set (`dsparams`, `sqlds`, `sslds`) is offered and standard script snippets (`proc`, `if`, `while`, ...) are NOT.

## Rationale

SSL's block syntax is verbose and unforgiving (colon-prefixed keywords,
mandatory `;` terminators, a distinct closer per block kind), so templates
that always emit matched, valid structures remove the most common class of
structural typos the diagnostics would otherwise flag — A5 makes that a
standing guarantee rather than a review habit. The set mirrors the patterns
the language forces on users — `DoProc` dispatch, `SQLExecute` with named
placeholders, `GetLastSSLError`/`GetLastSQLError` catch bodies — rather
than generic editor snippets, and data-source files get builder-directive
templates instead of script scaffolding because `:PROCEDURE` blocks are
meaningless there (f6e78ef). Filtering and final ordering against keyword
completions are left to the client; the server's contract is only what it
returns and where it stays silent (strings, comments).
