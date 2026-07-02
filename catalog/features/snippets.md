---
id: feature.snippets
title: Snippets
kind: feature
status: draft
authority: tool
schema_ref: null
config: []
tests:
  - internal/providers/providers_test.go
history: []
issues: []
---

## Behavior

- Snippets MUST be delivered through `textDocument/completion` as items with
  `kind: Snippet` and `insertTextFormat: Snippet`, using LSP placeholder
  syntax (`$1`, `${1:default}`, `$0`).
- The snippet set MUST cover the core SSL patterns:
  - procedures: `proc`, `procparams`;
  - control flow: `if`, `ifelse`, `while`, `for`, `forstep`, `case`
    (`:BEGINCASE` with `:CASE`/`:OTHERWISE`/`:EXITCASE`);
  - error handling: `try`, `tryfinally`, `catchssl`
    (`GetLastSSLError()`), `catchsql` (`GetLastSQLError()`);
  - declarations: `declare`, `public`, `include`;
  - dispatch/SQL: `doproc` (DoProc with argument array), `sql`
    (SQLExecute with named `?var?` placeholders).
- Snippet bodies MUST expand to syntactically valid SSL: colon-prefixed
  keywords, terminating semicolons, and matched block closers
  (`:ENDPROC`, `:ENDIF`, `:ENDWHILE`, `:NEXT`, `:ENDCASE`, `:ENDTRY`).
- Every snippet MUST carry tab stops for the user-supplied parts (names,
  conditions, bodies) so expansion never leaves the cursor stranded.
- Snippets MUST be offered alongside regular completions in code context and
  MUST NOT be offered when the cursor is inside a string literal or a
  comment (subject to the DoProc/ExecFunction procedure-name string
  exception, which offers bare names only — never snippet bodies).
- No snippet-specific configuration exists; there are no user-defined or
  per-project snippets.

## Acceptance

- A1: Given a completion request in code context, when the results are returned, then snippet items are present with `kind: Snippet` and `insertTextFormat: Snippet`, and their bodies contain `$`-style tab stops.
- A2: Given the `proc` snippet, when it is expanded, then the result is a complete `:PROCEDURE ...;` / `:ENDPROC;` block with a header comment and tab stops on the procedure name and body.
- A3: Given the `case` snippet, when it is expanded, then the result contains `:BEGINCASE;`, at least one `:CASE` with `:EXITCASE;`, an `:OTHERWISE;` branch, and `:ENDCASE;`.
- A4: Given the `sql` snippet, when it is expanded, then the result is a `SQLExecute("...")` call whose SQL body uses named `?value?` placeholder syntax.
- A5: Given every snippet in the set, when its body is inspected, then all block-opening keywords have their matching closers and statements end with `;` — no snippet may expand to SSL that the diagnostics pipeline flags as structurally broken.
- A6: Given the cursor inside an ordinary string literal or a comment, when completion is requested, then no snippet items are returned.

## Rationale

SSL's block syntax is verbose and unforgiving (colon-prefixed keywords,
mandatory `;` terminators, distinct closer per block kind), so templates
that always emit matched, valid structures remove the most common class of
structural typos the diagnostics would otherwise flag. The set mirrors the
patterns the language forces on users — `DoProc` dispatch, `SQLExecute`
with named placeholders, `GetLastSSLError`/`GetLastSQLError` catch bodies —
rather than generic editor snippets. Filtering and final ordering against
keyword completions are left to the client; the server's contract is only
what it returns and where it stays silent (strings, comments).
