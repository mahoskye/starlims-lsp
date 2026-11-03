---
id: feature.semantic_tokens
title: Semantic tokens
kind: feature
status: active
authority: tool
schema_ref: null
config:
  - ssl.diagnostics.globals
  - ssl.diagnostics.endpointPatterns
tests:
  - internal/providers/semantic_tokens_test.go
  - internal/server/semantic_tokens_test.go
history:
  - date: 2026-11-03
    ref: "issue #366"
    note: >-
      New standard feature for editors that highlight with a TextMate
      grammar (SSL Studio, any LSP client with semantic highlighting): the
      server colours the names a grammar cannot tell apart (D20).
issues: []
---

## Behavior

The server answers `textDocument/semanticTokens/full` and
`textDocument/semanticTokens/range` for a document the client has open, and
advertises both through `semanticTokensProvider`, without delta. A document
the client has not opened has no tokens: the result is `null`. Positions and
lengths are UTF-16, like every position this server sends. A token lies on
one line.

Only names get tokens. Keywords, operators, literals, strings and comments
stay with the client's grammar, with the one exception of a dispatch target.

The legend:

| Types | `function`, `method`, `parameter`, `variable`, `property`, `class` |
|-------|---|
| Modifiers | `declaration`, `defaultLibrary`, `readonly`, `public`, `unknown` |

`public` and `unknown` are this server's own (D20). What a name gets:

| Name | Type | Modifiers |
|------|------|-----------|
| The name in a `:PROCEDURE` header | `function`, or `method` in a class file | `declaration` |
| A name in `:PARAMETERS` | `parameter` | `declaration` |
| A name in `:DECLARE` | `variable` | `declaration` |
| A name in `:PUBLIC` | `variable` | `public`, `declaration` |
| A bare name | By its declaration (below) | |
| A predefined global or status keyword | `variable` | `defaultLibrary`, `readonly` |
| `Me`, `Base` | `variable` | `defaultLibrary` |
| An undeclared name | `variable` | `unknown` |
| `Name(...)` | `function` | `defaultLibrary` when the reference lists it |
| `Name{...}` | `class` | `defaultLibrary` when the reference lists it |
| The name after `:`, followed by `(` | `method` | |
| Any other name after `:` | `property` | |
| A `DoProc` / `ExecFunction` target that resolves | `function` | |

Rules:

- **Bare names** resolve as go to definition resolves them
  (feature.definition A4): a `:PARAMETERS` or `:DECLARE` of the procedure the
  name sits in, then a `:PUBLIC` or a configured global
  (`ssl.diagnostics.globals`; `public`), then a declaration at the top level
  of the file, then one anywhere else in the file or in an included script
  (`variable`, because a procedure can read what its caller declares). A name
  assigned with no declaration is a `variable`. A function, class, literal or
  operator word named without its call gets no token.
- **Unknown** agrees with diag.undeclared_variable: a name is `unknown` when
  that check reports it, whether or not the check is switched on, whatever
  its severity, and despite any suppression comment. Where the check reports
  a name once, at its first undeclared use, the token marks each undeclared
  use. A use after an assignment to the name is declared, as it is for the
  check. The check's gates apply: nothing is `unknown` in a data source, nor
  in a file with an `:INCLUDE` that was not read.
- **Unparsed statements** get no tokens: a statement diag.undeclared_variable
  would stay silent in, because it is text a comment's early `;` stranded or
  it fails to parse on its first line.
- **Unclassified names** get no token, so the grammar's colour stands: a name
  the parser cannot place, such as one after `:CLASS`, `:INHERIT`, `:LABEL`
  or `:REGION`.
- **Dispatch targets** resolve as go to definition resolves them
  (feature.definition): a one-part target names a procedure of this file, any
  other goes through the workspace index. The token covers the string's
  content, quotes excluded. A target that does not resolve has no token, so
  the string's own colour marks it. While the index is still being built,
  targets in files not yet read have no token; the client's next request
  corrects that.
- **Data sources.** A plain SQL file has no tokens. With a directive or
  `:PARAMETERS` header above an SQL body, only the header has tokens. An SSL
  data source has them throughout. A data source's undeclared names, such as
  a directive's bare value, get no token.

## Acceptance

- A1: Given an open document, when the client sends `textDocument/semanticTokens/full`, then the result is the tokens in the protocol's five-integer relative encoding, sorted by position; for a document that is not open the result is null, and a document with no tokens answers an empty `data`.
- A2: Given an initialize request, when the server answers, then `semanticTokensProvider` carries the legend, `full` and `range` are true, and no delta is offered.
- A3: Given a `:PROCEDURE` header, when tokens are built, then its name is `function` with `declaration`, and `method` with `declaration` in a class file.
- A4: Given `:PARAMETERS` and `:DECLARE` in a procedure, when tokens are built, then each binding is `parameter` or `variable` with `declaration`, each use (a `:DEFAULT` target included) is the same type without it, and another procedure's own declaration of the name is classified by that procedure's.
- A5: Given a `:PUBLIC` name, when tokens are built, then the binding and each use are `variable` with `public`, except in a procedure that declares the name itself; a configured global is `variable` with `public`.
- A6: Given a predefined global, a status keyword, `Me` and `Base`, when tokens are built, then the first two are `variable` with `defaultLibrary` and `readonly`, the last two `variable` with `defaultLibrary`, and a name the file declares over a predefined one is classified by its declaration.
- A7: Given calls, instantiations and member access, when tokens are built, then a listed function or class has `defaultLibrary` and an unlisted one does not, a member followed by `(` is `method` and any other `property`.
- A8: Given a name used with no declaration, when tokens are built, then every undeclared use is `variable` with `unknown`, a use after an assignment to it is plain `variable`, and an assignment target is plain `variable`.
- A9: Given any SSL fixture in the repository, when tokens and diag.undeclared_variable run with the same options, then the names with an `unknown` token are exactly the names the check reports, and each finding's position holds an `unknown` token; a fixture holding a suppression comment is checked in the first direction only.
- A10: Given a statement that fails to parse on its first line, or prose a comment's early `;` stranded, when tokens are built, then none of its names has a token, and a name whose first undeclared use lies there is not `unknown` anywhere.
- A11: Given an `:INCLUDE` that was not read, when tokens are built, then nothing is `unknown`; given a resolved `:INCLUDE`, a name the included script declares is `variable`, and a name it does not declare is `unknown`.
- A12: Given a plain SQL data source, when tokens are built, then there are none; given a header above an SQL body, only the header has tokens; given an SSL data source, nothing is `unknown`, and a directive's bare value has no token.
- A13: Given dispatch targets, when tokens are built, then a `DoProc` of a procedure of this file and an `ExecFunction` the index resolves are `function` over the string's content without quotes, a target that does not resolve has no token, and a `RunDS` target, an `:INCLUDE` path, any other string and any comment have none.
- A14: Given text with a character outside the Basic Multilingual Plane before a name on its line, when the client asks for tokens, then the name's start is in UTF-16 units.
- A15: Given any SSL fixture or fuzzed input, when tokens are built, then they lie in document order without overlap, each on one line, within the document, and each covers a name or a dispatch target.
- A16: Given a range, when the client sends `textDocument/semanticTokens/range`, then the result holds only the tokens that touch the range, the first encoded from the document's start; a document that is not open answers null.
- A17: Given keywords, operators and literals, when tokens are built, then none has a token, nor a function named without its call.

## Rationale

A grammar sees text, so it paints every bare name alike. The server already
knows what each name is, so an editor can show the difference without
parsing SSL. The types and modifiers come from the parser's identifier roles
and declaration statements, and the `unknown` verdict from the
undeclared-variable check's own walk, so a colour never disagrees with what
rename or a diagnostic says about the same name.

`unknown` does not follow the check's opt-in. D5 keeps the check off because
a false finding costs the reader a warning to weigh; a token costs nothing
unless a theme gives the modifier a colour, and the check's false-positive
guards apply either way.

Only names get tokens because the client's grammar colours the rest correctly
and sooner. Sending keywords and strings too would repeat its work on every
edit and take the colour of a half-typed statement away from the grammar.
