---
id: feature.lexical_tokens
title: Lexical tokens (ssl/lexicalTokens)
kind: feature
status: active
authority: tool
schema_ref: null
config: []
tests:
  - internal/providers/lexical_tokens_test.go
  - internal/server/custom_requests_test.go
history:
  - date: 2026-11-25
    ref: "issue #375"
    note: >-
      New custom request for a client that may not read SSL itself and
      still has to tell code from a comment or a string, and a keyword
      from a name (SSL Studio's table hover and its search of a script
      outside its comments). The issue asked for `comment`, `string` and
      `keyword` semantic tokens; those would colour over the client's
      grammar, so the lexer answers through a request of its own (D21).
issues: []
---

## Behavior

The server answers the custom request `ssl/lexicalTokens`, with params
`{ "textDocument": { "uri": ... } }`, for a document the client has open:

```json
{ "uri": "file:///...", "version": 7, "tokens": [ { "kind": "comment", "range": { ... } } ] }
```

`version` is the document version the tokens were read from, so a client
can drop a result a later edit has made stale. A document the client has
not opened has no tokens: the result is `null`. Positions are UTF-16, like
every position this server sends.

Each token has a `kind` and a `range`. The tokens lie in document order and
do not overlap. A range may span lines: a token is reported whole.

| Kind | What it is |
|------|------------|
| `comment` | A comment, from `/*` through the `;` that ends it |
| `string` | A string literal, quotes included: `"..."`, `'...'` or `[...]` |
| `keyword` | A colon keyword, colon included (`:IF`, `:TO`) |
| `text` | The body of a `:REGION` block: stored text, not code |
| `codeBlock` | A `{\|params\| body}` literal |

The tokens are the ones the lexer gives every other feature, so they follow
SSL's own rules, which are not the ones a reader of C-like code would
guess:

- **A comment ends at its first `;`**, and a `*/` ends nothing. A comment
  with no `;` runs to the end of the document.
- **A string has no escapes.** It ends at the next quote of its own kind,
  whatever comes before that quote; a `[` opens a string only where a value
  can start, so the `[1]` of `aRows[1]` is a subscript. A string that is
  never closed runs to the end of the document.
- **A comment is a token wherever it stands**, between the lines of one
  statement included.
- **A keyword is a `:` word in keyword position.** The `:` after a
  receiver is member access, so `oRule:Return` holds no keyword, and a line
  that starts with `:Trim()` after a line ending in a value continues that
  line's member chain. The word's case does not matter. A colon word in
  keyword position that is not a keyword of the language is reported too:
  that is how the lexer read it, and diag.unknown_keyword says so.
- **Opaque spans are reported whole.** The server does not read inside a
  `:REGION` body or a code-block literal, so nothing inside one is reported
  on its own: a quote or a `/*` in a region body is text.

Everything else gets no token: names, numbers, operators, punctuation, and
the literal words `NIL`, `.T.` and `.F.`, which are values. Names are
semantic tokens' (feature.semantic_tokens).

**Data sources.** A plain SQL data source is not SSL and has no tokens.
With a directive or `:PARAMETERS` header above an SQL body, only the header
has tokens. An SSL data source has them throughout. This is the split
semantic tokens use (feature.semantic_tokens A12).

The request is advertised as `capabilities.experimental.ssl.lexicalTokens`,
whose value is the version of the result's shape (D19). A new kind does not
raise it: a client ignores the kinds it does not know.

## Acceptance

- A1: Given an open document at version 7, when the client sends `ssl/lexicalTokens`, then the result carries its URI, version 7 and the tokens with UTF-16 positions; a document with nothing to report answers an empty `tokens`, and a document that is not open answers null.
- A2: Given an initialize request, when the server answers, then `capabilities.experimental.ssl.lexicalTokens` is the shape version, 1.
- A3: Given a comment, a statement with a string, and a comment whose text holds `*/`, when tokens are built, then each comment runs to its own `;` and the string between them is a `string`: the `*/` ends nothing.
- A4: Given a string that ends in a backslash before its closing quote, followed by a comment, when tokens are built, then the string ends at that quote and the comment is a `comment`.
- A5: Given a statement of two strings with a comment on a line between them, when tokens are built, then there are two `string` tokens and one `comment` token.
- A6: Given strings in double quotes, single quotes and brackets, each holding the other kinds of quote, an empty string and a subscript `aRows[1]`, when tokens are built, then each string is one `string` token with its quotes and the subscript has none.
- A7: Given colon keywords in any case, a `:FOR` header with `:TO` and `:STEP`, a member named like a keyword, a member chain continued on a line of its own, and `NIL`, `.T.`, `.F.` and the logical operators, when tokens are built, then exactly the colon keywords are `keyword` tokens, each covering its colon and word; a colon word in keyword position that the language does not have is one too.
- A8: Given a comment and a string that span lines, when tokens are built, then each is one token whose range spans those lines; a comment or a string that is never ended runs to the end of the document's text.
- A9: Given a `:REGION` body holding a `/*` and a quote, and a code-block literal holding a string, when tokens are built, then the body is one `text` token, the literal one `codeBlock` token, and nothing inside either is reported.
- A10: Given a plain SQL data source, when tokens are built, then there are none; given a `:PARAMETERS` header above an SQL body, only the header has tokens; given an SSL data source, it has them throughout; and the same text in a script is read as SSL.
- A11: Given any SSL fixture in the repository or a fuzzed input, when tokens are built, then they lie in document order without overlap, each non-empty, inside the document and of a listed kind, and every comment and string the lexer read is reported exactly once.
- A12: Given any SSL fixture in the repository, when lexical and semantic tokens are built for it, then no semantic token overlaps a `comment`, `keyword`, `text` or `codeBlock` token, and one that overlaps a `string` is a dispatch target lying inside it.
- A13: Given params that are not a JSON object of this shape, when the client sends `ssl/lexicalTokens`, then the request fails with an error.

## Rationale

A client that may not read SSL (D19) still needs to know where code stops
and text starts: to search a script for a name outside its comments, or to
offer a hover only on a word that could be a name. Worked out in the
client, those answers go wrong exactly where SSL differs from the
languages its syntax resembles. A reader that closes a comment at `*/`
takes the code after an early `;` for comment text, and one that honours a
backslash reads a Windows path straight through its closing quote. The
lexer already decides both, for every feature, so the request is a view of
its tokens and cannot disagree with the diagnostics or the formatter.

It is a request of its own, not more semantic-token types, because those
are a colour channel (D21). The kinds are the ones the need names, plus the
two spans the lexer keeps opaque, so a client is never told nothing about
text the server did not read. A range spans lines because the result is
data: a client asking whether an offset lies in a comment wants one range
to test, not one per line.

Only open documents are answered, as for the syntax tree. What a client
does with the answer, such as finding the tables a script's SQL names, is
the client's feature and not the server's (issue #370).
