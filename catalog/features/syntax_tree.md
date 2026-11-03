---
id: feature.syntax_tree
title: Syntax tree (ssl/syntaxTree)
kind: feature
status: active
authority: tool
schema_ref: null
config: []
tests:
  - internal/providers/syntax_tree_test.go
  - internal/server/custom_requests_test.go
history:
  - date: 2026-11-01
    ref: "issue #359"
    note: >-
      New custom request for editors that draw a document's structure
      (SSL Studio's blocks view), which must agree with this server's
      parse instead of parsing SSL themselves (D19).
issues: []
---

## Behavior

The server answers the custom request `ssl/syntaxTree`, with params
`{ "textDocument": { "uri": ... } }`, for a document the client has open:

```json
{ "uri": "file:///...", "version": 7, "nodes": [ ... ] }
```

`version` is the document version the tree was built from, so a client can
drop a tree a later edit has made stale. A document the client has not
opened has no tree: the result is `null`. Positions are UTF-16, like every
position this server sends.

Each node has a `kind`, a `range`, and, where they apply, a `keyword` (the
statement's leading keyword, uppercase without its colon: `"IF"`,
`"DECLARE"`), a `name` and `nameRange`, a `headerRange` (the statement or
comment that opens a block), a `closerRange` (the one that closes it) and
`children`. A range runs from the node's first character through its last,
the terminating `;` included.

The tree follows the statements and blocks of the parse every other
feature reads (`parser.Parse`): a block holds what the parser put in it,
also when the source is malformed. Statements are the leaves; an
expression inside a statement stays opaque text.

| Kind | What it is |
|------|------------|
| `procedure` | `:PROCEDURE Name;` … `:ENDPROC;`, named, with header and closer |
| `if`, `try` | `:IF` … `:ENDIF;`, `:TRY` … `:ENDTRY;`: children are its branches, closer is the end statement |
| `case` | `:BEGINCASE;` … `:ENDCASE;`, with header and closer; children are its `:CASE` / `:OTHERWISE` branches |
| `branch` | One arm: `:IF`, `:ELSE`, `:TRY`, `:CATCH`, `:FINALLY`, `:CASE` or `:OTHERWISE` (its `keyword`), with its opening statement as header and its body as children |
| `while`, `for` | `:WHILE` … `:ENDWHILE;`, `:FOR` … `:NEXT;`, with header and closer |
| `region` | `/* region Name;` … `/* endregion;`, named, the two markers as header and closer |
| `regionStart`, `regionEnd` | A region marker the tree cannot pair (below) |
| `textRegion` | `:REGION Name;` … `:ENDREGION;`, named; its body is one `text` child |
| `inlineCode` | `:BEGININLINECODE Name;` … `:ENDINLINECODE;`, named |
| `parameters`, `default` | A `:PARAMETERS` or `:DEFAULT` statement |
| `statement` | Any other statement |
| `banner` | The comment that opens the file |
| `docBlock` | The comment above a procedure |
| `comment` | Any other standalone comment |
| `blankLines` | A run of whitespace-only lines |
| `error` | Code the parser could not read (below) |

Rules:

- **Coverage.** In document order and without overlap, the top-level nodes
  and the whitespace between them make up the whole document. Inside every
  node, the header, the children and the closer do the same for the node's
  range. Every whitespace-only line is part of some node: a `blankLines`
  node, or a multi-line node that contains it.
- **Branches.** An `if` or `try` holds only branches, and the first branch's
  header is the `:IF` or `:TRY` statement; the node has no header of its
  own. Blank lines before the next branch or the closer belong to the branch
  above them.
- **Unclosed blocks.** A block the parser never saw closed has no
  `closerRange` and ends at its last child. Blank lines after that child
  belong to the enclosing list.
- **Comment regions** pair as folding pairs them (feature.folding A5): in
  document order, each `/* endregion;` closes the innermost open region.
  A pair whose two markers are in the same list of children becomes a
  `region` node holding the nodes between them. A marker whose partner sits
  at another depth (a region that crosses a block boundary), or that has no
  partner, stays a leaf: `regionStart` (named) or `regionEnd`. An unnamed
  region is named `Region`, as in the outline.
- **Doc blocks and the banner.** The comment above a procedure, which its
  hover reads, is its `docBlock`; blank lines and a `/*@private;` or
  `/*@protected;` annotation may sit between them. The first node of the
  file, when it is a comment, is the `banner`, unless it is the doc block of
  a procedure directly below it or a visibility annotation. A first comment separated from the
  procedure below it by a blank line is the banner.
- **Errors.** A statement holding a token the statement grammar cannot
  accept (diag.unexpected_token; the formatter's unparsed code, #351) is an
  `error` leaf. So is a closer no block claims (`:ENDIF;` with nothing
  open). An arm outside the block it belongs to (`:ELSE;` with no `:IF`) is
  an `error` node with the arm's header and body. The opening or closing
  statement of a block keeps its block even when it does not parse; its
  diagnostic reports it.

## Acceptance

- A1: Given any SSL fixture in the repository (the idempotence corpus, every scenario input, every SSL example fence of this catalog) or a fuzzed input, when the tree is built, then its pieces (leaves, headers, closers) lie in document order without overlap, only whitespace lies outside them, every whitespace-only line lies in a piece, every node lies inside its parent, a header starts its node and a closer ends it.
- A2: Given `:PROCEDURE Calc;` with `:PARAMETERS`, `:DEFAULT` and `:RETURN` and an `:ENDPROC;`, when the tree is built, then one `procedure` node named `Calc` (name range on the identifier) has header and closer ranges, a range ending after the closer's `;`, and the children `parameters`, `default`, `statement`.
- A3: Given a first comment followed by a blank line and a procedure with a comment and an annotation above it, when the tree is built, then the first comment is a `banner`, the comment above the procedure a `docBlock`, and the annotation a `comment`; a first comment directly above a procedure is its `docBlock`.
- A4: Given nested comment regions Outer and Inner in one list, when the tree is built, then Inner is a `region` child of the `region` Outer, with its name range; a region opened outside a procedure and closed inside it leaves a `regionStart` and a `regionEnd` leaf, and an orphan closer is a `regionEnd` leaf.
- A5: Given `:IF`/`:ELSE`, `:TRY`/`:CATCH`/`:FINALLY` and `:BEGINCASE` with `:CASE`/`:OTHERWISE`, when the tree is built, then the `if` and `try` nodes hold one `branch` per arm and close at their end statement, and the `case` node holds a `branch` per arm.
- A6: Given `:WHILE`, `:FOR`, `:REGION` and `:BEGININLINECODE` blocks, when the tree is built, then they are `while`, `for`, `textRegion` (with one `text` child) and `inlineCode` nodes, the last two named.
- A7: Given blank lines between top-level statements and between the two arms of an `:IF`, when the tree is built, then each run is one `blankLines` node, and the run before `:ELSE` is the last child of the first branch.
- A8: Given a statement with an unexpected token, a stray `:ENDIF;`, and an `:ELSE;` with no `:IF`, when the tree is built, then each is an `error` node, and the stray `:ELSE` keeps its body as children.
- A9: Given a `:PROCEDURE` holding an `:IF` that is never closed, when the tree is built, then the `if` has no closer and ends at its last statement, and the procedure still closes at its `:ENDPROC`.
- A10: Given an open document at version 7, when the client sends `ssl/syntaxTree`, then the result carries its URI, version 7 and the tree with UTF-16 positions; for a document that is not open the result is null.
- A11: Given an initialize request, when the server answers, then `capabilities.experimental.ssl.syntaxTree` is the shape version, 1.

## Rationale

An editor that shows a script as nested blocks has to agree with the
server exactly: a second parser in the client would disagree at the edges,
which are where malformed code lives. The tree is therefore a view of the
existing parse, not a new grammar, and malformed code keeps the shape the
parser gave it, marked where the statement grammar failed.

Coverage is the property a block editor relies on: if every character
belongs to exactly one node or to the whitespace between nodes, the client
can render, select and move nodes by their ranges without losing text.
`blankLines` nodes keep the author's spacing visible as part of that.

Headers and closers are ranges rather than child nodes so that `children`
holds only the body a view draws inside the block. Comment regions that
cross a block boundary cannot nest in a tree; leaving their markers as
leaves keeps them visible without inventing a nesting the source does not
have.
