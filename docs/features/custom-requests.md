# Custom Requests: Syntax Tree, Call Graph and Lexical Tokens

> **Normative source:** [`feature.syntax_tree`](../../catalog/features/syntax_tree.md), [`feature.call_graph`](../../catalog/features/call_graph.md) and [`feature.lexical_tokens`](../../catalog/features/lexical_tokens.md) in the behavior catalog, and [D19](../../catalog/DECISIONS.md) for the conventions all three follow. This page is a guide; when it disagrees with a catalog entry, the entry wins.

**Status:** IMPLEMENTED  
**Methods:** `ssl/syntaxTree`, `ssl/callGraph`, `ssl/lexicalTokens`  
**Source Files:** `internal/providers/syntax_tree.go`, `internal/providers/lexical_tokens.go`, `internal/server/syntax_tree.go`, `internal/server/call_graph.go`, `internal/server/lexical_tokens.go`, `internal/server/wrapper_handler.go`

---

## 1. Overview

Three requests outside the LSP standard, for clients that use what the server
knows instead of reading SSL themselves: a block editor that shows a script as
nested procedures, regions and control flow, a graph of scripts and
procedures joined by their calls, and any feature that has to tell code from
a comment or a string. All three are views of machinery the standard features
already use, so they agree with folding, the outline, go to definition and
the diagnostics.

A client checks for them in the `initialize` result:

```json
{ "capabilities": { "experimental": { "ssl": { "syntaxTree": 1, "callGraph": 1, "lexicalTokens": 1 } } } }
```

The number is the version of the result's shape. It goes up only for a change
a client could misread; new fields and new kinds do not change it, so a client
should ignore what it does not know.

---

## 2. `ssl/syntaxTree`

Request, for an open document:

```json
{ "textDocument": { "uri": "file:///scripts/Orders.srvscr" } }
```

Response for `:PROCEDURE Total;` / `:PARAMETERS nQty;` / `:ENDPROC;` (one
statement per line), positions in UTF-16:

```json
{
  "uri": "file:///scripts/Orders.srvscr",
  "version": 4,
  "nodes": [
    {
      "kind": "procedure", "keyword": "PROCEDURE", "name": "Total",
      "range":       { "start": { "line": 0, "character": 0 },  "end": { "line": 2, "character": 9 } },
      "nameRange":   { "start": { "line": 0, "character": 11 }, "end": { "line": 0, "character": 16 } },
      "headerRange": { "start": { "line": 0, "character": 0 },  "end": { "line": 0, "character": 17 } },
      "closerRange": { "start": { "line": 2, "character": 0 },  "end": { "line": 2, "character": 9 } },
      "children": [
        { "kind": "parameters", "keyword": "PARAMETERS",
          "range": { "start": { "line": 1, "character": 0 }, "end": { "line": 1, "character": 17 } } }
      ]
    }
  ]
}
```

Things a client can rely on:

- **Nothing is lost.** The top-level nodes and the whitespace between them
  make up the whole document; inside a node, its header, children and closer
  do the same. Blank lines are `blankLines` nodes. A client can draw, select
  and move nodes by their ranges.
- **Malformed code keeps the parser's shape.** An unclosed block has no
  `closerRange`; a statement the grammar cannot read, a stray closer, and an
  `:ELSE` with no `:IF` are `error` nodes.
- **`version`** is the document version the tree was built from. Drop a tree
  whose version is older than the buffer.
- **`if` and `try` hold branches.** Each `branch` carries its keyword (`IF`,
  `ELSE`, `TRY`, `CATCH`, `FINALLY`, and `CASE` / `OTHERWISE` inside a
  `case`) and its opening statement as `headerRange`.

The catalog entry lists every kind.

---

## 3. `ssl/callGraph`

Every param is optional. With none, the result is the whole workspace:

```json
{ "textDocument": { "uri": "file:///scripts/Helpers.srvscr" }, "procedure": "Calc", "depth": 1, "direction": "incoming" }
```

asks for `Calc` and its direct callers. `depth` omitted means no bound;
`direction` is `outgoing`, `incoming` or `both` (the default).

```json
{
  "nodes": [
    { "id": "file:///scripts/Orders.srvscr", "kind": "script", "name": "LIMS.Orders",
      "location": { "uri": "file:///scripts/Orders.srvscr", "range": { "start": { "line": 0, "character": 0 }, "end": { "line": 0, "character": 0 } } } },
    { "id": "file:///scripts/Orders.srvscr#main", "kind": "procedure", "name": "Main",
      "container": "file:///scripts/Orders.srvscr", "regions": ["Entry"],
      "location": { "uri": "file:///scripts/Orders.srvscr", "range": { "start": { "line": 2, "character": 0 }, "end": { "line": 9, "character": 0 } } } },
    { "id": "file:///scripts/Helpers.srvscr#calc", "kind": "procedure", "name": "Calc", "container": "file:///scripts/Helpers.srvscr", "location": { "...": "..." } },
    { "id": "file:///scripts/Helpers.srvscr", "kind": "script", "name": "LIMS.Helpers", "location": { "...": "..." } }
  ],
  "edges": [
    { "from": "file:///scripts/Orders.srvscr#main", "to": "file:///scripts/Helpers.srvscr#calc",
      "kind": "ExecFunction", "target": "LIMS.Helpers.Calc", "resolved": true,
      "location": { "uri": "file:///scripts/Orders.srvscr", "range": { "start": { "line": 4, "character": 15 }, "end": { "line": 4, "character": 32 } } } }
  ],
  "indexing": false
}
```

- **Node kinds:** `script` (`isClass` for a class file), `dataSource`,
  `procedure`, and `unresolved` for a target that resolved to nothing, so a
  missing script shows in the graph.
- **Edge kinds:** `DoProc`, `ExecFunction`, `RunDS`, `Include`. An edge starts
  at the procedure holding the call, or at the file for code outside every
  procedure. A call with several candidates gives one edge each, marked
  `ambiguous`.
- **Class edge kinds:** `CreateUdObject` (from the procedure holding
  `CreateUdObject("Category.Class", …)` to the class file), `Inherit` (from
  a class file to the parent its `:INHERIT` names) and `Method` (from the
  procedure holding `Me:Name()`, `Base:Name()` or `oVar:Name()` to the
  method, in the class or the nearest ancestor that declares it). `oVar`
  counts only when every assignment to it in that procedure is a
  `CreateUdObject` of one class. A `Method` edge's `target` is
  `receiver:Name`. New kinds do not raise the shape version: a client
  ignores the kinds it does not know.
- **Resolution** is go to definition's
  ([feature.cross_file_resolution](../../catalog/features/cross_file_resolution.md)).
  Open documents are read from their buffers, other files from the workspace
  index. While `indexing` is true, calls into files not yet indexed show as
  unresolved; ask again once it is false.

---

## 4. `ssl/lexicalTokens`

Request, for an open document:

```json
{ "textDocument": { "uri": "file:///scripts/Orders.srvscr" } }
```

Response for these three lines, positions in UTF-16:

```ssl
/* Reads the samples;
sFolder := "exports";
:RETURN sFolder;
```

```json
{
  "uri": "file:///scripts/Orders.srvscr",
  "version": 4,
  "tokens": [
    { "kind": "comment", "range": { "start": { "line": 0, "character": 0 },  "end": { "line": 0, "character": 21 } } },
    { "kind": "string",  "range": { "start": { "line": 1, "character": 11 }, "end": { "line": 1, "character": 20 } } },
    { "kind": "keyword", "range": { "start": { "line": 2, "character": 0 },  "end": { "line": 2, "character": 7 } } }
  ]
}
```

- **Kinds:** `comment`, `string` (quotes included), `keyword` (colon
  included), `text` (a `:REGION` body) and `codeBlock` (a `{|x| ...}`
  literal). Names, numbers, operators and the literal words get no token.
- **The tokens are the lexer's**, so they follow SSL's rules: a comment ends
  at its first `;` and a `*/` ends nothing, a string has no escapes, and the
  `:` after a receiver is member access, not a keyword.
- **A token is reported whole.** A comment or string that spans lines is one
  token with a multi-line range, and one that is never ended runs to the end
  of the document.
- **`text` and `codeBlock` are opaque.** The server does not read inside
  them, so nothing inside one is reported on its own.
- **A SQL data source has no tokens**; a header above a SQL body has its
  own.
- **`version`** is the document version the tokens were read from.

Use semantic tokens for names and colour
([semantic-tokens.md](./semantic-tokens.md)); this request is for knowing
where code stops and text starts ([D21](../../catalog/DECISIONS.md)).
