# Custom Requests: Syntax Tree and Call Graph

> **Normative source:** [`feature.syntax_tree`](../../catalog/features/syntax_tree.md) and [`feature.call_graph`](../../catalog/features/call_graph.md) in the behavior catalog, and [D19](../../catalog/DECISIONS.md) for the conventions both follow. This page is a guide; when it disagrees with a catalog entry, the entry wins.

**Status:** IMPLEMENTED  
**Methods:** `ssl/syntaxTree`, `ssl/callGraph`  
**Source Files:** `internal/providers/syntax_tree.go`, `internal/server/syntax_tree.go`, `internal/server/call_graph.go`, `internal/server/wrapper_handler.go`

---

## 1. Overview

Two requests outside the LSP standard, for clients that draw what the server
knows instead of parsing SSL themselves: a block editor that shows a script as
nested procedures, regions and control flow, and a graph of scripts and
procedures joined by their calls. Both are views of machinery the standard
features already use, so they agree with folding, the outline and go to
definition.

A client checks for them in the `initialize` result:

```json
{ "capabilities": { "experimental": { "ssl": { "syntaxTree": 1, "callGraph": 1 } } } }
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
- **Resolution** is go to definition's
  ([feature.cross_file_resolution](../../catalog/features/cross_file_resolution.md)).
  Open documents are read from their buffers, other files from the workspace
  index. While `indexing` is true, calls into files not yet indexed show as
  unresolved; ask again once it is false.
