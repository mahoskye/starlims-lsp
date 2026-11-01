---
id: feature.call_graph
title: Call graph (ssl/callGraph)
kind: feature
status: active
authority: tool
schema_ref: null
config: []
tests:
  - internal/server/custom_requests_test.go
history:
  - date: 2026-11-01
    ref: "issue #360"
    note: >-
      New custom request for editors that draw scripts and procedures
      joined by their calls (SSL Studio's graph view), resolved the way go
      to definition resolves them (D19).
issues: []
---

## Behavior

The server answers the custom request `ssl/callGraph`. Every param is
optional:

```json
{ "textDocument": { "uri": "..." }, "procedure": "Calc", "depth": 1, "direction": "incoming" }
```

The result is `{ "nodes": [...], "edges": [...], "indexing": false }`.

**Files.** The graph covers every file the workspace index holds and every
open document. An open document is read from its buffer; any other file
from the index, which reflects the file on disk. `indexing` is true while
the index is still being built, when calls into files not yet read show as
unresolved.

**Nodes** have an `id`, a `kind` and a `name`:

- `script`: one per script or class file (`isClass` true for a class). The
  id is the file URI; the name is `Category.Script`, or the script name when
  the file sits in no canonical layout (feature.cross_file_resolution A1-A3);
  the location is the file.
- `dataSource`: one per data-source file, likewise.
- `procedure`: one per procedure (or class method). The id is the file URI,
  `#`, and the lowercased name; `container` is the file's id; the location
  covers the lines from `:PROCEDURE` through `:ENDPROC`; `regions` names the
  comment regions around the `:PROCEDURE` line, outermost first.
- `unresolved`: a call target that resolved to nothing, named as written. A
  missing procedure of the same file (`DoProc("Gone")`) is one node per
  file; any other missing target is one node for the whole workspace,
  however many files call it.

**Edges** are call sites, one per call and resolution candidate: `from` is
the procedure holding the call, or the file's node for code outside every
procedure; `to` is the target node; `kind` is `DoProc`, `ExecFunction`,
`RunDS` or `Include`; `target` is the target string as written; `location`
is that string at the call site, quotes excluded; `resolved` says whether
the target resolved; `ambiguous` marks each edge of a call with more than
one candidate.

**Resolution** is go to definition's (feature.cross_file_resolution): a
1-part `DoProc` / `ExecFunction` target is a procedure of the same file,
matched case-insensitively; a dotted target, a `RunDS` target and an
`:INCLUDE` path resolve through the workspace index. A call that resolves
to a script's entry point points at the script node. Only targets written
as a string literal are calls (`ExtractCallSites`, #125).

**Scope.** Without `textDocument` the result is the whole graph. With it,
the scope is that document's file node and procedures, or, when `procedure`
names one, that procedure alone. The result is the nodes reachable from the
scope within `depth` edges (omitted: no bound; 0: the scope alone),
following edges in `direction`: `outgoing` (what the scope calls),
`incoming` (what calls it) or `both` (the default); plus the file node of
each procedure reached, and every edge between two reached nodes.

## Acceptance

- A1: Given a workspace with two scripts in a canonical layout, a data source and calls to missing targets, when the client sends `ssl/callGraph` with no params, then there is a `script` node per script (named `Category.Script`), a `dataSource` node, a `procedure` node per procedure with its container, its lines and its region path, and an `unresolved` node per missing target.
- A2: Given calls by `:INCLUDE`, a 1-part `DoProc`, a dotted `ExecFunction` and `RunDS`, when the graph is built, then each is an edge of its kind from the procedure holding it (the file for the `:INCLUDE`) to the resolved node, with `resolved` true and the location of the target string.
- A3: Given a dotted `ExecFunction` and a 1-part `DoProc` whose targets do not exist, when the graph is built, then each edge points at an `unresolved` node with `resolved` false, the 1-part one specific to the calling file.
- A4: Given `textDocument`, `procedure`, depth 1 and direction `incoming`, when the graph is built, then the result holds the procedure, its direct callers with their file nodes, and only the edges between them.
- A5: Given a document scope with depth 0, when the graph is built, then the result holds that file's nodes and the edges among them only; with direction `outgoing` and no depth it holds everything the file calls.
- A6: Given an open document whose buffer differs from the file on disk, when the graph is built, then its procedures and call sites come from the buffer.
- A7: Given an initialize request, when the server answers, then `capabilities.experimental.ssl.callGraph` is the shape version, 1.

## Rationale

A graph view needs the same answers as go to definition, for every call at
once. Building it from the workspace index keeps the two in agreement; the
graph adds no resolution rules of its own. Open buffers win over the index
for positions, as for references and rename (#125): the index is candidate
discovery, and positions sent to a client must match the text it holds.

Unresolved targets are nodes rather than dropped edges, because a missing
script is exactly what a graph view should show. A missing same-file
procedure stays per file, since two files' `DoProc("Gone")` are two
different mistakes; a missing dotted target names one script, so its
callers share the node.
