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
  - internal/providers/class_sites_test.go
history:
  - date: 2026-11-01
    ref: "issue #360"
    note: >-
      New custom request for editors that draw scripts and procedures
      joined by their calls (SSL Studio's graph view), resolved the way go
      to definition resolves them (D19).
  - date: 2026-11-18
    ref: "issue #369"
    note: >-
      Class edges: `CreateUdObject` to the class created, `Method` to the
      method called on `Me`, `Base` or an object whose class the procedure
      settles, and `Inherit` from a class to its parent (A13-A20). A project
      built on classes showed almost none of its structure, and SSL Studio
      was reading `:INHERIT` out of the text by itself. New kinds only, so
      the shape version stays 1 (D19).
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
`RunDS` or `Include`, or one of the class kinds below; `target` is the
target string as written; `location` is that string at the call site,
quotes excluded; `resolved` says whether the target resolved; `ambiguous`
marks each edge of a call with more than one candidate.

**Class edges** join classes to the code that uses them:

- `CreateUdObject`: `CreateUdObject("Category.Class", …)`, from the
  procedure holding it to the class file's node. Only a string literal
  names a class; `CreateUdObject()` and `CreateUdObject(aPropertyDefs)`
  make an `SSLExpando` and are no edge.
- `Inherit`: `:INHERIT Category.Class;`, from the class file's node to its
  parent's. Only a class file inherits: the statement in any other file is
  no edge.
- `Method`: a method call, from the procedure holding it to the method's
  `procedure` node. `target` is `receiver:Method` as written, and
  `location` covers it. The receiver is `Me` (the class holding the call),
  `Base` (its parent), or a variable whose class the procedure settles:
  every assignment to it in the procedure holding the call (or in the code
  outside every procedure) is a statement that is `CreateUdObject("Class",
  …)` and nothing else, always of one class, and it is not a parameter.
  The order of the statements is not read. A receiver that is a member or
  an element (`a:b:Go()`, `a[1]:Go()`) is not such a variable.

A class that resolves to nothing is an `unresolved` node, one for the
workspace, as for a missing script. A method is looked for in the
receiver's class, then in each ancestor in turn; the edge points at the
nearest class that declares it. A method no class in the chain declares is
no edge and no node.

**Resolution** is go to definition's (feature.cross_file_resolution): a
1-part `DoProc` / `ExecFunction` target is a procedure of the same file,
matched case-insensitively; a dotted target, a `RunDS` target and an
`:INCLUDE` path resolve through the workspace index. A call that resolves
to a script's entry point points at the script node. Only targets written
as a string literal are calls (`ExtractCallSites`, #125). A class name, in
`CreateUdObject` or after `:INHERIT`, resolves to class files only
(feature.cross_file_resolution, class resolution).

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
- A8: Given a procedure inside nested comment regions Outer and Inner, when the graph is built, then its `regions` is `["Outer", "Inner"]`, outermost first.
- A9: Given a `RunDS` target and an `:INCLUDE` path that do not exist, each called from two files, when the graph is built, then each target is one `unresolved` node that both files' edges point at, with `resolved` false.
- A10: Given a file the client has not opened, holding a character outside the Basic Multilingual Plane before a call's target string on the same line, when the graph is built, then the edge's location is in UTF-16 code units.
- A11: Given a dotted target that names two scripts (the same `Category.Script` under two folders), when the graph is built, then the call has one edge per script, each `resolved` and `ambiguous`.
- A12: Given a `textDocument` the graph holds no file for, when the graph is built, then `nodes` and `edges` are empty lists, not null; given params that are not a JSON object of this shape, the request fails with an error.
- A13: Given `CreateUdObject("Category.Class", …)` in a procedure, when the graph is built, then there is a `CreateUdObject` edge from the procedure to the class file's node, `resolved`, located at the class name with the quotes excluded; a class that resolves to nothing gives an edge to an `unresolved` node; and `CreateUdObject()`, `CreateUdObject(aPropertyDefs)` and a class name that is not a string literal give no edge.
- A14: Given a class file with `:INHERIT Category.Class;`, when the graph is built, then there is an `Inherit` edge from the class file's node to the parent's file node, located at the parent's name; a parent that resolves to nothing gives an edge to an `unresolved` node; and an `:INHERIT` in a file that is not a class gives no edge.
- A15: Given `Me:Name()` in a class method, `Base:Name()` in a class that inherits, and `oVar:Name()` on a variable created as a class, when the graph is built, then each is a `Method` edge from the procedure holding the call to the method's `procedure` node, with `target` `receiver:Name` and the location of that text.
- A16: Given a method call whose receiver's class does not declare the method, when the graph is built, then the edge points at the nearest ancestor that declares it; a method no class in the chain declares gives no edge and no `unresolved` node; and classes that inherit each other in a circle end the search.
- A17: Given `textDocument`, `procedure` naming a method, depth 1 and direction `incoming`, when the graph is built, then the result holds the procedures that call the method through `Method` edges, with their file nodes.
- A18: Given an open class document whose buffer differs from the file on disk, when the graph is built, then its `:INHERIT` and its method calls come from the buffer.
- A19: Given a class name that names two class files, when the graph is built, then a `CreateUdObject` of it has one edge per file and a method call on the object one edge per method found, each `resolved` and `ambiguous`.
- A20: Given `oVar:Name()` where the variable is a parameter, is never assigned, is assigned anything but a whole-statement `CreateUdObject` of one class in the procedure holding the call, or is a member or an element of something else, when the graph is built, then the call gives no edge.

## Rationale

A graph view needs the same answers as go to definition, for every call at
once. Building it from the workspace index keeps the two in agreement; the
graph adds no resolution rules of its own. Open buffers win over the index
for positions, as for references and rename (#125): the index is candidate
discovery, and positions sent to a client must match the text it holds.

The class edges say what the dispatch edges cannot: an object's methods
are called by name on a value, not through a target string. `Me` and
`Base` name their class by where they stand. A variable does only when the
procedure leaves no doubt, which is why one assignment that is not a
`CreateUdObject`, or a parameter, gives no edge at all: a line drawn to the
wrong class is worse than a line missing. The check reads assignments, not
their order, so a call written above the `CreateUdObject` is an edge too;
that is a mistake the diagnostics may report, not one the graph hides.

A method no class declares is not shown as missing, unlike a missing
script. Every object answers methods of its own (`AddProperty`,
`IsMethod`, `IsProperty`), which the index does not hold, so the server
cannot tell a built-in from a typing mistake there.

Unresolved targets are nodes rather than dropped edges, because a missing
script is exactly what a graph view should show. A missing same-file
procedure stays per file, since two files' `DoProc("Gone")` are two
different mistakes; a missing dotted target names one script, so its
callers share the node.
