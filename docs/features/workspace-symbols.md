# Workspace Symbols

> **Normative source:** [`feature.workspace_symbols`](../../catalog/features/workspace_symbols.md) in the behavior catalog. This page is a guide; when it disagrees with the catalog entry, the entry wins.

**Status:** IMPLEMENTED
**LSP Method:** `workspace/symbol`
**Source Files:** `internal/server/workspace_index.go`, `internal/server/handler.go`

---

## 1. Overview

The workspace symbols provider enables searching for symbols across the entire workspace. Users can use Go to Symbol in Workspace (Ctrl+T) to find procedures by name in any SSL file, not just open documents.

---

## 2. Capabilities

### 2.1 Search Behavior

| Aspect | Behavior |
|--------|----------|
| Query matching | Case-insensitive substring match |
| Symbol types | Procedures (Function kind for scripts, Method kind for class files) |
| Scope | All workspace files (`.srvscr`, `.ssl`, `.ssl.txt`, `.ds`, `.ds.txt`) |
| Priority | Open documents take precedence over indexed versions |
| Result cap | 500 symbols maximum |

### 2.2 Workspace Indexing

On initialization, the server:
1. Captures workspace root URIs from the client
2. Launches a background scan of all SSL files (bounded to 4 concurrent workers)
3. Registers file watchers for dynamic updates (`workspace/didChangeWatchedFiles`)

The index is maintained incrementally:
- File created/changed: re-indexed (unless currently open — open documents are always authoritative)
- File deleted: removed from index
- Document closed: re-indexed from disk to pick up saved changes

### 2.3 Response Format

```json
[
  {
    "name": "CalculateTotal",
    "kind": 12,
    "location": {
      "uri": "file:///path/to/file.srvscr",
      "range": { "start": { "line": 10, "character": 0 }, "end": { "line": 25, "character": 0 } }
    }
  }
]
```

### 2.4 Symbol Properties

| Property | Description |
|----------|-------------|
| `name` | Procedure name |
| `kind` | Function (12) for script files, Method (6) for class files |
| `location` | File URI and range |

---

## 3. Configuration

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| (None currently) | - | - | Workspace symbols has no specific configuration |

File extensions indexed: `.srvscr`, `.ssl`, `.ssl.txt`, `.ds`, `.ds.txt`

---

## 4. Edge Cases & Special Handling

### 4.1 Empty Query

An empty query returns all procedures from open documents and indexed files (up to 500 results).

### 4.2 No Workspace Root

If the client provides no `rootURI` or `workspaceFolders`, workspace indexing is skipped and only open documents are searched (original behavior).

### 4.3 Open Document Priority

When a file is both open and indexed, the open document version is used. The index entry is skipped to avoid duplicates.

### 4.4 Case Insensitivity

Query "calc" matches "Calculate", "CALCULATE", "CalculateTotal".

### 4.5 Class File Detection

Files starting with `:CLASS` have their procedures reported as Method (kind 6) rather than Function (kind 12).

### 4.6 Fuzzy Matching - NOT IMPLEMENTED

Currently uses substring matching. Fuzzy matching (e.g., "ct" matching "CalculateTotal") is not implemented.

---

## 5. Known Limitations

| Limitation | Notes |
|------------|-------|
| Procedures only | Variables, regions not included |
| No fuzzy matching | Substring match only |
| No cross-file go-to-definition | Index provides symbols but not namespace path resolution (planned) |
| No `:INCLUDE` resolution | Included symbols not surfaced (planned) |

---

## 6. Test Specifications

### 6.1 Basic Search

```ssl
/* File: helpers.srvscr (open);
:PROCEDURE CalculateTotal;
:ENDPROC;

:PROCEDURE CalculateAverage;
:ENDPROC;
```

```
/* Test: Search for "Calculate";
Query: "Calculate"
/* Expected:
   [
     { name: "CalculateTotal", kind: 12 },
     { name: "CalculateAverage", kind: 12 }
   ]
;
```

### 6.2 Case Insensitive Search

```
/* Test: Lowercase query matches;
Query: "calculate"
/* Expected: Same results as above;
```

### 6.3 Partial Match

```
/* Test: Substring match;
Query: "age"
/* Expected: Matches "CalculateAverage" (contains "age");
```

### 6.4 Empty Query

```
/* Test: Empty query returns all;
Query: ""
/* Expected: All procedures from open documents + indexed workspace files;
```

### 6.5 No Matches

```
/* Test: No matching procedures;
Query: "xyz123"
/* Expected: [] (empty array);
```

### 6.6 Indexed Files (Not Open)

```
/* File: Server Scripts/UTILS/HELPERS.srvscr (on disk, NOT open);
:PROCEDURE FormatDate;
:ENDPROC;
```

```
/* Test: Search finds procedures in files not currently open;
Query: "FormatDate"
/* Expected: [ { name: "FormatDate", kind: 12, location.uri: "file://.../HELPERS.srvscr" } ];
```

### 6.7 Open Document Takes Priority

```
/* File: test.srvscr is both open (with edits) and indexed;
/* Test: Open document version used, no duplicate;
Query: "TestProc"
/* Expected: Single result from the open document version;
```

### 6.8 Class File Methods

```
/* File: MyClass.srvscr (on disk);
:CLASS MyClass;
:PROCEDURE GetValue;
:ENDPROC;
```

```
/* Test: Class methods have Method kind;
Query: "GetValue"
/* Expected: [ { name: "GetValue", kind: 6 } ];
```

---

## 7. Implementation Notes

### 7.1 Architecture

The workspace index (`WorkspaceIndex`) is a separate data structure from the `DocumentManager`:
- `DocumentManager` holds open documents with full content, tokens, and AST
- `WorkspaceIndex` holds lightweight `IndexedProcedure` entries (name, parameters, line range) for all files on disk

The `handleWorkspaceSymbol` handler merges results from both sources, with open documents first.

### 7.2 File Watching

File watchers are registered dynamically via `client/registerCapability` during `handleInitialized`. Glob patterns cover all five SSL extensions.

### 7.3 Performance

- Background indexing: 4 concurrent workers, ~5-15 seconds for 10K files
- Symbol search: Linear scan over indexed procedures, microseconds at typical scale
- Result cap: 500 symbols maximum to prevent client overload
- Memory: ~50K procedures across 10K files well under 50MB
