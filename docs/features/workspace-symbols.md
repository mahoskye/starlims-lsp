# Workspace Symbols

**Status:** PARTIAL  
**LSP Method:** `workspace/symbol`  
**Source Files:** `internal/providers/symbols.go`, `internal/server/handler.go`

---

## 1. Overview

The workspace symbols provider enables searching for symbols across the workspace. Users can use Go to Symbol in Workspace (Ctrl+T) to find procedures by name.

---

## 2. Capabilities

### 2.1 Search Behavior

| Aspect | Behavior |
|--------|----------|
| Query matching | Case-insensitive substring match |
| Symbol types | Procedures only |
| Scope | Open documents only |

### 2.2 Response Format

```json
[
  {
    "name": "CalculateTotal",
    "kind": 12,
    "location": {
      "uri": "file:///path/to/file.ssl",
      "range": { "start": { "line": 10, "character": 0 }, "end": { "line": 10, "character": 20 } }
    },
    "containerName": "file.ssl"
  }
]
```

### 2.3 Symbol Properties

| Property | Description |
|----------|-------------|
| `name` | Procedure name |
| `kind` | Function (12) |
| `location` | File URI and range |
| `containerName` | Filename for context |

---

## 3. Configuration

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| (None currently) | - | - | Workspace symbols has no specific configuration |

---

## 4. Edge Cases & Special Handling

### 4.1 Empty Query

An empty query returns all procedures from open documents.

### 4.2 No Open Documents

If no documents are open, returns empty array.

### 4.3 Case Insensitivity

Query "calc" matches "Calculate", "CALCULATE", "CalculateTotal".

### 4.4 Fuzzy Matching - NOT IMPLEMENTED

Currently uses substring matching. Fuzzy matching (e.g., "ct" matching "CalculateTotal") is not implemented.

---

## 5. Known Limitations

| Limitation | Notes |
|------------|-------|
| Open documents only | Does not scan workspace files |
| No indexing | No background file scanning |
| Procedures only | Variables, regions not included |
| No fuzzy matching | Substring match only |

---

## 6. Test Specifications

### 6.1 Basic Search

```ssl
/* File: helpers.ssl (open);
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
     { name: "CalculateTotal", kind: 12, containerName: "helpers.ssl" },
     { name: "CalculateAverage", kind: 12, containerName: "helpers.ssl" }
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
/* Expected: All procedures from all open documents;
```

### 6.5 No Matches

```
/* Test: No matching procedures;
Query: "xyz123"
/* Expected: [] (empty array);
```

### 6.6 Multiple Open Documents

```
/* File: file1.ssl (open);
:PROCEDURE ProcA;
:ENDPROC;

/* File: file2.ssl (open);
:PROCEDURE ProcB;
:ENDPROC;
```

```
/* Test: Search across open documents;
Query: "Proc"
/* Expected:
   [
     { name: "ProcA", containerName: "file1.ssl" },
     { name: "ProcB", containerName: "file2.ssl" }
   ]
;
```

---

## 7. Related Issues

| Issue | Description | Status |
|-------|-------------|--------|
| (None) | Workspace indexing planned for v2.0 | Future |

---

## 8. Implementation Notes

### 8.1 Current Implementation

1. Iterate over all open documents
2. Get document symbols for each
3. Filter procedures matching query
4. Return combined results

### 8.2 Future: Workspace Indexing

For v2.0, workspace symbols should:
1. Scan all SSL files in workspace on startup
2. Build symbol index in memory
3. Watch for file changes and update index
4. Return results from index (much faster)

### 8.3 Performance

Current implementation is O(n × m) where n = open documents and m = procedures per document. Acceptable for small numbers but won't scale without indexing.
