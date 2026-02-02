# Document Symbols

**Status:** IMPLEMENTED  
**LSP Method:** `textDocument/documentSymbol`  
**Source Files:** `internal/providers/symbols.go`, `internal/server/handler.go`

---

## 1. Overview

The document symbols provider returns a hierarchical list of symbols in the document. These symbols appear in the editor's Outline view, breadcrumbs, and Go to Symbol dialog (Ctrl+Shift+O).

---

## 2. Capabilities

### 2.1 Symbol Types

| SSL Element | LSP SymbolKind | Value | Children |
|-------------|----------------|-------|----------|
| Procedure | Function | 12 | Parameters |
| Parameter | Variable | 13 | None |
| Public Variable | Variable | 13 | None |
| Region | Namespace | 3 | Contained symbols |

### 2.2 Symbol Properties

| Property | Description |
|----------|-------------|
| `name` | Symbol name (e.g., "MyProcedure") |
| `kind` | LSP SymbolKind |
| `range` | Full range of the symbol (start to end) |
| `selectionRange` | Range of the symbol name only |
| `children` | Nested symbols (parameters, contained procs) |

### 2.3 Hierarchical Structure

Symbols are returned in a tree structure:

```
Document
├── Region "Initialization"
│   └── Procedure "Setup"
│       ├── Parameter "sConfig"
│       └── Parameter "bDebug"
├── Procedure "Main"
│   └── Parameter "aArgs"
└── Public Variable "gCounter"
```

---

## 3. Configuration

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| (None currently) | - | - | Document symbols has no specific configuration |

---

## 4. Edge Cases & Special Handling

### 4.1 Regions

Regions are defined by comment markers:

```ssl
/* region MyRegion ;
:PROCEDURE Contained;
:ENDPROC;
/* endregion ;
```

Symbols within a region are nested under the region symbol.

### 4.2 Unclosed Regions

If a region is opened but not closed, it extends to the end of the file.

### 4.3 Overlapping Regions

Regions should not overlap. If they do, behavior is undefined.

### 4.4 Empty Procedures

Procedures with no parameters still appear in the symbol list.

### 4.5 Multiple Public Declarations

Each `:PUBLIC` declaration creates a separate symbol:

```ssl
:PUBLIC gVar1;
:PUBLIC gVar2, gVar3;  /* Creates two symbols */
```

---

## 5. Known Limitations

| Limitation | Notes |
|------------|-------|
| No class support | `:CLASS` blocks not recognized |
| No local variables | Only parameters shown, not `:DECLARE` |
| No constants | No special symbol type for constants |

---

## 6. Test Specifications

### 6.1 Procedure Symbols

```ssl
/* Test: Procedure appears in symbol list */
:PROCEDURE MyProcedure;
:PARAMETERS param1, param2;
:ENDPROC;
/* Expected symbols:
   [{
     name: "MyProcedure",
     kind: Function (12),
     range: lines 1-3,
     selectionRange: "MyProcedure" on line 1,
     children: [
       { name: "param1", kind: Variable (13) },
       { name: "param2", kind: Variable (13) }
     ]
   }]
*/
```

### 6.2 Public Variable Symbols

```ssl
/* Test: Public variables in symbol list */
:PUBLIC gCounter;
:PUBLIC gName, gVersion;

:PROCEDURE Test;
:ENDPROC;
/* Expected symbols:
   [
     { name: "gCounter", kind: Variable (13) },
     { name: "gName", kind: Variable (13) },
     { name: "gVersion", kind: Variable (13) },
     { name: "Test", kind: Function (12) }
   ]
*/
```

### 6.3 Region Symbols

```ssl
/* Test: Region contains nested symbols */
/* region Database Operations ;
:PROCEDURE GetData;
:ENDPROC;

:PROCEDURE SaveData;
:ENDPROC;
/* endregion ;

:PROCEDURE Main;
:ENDPROC;
/* Expected symbols:
   [
     {
       name: "Database Operations",
       kind: Namespace (3),
       children: [
         { name: "GetData", kind: Function (12) },
         { name: "SaveData", kind: Function (12) }
       ]
     },
     { name: "Main", kind: Function (12) }
   ]
*/
```

### 6.4 Procedure with No Parameters

```ssl
/* Test: Procedure without parameters */
:PROCEDURE SimpleProc;
x := 1;
:ENDPROC;
/* Expected symbols:
   [{
     name: "SimpleProc",
     kind: Function (12),
     children: []
   }]
*/
```

### 6.5 Selection Range Accuracy

```ssl
/* Test: Selection range covers only the name */
:PROCEDURE   VeryImportantProcedure  ;
:ENDPROC;
/* Expected:
   selectionRange should cover "VeryImportantProcedure" only,
   not the whitespace or semicolon
*/
```

### 6.6 Symbol Ordering

```ssl
/* Test: Symbols ordered by position in file */
:PROCEDURE SecondProc;
:ENDPROC;

:PROCEDURE FirstProc;
:ENDPROC;
/* Expected: SecondProc before FirstProc (file order, not alphabetical) */
```

---

## 7. Related Issues

| Issue | Description | Status |
|-------|-------------|--------|
| #12 | Document outliner not showing symbols | Fixed |

---

## 8. Implementation Notes

### 8.1 Algorithm

1. Parse document for procedure declarations
2. For each procedure, extract parameters from `:PARAMETERS` line
3. Parse for `:PUBLIC` declarations
4. Parse for region markers
5. Build hierarchical tree with regions as containers
6. Return tree to client

### 8.2 Range Calculation

- **Full range:** From `:PROCEDURE` to `:ENDPROC` (inclusive)
- **Selection range:** Just the procedure name portion

### 8.3 Performance

Symbol extraction should complete within 50ms. Results are cached until document changes.
