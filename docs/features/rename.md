# Rename Support

The SSL Language Server provides rename functionality (`textDocument/rename`) for renaming symbols across the current document.

## Overview

Rename allows you to change the name of a variable or procedure and automatically update all references to it within the same file.

## Supported Symbols

| Symbol Type | Supported | Notes |
|-------------|-----------|-------|
| Variables (`:DECLARE`) | ✅ Yes | Scope-aware |
| Parameters (`:PARAMETERS`) | ✅ Yes | Scoped to procedure |
| Public variables (`:PUBLIC`) | ✅ Yes | File-wide |
| Procedures | ✅ Yes | File-wide |
| User-defined identifiers | ✅ Yes | |

## Not Renameable

The following symbols cannot be renamed:

| Symbol Type | Reason |
|-------------|--------|
| Keywords (`:IF`, `:WHILE`, etc.) | Language constructs |
| Built-in functions (`Len`, `Trim`, etc.) | 367 built-in functions |
| Built-in classes (`CDataTable`, etc.) | 30 built-in classes |
| Literals (`.T.`, `.F.`, `NIL`) | Language constants |
| Operators (`.AND.`, `.OR.`, `.NOT.`) | Language operators |
| `Me` keyword | Self-reference in classes |
| Content inside strings | String literals preserved |
| Content inside comments | Comments preserved |

## How It Works

### 1. Prepare Rename (Validation)

When you position your cursor on a symbol and invoke rename, the server first validates:

1. **Context Check**: Cursor must not be inside a string or comment
2. **Symbol Check**: The word must be a renameable symbol (not a keyword, built-in, etc.)
3. **Word Identification**: The exact range and placeholder name are determined

If validation fails, the rename is rejected before you can enter a new name.

### 2. Rename Execution

When you provide a new name:

1. **New Name Validation**: The new name must be:
   - A valid SSL identifier (starts with letter or `_`, contains only alphanumeric and `_`)
   - Not a keyword
   - Not a built-in function name
   
2. **Reference Finding**: All references are found using scope-aware search:
   - Local variables and parameters: Only within their declaring procedure
   - Public variables and procedures: Throughout the file
   
3. **Text Edits**: All occurrences are replaced with the new name

## Scope Awareness

Rename respects SSL scoping rules:

```ssl
:PROCEDURE Proc1;
:DECLARE sValue;        /* Rename here... */
sValue := "one";        /* ...updates this... */
:ENDPROC;

:PROCEDURE Proc2;
:DECLARE sValue;        /* ...but NOT this (different scope) */
sValue := "two";
:ENDPROC;
```

## Case Insensitivity

SSL is case-insensitive. Renaming finds all case variations:

```ssl
:DECLARE sName;
sname := "test";        /* Found */
SNAME := sname;         /* Both found */
```

All variations will be renamed to the exact casing you specify.

## Usage

### VS Code

1. Position cursor on a variable or procedure name
2. Press `F2` or right-click → "Rename Symbol"
3. Enter the new name
4. Press `Enter` to confirm

### Other Editors

Most LSP-compatible editors support rename via:
- Keyboard shortcut (often `F2`)
- Right-click context menu
- Command palette: "Rename Symbol"

## Examples

### Renaming a Variable

**Before:**
```ssl
:PROCEDURE ProcessData;
:DECLARE sOutput;
sOutput := Transform(input);
:RETURN sOutput;
:ENDPROC;
```

**After renaming `sOutput` to `sResult`:**
```ssl
:PROCEDURE ProcessData;
:DECLARE sResult;
sResult := Transform(input);
:RETURN sResult;
:ENDPROC;
```

### Renaming a Procedure

**Before:**
```ssl
:PROCEDURE CalcTotal;
:RETURN 100;
:ENDPROC;

:PROCEDURE Main;
x := CalcTotal();
:ENDPROC;
```

**After renaming `CalcTotal` to `CalculateTotal`:**
```ssl
:PROCEDURE CalculateTotal;
:RETURN 100;
:ENDPROC;

:PROCEDURE Main;
x := CalculateTotal();
:ENDPROC;
```

## Known Limitations

| Limitation | Description |
|------------|-------------|
| Single-file only | Cannot rename across multiple files |
| String content | May incorrectly rename matching text inside strings (uses text-based search) |
| DoProc/ExecFunction | String arguments in `DoProc("ProcName")` are not updated |
| No preview | Some editors show preview; depends on client support |

## Error Cases

### Invalid New Name

```
Cannot rename to 'IF' - reserved keyword
Cannot rename to 'Len' - built-in function name
Cannot rename to 'my-var' - invalid identifier (contains hyphen)
```

### Not Renameable

```
Cannot rename - cursor is on a built-in function
Cannot rename - cursor is on a keyword
Cannot rename - cursor is inside a string
```

## Related Features

- [Find References](./references.md) - Find all occurrences without renaming
- [Go to Definition](./definition.md) - Navigate to symbol definition
- [Document Symbols](./document-symbols.md) - See all symbols in document

## Test Coverage

The rename feature includes 24 unit tests covering:
- PrepareRename validation (7 tests)
- Rename execution (7 tests)
- Identifier validation (2 tests)
- Symbol renameability checks (4 tests)
- Word range detection (3 tests)
- End-to-end integration (4 tests)
