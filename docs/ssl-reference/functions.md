# SSL Built-in Functions

This document summarizes SSL built-in functions. For the complete function reference, see:

**Primary Source:** [`ssl-style-guide/reference/ssl-unified-master-source-complete.json`](../../ssl-style-guide/reference/ssl-unified-master-source-complete.json)

The LSP includes signatures for **367 built-in functions** in:  
**LSP Source:** `internal/constants/signatures.go`

---

## Function Categories

### String Functions

| Function | Description |
|----------|-------------|
| `Len(s)` | Get string length |
| `SubStr(s, start, len)` | Extract substring |
| `Upper(s)` | Convert to uppercase |
| `Lower(s)` | Convert to lowercase |
| `Trim(s)` | Remove leading/trailing whitespace |
| `LTrim(s)` | Remove leading whitespace |
| `RTrim(s)` | Remove trailing whitespace |
| `Left(s, n)` | Get leftmost n characters |
| `Right(s, n)` | Get rightmost n characters |
| `PadL(s, n, c)` | Pad left to length n |
| `PadR(s, n, c)` | Pad right to length n |
| `StrTran(s, find, replace)` | Replace occurrences |
| `At(needle, haystack)` | Find substring position |
| `InStr(start, haystack, needle)` | Find substring from position |
| `Val(s)` | Convert string to number |
| `Str(n)` | Convert number to string |
| `Chr(n)` | Get character by ASCII code |
| `Asc(s)` | Get ASCII code of first character |

### Numeric Functions

| Function | Description |
|----------|-------------|
| `Abs(n)` | Absolute value |
| `Round(n, decimals)` | Round to decimals |
| `Int(n)` | Integer part (truncate) |
| `Mod(n, divisor)` | Modulo/remainder |
| `Max(a, b)` | Maximum of two values |
| `Min(a, b)` | Minimum of two values |
| `Sqrt(n)` | Square root |
| `Log(n)` | Natural logarithm |
| `Exp(n)` | e raised to power n |

### Date/Time Functions

| Function | Description |
|----------|-------------|
| `Date()` | Current date |
| `Time()` | Current time string |
| `DateTime()` | Current date and time |
| `Year(d)` | Extract year |
| `Month(d)` | Extract month |
| `Day(d)` | Extract day |
| `DoW(d)` | Day of week |
| `CToD(s)` | String to date |
| `DToC(d)` | Date to string |
| `DateDiff(d1, d2, unit)` | Difference between dates |

### Array Functions

| Function | Description |
|----------|-------------|
| `Len(a)` | Array length |
| `AAdd(a, item)` | Add item to array |
| `AIns(a, pos)` | Insert element |
| `ADel(a, pos)` | Delete element |
| `ASort(a)` | Sort array |
| `AScan(a, item)` | Find item in array |
| `ACopy(source, dest)` | Copy array |
| `ASize(a, newSize)` | Resize array |
| `ATail(a)` | Get last element |

### Database Functions

| Function | Description |
|----------|-------------|
| `SQLExecute(sql, dsName)` | Execute SQL, return dataset |
| `GetDataSet(sql, dsName)` | Get dataset from query |
| `RunSQL(sql, params)` | Execute SQL with parameters |
| `DbSeek(key)` | Seek to record |
| `DbGo(recNo)` | Go to record number |
| `DbSkip(n)` | Skip n records |
| `Eof()` | At end of file |
| `Bof()` | At beginning of file |
| `RecCount()` | Number of records |

### Object Functions

| Function | Description |
|----------|-------------|
| `CreateUDObject(class)` | Create user-defined object |
| `UsrMes(msg)` | Display user message |
| `InfoMes(msg)` | Display info message |
| `ErrMes(msg)` | Display error message |
| `Type(expr)` | Get type of expression |
| `Empty(expr)` | Check if empty |
| `ValType(expr)` | Get value type |

### Procedure Functions

| Function | Description |
|----------|-------------|
| `DoProc(name, args)` | Call procedure by name |
| `ExecFunction(path, args)` | Execute function by path |
| `ProcName()` | Current procedure name |
| `ProcLine()` | Current line number |

### File Functions

| Function | Description |
|----------|-------------|
| `File(path)` | Check if file exists |
| `FOpen(path, mode)` | Open file |
| `FClose(handle)` | Close file |
| `FRead(handle, buffer, bytes)` | Read from file |
| `FWrite(handle, data)` | Write to file |
| `FErase(path)` | Delete file |
| `FCopy(src, dest)` | Copy file |

---

## Function Signatures in LSP

The LSP provides function signatures with:
- Parameter names and types
- Optional parameter indicators
- Return type
- Description

Example hover/signature for `SQLExecute`:

```
function SQLExecute(cSQL: String, cDSName: String): Dataset

Executes a SQL statement and returns a dataset.

Parameters:
- cSQL: The SQL query to execute
- cDSName: Name for the resulting dataset

Returns: Dataset object containing query results
```

---

## Complete Reference

For the full list of 367+ functions with signatures:
- **JSON:** [`ssl-style-guide/reference/ssl-unified-master-source-complete.json`](../../ssl-style-guide/reference/ssl-unified-master-source-complete.json)
- **LSP:** `internal/constants/signatures.go`
