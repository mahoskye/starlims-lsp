# SSL Built-in Functions

This document summarizes SSL built-in functions. The LSP includes signatures for **367 built-in functions**.

**LSP Source:** `internal/constants/signatures.go`

---

## Function Categories

### String Functions

| Function | Description |
|----------|-------------|
| `Len(s)` | Get string length |
| `SubStr(s, start, len)` | Extract substring (1-based) |
| `Upper(s)` | Convert to uppercase |
| `Lower(s)` | Convert to lowercase |
| `AllTrim(s)` | Remove leading and trailing spaces |
| `Trim(s)` | Remove trailing spaces |
| `LTrim(s)` | Remove leading spaces |
| `Left(s, n)` | Get leftmost n characters |
| `Right(s, n)` | Get rightmost n characters |
| `StrTran(s, find, replace)` | Replace all occurrences |
| `At(needle, haystack)` | Find substring position (1-based, 0 if not found) |
| `LimsString(value)` | Convert any value to string |
| `Val(s)` | Convert string to number |
| `Chr(n)` | Get character by ASCII code |
| `Asc(s)` | Get ASCII code of first character |
| `Replicate(s, n)` | Repeat string n times |

### Numeric Functions

| Function | Description |
|----------|-------------|
| `Abs(n)` | Absolute value |
| `Round(n, decimals)` | Round to decimals |
| `Integer(n)` | Truncate to integer |
| `Max(a, b)` | Maximum of two values |
| `Min(a, b)` | Minimum of two values |
| `Sqrt(n)` | Square root |

### Date/Time Functions

| Function | Description |
|----------|-------------|
| `Today()` | Current date (no time) |
| `Now()` | Current date and time |
| `Year(d)` | Extract year |
| `Month(d)` | Extract month |
| `Day(d)` | Extract day |
| `Hour(d)` | Extract hour |
| `Minute(d)` | Extract minute |
| `Second(d)` | Extract second |
| `DOW(d)` | Day of week |
| `CToD(s)` | String to date |
| `DToS(d)` | Date to string (YYYYMMDD) |
| `DateAdd(d, n, part)` | Add interval to date |
| `DateDiff(d1, d2, part)` | Difference between dates |
| `DateToString(d, format)` | Format date |
| `DateFromString(s, format)` | Parse date from string |
| `LIMSDate(d, format)` | Format date (STARLIMS) |

### Array Functions

| Function | Description |
|----------|-------------|
| `Len(a)` | Array length |
| `aadd(a, item)` | Add item to array |
| `alen(a)` | Get array length |
| `ascan(a, item)` | Find item position (0 if not found) |
| `ascanexact(a, item)` | Find exact match position |
| `delarray(a, pos)` | Delete element at position |
| `ArrayNew(dim1, dim2, dim3)` | Create new array |
| `ArrayCalc(a, op, val)` | Perform calculation on array |
| `BuildArray(s, delim)` | Create array from delimited string |
| `BuildString(a, delim)` | Create string from array |
| `BuildStringForIn(a)` | Create SQL IN clause string |
| `SortArray(a)` | Sort array |
| `ExtractCol(a, col)` | Extract column from 2D array |

### Database Functions

| Function | Description |
|----------|-------------|
| `SQLExecute(sql, dsName)` | Execute SQL (supports `?varName?` params) |
| `RunSQL(sql, name, params)` | Execute DML (INSERT/UPDATE/DELETE) |
| `LSearch(sql, default, name, params)` | Single value lookup |
| `LSelect1(sql, name, params)` | Multi-row SELECT to 2D array |
| `GetDataSet(sql, params)` | Get XML dataset |
| `BeginLimsTransaction(name)` | Start transaction |
| `EndLimsTransaction(name, commit)` | End transaction |
| `LimsRecordsAffected()` | Get affected row count |
| `GetLastSQLError()` | Get last SQL error |

### Type & Validation Functions

| Function | Description |
|----------|-------------|
| `Empty(value)` | Check if empty/NIL/zero |
| `LimsType(value)` | Get type ("C","N","L","D","A","O","NIL") |
| `LimsTypeEx(value)` | Get full type name |
| `IsDefined(varName)` | Check if variable defined |
| `IsNumeric(value)` | Check if numeric |
| `ValidateNumeric(value)` | Validate numeric string |
| `Nothing(value)` | Check if NIL |

### Object Functions

| Function | Description |
|----------|-------------|
| `CreateUDObject(class)` | Create user-defined object |
| `CreateUDObject()` | Create anonymous object |
| `AddProperty(obj, name)` | Add property to object |
| `HasProperty(obj, name)` | Check if property exists |

### Procedure Functions

| Function | Description |
|----------|-------------|
| `DoProc(name, args)` | Call procedure in same file |
| `ExecFunction(path, args)` | Call procedure in different file |
| `PrmCount()` | Get parameter count |

### Error Handling Functions

| Function | Description |
|----------|-------------|
| `GetLastSSLError()` | Get last SSL error object |
| `RaiseError(msg)` | Raise custom error |
| `FormatErrorMessage(err)` | Format error for display |

### Message Functions

| Function | Description |
|----------|-------------|
| `UsrMes(msg, title)` | Display user message |
| `InfoMes(msg)` | Display info message |
| `ErrorMes(msg)` | Display error message |

### System Functions

| Function | Description |
|----------|-------------|
| `CreateGUID()` | Generate GUID |
| `GetSetting(name)` | Get system setting |
| `IIf(cond, true, false)` | Inline if |
| `lWait(seconds)` | Sleep/pause |
| `GetByName(varName)` | Get variable by name |
| `LKill(varName)` | Destroy variable |

### File Functions

| Function | Description |
|----------|-------------|
| `ReadText(path)` | Read text file |
| `WriteText(path, content)` | Write text file |
| `Directory(pattern)` | Directory listing |
| `FileSupport(id, request)` | File operations |

### Web/XML Functions

| Function | Description |
|----------|-------------|
| `ToXml(obj)` | Convert to XML |
| `FromXml(xml)` | Parse from XML |
| `ToJson(value)` | Convert to JSON |
| `FromJson(json)` | Parse from JSON |
| `HtmlEncode(s)` | HTML encode |
| `HtmlDecode(s)` | HTML decode |
| `UrlEncode(s)` | URL encode |
| `UrlDecode(s)` | URL decode |
| `GetFromSession(key)` | Get session value |
| `AddToSession(key, value)` | Set session value |

### .NET Integration

| Function | Description |
|----------|-------------|
| `LimsNETConnect(assembly, type)` | Connect to .NET assembly |
| `LimsNETCast(value, type)` | Cast to .NET type |
| `LimsNETTypeOf(type)` | Get .NET type |

---

## Top 30 Most-Used Functions

Based on production code analysis:

| Rank | Function | Usage Count |
|------|----------|-------------|
| 1 | `SQLExecute` | 32,822 |
| 2 | `Empty` | 14,372 |
| 3 | `DoProc` | 11,413 |
| 4 | `Len` | 11,058 |
| 5 | `LimsString` | 10,340 |
| 6 | `ExecFunction` | 8,914 |
| 7 | `UsrMes` | 8,638 |
| 8 | `Upper` | 8,119 |
| 9 | `aadd` | 5,804 |
| 10 | `Chr` | 4,634 |
| 11 | `AllTrim` | 4,486 |
| 12 | `RunSQL` | 2,873 |
| 13 | `SubStr` | 2,785 |
| 14 | `Now` | 2,750 |
| 15 | `GetSetting` | 2,720 |
| 16 | `Left` | 2,324 |
| 17 | `Val` | 2,233 |
| 18 | `LSearch` | 2,211 |
| 19 | `ExtractCol` | 2,130 |
| 20 | `At` | 2,002 |
| 21 | `Trim` | 1,958 |
| 22 | `Max` | 1,802 |
| 23 | `Time` | 1,705 |
| 24 | `IIf` | 1,661 |
| 25 | `StrTran` | 1,612 |
| 26 | `CreateUDObject` | 1,573 |
| 27 | `RaiseError` | 1,571 |
| 28 | `Today` | 1,412 |
| 29 | `GetDataSet` | 1,303 |
| 30 | `BuildString` | 1,280 |

---

## Function Signatures in LSP

The LSP provides function signatures with:
- Parameter names and types
- Optional parameter indicators
- Return type
- Description

Example hover/signature for `SQLExecute`:

```
SQLExecute(sCommandString, sFriendlyName?, bRollbackTransaction?, 
           bNullAsBlank?, aInvariantDateColumns?, sReturnType?, 
           sTableName?, bIncludeSchema?, bIncludeHeader?)

Universal database function. Supports ?varName? variable substitution.
Routes SELECT to array/XML, DML to RunSQL internally.

Parameters:
- sCommandString: The SQL query to execute
- sFriendlyName: Name for the resulting dataset (optional)
- ... additional optional parameters

Returns: Array, XML, Dataset, or Boolean depending on query type
```

---

## Function Casing

SSL functions are case-insensitive but should use documented casing:

### Lowercase (Array Functions)
```ssl
aadd(aArray, value);
alen(aArray);
ascan(aArray, value);
```

### PascalCase (Most Functions)
```ssl
AllTrim(sString);
SQLExecute(sSQL);
CreateUDObject("ClassName");
LimsString(nValue);  /* Note: NOT Str();
```

---

## Complete Reference

For the full list of 367+ functions with detailed signatures, see:

**LSP Source:** `internal/constants/signatures.go`
