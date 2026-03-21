# SSL Built-in Functions

This document summarizes SSL built-in functions. The LSP exposes a source-aligned inventory of **354 developer-facing built-in functions**.

**Primary Sources:** `dev/ssl-style-guide/README.md`, `dev/ssl-style-guide/agent-guides/ssl_agent_instructions.md`, `internal/constants/source_alignment.go`, `internal/constants/signatures.go`

Function names are case-insensitive at runtime, but the LSP presents the canonical source-aligned casing used by the style-guide materials.

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
| `AAdd(a, item)` | Add item to array |
| `ALen(a)` | Get array length |
| `AScan(a, item)` | Find item position (returns 1-based index, or 0 if not found) |
| `AScanExact(a, item)` | Find exact match position (returns 1-based index, or 0 if not found) |
| `DelArray(a, pos)` | Delete element at position |
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
| `LSelect(sql, name, params)` | Multi-row SELECT to 2D array |
| `LSelect1(sql, name, params)` | Multi-row SELECT to 2D array (behaves like `LSelect` when field list is omitted) |
| `LSelectC(sql, name, params)` | Multi-row SELECT (delegates to `LSelect`) |
| `GetDataSet(sql, params)` | Get XML dataset (positional `?` params) |
| `GetDataSetEx(sql, params)` | Get XML dataset (extended) |
| `GetDataSetWithSchemaFromSelect(sql, params)` | Get XML dataset with schema |
| `GetDataSetXMLFromSelect(sql, params)` | Get XML dataset from SELECT |
| `GetNETDataSet(sql, dsName, params, table, xml, schema)` | Get dataset/object output with positional parameters |
| `GetSSLDataset(sql)` | Get `SSLDataset` object |
| `GetTables(sql, params)` | Get tables result |
| `XmlExportSql(sql, params)` | Export SQL result as XML |
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
| `CreateUdObject(class)` | Create user-defined `:CLASS` instance (not a built-in class) |
| `CreateUdObject()` | Create empty dynamic object (`SSLExpando`) |
| `CreateUdObject({{"Prop", value}})` | Create anonymous object with named properties |
| `AddProperty(obj, name)` | Add property to object |
| `HasProperty(obj, name)` | Check if property exists |

### Procedure Functions

| Function | Description |
|----------|-------------|
| `DoProc(name, args)` | Call procedure by name in the current file/context |
| `ExecFunction(path, args)` | Call procedure by exported path/name |
| `PrmCount()` | Get parameter count |

When there are no arguments, prefer `DoProc("Name")` over `DoProc("Name", {})`, and likewise for `ExecFunction`.
Inside class methods, prefer `Me:MethodName()` / `Base:MethodName()` over `DoProc(...)` for sibling and inherited methods.

Only `SQLExecute` supports named `?varName?` substitution. `RunSQL`, `LSearch`, `LSelect`, `LSelect1`, `LSelectC`, `GetDataSet`, `XmlExportSql`, `GetTables`, and related dataset helpers use positional `?` placeholders with explicit value arrays.
`SQLExecute` also supports array expansion (`?aValues?`), object-property access (`?oUser:ID?`), and parameterless function calls such as `?Today()?`.
Built-in classes such as `Email`, `SSLDataset`, and `SSLRegex` must use curly-brace construction (`Email{}`), not `CreateUdObject("Email")`.

### Error Handling Functions

| Function | Description |
|----------|-------------|
| `GetLastSSLError()` | Get last SSL error object |
| `RaiseError(msg)` | Raise custom error |
| `FormatErrorMessage(err)` | Format error for display |

### Message Functions

| Function | Description |
|----------|-------------|
| `UsrMes(msg, details)` | Write a server log message unless UsrMes logging is disabled |
| `InfoMes(msg, details)` | Same logging behavior as `UsrMes` |
| `ErrorMes(msg, details)` | Always write an error log message, even when `UsrMes` logging is disabled |

### System Functions

| Function | Description |
|----------|-------------|
| `CreateGUID()` | Generate GUID |
| `GetSetting(name)` | Get system setting |
| `IIf(cond, true, false)` | Inline if |
| `LWait(seconds)` | Sleep/pause |
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
| 9 | `AAdd` | 5,804 |
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
| 26 | `CreateUdObject` | 1,573 |
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
SQLExecute(vCommandString, [vFriendlyName], [vRollbackExistingTransaction],
           [vNullAsBlank], [vInvariantDateColumns], [vReturnType],
           [sTableName], [vIncludeSchema], [vIncludeHeader]) → variant

Universal database function. Supports ?varName? variable substitution.
Routes SELECT to array/XML, DML to RunSQL internally.

Parameters:
- vCommandString: The SQL query or command to execute
- vFriendlyName: Friendly/logging name (optional)
- ... additional optional parameters

Returns: Variant result routed by SQLExecute
```

---

## Function Casing

SSL functions are case-insensitive but should use documented casing:

### Source-Aligned Canonical Spellings
```ssl
AAdd(aArray, value);
ALen(aArray);
AScan(aArray, value);
```

### PascalCase (Most Functions)
```ssl
AllTrim(sString);
SQLExecute(sSQL);
CreateUdObject("ClassName");
LimsString(nValue);
Str(nValue, 6, 2);   /* Numeric formatting with width/decimals;
```

---

## Complete Reference

For the full list of 354 source-aligned built-in functions with detailed signatures, see:

**LSP Sources:** `internal/constants/source_alignment.go`, `internal/constants/signatures.go`
