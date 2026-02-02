# SSL Language Server - Configuration Reference

This document specifies all configuration options available in the starlims-lsp language server. It serves as the authoritative reference for client configuration.

**Version:** 1.0  
**Last Updated:** 2025-02-02  
**Status:** Draft

---

## Table of Contents

1. [Overview](#1-overview)
2. [Configuration Structure](#2-configuration-structure)
3. [Formatting Options](#3-formatting-options)
4. [SQL Formatting Options](#4-sql-formatting-options)
5. [Diagnostic Options](#5-diagnostic-options)
6. [Internal Options](#6-internal-options)
7. [Configuration Examples](#7-configuration-examples)
8. [VS Code Integration](#8-vs-code-integration)

---

## 1. Overview

### 1.1 Configuration Delivery

Configuration is sent to the language server via the LSP `workspace/didChangeConfiguration` notification.

### 1.2 Configuration Namespace

All SSL Language Server options are nested under the `ssl` namespace:

```json
{
  "ssl": {
    "format": { ... },
    "diagnostics": { ... }
  }
}
```

### 1.3 When Configuration Applies

- **Format options:** Applied immediately to next format request
- **Diagnostic options:** Trigger re-analysis of all open documents

---

## 2. Configuration Structure

### 2.1 Complete Schema

```typescript
interface SSLConfiguration {
  ssl: {
    format: FormattingOptions;
    diagnostics: DiagnosticOptions;
  }
}

interface FormattingOptions {
  indentStyle: "tab" | "space";
  indentSize: number;
  maxLineLength: number;
  operatorSpacing: boolean;
  commaSpacing: boolean;
  semicolonEnforcement: boolean;
  blankLinesBetweenProcs: number;
  sql: SQLFormattingOptions;
}

interface SQLFormattingOptions {
  enabled: boolean;
  style: "standard" | "canonicalCompact" | "compact" | "expanded";
  keywordCase: "upper" | "lower" | "preserve";
  indentSize: number;
  maxLineLength: number;
}

interface DiagnosticOptions {
  hungarianNotation: boolean;
  hungarianPrefixes: string[];
  globals: string[];
}
```

---

## 3. Formatting Options

### 3.1 ssl.format.indentStyle

| Property | Value |
|----------|-------|
| **Type** | `string` |
| **Default** | `"tab"` |
| **Values** | `"tab"`, `"space"` |
| **File** | `internal/providers/formatting.go:13,26` |

Specifies whether to use tabs or spaces for indentation.

```json
{ "ssl.format.indentStyle": "tab" }
```

### 3.2 ssl.format.indentSize

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `4` |
| **Range** | `1` - `8` |
| **File** | `internal/providers/formatting.go:14,27` |

Number of spaces per indentation level. Only applies when `indentStyle` is `"space"`.

```json
{ "ssl.format.indentSize": 4 }
```

### 3.3 ssl.format.maxLineLength

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `90` |
| **Range** | `0` (unlimited) - `200` |
| **File** | `internal/providers/formatting.go:15,28` |

Maximum line length before wrapping. Set to `0` to disable line length enforcement.

```json
{ "ssl.format.maxLineLength": 90 }
```

### 3.4 ssl.format.operatorSpacing

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `true` |
| **File** | `internal/providers/formatting.go:16,29` |

When enabled, adds spaces around operators (`=`, `<>`, `+`, `-`, `*`, `/`, `:=`, etc.).

**Before formatting (operatorSpacing: false):**
```ssl
x:=a+b*c;
```

**After formatting (operatorSpacing: true):**
```ssl
x := a + b * c;
```

### 3.5 ssl.format.commaSpacing

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `true` |
| **File** | `internal/providers/formatting.go:17,30` |

When enabled, ensures a space after each comma in parameter lists.

**Before formatting (commaSpacing: false):**
```ssl
CallProc(a,b,c);
```

**After formatting (commaSpacing: true):**
```ssl
CallProc(a, b, c);
```

### 3.6 ssl.format.semicolonEnforcement

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `true` |
| **File** | `internal/providers/formatting.go:18,31` |

When enabled, ensures statements end with semicolons.

**Note:** Does NOT add semicolons after:
- Opening delimiters (`(`, `[`, `{`)
- Incomplete expressions
- Continuation keywords (`:TO`, `:STEP`)

### 3.7 ssl.format.blankLinesBetweenProcs

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `1` |
| **Range** | `0` - `3` |
| **File** | `internal/providers/formatting.go:19,32` |

Number of blank lines to insert between procedure definitions.

```json
{ "ssl.format.blankLinesBetweenProcs": 1 }
```

---

## 4. SQL Formatting Options

### 4.1 sql.format.sql.enabled

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `true` |
| **File** | `internal/providers/sql_formatter.go:10,27` |

When enabled, SQL strings passed to SQL functions are automatically formatted.

**SQL Functions Detected:**
- `SQLExecute`
- `GetDataSet`
- `GetDataSetWithSchemaFromSelect`
- `GetDataSetXMLFromSelect`
- `GetNetDataSet`
- `RunSQL`
- `LSearch`
- `LSelect`
- `LSelect1`
- `LSelectC`
- `GetDataSetEx`

### 4.2 ssl.format.sql.style

| Property | Value |
|----------|-------|
| **Type** | `string` |
| **Default** | `"standard"` |
| **Values** | `"standard"`, `"canonicalCompact"`, `"compact"`, `"expanded"` |
| **File** | `internal/providers/sql_formatter.go:11,28` |

SQL formatting style to apply.

#### Style Comparison

| Feature | standard | canonicalCompact | compact | expanded |
|---------|----------|------------------|---------|----------|
| Clause line breaks | Yes | Yes | No | Yes |
| AND/OR indentation | No | Yes | No | Yes |
| ON clause indentation | No | Yes | No | Yes |
| Smart column wrapping | No | Yes | No | Yes |
| Multi-column SELECT wrap | No | Yes | No | Always |

#### Style Examples

**Original SQL:**
```sql
SELECT id, name, email, phone, address FROM users INNER JOIN orders ON users.id = orders.user_id WHERE active = 1 AND status = 'open'
```

**standard:**
```sql
SELECT id, name, email, phone, address
FROM users
INNER JOIN orders
ON users.id = orders.user_id
WHERE active = 1 AND status = 'open'
```

**canonicalCompact:**
```sql
SELECT id, name, email, phone, address
FROM users
INNER JOIN orders
    ON users.id = orders.user_id
WHERE active = 1
    AND status = 'open'
```

**compact:**
```sql
SELECT id, name, email, phone, address FROM users INNER JOIN orders ON users.id = orders.user_id WHERE active = 1 AND status = 'open'
```

**expanded:**
```sql
SELECT
    id,
    name,
    email,
    phone,
    address
FROM users
INNER JOIN orders
    ON users.id = orders.user_id
WHERE active = 1
    AND status = 'open'
```

### 4.3 ssl.format.sql.keywordCase

| Property | Value |
|----------|-------|
| **Type** | `string` |
| **Default** | `"upper"` |
| **Values** | `"upper"`, `"lower"`, `"preserve"` |
| **File** | `internal/providers/sql_formatter.go:12,29` |

Case transformation for SQL keywords.

| Value | Example |
|-------|---------|
| `"upper"` | `SELECT`, `FROM`, `WHERE` |
| `"lower"` | `select`, `from`, `where` |
| `"preserve"` | Keeps original case |

### 4.4 ssl.format.sql.indentSize

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `4` |
| **Range** | `1` - `8` |
| **File** | `internal/providers/sql_formatter.go:13,30` |

Number of spaces per indentation level within SQL statements.

### 4.5 ssl.format.sql.maxLineLength

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `90` |
| **Range** | `0` (unlimited) - `200` |
| **File** | `internal/providers/sql_formatter.go:14,31` |

Maximum line length for SQL before wrapping. Used with `canonicalCompact` and `expanded` styles.

---

## 5. Diagnostic Options

### 5.1 ssl.diagnostics.hungarianNotation

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `false` |
| **File** | `internal/providers/diagnostics.go:49,62` |

When enabled, warns about variables using Hungarian notation prefixes.

```json
{ "ssl.diagnostics.hungarianNotation": true }
```

**Example Warning:**
```ssl
:DECLARE sName;  /* Warning: Hungarian notation prefix 's' detected in 'sName' */
```

### 5.2 ssl.diagnostics.hungarianPrefixes

| Property | Value |
|----------|-------|
| **Type** | `string[]` |
| **Default** | `["a", "b", "d", "n", "o", "s"]` |
| **File** | `internal/providers/diagnostics.go:50,63` |

List of single-character prefixes to detect as Hungarian notation.

| Prefix | STARLIMS Convention |
|--------|---------------------|
| `a` | Array |
| `b` | Boolean |
| `d` | Date |
| `n` | Numeric |
| `o` | Object |
| `s` | String |

**Detection Logic:**
1. Strip leading underscores
2. Check if name starts with a prefix from this list
3. Check if next character is uppercase
4. If both conditions met, flag as Hungarian notation

```json
{ "ssl.diagnostics.hungarianPrefixes": ["s", "n", "b"] }
```

### 5.3 ssl.diagnostics.globals

| Property | Value |
|----------|-------|
| **Type** | `string[]` |
| **Default** | `[]` |
| **File** | `internal/providers/diagnostics.go:51` |

List of global variable names. Assignment to these variables triggers an error.

```json
{
  "ssl.diagnostics.globals": [
    "gCurrentUser",
    "gAppName",
    "gLimsDate",
    "gDepartment"
  ]
}
```

**Behavior:**
- Variables in this list are treated as read-only
- Attempting to assign to them triggers an error
- Case-insensitive matching

**Example Error:**
```ssl
gCurrentUser := "test";  /* Error: Cannot assign to global variable 'gCurrentUser' */
```

**Future Enhancement:** These globals will also be recognized as "pre-declared" when undeclared variable checking is implemented.

---

## 6. Internal Options

These options are hardcoded and cannot be changed via client configuration.

### 6.1 CheckUnclosedBlocks

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Value** | `true` (always on) |
| **File** | `internal/providers/diagnostics.go:45,57` |

Always checks for unclosed block statements (`:IF` without `:ENDIF`, etc.).

### 6.2 CheckUnmatchedParens

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Value** | `true` (always on) |
| **File** | `internal/providers/diagnostics.go:46,58` |

Always checks for unmatched parentheses, brackets, and braces.

### 6.3 CheckUndeclaredVars

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Value** | `false` |
| **Status** | PLANNED |
| **File** | `internal/providers/diagnostics.go:47,59` |

Will check for usage of undeclared variables when implemented.

### 6.4 CheckUnusedVars

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Value** | `false` |
| **Status** | PLANNED |
| **File** | `internal/providers/diagnostics.go:48,60` |

Will check for declared but unused variables when implemented.

### 6.5 MaxBlockDepth

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Value** | `10` |
| **File** | `internal/providers/diagnostics.go:52,64` |

Maximum allowed block nesting depth. Exceeding this triggers a warning.

### 6.6 MaxNumberOfProblems

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Value** | `100` |
| **File** | `internal/server/server.go:66,74` |

Maximum number of diagnostics to report per document.

---

## 7. Configuration Examples

### 7.1 Minimal Configuration

```json
{
  "ssl": {}
}
```

Uses all defaults.

### 7.2 STARLIMS Style Guide Configuration

```json
{
  "ssl": {
    "format": {
      "indentStyle": "tab",
      "indentSize": 4,
      "maxLineLength": 90,
      "operatorSpacing": true,
      "commaSpacing": true,
      "semicolonEnforcement": true,
      "blankLinesBetweenProcs": 1,
      "sql": {
        "enabled": true,
        "style": "standard",
        "keywordCase": "upper",
        "indentSize": 4,
        "maxLineLength": 90
      }
    },
    "diagnostics": {
      "hungarianNotation": true,
      "hungarianPrefixes": ["a", "b", "d", "n", "o", "s"],
      "globals": []
    }
  }
}
```

### 7.3 Production Environment Configuration

```json
{
  "ssl": {
    "format": {
      "indentStyle": "space",
      "indentSize": 2,
      "maxLineLength": 120,
      "operatorSpacing": true,
      "commaSpacing": true,
      "semicolonEnforcement": true,
      "blankLinesBetweenProcs": 2,
      "sql": {
        "enabled": true,
        "style": "canonicalCompact",
        "keywordCase": "upper",
        "indentSize": 4,
        "maxLineLength": 100
      }
    },
    "diagnostics": {
      "hungarianNotation": false,
      "hungarianPrefixes": [],
      "globals": [
        "gCurrentUser",
        "gAppName", 
        "gLimsDate",
        "gDepartment",
        "gSiteCode",
        "gLanguage"
      ]
    }
  }
}
```

### 7.4 Compact SQL Configuration

```json
{
  "ssl": {
    "format": {
      "sql": {
        "enabled": true,
        "style": "compact",
        "keywordCase": "lower"
      }
    }
  }
}
```

### 7.5 Disable SQL Formatting

```json
{
  "ssl": {
    "format": {
      "sql": {
        "enabled": false
      }
    }
  }
}
```

---

## 8. VS Code Integration

### 8.1 settings.json Location

- **User:** `~/.config/Code/User/settings.json` (Linux/Mac) or `%APPDATA%\Code\User\settings.json` (Windows)
- **Workspace:** `.vscode/settings.json`

### 8.2 VS Code Settings Example

```json
{
  "ssl.format.indentStyle": "tab",
  "ssl.format.indentSize": 4,
  "ssl.format.maxLineLength": 90,
  "ssl.format.operatorSpacing": true,
  "ssl.format.commaSpacing": true,
  "ssl.format.semicolonEnforcement": true,
  "ssl.format.blankLinesBetweenProcs": 1,
  "ssl.format.sql.enabled": true,
  "ssl.format.sql.style": "standard",
  "ssl.format.sql.keywordCase": "upper",
  "ssl.format.sql.indentSize": 4,
  "ssl.format.sql.maxLineLength": 90,
  "ssl.diagnostics.hungarianNotation": false,
  "ssl.diagnostics.hungarianPrefixes": ["a", "b", "d", "n", "o", "s"],
  "ssl.diagnostics.globals": ["gCurrentUser", "gAppName"]
}
```

### 8.3 Configuration via Extension

The VS Code extension (`vs-code-ssl-formatter`) automatically sends configuration changes to the LSP. Settings changed in VS Code are immediately applied.

---

## Appendix A: Default Values Summary

| Option | Default |
|--------|---------|
| `ssl.format.indentStyle` | `"tab"` |
| `ssl.format.indentSize` | `4` |
| `ssl.format.maxLineLength` | `90` |
| `ssl.format.operatorSpacing` | `true` |
| `ssl.format.commaSpacing` | `true` |
| `ssl.format.semicolonEnforcement` | `true` |
| `ssl.format.blankLinesBetweenProcs` | `1` |
| `ssl.format.sql.enabled` | `true` |
| `ssl.format.sql.style` | `"standard"` |
| `ssl.format.sql.keywordCase` | `"upper"` |
| `ssl.format.sql.indentSize` | `4` |
| `ssl.format.sql.maxLineLength` | `90` |
| `ssl.diagnostics.hungarianNotation` | `false` |
| `ssl.diagnostics.hungarianPrefixes` | `["a","b","d","n","o","s"]` |
| `ssl.diagnostics.globals` | `[]` |

## Appendix B: Option Types

| Type | JSON Example |
|------|--------------|
| `string` | `"value"` |
| `number` | `4` |
| `boolean` | `true` or `false` |
| `string[]` | `["a", "b", "c"]` |
