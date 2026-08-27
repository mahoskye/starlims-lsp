# SSL Language Server - Configuration Reference

This document specifies all configuration options available in the starlims-lsp language server. It serves as the authoritative reference for client configuration.

**Version:** 1.3  
**Last Updated:** 2026-05-06  
**Status:** Current

---

## Table of Contents

1. [Overview](#1-overview)
2. [Configuration Structure](#2-configuration-structure)
3. [Formatting Options](#3-formatting-options)
4. [SQL Formatting Options](#4-sql-formatting-options)
5. [Diagnostic Options](#5-diagnostic-options)
6. [Inlay Hint Options](#6-inlay-hint-options)
7. [IntelliSense Settings](#7-intellisense-settings)
8. [Internal Options](#8-internal-options)
9. [Configuration Examples](#9-configuration-examples)
10. [VS Code Integration](#10-vs-code-integration)

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
    "diagnostics": { ... },
    "inlayHints": { ... },
    "intellisense": { ... }
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
    inlayHints: InlayHintOptions;
    intellisense: IntelliSenseOptions;
  }
}

interface IntelliSenseOptions {
  signatureHelp: {
    /**
     * When true, the server advertises '(' and ',' as signature-help
     * trigger characters so the popup opens while typing. When false
     * (default), signature help is available only on hover and explicit
     * invocation (Ctrl+Shift+Space). See issue #9.
     */
    autoTrigger: boolean;
  };
}

interface FormattingOptions {
  indentStyle: "tab" | "space";
  indentSize: number;
  maxLineLength: number;
  operatorSpacing: boolean;
  commaSpacing: boolean;
  semicolonEnforcement: boolean;
  blankLinesBetweenProcs: number;
  blankLineBetweenBlocks: boolean;
  trimTrailingWhitespace: boolean;
  maxConsecutiveBlankLines: number;
  builtinFunctionCase: "preserve" | "PascalCase";
  sql: SQLFormattingOptions;
}

interface SQLFormattingOptions {
  enabled: boolean;
  style: "standard" | "canonicalCompact" | "compact" | "expanded";
  keywordCase: "upper" | "lower" | "preserve";
  indentSize: number;
  maxLineLength: number;
  detectSQLStrings: boolean;
}

interface DiagnosticOptions {
  hungarianNotation: boolean;
  unusedVariables: boolean;
  unicodeLiteralPrefix: boolean;
  collateJustification: boolean;
  hungarianPrefixes: string[];
  globals: string[];
  maxBlockDepth: number;
  rules: { [slug: string]: "off" | "info" | "warn" | "warning" | "error" };
  endpointPatterns: string[];
}

interface InlayHintOptions {
  enabled: boolean;
  minParameterCount: number;
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

Number of spaces per indentation level. Only applies when `indentStyle` is `"space"`. The source guide's preferred default is tab indentation; the bundled value `4` is the fallback width for space-indented formatting, not the width of a tab-indented SSL block.

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
DoProc("CallProc",{a,b,c});
```

**After formatting (commaSpacing: true):**
```ssl
DoProc("CallProc", {a, b, c});
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

### 3.8 ssl.format.blankLineBetweenBlocks

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `true` |
| **File** | `internal/providers/formatting.go` |

Insert one blank line between sibling control-flow blocks (`:IF`, `:WHILE`,
`:FOR`, `:BEGINCASE`, `:TRY`) at the same indent level, so consecutive
blocks read as distinct units. See catalog entry
`fmt.blank_line_between_blocks` (added in v0.7.6, issue #15).

```json
{ "ssl.format.blankLineBetweenBlocks": true }
```

### 3.9 ssl.format.trimTrailingWhitespace

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `true` |
| **File** | `internal/providers/formatting.go` |

Remove trailing space/tab characters from every formatted line (post-pass,
added in v0.5.0). See catalog entry `fmt.trim_trailing_whitespace`.

```json
{ "ssl.format.trimTrailingWhitespace": true }
```

### 3.10 ssl.format.maxConsecutiveBlankLines

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `0` (disabled) |
| **File** | `internal/providers/formatting.go` |

Collapse runs of blank lines longer than this threshold. `0` preserves all
existing vertical whitespace. See catalog entry
`fmt.max_consecutive_blank_lines`.

```json
{ "ssl.format.maxConsecutiveBlankLines": 2 }
```

### 3.11 ssl.format.builtinFunctionCase

| Property | Value |
|----------|-------|
| **Type** | `"preserve"` \| `"PascalCase"` |
| **Default** | `"preserve"` |
| **File** | `internal/providers/formatting.go` |

`"preserve"` keeps the author's casing of built-in function names;
`"PascalCase"` rewrites call sites to the canonical inventory casing
(e.g. `sqlexecute(` → `SQLExecute(`). See catalog entry
`fmt.builtin_function_case`.

```json
{ "ssl.format.builtinFunctionCase": "preserve" }
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
- `GetNETDataSet`
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
| **Default** | `"canonicalCompact"` |
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

### 4.6 ssl.format.sql.detectSQLStrings

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `true` |
| **File** | `internal/providers/sql_formatter.go:15,32` |

When enabled, SQL strings are automatically detected and formatted in any string literal, not just those passed to SQL functions. Detection uses structural patterns to distinguish SQL from English sentences.

**Detected SQL Patterns:**
- `SELECT` with content (expression or FROM clause)
- `INSERT` with `INTO`
- `UPDATE` with `SET`
- `DELETE` with `FROM`
- `CREATE/ALTER/DROP` with object type (TABLE, VIEW, etc.)
- `TRUNCATE` with `TABLE`
- `WITH` (CTE) containing DML statement
- `EXEC/EXECUTE/CALL` with content

**Example:**
```ssl
/* With detectSQLStrings: true (default);
sSQL := "select * from users";
/* Becomes:;
sSQL := "
    SELECT *
    FROM users
";

/* With detectSQLStrings: false;
sSQL := "select * from users";
/* Stays unchanged (only SQL function args are formatted);
```

---

## 5. Diagnostic Options

### 5.1 ssl.diagnostics.hungarianNotation

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `false` |
| **File** | `internal/providers/diagnostics.go:49,62` |

When enabled, warns on declared variables that do not use an allowed Hungarian notation prefix.

```json
{ "ssl.diagnostics.hungarianNotation": true }
```

**Example Warning:**
```ssl
:DECLARE badName;  /* Warning: Variable 'badName' should use a Hungarian notation prefix;
```

### 5.2 ssl.diagnostics.unusedVariables

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `false` |
| **File** | `internal/providers/diagnostics.go:59,84` |

When enabled, reports declared variables that are never used (hint severity, code `unused_variable`). Opt-in per DECISIONS.md D5: usage counting is name-based and deliberately conservative, so the check is off unless requested.

```json
{ "ssl.diagnostics.unusedVariables": true }
```

### 5.3 ssl.diagnostics.hungarianPrefixes

| Property | Value |
|----------|-------|
| **Type** | `string[]` |
| **Default** | `["a", "b", "d", "fn", "n", "o", "s", "v"]` |
| **File** | `internal/providers/diagnostics.go:50,63` |

List of allowed Hungarian prefixes.

| Prefix | STARLIMS Convention |
|--------|---------------------|
| `a` | Array |
| `b` | Boolean |
| `d` | Date |
| `fn` | Code block |
| `n` | Numeric |
| `o` | Object |
| `s` | String |
| `v` | Variant / any |

**Validation Logic:**
1. Strip leading underscores
2. Allow loop-counter exceptions such as `i`, `j`, `k`, `x`, `y`, `z`
3. Check if the remaining name starts with a prefix from this list
4. Check if the next character is uppercase
5. Warn if no allowed prefix matches

```json
{ "ssl.diagnostics.hungarianPrefixes": ["s", "n", "b"] }
```

### 5.4 ssl.diagnostics.globals

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
gCurrentUser := "test";  /* Error: Cannot assign to global variable 'gCurrentUser';
```

When provider-level undeclared-variable or SQL-parameter validation is enabled, these globals are also treated as pre-declared names.

### 5.5 ssl.diagnostics.maxBlockDepth

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `4` |
| **Range** | `0` (disabled) or greater |
| **File** | `internal/server/server.go`, `internal/providers/diagnostics.go` |

Maximum allowed block nesting depth. Exceeding this triggers a warning. Set to `0` to disable the check.

```json
{ "ssl.diagnostics.maxBlockDepth": 4 }
```

### 5.6 ssl.diagnostics.rules

| Property | Value |
|----------|-------|
| **Type** | `object` (rule slug → severity) |
| **Default** | `{}` |
| **File** | `internal/server/server.go`, `internal/providers/diagnostics.go` (`applyRuleOverrides`) |

Per-rule severity overrides, keyed by the diagnostic's stable code slug
(the `Code` value shown with each diagnostic; canonical list in
`internal/providers/diagnostic_codes.go` and `catalog/diagnostics/`).
Recognized values: `"off"` (drop the diagnostic), `"info"`, `"warn"`,
`"warning"`, `"error"`. Slugs not present in the map pass through with
their default severity; unknown slugs are ignored. Added in v0.5.0
(catalog: DECISIONS.md D2).

```json
{
  "ssl.diagnostics.rules": {
    "hungarian_notation": "off",
    "equals_vs_strict_equals": "warning"
  }
}
```

### 5.7 ssl.diagnostics.endpointPatterns

| Property | Value |
|----------|-------|
| **Type** | `string[]` (glob patterns) |
| **Default** | `[]` |
| **File** | `internal/server/server.go`, `internal/providers/diagnostics.go` |

Files whose path matches any pattern are treated as SSL endpoint scripts:
the runtime-injected ambients `Request` and `Response` count as declared
and are not flagged by `undeclared_variable`. A file can also opt in with
an `Endpoint:` docblock. Added in v0.7.7 (catalog: DECISIONS.md D4).

```json
{ "ssl.diagnostics.endpointPatterns": ["**/endpoints/**", "**/*.srvscr"] }
```

### 5.8 ssl.diagnostics.unicodeLiteralPrefix

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `false` |
| **File** | `internal/server/server.go`, `internal/providers/diagnostics.go` |

When enabled, hints on `N'...'` Unicode literal prefixes in embedded SQL
(code `unicode_literal_prefix`, issue #196). Opt-in because whether the
prefix is needed is a schema property (NVARCHAR columns) the LSP cannot
see.

```json
{ "ssl.diagnostics.unicodeLiteralPrefix": true }
```

### 5.9 ssl.diagnostics.collateJustification

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `false` |
| **File** | `internal/server/server.go`, `internal/providers/diagnostics.go` |

When enabled, hints on `COLLATE` in embedded SQL when no comment directly
precedes the containing statement (code `unjustified_collate`, issue
#197). Opt-in because "justification comment above" is a team convention,
not a runtime rule.

```json
{ "ssl.diagnostics.collateJustification": true }
```

### 5.10 Suppression comments (in-file)

Not a configuration key, but part of the same rule-control surface: any
diagnostic can be suppressed in the source itself (added in v0.5.0,
catalog: DECISIONS.md D3).

```ssl
/* @ssl-disable hungarian_notation; */          suppresses for the whole file
/* @ssl-disable-next-line sql_injection; */     suppresses the next line only
/* @ssl-disable *; */                           wildcard: all rules
```

---

## 6. Inlay Hint Options

### 6.1 ssl.inlayHints.enabled

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `true` |
| **File** | `internal/server/server.go:31-36,324-326` |

Enables or disables inlay hints for built-in SSL functions and dispatch helpers
such as `DoProc` and `ExecFunction`.

```json
{
  "ssl": {
    "inlayHints": {
      "enabled": true
    }
  }
}
```

### 6.2 ssl.inlayHints.minParameterCount

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `2` |
| **Range** | `1` - `20` |
| **File** | `internal/server/server.go:31-36,324-326` |

Minimum number of parameters required before the server emits inlay hints for a
call site.

```json
{
  "ssl": {
    "inlayHints": {
      "minParameterCount": 2
    }
  }
}
```

---

## 7. IntelliSense Settings

### 7.1 ssl.intellisense.signatureHelp.autoTrigger

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `false` |
| **File** | `internal/server/server.go` (capabilities branch on initialize) |

When `true`, the server advertises `(` and `,` as signature-help trigger
characters and `,` as a retrigger character — the popup opens
automatically while typing inside a function call. When `false` (default),
no trigger characters are advertised; signature help is still available on
hover and on explicit invocation (`Ctrl+Shift+Space`).

The default was changed from `true` to `false` because the auto-popup
obscured the line being typed and reappeared on every keystroke after
being dismissed. See issue #9.

```json
{
  "ssl": {
    "intellisense": {
      "signatureHelp": {
        "autoTrigger": false
      }
    }
  }
}
```

---

## 8. Internal Options

These options are hardcoded and cannot be changed via client configuration.

### 8.1 CheckUnclosedBlocks

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Value** | `true` (always on) |
| **File** | `internal/providers/diagnostics.go:45,57` |

Always checks for unclosed block statements (`:IF` without `:ENDIF`, etc.).

### 8.2 CheckUnmatchedParens

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Value** | `true` (always on) |
| **File** | `internal/providers/diagnostics.go:46,58` |

Always checks for unmatched parentheses, brackets, and braces.

### 8.3 CheckUndeclaredVars

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Value** | `false` |
| **Status** | IMPLEMENTED (provider option only) |
| **File** | `internal/providers/diagnostics.go:47,59` |

Checks for usage of undeclared variables when enabled directly through `providers.DiagnosticOptions`. This option is not exposed via LSP client settings.

### 8.4 CheckUnusedVars

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Value** | `false` |
| **Status** | IMPLEMENTED (provider option only) |
| **File** | `internal/providers/diagnostics.go:48,60` |

Checks for declared but unused variables when enabled directly through `providers.DiagnosticOptions`. This option is not exposed via LSP client settings.

### 8.5 CheckSQLParams

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Value** | `false` |
| **Status** | IMPLEMENTED (provider option only) |
| **File** | `internal/providers/diagnostics.go:50,64` |

Checks named SQL placeholders against declared variables when enabled directly through `providers.DiagnosticOptions`. This option is not exposed via LSP client settings.

### 8.6 MaxNumberOfProblems

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Value** | `100` |
| **File** | `internal/server/server.go:66,74` |

Maximum number of diagnostics to report per document.

---

## 9. Configuration Examples

### 9.1 Minimal Configuration

```json
{
  "ssl": {}
}
```

Uses all defaults.

### 9.2 STARLIMS Style Guide Configuration

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
        "style": "canonicalCompact",
        "keywordCase": "upper",
        "indentSize": 4,
        "maxLineLength": 90
      }
    },
    "diagnostics": {
      "hungarianNotation": true,
      "hungarianPrefixes": ["a", "b", "d", "fn", "n", "o", "s", "v"],
      "globals": []
    }
  }
}
```

### 9.3 Production Environment Configuration

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

### 9.4 Compact SQL Configuration

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

### 9.5 Disable SQL Formatting

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

## 10. VS Code Integration

### 10.1 settings.json Location

- **User:** `~/.config/Code/User/settings.json` (Linux/Mac) or `%APPDATA%\Code\User\settings.json` (Windows)
- **Workspace:** `.vscode/settings.json`

### 10.2 VS Code Settings Example

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
  "ssl.format.sql.style": "canonicalCompact",
  "ssl.format.sql.keywordCase": "upper",
  "ssl.format.sql.indentSize": 4,
  "ssl.format.sql.maxLineLength": 90,
  "ssl.format.sql.detectSQLStrings": true,
  "ssl.diagnostics.hungarianNotation": false,
  "ssl.diagnostics.hungarianPrefixes": ["a", "b", "d", "fn", "n", "o", "s", "v"],
  "ssl.diagnostics.globals": ["gCurrentUser", "gAppName"]
}
```

### 10.3 Configuration via Extension

The VS Code extension (`vs-code-ssl-formatter`) automatically sends configuration changes to the LSP. Settings changed in VS Code are immediately applied.

---

## Appendix A: Default Values Summary

| Option | Default |
|--------|---------|
| `ssl.format.indentStyle` | `"tab"` |
| `ssl.format.indentSize` | `4` (space mode only) |
| `ssl.format.maxLineLength` | `90` |
| `ssl.format.operatorSpacing` | `true` |
| `ssl.format.commaSpacing` | `true` |
| `ssl.format.semicolonEnforcement` | `true` |
| `ssl.format.blankLinesBetweenProcs` | `1` |
| `ssl.format.sql.enabled` | `true` |
| `ssl.format.sql.style` | `"canonicalCompact"` |
| `ssl.format.sql.keywordCase` | `"upper"` |
| `ssl.format.sql.indentSize` | `4` |
| `ssl.format.sql.maxLineLength` | `90` |
| `ssl.format.sql.detectSQLStrings` | `true` |
| `ssl.diagnostics.hungarianNotation` | `false` |
| `ssl.diagnostics.unicodeLiteralPrefix` | `false` |
| `ssl.diagnostics.collateJustification` | `false` |
| `ssl.diagnostics.hungarianPrefixes` | `["a","b","d","fn","n","o","s","v"]` |
| `ssl.diagnostics.globals` | `[]` |
| `ssl.inlayHints.enabled` | `true` |
| `ssl.inlayHints.minParameterCount` | `2` |
| `ssl.intellisense.signatureHelp.autoTrigger` | `false` |

## Appendix B: Option Types

| Type | JSON Example |
|------|--------------|
| `string` | `"value"` |
| `number` | `4` |
| `boolean` | `true` or `false` |
| `string[]` | `["a", "b", "c"]` |
