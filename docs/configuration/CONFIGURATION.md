# SSL Language Server - Configuration Reference

This document specifies all configuration options available in the starlims-lsp language server. It serves as the authoritative reference for client configuration.

**Version:** 1.5  
**Last Updated:** 2026-10-13  
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

Configuration is sent to the language server as `initializationOptions` in the `initialize` request, via the LSP `workspace/didChangeConfiguration` notification, or both. Both routes accept the same `ssl` object and apply only the fields present; a configuration change re-validates every open document.

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
  blankLinesBetweenSections: boolean;
  trimTrailingWhitespace: boolean;
  maxConsecutiveBlankLines: number;
  builtinFunctionCase: "preserve" | "PascalCase";
  sql: SQLFormattingOptions;
}

interface SQLFormattingOptions {
  enabled: boolean;
  style: "standard" | "canonicalCompact" | "expanded"; // "compact" = deprecated alias (removal planned, see 4.2)
  keywordCase: "upper" | "lower" | "preserve";
  identifierCase: "preserve" | "lower" | "upper";
  indentSize: number;
  maxLineLength: number;
  detectSQLStrings: boolean;
}

interface DiagnosticOptions {
  hungarianNotation: boolean;
  hungarianTypes: boolean;
  unusedVariables: boolean;
  infoDiagnostics: boolean;
  hungarianPrefixes: string[];
  globals: string[];
  maxBlockDepth: number;
  maxProcedureCodeLines: number;
  maxScriptCodeLines: number;
  maxScriptProcedures: number;
  rules: { [slug: string]: "off" | "hint" | "info" | "warn" | "warning" | "error" };
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
| **File** | `internal/providers/formatting.go` |

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
| **File** | `internal/providers/formatting.go` |

Number of spaces per indentation level when `indentStyle` is `"space"`. In tab mode it is the width a tab counts as for line-length accounting (`maxLineLength`), so it affects where long lines wrap either way. The source guide's preferred default is tab indentation; the bundled value `4` is the fallback width for space-indented formatting. The configured style is applied whatever indentation a file already uses — an existing space-indented file is not detected and preserved. See catalog entry `fmt.indent_style`.

```json
{ "ssl.format.indentSize": 4 }
```

### 3.3 ssl.format.maxLineLength

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `90` |
| **Range** | `0` (unlimited) - `200` |
| **File** | `internal/providers/formatting.go` |

Maximum line length before wrapping. Set to `0` to disable line length enforcement.

```json
{ "ssl.format.maxLineLength": 90 }
```

### 3.4 ssl.format.operatorSpacing

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `true` |
| **File** | `internal/providers/formatting.go` |

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
| **File** | `internal/providers/formatting.go` |

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
| **File** | `internal/providers/formatting.go` |

When enabled, ensures statements end with semicolons.

**Note:** Does NOT add semicolons after:
- Opening delimiters (`(`, `[`, `{`), operators, `:=`, or a trailing comma
- A line inside an unclosed `(` / `{` / `[`
- Continuation keywords (`:TO`, `:STEP`), or before one (`:ELSE`, `:CASE`, `:OTHERWISE`, `:CATCH`, `:FINALLY`)
- Declaration keywords whose operand list starts on the next line (`:PARAMETERS`, `:DECLARE`, `:PUBLIC`, `:DEFAULT`, `:INCLUDE`, `:INHERIT`)
- A keyword whose operand starts on the next line (`:IF ⏎ bReady;`, `:RETURN ⏎ nValue;`)

See catalog entry `fmt.semicolon_enforcement`.

### 3.7 ssl.format.blankLinesBetweenProcs

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `1` |
| **Range** | `0` - `3` |
| **File** | `internal/providers/formatting.go` |

Exact number of blank lines between procedure (and `:REGION`) definitions. The count is normalized, not added: whatever blank lines the source has at an `:ENDPROC;`/`:ENDREGION;` → `:PROCEDURE`/`:REGION` boundary are replaced with this many. `0` leaves the boundary as written. See catalog entry `fmt.blank_lines_between_procs`.

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

### 3.8.1 ssl.format.blankLinesBetweenSections

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `true` |
| **File** | `internal/providers/formatting.go` |

Insert one blank line at each boundary between logical sections of a body
(style guide `blank_line_between_sections`): after the declarations
(`:PARAMETERS`/`:DEFAULT`/`:DECLARE`/`:PUBLIC`/`:INCLUDE`), before a
control block that follows code and after its closer, before a standalone
comment that follows code, and before a procedure's final `:RETURN`.
Nothing is inserted at the start or end of a body or where a blank line
already exists; the author's blank lines are never removed. See catalog
entry `fmt.blank_lines_between_sections`.

```json
{ "ssl.format.blankLinesBetweenSections": true }
```

### 3.9 ssl.format.trimTrailingWhitespace

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `true` |
| **File** | `internal/providers/formatting.go` |

Remove trailing space/tab characters from every formatted line (post-pass,
added in v0.5.0). Line ends inside a multi-line string literal, code block
or `:REGION` body are user content and are never trimmed. See catalog entry
`fmt.trim_trailing_whitespace`.

```json
{ "ssl.format.trimTrailingWhitespace": true }
```

### 3.10 ssl.format.maxConsecutiveBlankLines

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `2` |
| **File** | `internal/providers/formatting.go` |

Collapse runs of blank lines longer than this threshold. `0` preserves all
existing vertical whitespace. The default was `0` (disabled) before the
section-spacing rules; a run of up to two blank lines now survives, and a
longer one collapses to two. See catalog entry
`fmt.max_consecutive_blank_lines`.

```json
{ "ssl.format.maxConsecutiveBlankLines": 2 }
```

### 3.11 ssl.format.builtinFunctionCase

| Property | Value |
|----------|-------|
| **Type** | `"preserve"` \| `"PascalCase"` |
| **Default** | `"PascalCase"` |
| **File** | `internal/providers/formatting.go` |

`"PascalCase"` (default) rewrites built-in call sites to the canonical
inventory casing (e.g. `sqlexecute(` → `SQLExecute(`); member calls such as
`oVar:upper(...)` are never re-cased. `"preserve"` keeps the author's
casing. See catalog entry `fmt.builtin_function_case`.

```json
{ "ssl.format.builtinFunctionCase": "PascalCase" }
```

---

## 4. SQL Formatting Options

### 4.1 ssl.format.sql.enabled

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `true` |
| **File** | `internal/providers/sql_formatter.go` |

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
| **Values** | `"standard"`, `"canonicalCompact"`, `"expanded"` (`"compact"` is a deprecated alias for `canonicalCompact`) |
| **File** | `internal/providers/sql_formatter.go` |

SQL formatting style to apply. `"compact"` was retired in the 2026-08-28
style review (its half-multiline output was internally inconsistent) and
is accepted as an alias for `canonicalCompact`.

**Deprecation and removal plan for `"compact"`** (issue #257): the alias
stays while the VS Code extension still lists `compact` in its
`ssl.format.sql.style` enum. Removal order: (1) the extension drops the
enum value and migrates a stored `compact` to `canonicalCompact`; (2) the
next LSP minor release after that extension release drops the alias, at
which point an unrecognized style formats as `standard`. Both steps are
called out in each distribution's release notes.

#### Style Comparison

| Feature | standard | canonicalCompact | expanded |
|---------|----------|------------------|----------|
| Clause line breaks | Yes | Yes | Yes |
| AND/OR indentation | On overflow | Yes | Yes |
| ON clause indentation | No | Yes | Yes |
| Smart column wrapping | On overflow | Yes | Yes |
| Multi-column SELECT wrap | On overflow | Yes | Always |

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

### 4.3 ssl.format.sql.identifierCase

| Property | Value |
|----------|-------|
| **Type** | `string` |
| **Default** | `"preserve"` |
| **Values** | `"preserve"`, `"lower"`, `"upper"` |
| **File** | `internal/providers/sql_formatter.go` |

Casing applied to table/column/alias identifiers in reflowed SQL.
`"preserve"` (default) keeps the author's casing — the safe choice on
every dialect: force-folding breaks queries on SQL Server case-sensitive
collations, and the STARLIMS legacy corpus is uppercase-identifier
dominant. `"lower"` gives the community-guide look (safe on Oracle and
MSSQL CI collations only); `"upper"` matches the STARLIMS data
dictionary. Double-quoted identifiers and ODBC `{…}` escape interiors
are always preserved regardless of this setting.

```json
{ "ssl.format.sql.identifierCase": "preserve" }
```

### 4.4 ssl.format.sql.keywordCase

| Property | Value |
|----------|-------|
| **Type** | `string` |
| **Default** | `"upper"` |
| **Values** | `"upper"`, `"lower"`, `"preserve"` |
| **File** | `internal/providers/sql_formatter.go` |

Case transformation for SQL keywords.

| Value | Example |
|-------|---------|
| `"upper"` | `SELECT`, `FROM`, `WHERE` |
| `"lower"` | `select`, `from`, `where` |
| `"preserve"` | Keeps original case |

### 4.5 ssl.format.sql.indentSize

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `4` |
| **Range** | `1` - `8` |
| **File** | `internal/providers/sql_formatter.go` |

Number of spaces per indentation level within SQL statements.

### 4.6 ssl.format.sql.maxLineLength

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `90` |
| **Range** | `0` (unlimited) - `200` |
| **File** | `internal/providers/sql_formatter.go` |

Maximum line length for SQL before wrapping. Used with `canonicalCompact` and `expanded` styles.

### 4.7 ssl.format.sql.detectSQLStrings

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `true` |
| **File** | `internal/providers/sql_formatter.go` |

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
| **File** | `internal/providers/diagnostics.go` |

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
| **File** | `internal/providers/diagnostics.go` |

When enabled, reports declared variables that are never used (hint severity, code `unused_variable`). Opt-in per DECISIONS.md D5: usage counting is name-based and deliberately conservative, so the check is off unless requested.

```json
{ "ssl.diagnostics.unusedVariables": true }
```

### 5.3 ssl.diagnostics.hungarianPrefixes

| Property | Value |
|----------|-------|
| **Type** | `string[]` |
| **Default** | `["a", "b", "d", "fn", "n", "o", "s", "v"]` |
| **File** | `internal/providers/diagnostics.go` |

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
| **File** | `internal/providers/diagnostics.go` |

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

Maximum allowed block nesting depth. Exceeding it reports `max_block_depth`, an info-tier diagnostic: it is visible only with `ssl.diagnostics.infoDiagnostics` (or `--validate --info`) or an explicit `ssl.diagnostics.rules` entry. Set to `0` to disable the check.

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
Recognized values: `"off"` (drop the diagnostic), `"hint"`, `"info"`,
`"warn"`, `"warning"`, `"error"`. Slugs and values are matched
case-insensitively. Slugs not present in the map pass through with
their default severity; unknown slugs are ignored, and so is an entry
with any other value — it changes nothing, and does not exempt the rule
from the info gate (`ssl.diagnostics.infoDiagnostics`). Added in v0.5.0;
`"hint"` and case-insensitive matching added in the release after v0.24.0
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
| **Type** | `string[]` (case-insensitive path substrings) |
| **Default** | `[]` |
| **File** | `internal/server/server.go`, `internal/providers/diagnostics.go` |

Files whose lowercased URI contains any pattern as a plain substring
(not a glob — `*` has no special meaning) are treated as SSL endpoint scripts:
the runtime-injected ambients `Request` and `Response` count as declared
and are not flagged by `undeclared_variable`. A file can also opt in with
an `Endpoint:` docblock. Added in v0.7.7 (catalog: DECISIONS.md D4).

```json
{ "ssl.diagnostics.endpointPatterns": ["/endpoints/", "_api.srvscr"] }
```

### 5.8 ssl.diagnostics.infoDiagnostics

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `false` |
| **File** | `internal/server/server.go`, `internal/providers/diagnostics.go` |

Enables the info severity tier. Info diagnostics are advisory detail —
style observations and idiom notes — aimed at assistant/LLM consumers
and teams that want the full picture. They are dropped by default so the
everyday surface stays errors/warnings/hints. A rule explicitly listed
in `ssl.diagnostics.rules` always shows regardless of this gate (that is
also the per-rule promotion path). The `--validate` CLI mirrors this
with `--info`.

Info-tier rules: `equals_vs_strict_equals`, `not_preferred_operator`,
`empty_optional_param_array`, `identifier_too_long`,
`complex_sql_placeholder`, `include_early`, `nested_iif`,
`class_member_order`, `nil_not_empty_string`, `max_block_depth`,
`limit_public_vars`, `max_params_warning`, `negative_logic`,
`c_style_comment_closer`, `unicode_literal_prefix`,
`unjustified_collate`, `spaced_skip_commas`,
`visibility_annotation_usage`, `procedure_length_review`,
`script_size_review`.

```json
{ "ssl.diagnostics.infoDiagnostics": true }
```

### 5.9 ssl.diagnostics.hungarianTypes

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `false` |
| **File** | `internal/providers/diagnostics.go` (`CheckHungarianTypes`) |

When enabled, warns where a variable's Hungarian prefix promises one type
and its assigned expression produces another (code
`hungarian_type_mismatch`).

Gated separately from [`ssl.diagnostics.hungarianNotation`](#51-ssldiagnosticshungariannotation),
and neither setting enables the other. They are different findings:
`hungarian_notation` audits a codebase against the naming convention and
reports every declared name lacking a prefix, so on a codebase that does
not use the convention it reports most of the file. This check reports a
self-contradiction instead, and only ever inspects names that already
carry a prefix — so it stays quiet on exactly the code the other floods.
Enable this one alone to get the correctness signal without the
convention audit.

```json
{ "ssl.diagnostics.hungarianTypes": true }
```

**Example Warning:**
```ssl
:DECLARE nCode, sText;
nCode := SubStr(sText, 1, 4);  /* Warning: 'nCode' promises a number by its prefix, but this expression produces a string;
```

### 5.10 Suppression comments (in-file)

Not a configuration key, but part of the same rule-control surface: any
diagnostic can be suppressed in the source itself (added in v0.5.0,
catalog: DECISIONS.md D3).

```ssl
/* @ssl-disable hungarian_notation;
/* @ssl-disable-next-line sql_injection -- the table name is a constant;
/* @ssl-disable *;
```

The first suppresses a rule for the whole file, the second for the next
line only (with a reason), and the third is the wildcard for all rules.
Several rules are comma-separated. The comment ends at its `;` like any
SSL comment: do not add a closing `*/`, which would be code after the
comment. An optional reason follows the rule list after `--`, `—` or `:`
and is ignored by matching; writing one is recommended, so a later reader
knows whether the suppression still applies.

### 5.11 ssl.diagnostics.maxProcedureCodeLines

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `80` |
| **Range** | `0` (disabled) or greater |
| **File** | `internal/server/server.go`, `internal/providers/size_review.go` |

Review trigger for procedure length. A procedure or class method with
more than this many code lines reports `procedure_length_review`, an
info-tier diagnostic (visible with `ssl.diagnostics.infoDiagnostics`,
`--validate --info`, or an explicit `ssl.diagnostics.rules` entry). Code
lines exclude blank lines and comment-only lines (a comment runs from
`/*` to its first `;`), and the count runs from the `:PROCEDURE` line
through the `:ENDPROC` line. The default is ssl-style-guide's
`procedure_length_review` threshold, measured on STARLIMS 12's base layer.
Set to `0` to disable.

```json
{ "ssl.diagnostics.maxProcedureCodeLines": 80 }
```

### 5.12 ssl.diagnostics.maxScriptCodeLines

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `400` |
| **Range** | `0` (disabled) or greater |
| **File** | `internal/server/server.go`, `internal/providers/size_review.go` |

Review trigger for server-script size, counted like
`maxProcedureCodeLines` over the whole file. A server script over this
many code lines, or over `maxScriptProcedures` local procedures, reports
`script_size_review` (info tier) once, on line 1. Class files and data
sources are not measured. The default is ssl-style-guide's
`script_size_review` threshold. Set to `0` to disable the line trigger.

```json
{ "ssl.diagnostics.maxScriptCodeLines": 400 }
```

### 5.13 ssl.diagnostics.maxScriptProcedures

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Default** | `12` |
| **Range** | `0` (disabled) or greater |
| **File** | `internal/server/server.go`, `internal/providers/size_review.go` |

Review trigger for the number of local procedures in a server script; the
other half of `script_size_review` (see `maxScriptCodeLines`). Set to `0`
to disable the procedure-count trigger.

```json
{ "ssl.diagnostics.maxScriptProcedures": 12 }
```

---

## 6. Inlay Hint Options

### 6.1 ssl.inlayHints.enabled

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Default** | `true` |
| **File** | `internal/server/server.go` |

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
| **File** | `internal/server/server.go` |

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

Trigger characters are advertised once, in the `initialize` response, so
this setting is read from `initializationOptions` at startup. Changing it
later through `workspace/didChangeConfiguration` takes effect only after
the server restarts.

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

These are provider-level options. Most are fixed; where one is reachable from a client setting or a CLI flag, its section says so.

### 8.1 CheckUnclosedBlocks

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Value** | `true` (always on) |
| **File** | `internal/providers/diagnostics.go` |

Always checks for unclosed block statements (`:IF` without `:ENDIF`, etc.).

### 8.2 CheckUnmatchedParens

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Value** | `true` (always on) |
| **File** | `internal/providers/diagnostics.go` |

Always checks for unmatched parentheses, brackets, and braces.

### 8.3 CheckUndeclaredVars

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Value** | `false` |
| **Status** | IMPLEMENTED (provider option only) |
| **File** | `internal/providers/diagnostics.go` |

Checks for usage of undeclared variables (`undeclared_variable`). Not exposed as an LSP client setting; the `--validate --strict` CLI profile enables it.

### 8.4 CheckUnusedVars

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Value** | `false` |
| **Status** | IMPLEMENTED (client setting `ssl.diagnostics.unusedVariables`) |
| **File** | `internal/providers/diagnostics.go` |

Checks for declared but unused variables (`unused_variable`). Exposed to LSP clients as [`ssl.diagnostics.unusedVariables`](#52-ssldiagnosticsunusedvariables); the `--validate --strict` CLI profile also enables it.

### 8.5 CheckSQLParams

| Property | Value |
|----------|-------|
| **Type** | `boolean` |
| **Value** | `false` |
| **Status** | IMPLEMENTED (provider option only) |
| **File** | `internal/providers/diagnostics.go` |

Checks named SQL placeholders against declared variables (`invalid_sql_param`). Not exposed as an LSP client setting; the `--validate --strict` CLI profile enables it.

### 8.6 MaxNumberOfProblems

| Property | Value |
|----------|-------|
| **Type** | `number` |
| **Value** | `100` |
| **File** | `internal/server/server.go` |

Maximum number of diagnostics the server publishes per document. Not
client-configurable: the VS Code extension's own `ssl.maxNumberOfProblems`
setting is not forwarded to the server and only caps the extension's
built-in fallback diagnostics (used when the language server is disabled).
`--validate` applies no cap.

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

### 9.4 Expanded SQL Configuration

```json
{
  "ssl": {
    "format": {
      "sql": {
        "enabled": true,
        "style": "expanded",
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

### 10.2 Setting Names in VS Code

The VS Code extension (`vs-code-ssl-formatter`) exposes its own setting
names and rebuilds the `ssl.*` tree above from them; it sends that tree in
`initializationOptions` at startup and again on every relevant
configuration change. The server keys in sections 3–7 are therefore **not**
the names a VS Code user writes in `settings.json`. Mapping as of extension
1.23.0:

| VS Code setting | Server key | Notes |
|-----------------|------------|-------|
| `ssl.format.indentStyle` | `ssl.format.indentStyle` | |
| `editor.tabSize` (tab mode) / `ssl.format.indentWidth` (space mode) | `ssl.format.indentSize` | |
| `ssl.format.wrapLength` | `ssl.format.maxLineLength` **and** `ssl.format.sql.maxLineLength` | one setting drives both |
| `ssl.format.operatorSpacing`, `commaSpacing`, `semicolonEnforcement`, `blankLinesBetweenProcs`, `trimTrailingWhitespace`, `maxConsecutiveBlankLines`, `builtinFunctionCase` | same name under `ssl.format` | |
| `ssl.format.sql.enabled`, `style`, `keywordCase`, `identifierCase` | same name under `ssl.format.sql` | `style` still offers the deprecated `compact` (see 4.2) |
| `ssl.format.sql.indentSpaces` | `ssl.format.sql.indentSize` | |
| `ssl.naming.hungarianNotation.enabled` | `ssl.diagnostics.hungarianNotation` | extension default `true`; server default `false` |
| `ssl.naming.hungarianNotation.checkTypes` | `ssl.diagnostics.hungarianTypes` | extension default `true`; server default `false` |
| `ssl.naming.hungarianNotation.prefixes` | `ssl.diagnostics.hungarianPrefixes` | |
| `ssl.globals` | `ssl.diagnostics.globals` | |
| `ssl.styleGuide.limitBlockDepth` | `ssl.diagnostics.maxBlockDepth` | |
| `ssl.diagnostics.rules`, `endpointPatterns`, `unusedVariables`, `infoDiagnostics` | same name | the extension also re-applies `rules` client-side |
| `ssl.intellisense.inlayHints.enabled`, `minParameterCount` | `ssl.inlayHints.enabled`, `minParameterCount` | |
| `ssl.intellisense.signatureHelp.autoTrigger` | `ssl.intellisense.signatureHelp.autoTrigger` | restart to apply (see 7.1) |

Not settable from VS Code (server defaults always apply):
`ssl.format.blankLineBetweenBlocks` (`true`),
`ssl.format.blankLinesBetweenSections` (`true`) and
`ssl.format.sql.detectSQLStrings` (`true`).

The two Hungarian defaults differ deliberately: the server keeps noisy
checks opt-in for every client (DECISIONS.md D5), and the extension, whose
audience is authors writing new SSL, turns them on.

```json
{
  "ssl.format.indentStyle": "tab",
  "ssl.format.wrapLength": 90,
  "ssl.format.operatorSpacing": true,
  "ssl.format.commaSpacing": true,
  "ssl.format.semicolonEnforcement": true,
  "ssl.format.blankLinesBetweenProcs": 1,
  "ssl.format.trimTrailingWhitespace": true,
  "ssl.format.maxConsecutiveBlankLines": 0,
  "ssl.format.builtinFunctionCase": "PascalCase",
  "ssl.format.sql.enabled": true,
  "ssl.format.sql.style": "canonicalCompact",
  "ssl.format.sql.indentSpaces": 4,
  "ssl.naming.hungarianNotation.enabled": false,
  "ssl.globals": ["gCurrentUser", "gAppName"]
}
```

### 10.3 Configuration via Extension

Settings changed in VS Code are sent to the server immediately, except
`ssl.intellisense.signatureHelp.autoTrigger` (see 7.1).

### 10.4 CLI Consumers

`--validate` and `--format` read no settings file. `--validate` starts
from the server defaults and takes flags instead (`--info`, `--hungarian`,
`--hungarian-types`, `--strict`, `--ds`); `--format` starts from the
formatting defaults and takes `--indent-style`, `--indent-size`,
`--max-line-length`, `--no-sql`, and `--ds`. See `starlims-lsp --validate --help`
and `starlims-lsp --format --help`.

---

## Appendix A: Default Values Summary

| Option | Default |
|--------|---------|
| `ssl.format.indentStyle` | `"tab"` |
| `ssl.format.indentSize` | `4` (spaces per level; tab width for line length) |
| `ssl.format.maxLineLength` | `90` |
| `ssl.format.operatorSpacing` | `true` |
| `ssl.format.commaSpacing` | `true` |
| `ssl.format.semicolonEnforcement` | `true` |
| `ssl.format.blankLinesBetweenProcs` | `1` |
| `ssl.format.blankLineBetweenBlocks` | `true` |
| `ssl.format.blankLinesBetweenSections` | `true` |
| `ssl.format.trimTrailingWhitespace` | `true` |
| `ssl.format.maxConsecutiveBlankLines` | `2` |
| `ssl.format.builtinFunctionCase` | `"PascalCase"` |
| `ssl.format.sql.enabled` | `true` |
| `ssl.format.sql.style` | `"canonicalCompact"` |
| `ssl.format.sql.keywordCase` | `"upper"` |
| `ssl.format.sql.identifierCase` | `"preserve"` |
| `ssl.format.sql.indentSize` | `4` |
| `ssl.format.sql.maxLineLength` | `90` |
| `ssl.format.sql.detectSQLStrings` | `true` |
| `ssl.diagnostics.hungarianNotation` | `false` |
| `ssl.diagnostics.hungarianTypes` | `false` |
| `ssl.diagnostics.unusedVariables` | `false` |
| `ssl.diagnostics.infoDiagnostics` | `false` |
| `ssl.diagnostics.hungarianPrefixes` | `["a","b","d","fn","n","o","s","v"]` |
| `ssl.diagnostics.globals` | `[]` |
| `ssl.diagnostics.maxBlockDepth` | `4` |
| `ssl.diagnostics.maxProcedureCodeLines` | `80` |
| `ssl.diagnostics.maxScriptCodeLines` | `400` |
| `ssl.diagnostics.maxScriptProcedures` | `12` |
| `ssl.diagnostics.rules` | `{}` |
| `ssl.diagnostics.endpointPatterns` | `[]` |
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
