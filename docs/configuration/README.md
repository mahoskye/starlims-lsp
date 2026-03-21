# Configuration

This directory contains configuration documentation for the SSL Language Server.

## Contents

| Document | Description |
|----------|-------------|
| [CONFIGURATION.md](CONFIGURATION.md) | Complete configuration reference with all options |

## Quick Links

- **Formatting options**: [CONFIGURATION.md#3-formatting-options](CONFIGURATION.md#3-formatting-options)
- **SQL formatting**: [CONFIGURATION.md#4-sql-formatting-options](CONFIGURATION.md#4-sql-formatting-options)
- **Diagnostics**: [CONFIGURATION.md#5-diagnostic-options](CONFIGURATION.md#5-diagnostic-options)
- **Examples**: [CONFIGURATION.md#7-configuration-examples](CONFIGURATION.md#7-configuration-examples)
- **VS Code setup**: [CONFIGURATION.md#8-vs-code-integration](CONFIGURATION.md#8-vs-code-integration)

## Minimal Configuration

```json
{
  "ssl": {
    "format": {
      "indentStyle": "tab",
      "indentSize": 4
    }
  }
}
```

`indentStyle: "tab"` matches the source guide. `indentSize` only applies when `indentStyle` is `"space"`; the default `4` is the fallback space width, while tab-indented SSL code uses one tab per indentation level.

See [CONFIGURATION.md](CONFIGURATION.md) for the complete reference.
