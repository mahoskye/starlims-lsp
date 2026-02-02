# Feature Specifications

This directory contains detailed specifications for each LSP feature implemented by starlims-lsp.

---

## Document-Driven Approach

Each feature document includes:

1. **Overview** - What the feature does
2. **Capabilities** - Detailed behavior specification
3. **Configuration** - Relevant settings
4. **Edge Cases** - Special handling requirements
5. **Known Limitations** - Current gaps
6. **Test Specifications** - Executable test scenarios

---

## Feature Index

### Language Features

| Feature | Status | Document |
|---------|--------|----------|
| Completion | IMPLEMENTED | [completion.md](./completion.md) |
| Hover | IMPLEMENTED | [hover.md](./hover.md) |
| Signature Help | IMPLEMENTED | [signature-help.md](./signature-help.md) |
| Go to Definition | IMPLEMENTED | [definition.md](./definition.md) |
| Find References | IMPLEMENTED | [references.md](./references.md) |
| Document Symbols | IMPLEMENTED | [document-symbols.md](./document-symbols.md) |
| Workspace Symbols | PARTIAL | [workspace-symbols.md](./workspace-symbols.md) |
| Folding Ranges | IMPLEMENTED | [folding-ranges.md](./folding-ranges.md) |
| Formatting | IMPLEMENTED | [formatting.md](./formatting.md) |
| Diagnostics | PARTIAL | [diagnostics.md](./diagnostics.md) |
| Snippets | IMPLEMENTED | [snippets.md](./snippets.md) |

---

## Status Legend

| Status | Meaning |
|--------|---------|
| IMPLEMENTED | Fully functional |
| PARTIAL | Core works, some gaps remain |
| PLANNED | Specified but not implemented |

---

## Feature Template

New features should follow this structure:

```markdown
# Feature Name

**Status:** IMPLEMENTED | PARTIAL | PLANNED
**LSP Method:** `textDocument/xxx`
**Source Files:** `internal/providers/xxx.go`

---

## 1. Overview

Brief description of what this feature does.

## 2. Capabilities

### 2.1 Capability A
Detailed behavior specification.

### 2.2 Capability B
...

## 3. Configuration

Relevant settings that affect this feature.

## 4. Edge Cases & Special Handling

Specific situations requiring special behavior.

## 5. Known Limitations

Current gaps or constraints.

## 6. Test Specifications

### 6.1 Test Category
\`\`\`ssl
/* Test: Description;
<ssl code>
/* Expected: <LSP response or diagnostic>;
\`\`\`

## 7. Related Issues

Links to relevant GitHub issues.
```

---

## Quick Links

- [Status Dashboard](../STATUS.md) - Implementation overview
- [Configuration Reference](../configuration/CONFIGURATION.md) - All settings
- [Architecture](../vision/ARCHITECTURE.md) - Technical overview
