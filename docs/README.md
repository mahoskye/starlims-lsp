# STARLIMS LSP Documentation

Welcome to the starlims-lsp documentation. This language server provides rich editing support for the STARLIMS Scripting Language (SSL) in any LSP-compatible editor.

---

## Quick Navigation

| Section | Description |
|---------|-------------|
| [Vision](./vision/) | Project goals, architecture, and roadmap |
| [Features](./features/) | Detailed specifications for each LSP feature |
| [SSL Reference](./ssl-reference/) | SSL language syntax, grammar, and conventions |
| [Configuration](./configuration/) | All available configuration options |
| [Status Dashboard](./STATUS.md) | Current implementation status at a glance |
| [AGENTS.md](../AGENTS.md) | SSL coding conventions for AI agents |

---

## Documentation Philosophy

This project follows a **document-driven development** approach:

1. **Specification First:** Each feature is fully specified before implementation
2. **Test Scenarios:** Every spec includes test cases for validation
3. **Living Documents:** Documentation evolves with the codebase
4. **Single Source of Truth:** These docs are the authoritative reference

---

## Project Overview

### What is starlims-lsp?

A Language Server Protocol (LSP) implementation for SSL that provides:

- **Auto-completion** for keywords, functions, classes, and variables
- **Hover information** with function signatures and documentation
- **Signature help** showing parameter info while typing function calls
- **Go to definition** for procedures and variables
- **Find references** across the document
- **Document symbols** for outline and navigation
- **Code formatting** with configurable styles
- **Diagnostics** for syntax errors and style issues

### Quick Start

```bash
# Install the language server
go install github.com/mahoskye/starlims-lsp/cmd/starlims-lsp@latest

# Or build from source
make build
```

See the [main README](../README.md) for full installation instructions.

---

## Document Index

### Vision & Architecture

| Document | Purpose |
|----------|---------|
| [VISION.md](./vision/VISION.md) | Project goals and philosophy |
| [ARCHITECTURE.md](./vision/ARCHITECTURE.md) | LSP architecture and component overview |
| [ROADMAP.md](./vision/ROADMAP.md) | Prioritized feature roadmap |

### Feature Specifications

| Feature | Status | Document |
|---------|--------|----------|
| Completion | IMPLEMENTED | [completion.md](./features/completion.md) |
| Hover | IMPLEMENTED | [hover.md](./features/hover.md) |
| Signature Help | IMPLEMENTED | [signature-help.md](./features/signature-help.md) |
| Go to Definition | IMPLEMENTED | [definition.md](./features/definition.md) |
| Find References | IMPLEMENTED | [references.md](./features/references.md) |
| Document Symbols | IMPLEMENTED | [document-symbols.md](./features/document-symbols.md) |
| Workspace Symbols | PARTIAL | [workspace-symbols.md](./features/workspace-symbols.md) |
| Formatting | IMPLEMENTED | [formatting.md](./features/formatting.md) |
| Diagnostics | PARTIAL | [diagnostics.md](./features/diagnostics.md) |
| Folding Ranges | IMPLEMENTED | [folding-ranges.md](./features/folding-ranges.md) |
| Snippets | IMPLEMENTED | [snippets.md](./features/snippets.md) |

### SSL Language Reference

| Document | Purpose |
|----------|---------|
| [README.md](./ssl-reference/README.md) | SSL language overview and resources |
| [syntax.md](./ssl-reference/syntax.md) | SSL syntax rules |
| [grammar.md](./ssl-reference/grammar.md) | Formal EBNF grammar |
| [style-guide.md](./ssl-reference/style-guide.md) | Coding conventions |
| [functions.md](./ssl-reference/functions.md) | Built-in functions reference |
| [classes.md](./ssl-reference/classes.md) | Built-in classes reference |
| [gotchas.md](./ssl-reference/gotchas.md) | Common SSL pitfalls |

### Configuration

| Document | Purpose |
|----------|---------|
| [CONFIGURATION.md](./configuration/CONFIGURATION.md) | All configuration options |

---

## Contributing

See the [main README](../README.md) for contribution guidelines.

---

## Version History

| Version | Date | Notes |
|---------|------|-------|
| 1.0 | 2025-02-02 | Initial documentation consolidation |
