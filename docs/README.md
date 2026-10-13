# STARLIMS LSP Documentation

Welcome to the starlims-lsp documentation. This language server provides rich editing support for the STARLIMS Scripting Language (SSL) in any LSP-compatible editor, and a `--validate` CLI for scripts and CI.

---

## Quick Navigation

| Section | Description |
|---------|-------------|
| [Behavior catalog](../catalog/) | **Normative** behavior of every diagnostic, feature, and formatter rule |
| [Status Dashboard](./STATUS.md) | Implementation status, generated from the catalog |
| [Diagnostic Rules](./reference/DIAGNOSTICS.md) | One row per diagnostic code, generated from the catalog |
| [Configuration](./configuration/) | All available configuration options |
| [Features](./features/) | Descriptive guides for each LSP feature |
| [SSL Reference](./ssl-reference/) | SSL language syntax, grammar, and conventions (summary) |
| [Vision](./vision/) | Project goals, architecture, and roadmap |
| [AGENTS.md](../AGENTS.md) | SSL coding conventions for AI agents |

---

## Where the truth lives

When two sources disagree, the one earlier in this chain wins, and the
later one has a bug:

1. **Language rules** — the upstream [`ssl-style-guide`](https://github.com/mahoskye/ssl-style-guide)
   schema (`ssl-style-guide.schema.yaml`) for SSL language and style rules,
   and the published [`starlims-ssl-reference`](https://github.com/mahoskye/starlims-ssl-reference)
   for element behavior (functions, classes, keywords, operators). The LSP
   models STARLIMS 12 as verified by that reference
   ([D17](../catalog/DECISIONS.md)).
2. **Behavior catalog** — [`catalog/`](../catalog/) states what *this tool*
   does about each rule: severity, ranges, false-positive guards,
   configuration. It is normative ([D9](../catalog/DECISIONS.md)) and its
   examples are executed by `go test ./internal/catalog/`.
3. **Code** — must conform to the catalog; a disagreement is a bug in one
   of them, resolved explicitly.
4. **Prose docs** — everything under `docs/` (except the generated
   `STATUS.md` and `reference/DIAGNOSTICS.md`), plus `README.md` and
   `AGENTS.md`. These explain and summarize; they never define behavior.
   If a prose doc contradicts the catalog, fix the prose doc.

New behavior is specified in the catalog first, then implemented
(see [catalog/README.md](../catalog/README.md#lifecycle)).

---

## Project Overview

### What is starlims-lsp?

A Language Server Protocol (LSP) implementation for SSL that provides:

- **Auto-completion** for keywords, functions, classes, and variables
- **Hover information** with function signatures and documentation
- **Signature help** and **parameter-name inlay hints** for function calls
- **Go to definition**, **find references**, and **rename**, including across files in the workspace
- **Document and workspace symbols** for outline and navigation
- **Folding ranges**
- **Code formatting**, including SQL embedded in strings
- **Diagnostics** for syntax errors, compile errors, and style issues

See [STATUS.md](./STATUS.md) for the per-entry status.

### Quick Start

Download a binary from the [releases page](https://github.com/mahoskye/starlims-lsp/releases), or build from a clone:

```bash
make build      # into bin/
make install    # into your Go bin directory
```

See the [main README](../README.md) for full installation instructions.

---

## Document Index

### Vision & Architecture

| Document | Purpose |
|----------|---------|
| [VISION.md](./vision/VISION.md) | Project goals and philosophy |
| [ARCHITECTURE.md](./vision/ARCHITECTURE.md) | LSP architecture and component overview |
| [ROADMAP.md](./vision/ROADMAP.md) | What has shipped and the current direction |

### Feature Guides

Descriptive guides. The normative contract for each feature is its
`catalog/features/` entry, linked from [STATUS.md](./STATUS.md).

| Feature | Guide |
|---------|-------|
| Completion | [completion.md](./features/completion.md) |
| Hover | [hover.md](./features/hover.md) |
| Signature Help | [signature-help.md](./features/signature-help.md) |
| Inlay Hints | [inlay-hints.md](./features/inlay-hints.md) |
| Go to Definition | [definition.md](./features/definition.md) |
| Find References | [references.md](./features/references.md) |
| Rename | [rename.md](./features/rename.md) |
| Document Symbols | [document-symbols.md](./features/document-symbols.md) |
| Workspace Symbols | [workspace-symbols.md](./features/workspace-symbols.md) |
| Formatting | [formatting.md](./features/formatting.md) |
| Diagnostics | [diagnostics.md](./features/diagnostics.md) |
| Folding Ranges | [folding-ranges.md](./features/folding-ranges.md) |
| Snippets | [snippets.md](./features/snippets.md) |

### SSL Language Reference

A bundled public summary. The upstream style guide and SSL reference win
when this summary lags.

| Document | Purpose |
|----------|---------|
| [README.md](./ssl-reference/README.md) | SSL language overview and resources |
| [syntax.md](./ssl-reference/syntax.md) | SSL syntax rules |
| [grammar.md](./ssl-reference/grammar.md) | Formal EBNF grammar |
| [style-guide.md](./ssl-reference/style-guide.md) | Coding conventions |
| [functions.md](./ssl-reference/functions.md) | Built-in functions reference |
| [classes.md](./ssl-reference/classes.md) | Built-in classes reference |
| [data-sources.md](./ssl-reference/data-sources.md) | Data-source files |
| [gotchas.md](./ssl-reference/gotchas.md) | Common SSL pitfalls |

### Configuration

| Document | Purpose |
|----------|---------|
| [CONFIGURATION.md](./configuration/CONFIGURATION.md) | All configuration options |

### Plans and Reviews

Design plans and review write-ups. Entries whose work has shipped carry a
status line at the top saying so; they are kept as a record, not as a
description of current behavior.

| Directory | Contents |
|-----------|----------|
| [plans/](./plans/) | Implementation plans |
| [reviews/](./reviews/) | Review write-ups |

---

## Contributing

See the [main README](../README.md) for contribution guidelines.
