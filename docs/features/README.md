# Feature Guides

This directory contains descriptive guides for each LSP feature implemented by starlims-lsp.

> **Authority:** these guides explain features; they do not define them.
> The normative contract for each feature — its acceptance criteria,
> enforced by tests — is its entry under [`catalog/features/`](../../catalog/features/)
> ([D9](../../catalog/DECISIONS.md)). When a guide and its catalog entry
> disagree, the catalog wins and the guide needs fixing.

New behavior is specified in the catalog first (see
[catalog/README.md](../../catalog/README.md#lifecycle)); a guide here is
optional and written afterwards.

---

## Feature Index

| Feature | Guide | Catalog entry |
|---------|-------|---------------|
| Completion | [completion.md](./completion.md) | [feature.completion](../../catalog/features/completion.md) |
| Hover | [hover.md](./hover.md) | [feature.hover](../../catalog/features/hover.md) |
| Signature Help | [signature-help.md](./signature-help.md) | [feature.signature_help](../../catalog/features/signature_help.md) |
| Go to Definition | [definition.md](./definition.md) | [feature.definition](../../catalog/features/definition.md) |
| Find References | [references.md](./references.md) | [feature.references](../../catalog/features/references.md) |
| Rename | [rename.md](./rename.md) | [feature.rename](../../catalog/features/rename.md) |
| Inlay Hints | [inlay-hints.md](./inlay-hints.md) | [feature.inlay_hints](../../catalog/features/inlay_hints.md) |
| Document Symbols | [document-symbols.md](./document-symbols.md) | [feature.document_symbols](../../catalog/features/document_symbols.md) |
| Workspace Symbols | [workspace-symbols.md](./workspace-symbols.md) | [feature.workspace_symbols](../../catalog/features/workspace_symbols.md) |
| Folding Ranges | [folding-ranges.md](./folding-ranges.md) | [feature.folding](../../catalog/features/folding.md) |
| Formatting | [formatting.md](./formatting.md) | [feature.formatting](../../catalog/features/formatting.md) |
| Diagnostics | [diagnostics.md](./diagnostics.md) | [feature.diagnostics_pipeline](../../catalog/features/diagnostics_pipeline.md) |
| Snippets | [snippets.md](./snippets.md) | [feature.snippets](../../catalog/features/snippets.md) |

Cross-file resolution, which definition, references, and rename share,
has no guide; its contract is
[feature.cross_file_resolution](../../catalog/features/cross_file_resolution.md).

Implementation status for every feature, diagnostic, and formatter rule is
generated from the catalog into [STATUS.md](../STATUS.md).

---

## Quick Links

- [Status Dashboard](../STATUS.md) - Implementation overview
- [Diagnostic Rules](../reference/DIAGNOSTICS.md) - One row per diagnostic code
- [Configuration Reference](../configuration/CONFIGURATION.md) - All settings
- [Architecture](../vision/ARCHITECTURE.md) - Technical overview
