# Project Vision

This document outlines the goals, philosophy, and guiding principles of the starlims-lsp project.

---

## Mission Statement

**Provide first-class IDE support for STARLIMS Scripting Language (SSL) developers through a standards-compliant Language Server Protocol implementation.**

---

## Project Goals

### 1. Rich Editing Experience

Enable SSL developers to work with the same productivity features available in modern programming languages:

- Intelligent auto-completion
- Contextual hover documentation
- Go-to-definition navigation
- Find all references
- Code formatting
- Real-time error detection

### 2. Editor Agnosticism

By implementing the Language Server Protocol, we support any LSP-compatible editor:

- Visual Studio Code
- Neovim
- Sublime Text
- Emacs
- JetBrains IDEs
- And many more

### 3. Correctness Through Specification

Every feature is thoroughly documented before implementation. This ensures:

- Consistent behavior
- Complete test coverage
- Clear expectations for users
- Maintainable codebase

### 4. SSL Language Awareness

Deep understanding of SSL's unique characteristics:

- Case-insensitive syntax
- Colon-prefixed keywords (`:IF`, `:PROCEDURE`)
- Period-wrapped operators (`.AND.`, `.OR.`)
- Semicolon-terminated comments (`/* comment ;`)
- Dynamic typing with Hungarian notation conventions
- Embedded SQL in string literals

---

## Document-Driven Development

This project follows a **document-driven approach** where documentation is not an afterthought but a core part of the development process.

### Principles

1. **Spec First, Code Second**
   - Every feature has a specification document
   - The spec defines expected behavior, edge cases, and test scenarios
   - Implementation follows the specification

2. **Documentation as Contract**
   - Feature documents serve as contracts between the code and users
   - If behavior differs from documentation, it's a bug
   - Documentation updates require conscious decision-making

3. **Test Scenarios in Specs**
   - Each feature document includes executable test scenarios
   - Tests validate that implementation matches specification
   - Edge cases are documented and tested

4. **Living Documentation**
   - Documents evolve with the codebase
   - Version history tracks significant changes
   - Deprecated features are marked, not silently removed

### Benefits

- **Clarity:** New contributors understand expected behavior
- **Quality:** Specifications catch design issues early
- **Maintenance:** Clear docs make refactoring safer
- **Trust:** Users can rely on documented behavior

---

## Design Principles

### 1. Fail Gracefully

The language server should never crash or hang. When encountering invalid input:

- Continue analysis after errors
- Provide partial results where possible
- Log issues for debugging without disrupting the user

### 2. Fast Response Times

IDE features must be responsive. Targets:

- Completion: < 100ms
- Hover: < 50ms
- Formatting: < 500ms for typical files
- Diagnostics: < 200ms after typing stops

### 3. Minimal Configuration

Work well out of the box with sensible defaults:

- Tab indentation (STARLIMS convention)
- Standard formatting rules
- Essential diagnostics enabled
- Optional features opt-in

### 4. Respect User Intent

When formatting or transforming code:

- Preserve intentional structure where possible
- Don't remove user comments
- Maintain semantic meaning
- Apply consistent style without being destructive

### 5. SSL-Specific Understanding

Honor SSL's unique characteristics:

- Case-insensitivity in comparisons
- Block comments ending with semicolons
- Property access syntax (`:PropertyName`)
- Dynamic variable creation via assignment

---

## Scope and Boundaries

### In Scope

| Capability | Description |
|------------|-------------|
| Text Document Features | Completion, hover, signature help, formatting, diagnostics |
| Navigation | Go to definition, find references, document symbols |
| Single-File Analysis | Full analysis within open documents |
| SSL Syntax | Complete SSL language support |
| Embedded SQL | Formatting and basic analysis of SQL in strings |

### Out of Scope (Currently)

| Capability | Reason |
|------------|--------|
| Cross-File Analysis | Requires workspace indexing infrastructure |
| STARLIMS Runtime | No connection to STARLIMS server |
| Database Schema | No access to database structure |
| Debugging | Separate concern from language server |
| Build/Deploy | Not a language server responsibility |

### Future Considerations

| Capability | Notes |
|------------|-------|
| Workspace Indexing | Would enable cross-file navigation |
| Inlay Hints | Parameter name hints in function calls |
| Semantic Tokens | Enhanced syntax highlighting |
| Call Hierarchy | Incoming/outgoing call analysis |

---

## Target Users

### Primary: STARLIMS Developers

Developers writing SSL code in enterprise laboratory environments:

- Need reliable code assistance
- Work with large, complex codebases
- Value consistency and quality
- May not have prior LSP experience

### Secondary: Tool Builders

Teams building STARLIMS development tools:

- Extension developers
- CI/CD pipeline builders
- Code quality tool authors

---

## Success Metrics

### User Experience

- Zero crashes during normal operation
- Accurate completions for SSL built-ins
- Correct formatting that respects SSL conventions
- Helpful diagnostics that catch real issues

### Technical Quality

- Comprehensive test coverage
- Clear, maintainable code
- Complete documentation
- Responsive performance

### Community

- Easy onboarding for new contributors
- Clear contribution guidelines
- Responsive issue handling
- Regular releases

---

## Related Documents

- [ARCHITECTURE.md](./ARCHITECTURE.md) - Technical architecture overview
- [ROADMAP.md](./ROADMAP.md) - Prioritized feature roadmap
- [../STATUS.md](../STATUS.md) - Current implementation status
