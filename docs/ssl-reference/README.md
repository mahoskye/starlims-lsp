# SSL Language Reference

This section provides reference documentation for the STARLIMS Scripting Language (SSL). Rather than duplicating content, these documents point to comprehensive reference materials maintained in the `ssl-style-guide` directory.

---

## Quick Reference

| Topic | Document | Source |
|-------|----------|--------|
| [Syntax Overview](./syntax.md) | SSL syntax rules | `ssl_agent_instructions.md` |
| [Formal Grammar](./grammar.md) | EBNF specification | `ssl-ebnf-grammar-complete.md` |
| [Style Guide](./style-guide.md) | Coding conventions | `abbot-starlims-style-guide.md` |
| [Built-in Functions](./functions.md) | Function reference | `ssl-unified-master-source-complete.json` |
| [Built-in Classes](./classes.md) | Class reference | Various sources |
| [Common Gotchas](./gotchas.md) | SSL pitfalls | `ssl_agent_instructions.md` |

---

## Source Materials

The comprehensive SSL reference materials are located in:

```
ssl-style-guide/
├── reference/
│   ├── ssl_agent_instructions.md      # Complete SSL v11 reference
│   ├── abbot-starlims-style-guide.md  # Official STARLIMS conventions
│   ├── ssl-ebnf-grammar-complete.md   # Formal EBNF grammar
│   └── ssl-unified-master-source-complete.json  # Function signatures
└── ssl-style-guide/
    ├── ssl-style-guide.schema.yaml    # Machine-readable style rules
    └── tree-sitter-ssl/               # Tree-sitter grammar
```

---

## SSL Language Overview

### What is SSL?

STARLIMS Scripting Language (SSL) is a domain-specific language used within the STARLIMS Laboratory Information Management System (LIMS). It combines features from various languages:

- **Syntax:** Similar to xBase/Clipper with colon-prefixed keywords
- **Typing:** Dynamic typing with optional Hungarian notation
- **Comments:** Block comments using `/* ... ;` syntax
- **Operators:** Logical operators wrapped in periods (`.AND.`, `.OR.`)

### Key Characteristics

| Feature | Description |
|---------|-------------|
| Case Sensitivity | Case-insensitive (keywords, identifiers) |
| Statement Terminator | Semicolon (`;`) |
| Keywords | Colon-prefixed (`:IF`, `:WHILE`, `:PROCEDURE`) |
| Logical Operators | Period-wrapped (`.AND.`, `.OR.`, `.NOT.`) |
| Comments | Block style: `/* comment text ;` |
| String Literals | Single or double quotes (`"text"` or `'text'`) |
| Property Access | Colon notation (`object:property`) |

### Example Code

```ssl
:PROCEDURE CalculateTotal;
:PARAMETERS nPrice, nQuantity;
:DECLARE nTotal;

nTotal := nPrice * nQuantity;

:IF nTotal > 1000;
    nTotal := nTotal * 0.9;  /* 10% discount ;
:ENDIF;

:RETURN nTotal;
:ENDPROC;
```

---

## Related Documentation

- [Configuration](../configuration/CONFIGURATION.md) - LSP settings
- [Features](../features/) - LSP feature specifications
- [Architecture](../vision/ARCHITECTURE.md) - How the LSP works
