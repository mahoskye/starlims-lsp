# SSL Language Reference

This section provides reference documentation for the STARLIMS Scripting Language (SSL).

> **Quick Reference:** See [AGENTS.md](../../AGENTS.md) in the project root for AI agent coding conventions.

---

## Quick Reference

| Topic | Document | Description |
|-------|----------|-------------|
| [Syntax Overview](./syntax.md) | SSL syntax rules | Keywords, operators, control flow |
| [Formal Grammar](./grammar.md) | EBNF specification | Complete grammar definition |
| [Style Guide](./style-guide.md) | Coding conventions | Naming, formatting, best practices |
| [Built-in Functions](./functions.md) | Function reference | 367+ built-in functions |
| [Built-in Classes](./classes.md) | Class reference | 30 built-in classes |
| [Common Gotchas](./gotchas.md) | SSL pitfalls | Common mistakes and how to avoid them |

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
| Case Sensitivity | Keywords: UPPERCASE. Identifiers: case-insensitive |
| Statement Terminator | Semicolon (`;`) — everything including comments |
| Keywords | Colon-prefixed (`:IF`, `:WHILE`, `:PROCEDURE`) |
| Logical Operators | Period-wrapped (`.AND.`, `.OR.`, `.NOT.`) |
| Comments | Block style: `/* comment text ;` |
| String Literals | Double, single, or bracket quotes |
| Property Access | Colon notation (`object:property`) |
| Array Indexing | 1-based (first element is `[1]`) |

### Example Code

```ssl
:PROCEDURE CalculateTotal;
:PARAMETERS nPrice, nQuantity;
:DEFAULT nQuantity, 1;
:DECLARE nTotal;

nTotal := nPrice * nQuantity;

:IF nTotal > 1000;
    nTotal := nTotal * 0.9;  /* 10% discount;
:ENDIF;

:RETURN nTotal;
:ENDPROC;
```

---

## Quick Syntax Reference

### Essential Patterns

```ssl
/* Variable declaration and assignment;
:DECLARE sName, nCount;
sName := "Test";
nCount := 0;

/* Procedure parameters with defaults;
:PARAMETERS sInput, nValue;
:DEFAULT sInput, "";
:DEFAULT nValue, 0;

/* Conditional;
:IF condition .AND. otherCondition;
    /* code;
:ELSE;
    /* code;
:ENDIF;

/* Loop with 1-based array;
:FOR i := 1 :TO Len(aItems);
    DoProc("ProcessItem", {aItems[i]});
:NEXT;

/* Case statement (EXITCASE required);
:BEGINCASE;
:CASE nVal == 1;
    DoOne();
    :EXITCASE;
:OTHERWISE;
    DoDefault();
    :EXITCASE;
:ENDCASE;

/* Error handling;
:TRY;
    aResults := SQLExecute(sSQL);
:CATCH;
    oErr := GetLastSSLError();
:ENDTRY;

/* Procedure call (never direct);
result := DoProc("MyProcedure", {param1, param2});
result := ExecFunction("Module.MyProcedure", {param1});

/* Object property access;
oEmail:Subject := "Test";
nCount := oDataset:RowCount;
```

---

## LSP Source Files

The Language Server Protocol implementation sources:

| Purpose | File |
|---------|------|
| Function signatures | `internal/constants/signatures.go` |
| Built-in classes | `internal/constants/constants.go` |
| Parser | `internal/parser/parser.go` |
| Diagnostics | `internal/analysis/diagnostics.go` |

---

## Related Documentation

- [Configuration](../configuration/CONFIGURATION.md) - LSP settings
- [Features](../features/) - LSP feature specifications
- [Architecture](../vision/ARCHITECTURE.md) - How the LSP works
