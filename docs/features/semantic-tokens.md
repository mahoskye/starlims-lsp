# Semantic Tokens

> **Normative source:** [`feature.semantic_tokens`](../../catalog/features/semantic_tokens.md) in the behavior catalog, and [D20](../../catalog/DECISIONS.md) for why it covers names only. This page is a guide; when it disagrees with the catalog entry, the entry wins.

**Status:** IMPLEMENTED  
**Methods:** `textDocument/semanticTokens/full`, `textDocument/semanticTokens/range`  
**Source Files:** `internal/providers/semantic_tokens.go`, `internal/server/semantic_tokens.go`

---

## 1. Overview

A TextMate grammar sees text, so it paints every bare name the same. The
server knows what each name is. It sends tokens for names only; keywords,
operators, literals, strings and comments stay with the grammar, which also
keeps colouring any name the server sends nothing for.

The legend, from the `initialize` result:

```json
{
  "semanticTokensProvider": {
    "legend": {
      "tokenTypes": ["function", "method", "parameter", "variable", "property", "class"],
      "tokenModifiers": ["declaration", "defaultLibrary", "readonly", "public", "unknown"]
    },
    "full": true,
    "range": true
  }
}
```

`public` and `unknown` are this server's own modifiers. A client that does not
know them still reads the token as a `variable`.

---

## 2. What a name gets

```ssl
:PROCEDURE Calc;               /* Calc: function, declaration;
:PARAMETERS nQty;              /* nQty: parameter, declaration;
:DECLARE nTotal;               /* nTotal: variable, declaration;
:PUBLIC gRate;                 /* gRate: variable, public, declaration;
nTotal := nQty * gRate;        /* variable, parameter, variable public;
:RETURN Round(nTotal + nOops); /* Round: function, defaultLibrary; nOops: variable, unknown;
:ENDPROC;
```

A class file's procedures are `method`. Predefined globals and status keywords
are `variable` with `defaultLibrary` and `readonly`. After `:`, a name followed
by `(` is a `method` and any other a `property`. `Email{}` is a `class`, with
`defaultLibrary` because the reference lists it.

A `DoProc` or `ExecFunction` target that resolves is a `function` over the
string's content, quotes excluded. One that does not resolve has no token, so
the string's own colour shows it does not.

---

## 3. Things a client can rely on

- **`unknown` agrees with `undeclared_variable`**, whether or not that check
  is on. The check reports a name once; the token marks each undeclared use.
  It is never set in a data source, nor in a file with an `:INCLUDE` the
  server could not read.
- **No token means the grammar's colour stands.** That is what a statement
  that does not parse, a name after `:CLASS` or `:LABEL`, and a plain SQL
  file get.
- **Positions and lengths are UTF-16**, and a token lies on one line.
- **`range` is a filter.** The server classifies the whole document either
  way, because a name's meaning depends on declarations elsewhere in it; a
  range only limits what is sent.
- **While the workspace index is being built**, a target in a file not yet
  read has no token. The next request corrects it.

The server offers no delta, and no setting. An editor's own switch
(`editor.semanticHighlighting.enabled` in VS Code) turns tokens off.
