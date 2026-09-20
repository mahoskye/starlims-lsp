---
id: diag.unexpected_token
title: Unexpected token in a statement
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/parser/expr_test.go
  - internal/providers/unexpected_token_test.go
  - cmd/starlims-lsp/validate_test.go
history:
  - date: 2026-09-19
    ref: "issue #240"
    note: >-
      Specified after reproducing #240 against v0.21.0: `:FOR EACH x IN y;`,
      a line of English prose, `foo bar baz;`, and a statement missing its
      `;` before the next line all validated clean. The parser already
      computes statement completeness (StatementExprs.Complete) and knows
      the first leftover token, but nothing surfaces it — the only consumer
      used the flag to suppress call checks. Maintainer framing: this is a
      syntax-level error independent of declaration state. LIMS accepts
      undeclared variables; what it rejects is a token after an expression
      that is not a valid continuation. So it is default-on at error
      severity per DECISIONS.md D5 and never routes through
      undeclared_variable.
  - date: 2026-09-19
    ref: "issue #240 (implementation)"
    note: >-
      Implemented on the expression AST: the expression parser now records
      the first token it could not accept, the statement extractor turns
      that plus any leftover into StatementExprs.Unexpected/Expected, and
      checkUnexpectedTokens owns the wording and the deferrals. Two things
      the spec-first pass surfaced and the code honors: a bare `:RETURN;`
      is an optional operand, not a missing one; and the deferral list is
      wider than first written — unknown_keyword, scientific_notation,
      and the delimiter rules also name tokens this rule would otherwise
      report twice.
  - date: 2026-09-19
    ref: "issue #240 (production-corpus gate)"
    note: >-
      First run over 6,228 production files: 26 hits, no other rule's
      count moved. 20 of the 26 were assignment used as an expression —
      chained `a := b := c`, an element of an array literal, a `:RETURN`
      value — which that codebase uses freely and LIMS compiles, so the
      AST now parses assignment as an expression in every value position
      and the entry says so. One was a line-leading member call the lexer
      reads as a keyword, already a false positive of unknown_keyword;
      this rule now defers to it. The remainder are true positives: empty
      `:DEFAULT x,;` values and `*****` banner lines standing outside a
      header comment that a stray `;` ended early (the style guide has no
      `*` comment form, so those lines are code to LIMS).
  - date: 2026-09-20
    ref: "issue #240 follow-up"
    note: >-
      The lexer now reads a line-leading `:Method(` after a receiver as
      member access, so the call-chain fence holds by being complete
      rather than by deferring to unknown_keyword; the deferral itself
      stays for genuine unknown keywords.
  - date: 2026-09-20
    ref: "issue #244"
    note: >-
      Unary plus settled: SSL's unary operators are `-`, `!`, and `.NOT.`
      only (ssl-ebnf-grammar.md, UnaryOperator), and a numeric literal
      never carries a leading sign, so `x := +5;` does not compile. The
      flag the parser already produced is now pinned by a fence and a
      test rather than left incidental.
  - date: 2026-09-20
    ref: "issue #246"
    note: >-
      The scientific_notation deferral narrowed to the shapes that rule
      actually reports (a number with no decimal point, or one starting
      with the point, glued to an `e` identifier). A well-formed number
      glued to a stray `E` — `2.0E+nVar`, which used to defer to a rule
      that then stayed silent — is now reported here.
issues: ["#240", "#244", "#246"]
---

## Behavior

Flags the first token in a statement that the SSL statement grammar cannot
accept at that position. A statement's tokens are consumed as one
grammatical statement; when consumption stops before the terminating `;`
(or before end of file when there is no terminator), the token it stopped
on is unexpected. The range covers that token and the message names its
class, its text, and what was expected
(`Unexpected identifier 'is' - expected an operator or ';'`).

The statements in scope are the ones the parser models:

- expression and assignment statements (`nX := expr;`, `DoProc(...);`,
  `nX++;`). After a complete expression the only acceptable continuations
  are an operator, an assignment operator, a call `(`, a subscript `[`,
  member access `:`, a constructor `{`, or the `;`;
- keyword statements that carry an expression: `:IF`, `:WHILE`, `:CASE`,
  `:RETURN`, `:DEFAULT`;
- the `:FOR` header, whose only accepted shape is
  `:FOR <identifier> := <expr> :TO <expr> [:STEP <expr>];`. The first token
  that departs from that shape is flagged and the message names the
  expected piece (`Unexpected identifier 'nItem' in ':FOR' header -
  expected ':='`). When the loop-variable position holds the word `EACH`
  the message also says SSL has no `:FOR EACH` form. Block pairing is
  independent of header validity: a malformed header still opens its
  `:FOR` block, so `unclosed_block` and `mismatched_block_end` are
  unaffected.

A required operand that is missing is unexpected at whatever token stands
in its place, including the `;` itself (`nCount := ;`, `:IF;`). Where the
operand is optional the terminator is expected: `:RETURN;` is complete.
SSL has no unary plus — its unary operators are `-`, `!`, and `.NOT.`, and
a numeric literal never carries a leading sign — so in `x := +5;` the `+`
is unexpected where an expression was due (issue #244).

Assignment is an expression wherever an operand can stand. `a := b := c;`,
`oObj:Prop := oObj:Items[1] := x;`, `:RETURN x := .T.;`, `{ .T., n += 1 }`,
`DoProc("P", {a := 1})`, and `(i += 1) <= n` all parse in full — the tree
carries the assignment as a binary node nesting to the right — so an
assignment operator is unexpected only where no operand precedes it.

One diagnostic per statement, on the first unexpected token. The rest of
the statement through its `;` is skipped and checking resumes at the next
statement, so one bad line never cascades. When the unexpected token is
the first significant token on its line, the message adds that the
previous statement may be missing its `;`.

Declaration state is irrelevant. An undeclared identifier inside a
well-formed expression belongs to `undeclared_variable` (opt-in); a
declared identifier followed by a token that cannot act on it belongs
here. Neither rule consults the other.

Always on; there is no configuration key. Per-rule severity or `off` is
available through `ssl.diagnostics.rules`.

It must NOT flag:

- prose inside a comment or a string literal — that is literal text
  (DECISIONS.md D7);
- a statement continued across lines. Newlines are whitespace, so
  `nTotal := nA` / `+ nB;` and a `:FOR` header broken before `:TO` are
  single well-formed statements;
- a statement whose first unexpected token is already reported by a
  lexer- or operator-level rule: an unknown token (`unknown_token`,
  including the `.identifier` shape that `dot_property_access` owns), a
  C-style operator or glued `===`/`!==` pair
  (`invalid_operator_sequence`), a bare `AND`/`OR`/`NOT` word in operator
  position (`bare_logical_operator`), a colon form that is not an SSL
  keyword (`unknown_keyword` / `endfor_invalid`), the exponent shapes
  `scientific_notation` owns — an identifier glued to a number that has
  no decimal point (`7e2`, `9E+1`) or starts with one (`.5e1`) — a
  closer with no matching opener (`unmatched_delimiter` /
  `mismatched_delimiter`), or any token inside a statement whose opener is
  never closed (`unclosed_delimiter`, which swallows the rest of the file
  into that statement). One finding per cause. A closer that does match
  but arrives where an operand was due (`x := (1 + );`) is this rule's. A
  `:=` inside a condition is an assignment expression, complete and
  `assignment_in_condition`'s to warn about;
- a bare expression statement such as `This;` — nothing follows the
  expression but its terminator;
- a final statement that ends at end of file with no `;`. No token follows
  it, so nothing is unexpected; a missing-terminator rule would be its own
  entry;
- data-source files, for which the rule is off entirely — their SSL is
  directive syntax outside the expression grammar and their bodies are
  SQL (feature.diagnostics_pipeline) — and `:REGION` bodies, which the
  lexer never reads as SSL.

An unterminated string literal swallows the rest of the file and hides
everything this rule would otherwise see. That is a lexer condition and
wants its own entry.

## Examples

### Flags

```ssl
:DECLARE nCount;
nCount := 1;
This is a bunch of text but it does not evaluate as wrong
nCount := 2;
```

### Flags

```ssl
:DECLARE aItems, nTotal;
:FOR EACH nItem IN aItems;
	nTotal += nItem;
:NEXT;
```

### Flags

```ssl
:DECLARE i;
:FOR i := 1 10;
:NEXT;
```

### Flags

```ssl
:DECLARE nCount;
nCount := 1
nCount := 2;
```

### Does not flag

```ssl
:DECLARE i, aItems;
:FOR i := 1 :TO Len(aItems);
:NEXT;
:FOR i := 10 :TO 1 :STEP -1;
:NEXT;
```

### Does not flag

```ssl
:DECLARE sText;
/* This is a bunch of text but it does not evaluate as wrong;
sText := "This is a bunch of text but it does not evaluate as wrong.";
```

### Does not flag

```ssl
:DECLARE nTotal, nA, nB, i, nMax;
nTotal := nA
	+ nB;
:FOR i := 1
	:TO nMax;
:NEXT;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	nTotal := nMissing + 1;
	:RETURN;
:ENDPROC;
```

### Does not flag

```ssl
:DECLARE nTotal, oObj, bA, bB;
nTotal := oObj.Value;
:IF bA AND bB;
	nTotal := 1;
:ENDIF;
```

### Does not flag

```ssl
:DECLARE nCount, nTotal, x;
:IF nCount := 1;
	nTotal := 7e2;
:ENDIF;
x := (1 + 2;
```

### Does not flag

```ssl
:DECLARE a, b, c, x, n, oObj, txt, sTitle;
a := b := c;
oObj:Prop := oObj:Items[1] := x;
:RETURN { .T., n += 1 };
:RETURN txt:ToString()
	:Replace("##TITLE##", sTitle);
```

### Flags

```ssl
:DECLARE x;
x := (1 + );
```

### Flags

```ssl
:DECLARE x;
x := +5;
```

### Flags

```ssl
:DECLARE nX, nVar;
nX := 2.0E+nVar;
```

## Rationale

Code that reaches this rule cannot compile. LIMS rejects a token the
grammar cannot place, whatever the identifiers around it are declared as,
and per DECISIONS.md D5 that is the definition of a default-on error. It
is also the one class of finding that has to flip the `--validate` CLI's
`valid` flag, which warnings never do. The rule was specified after #240
showed the parser already knew each of the four Flags fences was
incomplete and threw the knowledge away.

Two decisions keep it quiet. The anchor is the single token consumption
stopped on, with recovery at the statement's `;`, so nothing cascades. And
it defers to any lexer- or operator-level rule that already names the same
token, so a user sees one finding per mistake. Declaration-independence
is deliberate: `undeclared_variable` stays opt-in because LIMS tolerates
what it reports; this rule reports only what LIMS refuses.
