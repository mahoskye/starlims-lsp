# Production-corpus formatting review — 2026-08-28

> Carved into issues (2026-08-28): H1→#215, H2 (+I4 pointer)→#216,
> H3/H4/H5→#217, H6→#218, H7/H8/C1/C3→#219, I1–I6→#220; upstream
> C1/C2 reference-doc challenge→ssl-style-guide#61.

Adversarial review of `--format` / `textDocument/formatting` against a
private production-representative corpus: 6,228 STARLIMS server scripts
and data sources (5,136 formatted as SSL, 1,092 correctly passed through
as SQL-mode data sources). Method: format every file in-process, compare
token streams under two normalizations (whitespace-insensitive "hard" for
content mutations; single-space "soft" for respacing), re-format the
output for idempotence, and re-validate for diagnostic deltas. All four
SQL styles (`canonicalCompact`, `standard`, `compact`, `expanded`) were
additionally exercised on controlled Oracle-flavored and SQL
Server-flavored samples. Findings are numbered H* continuing the F*/G*
series from the 2026-07-22 review. Corpus snippets herein are anonymized;
counts refer to the private corpus.

Headline: **zero panics** across 5,136 files and the F-series comment
guarantees held for ordinary comment positions — but two P0 content
corruptions survive, both at the seam between SSL strings and the SQL
formatter, plus one comment-placement gap the F-series battery missed.

## Severity index

| # | Finding | Class |
|---|---------|-------|
| H1 | Comments deleted in expression-continuation position | P0 corruption (data loss) |
| H2 | SQL reflow inside concatenation-continued `'` literals injects whitespace/newlines into literal content (`{d '`, `IN ('`, `LIKE ('`) | P0 corruption (semantics) |
| H3 | ODBC `{fn …}` escape glued to the next token (`}AS alias`, `}alias`) | P1 emits invalid SQL |
| H4 | `?expr?` placeholders with interior quotes respaced (`?'<<name>>'?` → `? '<<name>>' ?`) despite the preserve-verbatim rule | P1 likely breaks substitution |
| H5 | Case-folding reaches inside ODBC escapes: type names lowercased (`SQL_VARCHAR` → `sql_varchar`), scalar-function casing inconsistent | P2 convention/portability |
| H6 | Non-idempotent output on 1,008/5,136 files (19.6%) | P1 idempotence |
| H7 | `standard` ignores max line length in predicates and open-paren-aligns continuations to ~col 70; `compact` emits half-multiline output | P2 style-engine |
| H8 | Single-line overflow rewrite pads string content (`"select …"` → `" select … "`) | P3 string-value noise |

## Findings

### H1 — comment deletion in expression continuations (P0)

A comment between an operator and its continued operand is silently
dropped:

```ssl
sPath := sBasePath      /*+ sWorkingDir;
sSql  := sSql +         /*sExtraCriteria +;
         sOrderByClause;
```

Both comments vanish from the formatted output. 3 corpus files hit this;
each lost a commented-out alternative the author deliberately kept. The
2026-07-22 follow-up validated comment preservation (A3) for statement
and trailing positions — mid-expression position was not in the battery.
Comments must be preserved in **every** token position or the formatter
must refuse the edit.

### H2 — corruption of concatenation-continued SQL literals (P0)

Production SQL is widely built by splicing values into single-quoted
literals across SSL string concatenation, so individual SSL strings end
(or begin) **inside** an open SQL `'` literal — detectable as an odd
number of `'` characters in the string. The SQL reflow treats the
literal-interior boundary as ordinary SQL and pads it:

```ssl
/* before;
sSql := "update t set d = {d '" + sDate + "'} where id = ?";
sSql := "delete from t where name not in('" + sList + "')";
/* after — literal content changed;
sSql := "
    UPDATE t SET
        d = {d '
" + sDate + "'} where id = ?";
sSql := "
    DELETE FROM t
    WHERE name NOT IN ('
    " + sList + "')";
```

The ODBC date escape becomes `{d '\n2026-…'}` (malformed; per the ODBC
spec the escape interior is driver-parsed syntax) and the IN-list's first
element gains leading whitespace (changed comparison semantics). One
corpus file's mutation was caught by validation as a new error; 403
corpus files contain reformatted odd-quote strings and are exposed to
this class. **Fix shape:** a string whose content has an unbalanced `'`
count is never safely reformattable — byte-preserve it (and ideally its
concatenation siblings). Cheap, total, and loses almost nothing: these
strings are fragments, not statements.

### H3 — ODBC escape gluing (P1)

```sql
SELECT {fn IFNULL(sc.owner, '')}AS owner,
       {fn CONVERT(sc.itemid, sql_varchar)}itemid
```

No space is emitted between the closing `}` of an `{fn …}` escape and
the following alias/token. `}AS` / `}itemid` is not valid SQL on either
target DBMS. Reproduces in every style.

### H4 — placeholder respacing (P1)

`sql_in_strings` promises "`?param?` placeholders are preserved
verbatim", but a placeholder whose interior is a quoted expression is not
recognized as one:

```sql
VALUES (?sRun?, ?nCount?, SYSDATE, ?'<<username>>'?)
/* becomes;
VALUES (?sRun?, ?nCount?, SYSDATE, ? '<<username>>' ?)
```

Whether the runtime still substitutes `? '<<username>>' ?` is unverified;
formatting must not gamble on it. The placeholder scanner should treat
`?…?` spans with arbitrary interior (quotes included) as atomic.

### H5 — case-folding inside ODBC escapes (P2)

Within `{fn …}`: recognized scalar functions are uppercased
(`IFNULL`, `CONVERT`) while unrecognized ones pass through
(`timestampadd`, `current_timestamp`), and ODBC **type names** are
lowercased as if they were column identifiers (`SQL_VARCHAR` →
`sql_varchar`, `SQL_TSI_DAY` → `sql_tsi_day`). The escape interior is
ODBC grammar, not schema identifiers; conventional casing is uppercase.
Rule: inside `{…}` escapes, uppercase the scalar-function name and any
`SQL_*` token; never identifier-fold.

### H6 — non-idempotence on 19.6% of the corpus (P1)

1,008 of 5,136 SSL-formatted files differ when formatted twice. Observed
classes: (a) SQL-string wrap points shifting between passes (the
dominant class — pass 2 re-wraps lines pass 1 produced); (b) skip-comma
spacing oscillation — `RunSQL(…,,);` formats to `…,, );` on the second
pass only; (c) comment-adjacent line drift. This is the corpus-scale
successor to the #103 known-failures ratchet: the fixture list can now be
grown from real files instead of synthetic probes. Idempotence is the
formatter's basic contract — a CI `--check` gate flaps on every one of
these files.

### H7 — `standard` and `compact` style-engine defects (P2)

- `standard` packs every WHERE predicate onto one line (130+ chars,
  ignoring `maxLineLength`) and aligns function continuations under the
  open paren, pushing DECODE arms to column ~70. Open-paren alignment at
  depth is a recognized anti-pattern (alignment breaks under rename,
  drifts with nesting) — continuations should break-and-indent instead.
- `compact` is internally inconsistent: multi-line DECODE arms followed
  by a single ~300-character line holding FROM/WHERE/ORDER BY. Either a
  true one-liner (its plausible contract) or clause breaks — not both.

### H8 — string-value padding on overflow rewrites (P3)

Single-line SQL that overflows the line is rewritten with leading/
trailing spaces inside the string (`"select …"` → `" select … "`, or the
rule-F newline form). DB-neutral, but it changes the runtime string
value (logging, hashing, comparisons) and produces diff noise on 3,310
corpus files. Worth an explicit policy statement in `sql_in_strings`:
when the value must change, prefer the rule-F newline form consistently
and say so; never pad without relayout.

## Convention challenges (researched)

- **C1 — identifier lowercasing is dialect-conditional.** The canonical
  style follows the community default (uppercase keywords, lowercase
  snake_case identifiers — Holywell et al.), which is safe on Oracle
  (unquoted identifiers fold) but **not on SQL Server case-sensitive
  collations**, where `RESULTS` and `results` are different objects.
  STARLIMS environments are specifically Oracle *or* MSSQL, and the
  legacy corpus is uppercase-identifier-dominant. Recommendation: add
  `ssl.format.sql.identifierCase: preserve | lower | upper` with
  **preserve** as default; document `lower` as Oracle-safe/MSSQL-CI-only.
  (Upstream: the canonical-compact reference is titled "Oracle SQL" —
  either scope it or make it dialect-aware.)
- **C2 — dialect first-class citizenship.** Corpus SQL is a clean split:
  Oracle idiom (`SYSDATE`, `DUAL`, `(+)`, `DECODE`, `ROWNUM`) and
  MSSQL/ODBC idiom (`{fn}`, `{d}`, `GETDATE`, TVPs). The formatter
  handles Oracle constructs well and stumbles on the ODBC escape layer
  (H3/H4/H5). Treat `{…}` escapes as atomic spans with their own casing
  rule; add a regression battery per dialect.
- **C3 — post-expression packing.** After a multi-line expression
  (DECODE), canonicalCompact packs `AS alias, next_column` onto the
  closing-paren line — the alias is the hardest thing to find in the
  statement. Break after the closing paren: alias stays with the arm
  block, next column starts a fresh continuation line.
- **C4 — `--check` needs H6 fixed first.** A CI formatting gate over this
  corpus would flap on ~20% of files; idempotence precedes adoption.

## Info-tier proposals (beyond the formatter)

Advisory observations the formatter cannot make, proposed as info-tier
rules (all auto-gated by `ssl.diagnostics.infoDiagnostics`):

- **I1 `sql_comma_join`** — comma-separated FROM lists (`FROM orders o,
  ordtask t WHERE …`); suggest ANSI `INNER JOIN … ON`. Dominant legacy
  pattern in the corpus; the single highest-leverage readability upgrade.
- **I2 `sql_legacy_outer_join`** — Oracle `(+)`; suggest
  `LEFT/RIGHT JOIN`. Oracle-specific advisory; pairs with I1.
- **I3 `sql_inconsistent_alias`** — one SELECT list mixing bare aliases
  (`col alias`) and explicit `AS alias`; suggest uniform explicit `AS`.
- **I4 `sql_literal_splice`** — an odd-quote SQL string (a `'` literal
  continued across concatenation); suggest `?param?` placeholders. Doubly
  valuable: the pattern is H2's corruption surface **and**
  `sql_injection`'s attack surface — the advisory names the idiom before
  either bites.
- **I5 `sql_dialect_mix`** — Oracle-only and MSSQL-only idioms in one
  statement; a portability note since environments are one or the other.
- **I6 `sql_select_star`** — `SELECT *` in embedded production SQL;
  suggest explicit column lists (schema-drift resilience).

## Sources

- [SQL style guide — Simon Holywell](https://www.sqlstyle.guide/)
- [T-SQL style guide — John McCall](https://lowlydba.github.io/tsqlstyle.guide/)
- [SQL Server: case-sensitive collation identifier warning (DSCT01000)](https://learn.microsoft.com/en-us/azure-data-studio/extensions/dsct/conversion-messages/dsct01000)
- [Oracle ORA-00904 and unquoted identifier folding](https://dbplus.tech/en/2025/02/13/ora-00904-invalid-identifier-in-oracle-databases/)
- [ODBC escape sequences — Microsoft Learn](https://learn.microsoft.com/en-us/sql/odbc/reference/develop-app/escape-sequences-in-odbc?view=sql-server-ver16)
