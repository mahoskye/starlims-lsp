---
id: fmt.sql_in_strings
title: SQL formatting inside string literals
kind: formatter
status: active
authority: tool
schema_ref: null
config:
  - ssl.format.sql.enabled
  - ssl.format.sql.style
  - ssl.format.sql.keywordCase
  - ssl.format.sql.indentSize
  - ssl.format.sql.maxLineLength
  - ssl.format.sql.detectSQLStrings
tests:
  - internal/providers/sql_formatter_test.go
history:
  - date: 2025-11-19
    ref: "vs-code-ssl-formatter v1.1.0, issues #8/#28"
    note: >-
      Original extension policy: never modify content inside string
      literals — spacing and indentation in strings are literal text.
  - date: 2025-12-05
    ref: "vs-code-ssl-formatter PRs #50/#51 (v1.2.x)"
    note: >-
      Policy reversed for detected SQL: SQL formatting inside strings
      enabled by default on the canonicalCompact engine.
  - date: 2026-05-01
    ref: "vs-code-ssl-formatter#64; commits 486e596..f387b3e, dc510af (v0.7.3)"
    note: >-
      LSP re-guarded the same policy after the formatter mangled
      single-line SQL assignments: single-line SQL that fits its line is
      left untouched; rules A–F pin the multi-line layout.
  - date: 2026-07-22
    ref: "issue #81"
    note: >-
      Bracket-quoted strings reflow with a ']' closer. The reflow used to
      write the opening delimiter at both ends, leaving an unterminated
      bracket string that swallowed the rest of the file on the next pass.
  - date: 2026-07-22
    ref: "issue #82"
    note: >-
      Detection hardened against English prose: runs of three or more
      bare words, prose-shaped SELECT lists, and SET/target clauses
      without SQL shape are rejected; only argument 0 of a known SQL
      function is a SQL candidate.
  - date: 2026-08-07
    ref: "issue #140"
    note: >-
      Split SQL-string assignments rejoin: a source line break between
      ':=' and a detected SQL string, or between the string and its ';',
      was preserved forever — the layout depended on how the input
      happened to be split instead of converging on rule F / the
      fits-inline form. Single line breaks at those two seams now join
      before layout so every input reaches the same canonical shape.
  - date: 2026-08-28
    ref: "issue #216 (production-corpus formatting review H2)"
    note: >-
      Odd-quote guard added: concatenation-continued character literals
      were being respaced inside the literal (newlines injected into
      {d '...'} date escapes, IN/LIKE patterns gaining whitespace — one
      corpus mutation surfaced as a new validation error). Detected-SQL
      strings with an unbalanced single-quote count are now
      byte-preserved.
  - date: 2026-08-28
    ref: "issue #218 (production-corpus idempotence)"
    note: >-
      SQL '--' line comments force a line break before the next token:
      the reflow was gluing following code into the comment ('-- note' +
      'NOT EXISTS' -> '-- noteNOT EXISTS'), which a DBMS reads as the
      predicate swallowed by the comment — a content-semantics hazard
      found via the idempotence sweep.
  - date: 2026-08-28
    ref: "issue #217"
    note: >-
      ODBC escapes and placeholders made atomic (issue #217, production-
      corpus review H3/H4/H5): '}' no longer glues to a following alias,
      placeholder spans with quoted interiors are byte-preserved instead
      of respaced, scalar-function names after '{fn' and SQL_* type
      names are uppercase, and escape interiors keep the author's
      identifier casing.
  - date: 2026-08-28
    ref: "issue #219 (style-review decisions)"
    note: >-
      2026-08-28 style-review decisions (issue #219, corpus owner):
      identifierCase preserve|lower|upper added with preserve default
      (force-folding is wrong on SQL Server CS collations and rewrites
      the uppercase house style); 'compact' retired as a deprecated
      alias for canonicalCompact; 'standard' now respects MaxLineLength
      (proactive wrapping) with fixed one-level continuation indent
      instead of open-paren alignment; rewrites always take the rule-F
      multi-line form, never in-place padding; and the SELECT item after
      a multi-line group starts a fresh continuation line so aliases
      stay with their block (C3).
  - date: 2026-10-13
    ref: "issue #271"
    note: >-
      CASE lays out the same way in every clause, as the compact SQL
      reference shows it: outside the SELECT list CASE used to move to
      its own line at the clause column with WHEN/ELSE only 2 under it
      (in UPDATE SET, left of its own assignment), a nested CASE in a
      THEN lost its indent, and THEN values never broke. CASE now stays
      where it is written, WHEN/ELSE sit 4 past it, END aligns with it,
      and THEN values break 4 past WHEN as a set. A sign after a keyword
      such as ELSE stays attached ('ELSE -1' had become 'ELSE - 1').
  - date: 2026-10-13
    ref: "issue #286"
    note: >-
      A WHEN predicate that fits on its WHEN line keeps its AND/OR inline,
      as the style guide's §5.2 example does; only an overflowing
      predicate breaks before each AND/OR (Rule B). Corpus: 30 of 6,228
      files format differently, all idempotent.
  - date: 2026-10-13
    ref: "issue #327"
    note: >-
      A complex placeholder (`?sPrefix + sSuffix?`) is atomic. The SQL
      lexer's lookahead stopped at the first space or operator, so the
      reflow respaced its interior (`? sPrefix + sSuffix ?`), changing the
      expression SSL binds.
  - date: 2026-10-13
    ref: "issue #327"
    note: >-
      The SQL engine measures the SSL indent it sits at in columns, a tab
      as ssl.format.indentSize. It counted the indent string's bytes, a
      tab as one column, so packed SQL lines inside a procedure reached
      91-94 columns.
  - date: 2026-10-13
    ref: "issue #327"
    note: >-
      Detection gaps closed: a statement led by a SQL comment or hint, one
      holding COLLATE (now a keyword, so `col COLLATE name` is no longer a
      three-bare-word prose run), and one naming double-quoted identifiers
      (`FROM "Orders"`) are SQL. A block comment stays separated from the
      code after it. The fits-inline test counts the code that must stay
      on the line after the string (`");`, up to the first comma or
      operator break), so a query ending exactly at the limit is reflowed
      instead of the statement wrapping after ':='.
issues: ["#81", "#82", "#327"]
---

## Behavior

This entry owns the *string boundary* — when a string is handed to the
SQL engine and how the quotes and surrounding SSL lay out (rules A–F
below). The multi-line SQL layout itself (clause breaks, indents,
INSERT/VALUES/DECODE/MERGE/CTE shapes) is governed by
`sql-canonical-compact-reference.md` in the ssl-style-guide repo — the
authoritative S-rules; layout fixes cite that document, not this entry.

String literals are literal text and are never reformatted — with one
deliberate exception: a string whose content is detected as a SQL statement
is formatted by the SQL engine when `ssl.format.sql.enabled` is on
(default). Detection applies to any string literal when
`ssl.format.sql.detectSQLStrings` is on (default); with it off, only
strings inside known SQL function calls (SQLExecute, LSearch, RunSQL, …)
are considered. Within the exception:

- Single-line SQL that fits within `ssl.format.maxLineLength` on its
  current line is left exactly as written — including its keyword casing
  and spacing (vs-code-ssl-formatter#64).
- Single-line SQL that overflows the line, and any SQL string already
  containing newlines, is reflowed to the configured
  `ssl.format.sql.style` / `ssl.format.sql.keywordCase` /
  `ssl.format.sql.identifierCase` (defaults: canonicalCompact, upper,
  preserve). A rewrite ALWAYS takes the rule-F multi-line form — a
  changed string is never emitted as a padded single line (issue #219
  decision: the string's runtime value changes only when real relayout
  happens). The opening quote stays on the assignment line
  (rule F), the SQL starts on the next line with each clause indented by
  `ssl.format.sql.indentSize` spaces past the statement's base indent, and
  the closing quote lands on its own line at the base indent, glued to the
  trailing punctuation (rule E).
- A string whose content holds an unbalanced (odd) number of single
  quotes is byte-preserved even when it is detected as SQL: it ends or
  begins inside an open SQL character literal continued across SSL
  concatenation (`"… where d = {d '" + sDate + "'}"`), and any respacing
  there rewrites literal content — broken ODBC `{d '…'}` escapes,
  `IN ('`/`LIKE ('` patterns gaining whitespace (issue #216, production
  corpus: 403 exposed files). Fragments are not statements; skipping
  them loses nothing.
- Strings not detected as SQL are byte-preserved regardless of settings.
  Detection is structural and rejects English prose even when it contains
  SQL trigger words: a run of three or more consecutive bare words, a
  SELECT list that is not `*` / a single expression / comma-separated, a
  SET clause with no `ident =` shape, or a FROM/INTO target followed by
  more prose all disqualify the string (issue #82).
- Within a known SQL function call, only the SQL argument (position 0) is
  ever a candidate — friendly names, LSearch default values, and parameter
  arrays are byte-preserved even when they look like SQL (issue #82).
- SQL parameter placeholders (`?param?`) are preserved verbatim — as an
  atomic span whatever the interior holds, quoted content included
  (`?'<<name>>'?`), and a complex expression too (`?sPrefix + sSuffix?`,
  recognized by the same grammar complex_sql_placeholder uses, so the
  diagnostic and the formatter agree on its extent; issue #327). A
  positional `?` is never merged with what follows it: an interior holding
  `=`, `<` or `>` is not a placeholder (`a=?AND b=?`). (`?'<<name>>'?` is
  a corpus pattern of dubious validity: the formatter
  neither legitimizes nor rewrites it; byte-preservation is the only
  safe treatment for suspect spans, and flagging them is diagnostic
  territory, not layout).
- ODBC escape sequences (`{fn …}`, `{d …}`, `{ts …}`, …) are atomic
  spans of ODBC grammar, not schema identifiers (issue #217): a space
  separates the closing `}` from a following token (`… )} AS alias`);
  the marker is canonical lowercase; the scalar-function name after
  `{fn` and any `SQL_*` token are uppercase; every other interior token
  keeps the author's casing — escape interiors are never
  identifier-folded.
- Split assignments converge (issue #140): a single source line break
  between `:=` and a detected SQL string, and between the string and its
  terminating `;`, is joined before layout — so
  `sSQL :=` / `"SELECT ..."` / `;` on three lines reaches the same
  canonical result as the one-line form (inline when it fits, rule F
  when it doesn't). Non-SQL strings keep their user line breaks, and
  blank lines (two or more newlines) at those seams are never joined.

- CASE expressions lay out the same way in every clause (issue #271,
  reference §2.3, §2.10, §5.1–5.3). A multi-line CASE stays where it is
  written — `WHERE CASE`, `ORDER BY CASE`, `SET col = CASE`, after
  `AND` or an operator — with three exceptions: a CASE item following a
  comma in the SELECT list starts its own line at the SELECT-list
  column; a CASE that is a THEN/ELSE value starts its own line 4 past
  its WHEN/ELSE; and a CASE written too far right for its WHEN lines
  (or whose THEN values fit only once moved) continues on the next
  line like any other wrap. WHEN/ELSE sit 4 columns past CASE, END
  aligns with CASE. A WHEN predicate stays on its WHEN line when
  `WHEN <predicate> THEN` fits there; otherwise it breaks before each
  top-level AND/OR, which sit one indent past WHEN (Rule B). A predicate
  holding a subquery always breaks. THEN values are inline or broken as a set: when
  any branch would overflow with its value inline, or any value is
  itself a CASE, every THEN value of that CASE starts its own line 4
  past its WHEN (a value opening a subquery stays on the THEN line —
  subqueries open on their parent line). An ELSE value stays inline
  unless it overflows or is a CASE. A predicate that leaves no room for
  THEN wraps THEN to the WHEN column with the value following it. A
  CASE that fits on its line inside a function's argument list or
  other parenthesized expression (`SUM(CASE WHEN … END)`), or inside
  an `OVER (…)` spec, stays inline whole. Layout never alters a token:
  a sign after an operand-introducing keyword stays attached
  (`ELSE -1`, `THEN -x`), while `END - 1` remains a subtraction.
- Scope: this entry covers SQL inside SSL string literals. A data
  source whose whole body is SQL (SQL mode, DECISIONS.md D15) is not
  reformatted at all — not by the editor's formatting requests and not
  by `--format`, which applies the same content classifier. That
  pass-through is intended and is specified by feature.formatting A9:
  formatting such bodies with the SQL engine is deferred until the SQL
  lexer understands Oracle-style `:bind` variables.

## Examples

Short SQL that fits its line is untouched, casing and all; non-SQL strings
are always untouched:

### Idempotent

```ssl
sSql := "select * from users where id = ?id?";
sMsg := "sample not found for the given identifier";
```

English strings containing SQL trigger words are byte-preserved whether
multi-line or over-long single-line — under the pre-#82 detector these
were rewritten as SQL (the over-long fence also relies on the #85 no-gain
wrap guard; SQL-function default-value arguments are pinned by Go tests):

### Idempotent

```ssl
sMsg := "Select the samples from the rack
and update the status column";
sMsgA := "Select the samples from the rack and update the status column before continuing with the run";
```

Concatenation-continued character literals (odd single-quote count) are
byte-preserved even though the content is detected as SQL — respacing
would rewrite the `{d '…'}` escape interior and the IN-list pattern
(issue #216). Over-long lines of this shape may still wrap at the `+`
(ordinary SSL wrapping); the string bytes themselves never change:

### Idempotent

```ssl
sSql := "update runs set fromsampd = {d '" + sDate + "'} where sessionid = ?";
sSql2 := "delete from quotedetails where product not in('" + sList + "')";
```

ODBC escapes format as atomic spans — `}` separated from a following
alias, `{fn` names and `SQL_*` tokens uppercase, interior identifiers
keeping the author's casing — and a placeholder's interior is never
respaced:

### Before

```ssl
aRows := SQLExecute("select {fn ifnull(sc.owner,'')} as owner, {fn convert(SC.ITEMID, SQL_VARCHAR)}itemid from limssourcecontrol sc where sc.owner = ?sUser? and sc.moddate > {fn timestampadd(SQL_TSI_DAY, -30, current_timestamp)}");
```

### After

```ssl
aRows := SQLExecute("
    SELECT {fn IFNULL(sc.owner, '')} AS owner, {fn CONVERT(SC.ITEMID, SQL_VARCHAR)} itemid
    FROM limssourcecontrol sc
    WHERE sc.owner = ?sUser?
      AND sc.moddate > {fn TIMESTAMPADD(SQL_TSI_DAY, -30, current_timestamp)}
");
```

A `--` line comment inside reflowed SQL always ends its output line —
the following token starts a new line at the current nesting indent, or
the DBMS would read it as part of the comment:

### Before

```ssl
aRows := SQLExecute("select r.id from rules r where r.active = 1
  and ( -- rules with no mappings
    not exists (select m.id from mappings m where m.ruleid = r.id)
  ) and r.site = ?sSite?");
```

### After

```ssl
aRows := SQLExecute("
    SELECT r.id
    FROM rules r
    WHERE r.active = 1
      AND ( -- rules with no mappings
        NOT EXISTS (
            SELECT m.id
            FROM mappings m
            WHERE m.ruleid = r.id
        ))
      AND r.site = ?sSite?
");
```

A single-line SQL string that overflows 90 columns is reflowed by the SQL
engine (canonicalCompact, uppercase keywords), placeholders intact:

### Before

```ssl
sSql := "SELECT sample_id, sample_name, sample_status FROM samples WHERE sample_status = ?status? ORDER BY sample_id";
```

### After

```ssl
sSql := "
    SELECT sample_id, sample_name, sample_status
    FROM samples
    WHERE sample_status = ?status?
    ORDER BY sample_id
";
```

Bracket-quoted strings (the idiomatic style for SQL holding embedded
quotes) reflow the same way; the opening `[` closes with `]` (issue #81):

### Before

```ssl
sSql := [SELECT sample_id, sample_name, sample_status FROM samples WHERE sample_status = ?status? ORDER BY sample_id];
```

### After

```ssl
sSql := [
    SELECT sample_id, sample_name, sample_status
    FROM samples
    WHERE sample_status = ?status?
    ORDER BY sample_id
];
```

A split assignment — line break after `:=` and a dangling `;` — rejoins
and then takes the normal layout: inline when short (issue #140):

### Before

```ssl
sSQL :=
	"SELECT 1"
;
```

### After

```ssl
sSQL := "SELECT 1";
```

The same split shape with an over-long SQL string rejoins and reflows to
rule F:

### Before

```ssl
sSql :=
	"SELECT sample_id, sample_name, sample_status FROM samples WHERE sample_status = ?status? ORDER BY sample_id"
;
```

### After

```ssl
sSql := "
    SELECT sample_id, sample_name, sample_status
    FROM samples
    WHERE sample_status = ?status?
    ORDER BY sample_id
";
```

A split non-SQL string keeps its user layout — joining is scoped to
detected SQL:

### Idempotent

```ssl
sMsg :=
	"sample not found for the given identifier"
;
```

SQL that already spans lines is renormalized to the same layout:

### Before

```ssl
sSql := "SELECT sample_id
FROM samples
WHERE sample_status = ?status?";
```

### After

```ssl
sSql := "
    SELECT sample_id
    FROM samples
    WHERE sample_status = ?status?
";
```

CASE keeps its place after the clause keyword in `WHERE`, with WHEN/ELSE
4 past CASE and END aligned with it — the reference's own "CASE in WHERE"
example (issue #271; CASE used to drop to the clause column with WHEN 2
under it):

### Before

```ssl
aRows := SQLExecute("SELECT ordno, testcode, result_value FROM ordresult r INNER JOIN specifications s ON s.testcode = r.testcode WHERE CASE WHEN r.result_value IS NULL THEN 'Missing' WHEN r.result_value < s.low_limit THEN 'Below' ELSE 'Pass' END != 'Pass'");
```

### After

```ssl
aRows := SQLExecute("
    SELECT ordno, testcode, result_value
    FROM ordresult r
    INNER JOIN specifications s
      ON s.testcode = r.testcode
    WHERE CASE
              WHEN r.result_value IS NULL THEN 'Missing'
              WHEN r.result_value < s.low_limit THEN 'Below'
              ELSE 'Pass'
          END != 'Pass'
");
```

The same shape in `ORDER BY`:

### Before

```ssl
aRows := SQLExecute("SELECT ordno, testcode FROM ordtask ORDER BY CASE WHEN status = 'L' THEN 1 WHEN status = 'C' THEN 2 ELSE 3 END, ordno");
```

### After

```ssl
aRows := SQLExecute("
    SELECT ordno, testcode
    FROM ordtask
    ORDER BY CASE
                 WHEN status = 'L' THEN 1
                 WHEN status = 'C' THEN 2
                 ELSE 3
             END, ordno
");
```

And in an `UPDATE ... SET` assignment, where WHEN used to land left of
the assignment it belongs to:

### Before

```ssl
RunSQL("UPDATE ordtask SET status = CASE WHEN result_value > high_limit THEN 'High' WHEN result_value < low_limit THEN 'Low' ELSE 'Normal' END WHERE ordno = ?", "", {sOrdNo});
```

### After

```ssl
RunSQL("
    UPDATE ordtask SET
        status = CASE
                     WHEN result_value > high_limit THEN 'High'
                     WHEN result_value < low_limit THEN 'Low'
                     ELSE 'Normal'
                 END
    WHERE ordno = ?
", "", {sOrdNo});
```

A CASE that is a THEN value starts its own line 4 past its WHEN, with its
own branches 4 past it (reference §5.3; it used to collapse back to the
outer CASE's column):

### Before

```ssl
aRows := SQLExecute("SELECT ordno, CASE WHEN status = 'Complete' THEN CASE WHEN result_value IS NULL THEN 'No Result' WHEN result_value > threshold THEN 'OOS' ELSE 'Within Spec' END ELSE 'Incomplete' END AS evaluation FROM ordresult");
```

### After

```ssl
aRows := SQLExecute("
    SELECT ordno,
           CASE
               WHEN status = 'Complete' THEN
                   CASE
                       WHEN result_value IS NULL THEN 'No Result'
                       WHEN result_value > threshold THEN 'OOS'
                       ELSE 'Within Spec'
                   END
               ELSE 'Incomplete'
           END AS evaluation
    FROM ordresult
");
```

When a branch would overflow with its value inline, every THEN value of
that CASE goes 4 past its WHEN; a short ELSE stays inline, and its sign
stays attached (reference §5.2; `ELSE -1` used to become `ELSE - 1`):

### Before

```ssl
aRows := SQLExecute("SELECT ordno, testcode, CASE WHEN status = 'Complete' THEN ROUND(result_value / baseline_value * 100, 2) + bias_adjustment WHEN status = 'Pending' THEN 0 ELSE -1 END AS pct_of_baseline FROM ordresult");
```

### After

```ssl
aRows := SQLExecute("
    SELECT ordno, testcode,
           CASE
               WHEN status = 'Complete' THEN
                   ROUND(result_value / baseline_value * 100, 2) + bias_adjustment
               WHEN status = 'Pending' THEN
                   0
               ELSE -1
           END AS pct_of_baseline
    FROM ordresult
");
```

A CASE that fits inside an aggregate's argument list stays inline
(reference §2.7):

### Before

```ssl
aRows := SQLExecute("SELECT customer_id, COUNT(*) AS total_tasks, SUM(CASE WHEN status = 'Complete' THEN 1 ELSE 0 END) AS done FROM order_tasks GROUP BY customer_id");
```

### After

```ssl
aRows := SQLExecute("
    SELECT customer_id, COUNT(*) AS total_tasks,
           SUM(CASE WHEN status = 'Complete' THEN 1 ELSE 0 END) AS done
    FROM order_tasks
    GROUP BY customer_id
");
```

A short WHEN predicate keeps its AND/OR on the WHEN line; a long one
breaks before each AND/OR, one step past WHEN (issue #286):

### Before

```ssl
aRows := SQLExecute("SELECT s.sample_id, CASE WHEN s.status = 'A' AND s.priority = 1 THEN 'urgent' WHEN s.status_code_for_the_sample = 'B' AND s.priority_level_of_the_sample = 2 AND s.department_code = 'QC' THEN 'review' ELSE 'normal' END AS label FROM samples s");
```

### After

```ssl
aRows := SQLExecute("
    SELECT s.sample_id,
           CASE
               WHEN s.status = 'A' AND s.priority = 1 THEN 'urgent'
               WHEN s.status_code_for_the_sample = 'B'
                   AND s.priority_level_of_the_sample = 2
                   AND s.department_code = 'QC' THEN 'review'
               ELSE 'normal'
           END AS label
    FROM samples s
");
```

A complex placeholder keeps its interior byte for byte (issue #327):

### Before

```ssl
:DECLARE aRows, sPrefix, sSuffix, nBase;
aRows := SQLExecute("select ordno, ordkey, ordseq, status from orders where ordkey = ?sPrefix + sSuffix? and ordseq > ?nBase * 2?");
```

### After

```ssl
:DECLARE aRows, sPrefix, sSuffix, nBase;
aRows := SQLExecute("
    SELECT ordno, ordkey, ordseq, status
    FROM orders
    WHERE ordkey = ?sPrefix + sSuffix?
      AND ordseq > ?nBase * 2?
");
```

## Rationale

This is the formatter's most-reversed decision (see history): "never touch
strings" lost to the value of readable embedded SQL, then over-eager SQL
rewriting broke short assignments (#64). The settled contract is the narrow
middle: only detected SQL, only when it doesn't already fit on one line,
everything else byte-preserved. Any future change to this boundary edits
this entry first.
