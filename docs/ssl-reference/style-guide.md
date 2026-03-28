# SSL Style Guide

This document summarizes SSL coding conventions and best practices.

> **See also:** [AGENTS.md](../../AGENTS.md) for quick AI agent reference.
> **Authority:** `dev/ssl-style-guide/` is the source of truth when this public summary lags behind.

---

## Naming Conventions

### Hungarian Notation

STARLIMS uses Hungarian notation prefixes to indicate variable types:

| Prefix | Type | Example |
|--------|------|---------|
| `s` | String | `sCustomerName`, `sMessage`, `sSQL` |
| `n` | Numeric | `nOrderTotal`, `nCount`, `nIndex` |
| `b` | Boolean | `bIsActive`, `bExists`, `bSuccess` |
| `d` | Date | `dCreatedOn`, `dDueDate`, `dExpiry` |
| `o` | Object | `oConnection`, `oDataset`, `oEmail` |
| `a` | Array | `aItems`, `aResults`, `aRows` |
| `fn` | Code block | `fnTransform`, `fnPredicate` |
| `v` | Variant / unknown | `vResult`, `vValue` |

### Identifier Naming

| Element | Convention | Example |
|---------|------------|---------|
| Procedures | PascalCase with Verb+Noun pattern | `ValidateOrder`, `ProcessQCSample` |
| Classes | PascalCase | `InvoiceManager`, `DataHandler` |
| Variables | camelCase with prefix | `sUserName`, `nTotal`, `vResult` |
| Constants | UPPER_SNAKE_CASE | `MAX_RETRIES`, `DEFAULT_PATH` |
| UDO Properties | lowerCamelCase (no type prefix) | `oOrder:orderNo`, `oState:isValid` |

### Exceptions to Hungarian Notation

- **Loop counters:** Single letters `i`, `j`, `k`, `x`, `y`, `z` are acceptable
- **Global constants:** `ALL_CAPS_NAMING` (e.g., `MAX_COUNT`, `DEFAULT_PATH`)
- **Acronyms:** Preserve established acronym casing only when it already exists in the surrounding codebase or required external names
- **SSL constants:** `NIL`, `.T.`, `.F.`

### Length Limits

- Variable names: maximum **20 characters** (excluding prefix)
- Function/procedure names: maximum **30 characters**

### Naming Guidelines

- Use abbreviations sparingly and only when obvious
- Avoid underscores in variable names for general use (discouraged, not prohibited)
- **Exception:** Underscore-prefixed class members (e.g. `_sInternal`) are a deliberate private-by-convention pattern. They are excluded from reflection-based access and should not be renamed or removed. Do not treat them as style violations.
- Keep class-context references in their canonical forms: `Me:Member`, `Base:Member`, `Constructor`

---

## Formatting Standards

### Indentation

- Use **tabs** (preferred) or consistent spaces
- Indent block contents by one level
- Align continuation lines for readability
- **Class files:** Do not add an extra indentation level for the class body itself. Because SSL has no `:ENDCLASS` and the class extends to the end of the file, the formatter does not indent the class body relative to `:CLASS`.

```ssl
:PROCEDURE ProcessData;
:PARAMETERS sInput;
:DECLARE nCount, aResults;

    nCount := 0;
    
    :IF .NOT. Empty(sInput);
        aResults := SQLExecute("SELECT * FROM Table WHERE Field = ?sInput?");
        
        :IF Len(aResults) > 0;
            :FOR i := 1 :TO Len(aResults);
                nCount += 1;
            :NEXT;
        :ENDIF;
    :ENDIF;

:RETURN nCount;
:ENDPROC;
```

### Spacing

| Context | Rule | Example |
|---------|------|---------|
| Assignment | Space around `:=` | `x := 1` |
| Comparison | Space around operators | `x > 0` |
| After commas | One space | `DoProc("MyProc", {sName, nCount, bFlag})` |
| Before semicolon | No space | `statement;` |
| Inside parentheses | No space | `DoProc("MyProc", {sName, nCount})` not `DoProc( "MyProc", { sName, nCount } )` |
| Skipped params | Adjacent commas, no space | `RunSQL(sSQL,, {aVals})` not `RunSQL(sSQL, , {aVals})` |
| Member access | Prefer no spaces around `:` | `oEmail:Subject` not `oEmail : Subject` |

### Not-Preferred Operators

The following comparison operators are valid SSL but not preferred — use `!=` instead:

| Operator | Use Instead |
|----------|-------------|
| `<>` | `!=` |
| `#` | `!=` |

### Line Length

- Maximum **90 characters** recommended
- Break long lines at logical points (after comma, before operator)

```ssl
/* Long SQL - break at logical points;
sSQL := "SELECT CustomerID, CustomerName, OrderTotal " +
        "FROM Orders " +
        "WHERE Status = ?sStatus? " +
        "ORDER BY OrderDate";
```

### Blank Lines

- One blank line between procedures
- Blank line before major sections within procedures
- Blank line after declarations, before logic

---

## Code Structure

### Procedure Calls

Custom procedures are not called directly:

```ssl
vResult := DoProc("LocalProcedure", {sValue, nCount});
vResult := ExecFunction("Category.Script.ProcedureName", {sValue});
```

`DoProc(...)` is a **compile-time error** inside `:CLASS` methods. Use `Me:MethodName()` / `Base:MethodName()` for sibling and inherited methods instead.

When optional trailing arguments are omitted, leave them out entirely rather than passing empty arrays or empty strings:

```ssl
DoProc("NoArgProc");
GetDataSet(sSQL);
```

### Procedure Organization

```ssl
/*
 * Procedure: ProcedureName
 * Description: Brief description
 * Parameters:
 *   sInput - description
 *   nCount - description
 * Returns: description
;
:PROCEDURE ProcedureName;
:PARAMETERS sInput, nCount;
:DEFAULT sInput, "";
:DEFAULT nCount, 0;
:DECLARE sResult, aRows;

/* Initialization;
sResult := "";

/* Main logic;
:TRY;
    /* Process data;
:CATCH;
    /* Handle error;
:ENDTRY;

:RETURN sResult;
:ENDPROC;
```

**Recommended order:**
1. Header comment with documentation
2. `:PARAMETERS` (if any)
3. `:DEFAULT` values (if any)
4. `:DECLARE` local variables
5. Initialization
6. Main logic
7. `:RETURN` (if applicable)
8. `:ENDPROC;`

**Placement rules:**
- Script-level `:PARAMETERS` must appear before top-level script statements, though leading `:PROCEDURE` definitions may come first
- Inside a procedure, `:PARAMETERS` must immediately follow `:PROCEDURE`
- `:DEFAULT` must immediately follow `:PARAMETERS` (placing any statement between them is a compile error)

### Control Flow Best Practices

**Prefer `:EXITCASE;` at the end of every `:CASE` and `:OTHERWISE` block unless multi-match behavior is intentional:**
```ssl
:BEGINCASE;
:CASE condition;
    sResult := "matched";
    :EXITCASE;
:OTHERWISE;
    sResult := "default";
    :EXITCASE;
:ENDCASE;
```

**Keep nesting shallow:**
- Maximum 3-4 levels of nesting
- Extract complex logic to separate procedures

**Prefer positive logic:**
```ssl
/* Prefer this;
:IF bIsValid;
    sStatus := "valid";
:ELSE;
    sStatus := "invalid";
:ENDIF;

/* Over this;
:IF .NOT. bIsValid;
    sStatus := "invalid";
:ELSE;
    sStatus := "valid";
:ENDIF;
```

---

## Comments

### Block Comments

```ssl
/* Single-line comment;

/* Multi-line comment
   that spans multiple lines
   and ends with semicolon
;
```

### Documentation Headers

```ssl
/*
 * Procedure: CalculateTotal
 * Description: Calculates order total with applicable discounts
 * Parameters:
 *   nPrice    - Unit price
 *   nQuantity - Number of items
 *   bApplyDiscount - Whether to apply discount
 * Returns: Numeric total
 * Author: INITIALS
 * Date: YYYY-MM-DD
;
:PROCEDURE CalculateTotal;
```

### Comment Guidelines

- Explain "why" not "what"
- Keep comments concise
- Use sentence case
- Place comments above the code they describe
- All comments must end with semicolon
- Never place an extra semicolon inside comment text; the first semicolon terminates the comment and the remaining text becomes executable code

### Visibility Annotations (Scripts Only)

Place `/*@private;` or `/*@protected;` on its own line immediately before `:PROCEDURE` to restrict access from external callers:

```ssl
/*@private;
:PROCEDURE InternalHelper;
    /* Not accessible via DoProc/ExecFunction from outside this script;
:ENDPROC;
```

- Both annotations make the procedure inaccessible via `DoProc` / `ExecFunction` from external scripts.
- **These annotations have no effect on class methods.** Class methods are always public regardless.

### Code Organization

- Prefer comment regions (`/* region ...;` / `/* endregion;`) for editor grouping
- Treat `:REGION` / `:ENDREGION` as legacy functional text-capture constructs (body text stored for runtime retrieval via `GetRegion(sValue, vSrc, vDst)`), not formatting aids

---

## SQL Standards

> **Full reference:** `dev/ssl-style-guide/ssl-style-guide/sql-canonical-compact-reference.md` is the authoritative Oracle SQL formatting specification. The rules below are a normative summary.

### Canonical Compact Style

SQL embedded in SSL strings uses **canonical compact** style — a space-efficient format optimised for readability within SSL string literals.

#### Core Rules

| Rule | Detail |
|------|--------|
| Major clauses | Start at column 0 (relative to SQL block indent) |
| SELECT columns | Pack onto lines up to ~90 chars; break complex expressions to own line |
| SELECT continuations | Aligned to first column (col 7) |
| Continuation lines | Aligned under the first token of their parent clause |
| AND/OR | Indented 2 spaces under their parent clause |
| ON | Indented 2 spaces under JOIN |
| HAVING | Indented 2 spaces under GROUP BY |
| WHEN/ELSE | Indented 2 spaces under CASE |
| Keyword casing | UPPERCASE — all SQL keywords and built-in functions |
| Identifier casing | lowercase — table names, column names, aliases |
| External casing | Preserve when schema/object requires it |
| Comma style | Trailing commas |
| Max line length | ~90 characters (soft target, break at logical points) |
| Subqueries | Indented one level inside parentheses |
| SSL embedding | Entire SQL block indented 4 spaces inside the string literal |

#### Indentation Reference

```
SELECT col1, col2,        ← major clause, col 0
       col3, col4         ← continuation, aligned to first column (col 7)
FROM table1               ← major clause, col 0
INNER JOIN table2         ← major clause, col 0
  ON table2.id = table1.id  ← ON indented 2 under JOIN
  AND table2.col = table1.col ← AND in ON: same indent as ON
WHERE condition1          ← major clause, col 0
  AND condition2          ← AND/OR indented 2 under parent
GROUP BY col1             ← major clause, col 0
  HAVING COUNT(*) > 1     ← HAVING indented 2 under GROUP BY
ORDER BY col1             ← major clause, col 0
```

### SSL Embedding

SQL embedded in SSL strings is indented 4 spaces from the string opening. All SQL indentation rules apply relative to that base indent:

```ssl
aResults := SQLExecute("
    SELECT col1, col2
    FROM table1
    WHERE col1 = ?sValue?
");
```

### SELECT

**Basic SELECT:**
```ssl
aResults := SQLExecute("
    SELECT ordno, testcode, status, result
    FROM ordtask
    WHERE status = 'Logged'
    ORDER BY ordno
");
```

**SELECT list packing:** Pack columns onto lines up to the ~90 char limit. Short aliases and simple function calls (`UPPER(col)`, `COUNT(*)`) can share a line. Break to a new line when a column has a complex expression (CASE, nested functions) or when clarity demands it.

```ssl
sSQL := "
    SELECT o.ordno, t.testcode, t.status,
           UPPER(t.result) AS result_upper
    FROM orders o
    INNER JOIN ordtask t
      ON t.ordno = o.ordno
    WHERE o.status = ?sStatus?
      AND o.logdate >= ?dStartDate?
    ORDER BY o.ordno
";
```

### JOINs

```ssl
/* INNER JOIN;
sSQL := "
    SELECT o.ordno, t.testcode, t.description
    FROM orders o
    INNER JOIN ordtask t
      ON t.ordno = o.ordno
    WHERE o.status = 'Logged'
";

/* LEFT OUTER JOIN with multiple ON conditions;
sSQL := "
    SELECT o.ordno, t.testcode, r.result_value
    FROM orders o
    LEFT OUTER JOIN ordtask t
      ON t.ordno = o.ordno
    LEFT OUTER JOIN ordresult r
      ON r.ordno = t.ordno
      AND r.testcode = t.testcode
    WHERE o.status = 'Logged'
";
```

### Filtering

**BETWEEN:**
```ssl
sSQL := "
    SELECT ordno, logdate
    FROM orders
    WHERE logdate BETWEEN TO_DATE('2025-01-01', 'YYYY-MM-DD')
                      AND TO_DATE('2025-12-31', 'YYYY-MM-DD')
";
```

**IN with subquery:**
```ssl
sSQL := "
    SELECT ordno, status
    FROM orders
    WHERE ordno IN (
        SELECT ordno
        FROM ordtask
        WHERE testcode = 'pH'
    )
";
```

**Compound conditions:**
```ssl
sSQL := "
    SELECT ordno, testcode, status
    FROM ordtask
    WHERE (status = 'Logged' OR status = 'Pending')
      AND testcode IN ('pH', 'Conductivity')
      AND ordno LIKE '2025%'
";
```

### Aggregation

```sql
SELECT testcode, COUNT(*) AS task_count,
       AVG(result_value) AS avg_result
FROM ordresult
WHERE status = 'Complete'
GROUP BY testcode
  HAVING COUNT(*) > 5
ORDER BY task_count DESC
```

### Subqueries

```sql
/* Scalar subquery in SELECT list;
SELECT o.ordno,
       (SELECT COUNT(*)
        FROM ordtask t
        WHERE t.ordno = o.ordno) AS task_count
FROM orders o
WHERE o.status = 'Logged'
```

```sql
/* EXISTS;
SELECT o.ordno, o.status
FROM orders o
WHERE EXISTS (
    SELECT 1
    FROM ordtask t
    WHERE t.ordno = o.ordno
      AND t.status = 'Failed'
)
```

```sql
/* Inline view;
SELECT t.testcode, t.task_count
FROM (
    SELECT testcode, COUNT(*) AS task_count
    FROM ordtask
    WHERE status = 'Complete'
    GROUP BY testcode
) t
WHERE t.task_count > 10
```

### CTEs (Common Table Expressions)

```sql
WITH task_counts AS (
    SELECT ordno, COUNT(*) AS cnt
    FROM ordtask
    WHERE status = 'Complete'
    GROUP BY ordno
)
SELECT o.ordno, o.status, tc.cnt
FROM orders o
INNER JOIN task_counts tc
  ON tc.ordno = o.ordno
WHERE tc.cnt > 5
ORDER BY tc.cnt DESC
```

**Chained CTEs** — comma at the end of each CTE except the last:

```sql
WITH active_orders AS (
    SELECT ordno, customer_id
    FROM orders
    WHERE status = 'Active'
),
order_tasks AS (
    SELECT ao.ordno, t.testcode, t.status
    FROM active_orders ao
    INNER JOIN ordtask t
      ON t.ordno = ao.ordno
)
SELECT ordno, testcode, status
FROM order_tasks
ORDER BY ordno
```

### Set Operations

Surround set operators (`UNION`, `UNION ALL`, `INTERSECT`, `MINUS`) with blank lines:

```sql
SELECT ordno, testcode
FROM ordtask
WHERE status = 'Logged'

UNION ALL

SELECT ordno, testcode
FROM ordtask_archive
WHERE status = 'Logged'
ORDER BY ordno
```

### INSERT

Opening `(` on the `INSERT INTO` line, columns indented 4 spaces, closing `)` on its own line. Same pattern for `VALUES`:

```sql
INSERT INTO ordresult (
    result_id, ordno, testcode, result_value,
    status, logdate
)
VALUES (
    seq_ordresult.NEXTVAL, '2025-001', 'pH', 7.2,
    'Complete', SYSDATE
)
```

### UPDATE

`SET` on the same line as `UPDATE`, assignments indented 4 spaces below:

```sql
UPDATE ordtask SET
    status = 'Complete',
    completed_date = SYSDATE,
    completed_by = 'admin'
WHERE ordno = '2025-001'
  AND testcode = 'pH'
```

### DELETE

**Basic:**
```sql
DELETE FROM audit_log
WHERE log_date < ADD_MONTHS(SYSDATE, -24)
```

**With EXISTS:**
```sql
DELETE FROM ordresult r
WHERE EXISTS (
    SELECT 1
    FROM ordtask t
    WHERE t.ordno = r.ordno
      AND t.testcode = r.testcode
      AND t.status = 'Cancelled'
)
```

### MERGE

```sql
MERGE INTO ordtask_summary tgt
USING (
    SELECT ordno, testcode,
           COUNT(*) AS result_count,
           AVG(result_value) AS avg_result
    FROM ordresult
    GROUP BY ordno, testcode
) src
ON (tgt.ordno = src.ordno AND tgt.testcode = src.testcode)
WHEN MATCHED THEN
    UPDATE SET tgt.result_count = src.result_count,
               tgt.avg_result = src.avg_result,
               tgt.updated_date = SYSDATE
    DELETE WHERE tgt.result_count = 0
WHEN NOT MATCHED THEN
    INSERT (
        ordno, testcode, result_count, avg_result, created_date
    )
    VALUES (
        src.ordno, src.testcode, src.result_count,
        src.avg_result, SYSDATE
    )
```

**MERGE formatting rules:**
- `MERGE INTO`, `USING`, and `ON` at column 0
- `WHEN MATCHED THEN` / `WHEN NOT MATCHED THEN` at column 0
- `UPDATE SET` / `INSERT` / `DELETE WHERE` indented 4 spaces under WHEN
- SET column assignments aligned with continuation indented to match first assignment

### INSERT ALL (Multi-Table)

```sql
INSERT ALL
    WHEN status = 'Complete' THEN
        INTO completed_tasks (
            ordno, testcode, completed_date
        )
        VALUES (
            ordno, testcode, SYSDATE
        )
    WHEN status = 'Failed' THEN
        INTO failed_tasks (
            ordno, testcode, failed_date
        )
        VALUES (
            ordno, testcode, SYSDATE
        )
SELECT ordno, testcode, status
FROM ordtask
WHERE logdate = TRUNC(SYSDATE)
```

### Recursive CTEs

```sql
WITH RECURSIVE org_tree (emp_id, mgr_id, emp_name, lvl) AS (
    SELECT emp_id, mgr_id, emp_name, 1
    FROM employees
    WHERE mgr_id IS NULL
    UNION ALL
    SELECT e.emp_id, e.mgr_id, e.emp_name, ot.lvl + 1
    FROM employees e
    INNER JOIN org_tree ot
      ON ot.emp_id = e.mgr_id
)
SELECT emp_id, emp_name, lvl
FROM org_tree
ORDER BY lvl, emp_name
```

### CASE Expressions

`WHEN`/`ELSE` indented 2 spaces under `CASE`. Within one `CASE` block, either all branches inline or all broken — do not mix:

```sql
SELECT ordno,
       CASE
           WHEN status = 'L' THEN 'Logged'
           WHEN status = 'C' THEN 'Complete'
           ELSE 'Unknown'
       END AS status_desc
FROM ordtask
```

**THEN/ELSE values on their own line** — when any WHEN branch has a condition or value too long to fit inline, break THEN values to the next line:

```sql
SELECT ordno, testcode,
       CASE
           WHEN status = 'Complete' AND result_value IS NOT NULL THEN
               ROUND(result_value / baseline_value * 100, 2)
           WHEN status = 'Complete' AND result_value IS NULL THEN
               0
           ELSE -1
       END AS pct_of_baseline
FROM ordresult
```

**Nested CASE:**

```sql
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
```

### Advanced Expressions

**DECODE (Oracle legacy):**
```sql
SELECT ordno,
       DECODE(status,
              'L', 'Logged',
              'C', 'Complete',
              'X', 'Cancelled',
              'Unknown') AS status_desc
FROM orders
```

**NVL / NVL2 / COALESCE / NULLIF:**
```sql
SELECT ordno,
       NVL(result_value, 0) AS result_safe,
       NVL2(comment, 'Has Comment', 'No Comment') AS comment_flag,
       COALESCE(override_value, result_value, default_value) AS final_value,
       NULLIF(status, 'N/A') AS clean_status
FROM ordresult
```

**CAST / Type Conversion:**
```sql
SELECT ordno,
       CAST(result_value AS NUMBER(10, 2)) AS rounded_result,
       TO_CHAR(logdate, 'YYYY-MM-DD HH24:MI:SS') AS log_timestamp,
       TO_DATE(date_string, 'YYYY-MM-DD') AS parsed_date,
       TO_NUMBER(string_val, '999.99') AS numeric_val
FROM ordresult
```

**Nested function calls** — keep inline when shallow (2 levels), break at logical points when deep:
```sql
SELECT ordno,
       TO_CHAR(
           TRUNC(
               ADD_MONTHS(logdate, -6),
               'MM'
           ),
           'YYYY-MM-DD'
       ) AS six_months_prior_month
FROM orders
```

### Analytic / Window Functions

```sql
SELECT ordno, testcode, result_value,
       ROW_NUMBER() OVER (
           PARTITION BY ordno
           ORDER BY testcode
       ) AS rn,
       RANK() OVER (
           PARTITION BY testcode
           ORDER BY result_value DESC
       ) AS val_rank
FROM ordresult
WHERE status = 'Complete'
```

Short window specs stay on one line:
```sql
SELECT ordno, testcode,
       ROW_NUMBER() OVER (ORDER BY ordno) AS rn
FROM ordtask
```

**Windowing clauses (ROWS/RANGE BETWEEN):**
```sql
SELECT ordno, logdate, result_value,
       AVG(result_value) OVER (
           PARTITION BY testcode
           ORDER BY logdate
           ROWS BETWEEN 6 PRECEDING AND CURRENT ROW
       ) AS moving_avg_7
FROM ordresult
WHERE testcode = 'pH'
ORDER BY logdate
```

**LAG / LEAD:**
```sql
SELECT ordno, logdate, result_value,
       LAG(result_value, 1) OVER (
           PARTITION BY testcode
           ORDER BY logdate
       ) AS prev_result,
       LEAD(result_value, 1) OVER (
           PARTITION BY testcode
           ORDER BY logdate
       ) AS next_result
FROM ordresult
ORDER BY testcode, logdate
```

**LISTAGG:**
```sql
SELECT ordno,
       LISTAGG(testcode, ', ') WITHIN GROUP (
           ORDER BY testcode
       ) AS test_list
FROM ordtask
WHERE status = 'Logged'
GROUP BY ordno
```

### Oracle-Specific Constructs

**Hierarchical queries:**
```sql
SELECT LEVEL, emp_id, emp_name,
       SYS_CONNECT_BY_PATH(emp_name, '/') AS path,
       CONNECT_BY_ROOT emp_name AS root_mgr
FROM employees
START WITH mgr_id IS NULL
CONNECT BY PRIOR emp_id = mgr_id
ORDER SIBLINGS BY emp_name
```

**PIVOT:**
```sql
SELECT *
FROM (
    SELECT ordno, testcode, result_value
    FROM ordresult
    WHERE ordno = '2025-001'
)
PIVOT (
    AVG(result_value)
    FOR testcode IN (
        'pH' AS ph,
        'Conductivity' AS cond,
        'Turbidity' AS turb
    )
)
```

**LATERAL inline view:**
```sql
SELECT o.ordno, o.status, lt.latest_result
FROM orders o,
     LATERAL (
         SELECT MAX(r.result_value) AS latest_result
         FROM ordresult r
         WHERE r.ordno = o.ordno
     ) lt
WHERE o.status = 'Active'
```

**Flashback Queries:**
```sql
SELECT ordno, status
FROM orders AS OF TIMESTAMP
    (SYSTIMESTAMP - INTERVAL '1' HOUR)
WHERE ordno = '2025-001'
```

```sql
SELECT ordno, status
FROM orders AS OF SCN 123456789
WHERE ordno = '2025-001'
```

**RETURNING clause (in SSL context):**
```sql
INSERT INTO orders (
    ordno, status, logdate
)
VALUES (
    '2025-999', 'Logged', SYSDATE
)
RETURNING order_id INTO ?nNewId?
```

**FOR UPDATE:**
```sql
SELECT ordno, status
FROM orders
WHERE status = 'Pending'
  AND customer_id = ?sCustomerId?
FOR UPDATE OF status NOWAIT
```

### DDL

**CREATE TABLE:**
```sql
CREATE TABLE ordresult (
    result_id   NUMBER(10)     NOT NULL,
    ordno       VARCHAR2(20)   NOT NULL,
    testcode    VARCHAR2(30)   NOT NULL,
    result_value NUMBER(15, 5),
    status      VARCHAR2(10)   DEFAULT 'Pending',
    logdate     DATE           DEFAULT SYSDATE,
    CONSTRAINT pk_ordresult PRIMARY KEY (result_id),
    CONSTRAINT fk_ordresult_order
        FOREIGN KEY (ordno) REFERENCES orders (ordno),
    CONSTRAINT ck_ordresult_status
        CHECK (status IN ('Pending', 'Complete', 'Failed'))
)
```

**CREATE VIEW:**
```sql
CREATE OR REPLACE VIEW vw_active_tasks AS
SELECT o.ordno, o.status AS order_status,
       t.testcode, t.status AS task_status,
       t.logdate
FROM orders o
INNER JOIN ordtask t
  ON t.ordno = o.ordno
WHERE o.status = 'Active'
```

**CREATE INDEX:**
```sql
CREATE INDEX idx_ordtask_status
    ON ordtask (status)
```

**ALTER TABLE:**
```sql
ALTER TABLE ordtask
    ADD (completed_date DATE,
         completed_by   VARCHAR2(50))
```

**DROP / TRUNCATE:**
```sql
DROP TABLE ordtask_archive PURGE
```
```sql
TRUNCATE TABLE temp_results
```

### Ordering

```sql
SELECT ordno, testcode, logdate
FROM ordtask
ORDER BY logdate DESC NULLS LAST,
         testcode ASC NULLS FIRST,
         ordno
```

### Optimizer Hints

Place optimizer hints immediately after the action keyword — never strip them without understanding their purpose:

```sql
SELECT /*+ INDEX(o idx_orders_status) */ o.ordno, o.status
FROM orders o
WHERE o.status = 'Logged'
```

### SQL Parameters

**SQLExecute (named parameters):**
```ssl
sSQL := "SELECT * FROM Orders WHERE CustomerID = ?sCustomerID?";
aResults := SQLExecute(sSQL);
```

**RunSQL/LSearch (positional parameters):**
```ssl
sSQL := "UPDATE Orders SET Status = ? WHERE OrderID = ?";
bSuccess := RunSQL(sSQL,, {sNewStatus, nOrderID});

sName := LSearch("SELECT Name FROM Customers WHERE ID = ?", "",, {nCustomerID});
```

`LSelect`, `LSelect1`, `LSelectC`, `GetDataSet`, `GetDataSetEx`, `GetDataSetWithSchemaFromSelect`, `GetDataSetXMLFromSelect`, `GetNETDataSet`, `XmlExportSql`, and `GetTables` follow the same positional-parameter convention.

`SQLExecute` also supports advanced named placeholder patterns (derived from `ssl-style-guide.schema.yaml`, not from `sql-canonical-compact-reference.md`):

| Pattern | Description | Example |
|---------|-------------|---------|
| `?name?` | Simple variable binding | `?sCustomerID?` |
| `?obj:Prop?` | Object-property access | `?oUser:ID?` |
| `?obj:method()?` | Parameterless object method | `?oSeq:GetNext()?` |
| `?arr[i]?` | Array element access | `?aValues[1]?` |
| `?Func()?` | Parameterless function call | `?Today()?` |
| `?'value'?` / `?123?` | Constant literal | `?'ACTIVE'?` |
| `?expr + expr?` | Complex expression (evaluated each execution; prefer pre-computed variable) | `?sPrefix + sCode?` |
| `?aValues?` | Array expansion (auto-expands into positional `?` placeholders; a 3-element array becomes `?,?,?`) | `WHERE id IN (?aIDs?)` |

### Database Function Selection

| Function | Use Case | Parameter Style |
|----------|----------|-----------------|
| `SQLExecute` | General queries | `?varName?` (named) |
| `RunSQL` | INSERT/UPDATE/DELETE | `?` (positional) |
| `LSearch` | Single value lookup | `?` (positional) |
| `LSelect` / `LSelect1` / `LSelectC` | Multi-row SELECT | `?` (positional) |
| `GetDataSet` | XML dataset output | `?` (positional) |
| `XmlExportSql` | Export SQL result as XML | `?` (positional) |
| `GetTables` | Get tables result | `?` (positional) |

---

## Error Handling

### TRY/CATCH Pattern

```ssl
:TRY;
    /* Risky operation;
    aResults := SQLExecute(sSQL);
    
    :IF Len(aResults) = 0;
        RaiseError("No results found");
    :ENDIF;
:CATCH;
    /* Handle error;
    oErr := GetLastSSLError();
    sMsg := "Error: " + oErr:Description;
    UsrMes(sMsg, "Error");
    :RETURN NIL;
:FINALLY;
    /* Cleanup (always runs);
    :IF .NOT. Empty(oConnection);
        oConnection := NIL;
    :ENDIF;
:ENDTRY;
```

### Error Handling Guidelines

- Use structured `:TRY`/`:CATCH` over legacy `:ERROR` / `:RESUME`
- A `:TRY` block must contain at least one statement before `:CATCH` or `:FINALLY`
- A `:TRY` block must include at least one `:CATCH` or `:FINALLY`
- Only one `:CATCH` block is allowed, and `:CATCH` does not name an exception variable
- Legacy `:ERROR` handlers must contain at least one statement before `:RESUME` or the next scope boundary
- Always log errors with contextual information
- Never swallow errors silently
- Clean up resources in `:FINALLY` blocks
- `:RETURN`, `:EXITFOR`, `:EXITWHILE`, and `:LOOP` are compile-time errors inside `:FINALLY`

---

## Performance Guidelines

### SQL Optimization

- Avoid `SELECT *` — specify needed columns
- Use `EXISTS` over `DISTINCT` when checking for existence
- Use `BETWEEN` for range queries instead of multiple conditions
- Prefer derived tables over correlated subqueries
- Minimize database round-trips

### Array Operations

- Preallocate arrays when size is known
- Use appropriate array functions (`AScan`, `AScanExact`)
- Prefer `SQLExecute` array expansion (`?aValues?`) for dynamic `IN` clauses over manual string building

### General

- Pre-compute complex expressions before SQL
- Avoid complex expressions in SQL parameter placeholders
- Use `LSearch` for single-value lookups (faster than full query)

---

## Security Best Practices

### SQL Injection Prevention

- **Always use parameterized queries**
- Never concatenate user input directly into SQL strings
- Validate parameter counts match placeholders

```ssl
/* WRONG - SQL injection risk;
sSQL := "SELECT * FROM Users WHERE Name = '" + sUserInput + "'";

/* CORRECT - parameterized;
sSQL := "SELECT * FROM Users WHERE Name = ?sUserInput?";
aResults := SQLExecute(sSQL);
```

### Input Validation

- Validate all inputs before processing
- Use appropriate data types
- Check array bounds before access

---

## Function Casing Reference

SSL functions are case-insensitive at runtime but should use their exact documented casing for consistency. Most follow PascalCase; canonical exceptions preserve non-PascalCase forms exactly.

### Canonical Exceptions (Non-PascalCase)

These functions do **not** follow PascalCase — always use their exact documented form:

`_AND`, `_OR`, `_XOR`, `_NOT`, `DOW`, `DOY`, `LIMSDate`

### Common Casing Mistakes

| Wrong | Correct | Reason |
|-------|---------|--------|
| `AADD(arr, val)` | `AAdd(arr, val)` | PascalCase, not all-caps |
| `LIMSSTRING(value)` | `LimsString(value)` | PascalCase |
| `ALLTRIM(s)` | `AllTrim(s)` | PascalCase |
| `DATEADD(...)` | `DateAdd(...)` | PascalCase |

### Array Functions (PascalCase)
```ssl
AAdd(aArray, value);
ALen(aArray);
AScan(aArray, value);
AScanExact(aArray, value);
```

### String Functions (PascalCase)
```ssl
AllTrim(sString);
SubStr(sString, nStart, nLen);
StrTran(sString, sFind, sReplace);
Upper(sString);
Lower(sString);
```

### Type Functions (PascalCase)
```ssl
LimsString(value);  /* General value-to-string conversion;
Str(nValue, 6, 2);  /* Numeric formatting with width/decimals;
Empty(value);       /* Check if empty/nil/zero;
Val(sString);       /* Convert to number;
LimsTypeEx(value);  /* Get type name;
```

### Database Functions (PascalCase)
```ssl
SQLExecute(sSQL);
RunSQL(sSQL, sFriendlyName, aParams);
LSearch(sSQL, default, sFriendlyName, aParams);
GetDataSet(sSQL, aParams);
```

---

## Predefined Global Variables

SSL provides read-only predefined globals available in all scripts:

| Variable | Type | Description |
|----------|------|-------------|
| `MYUSERNAME` | String | Current user's username |

Do not assign to `MYUSERNAME`; it is a runtime-provided read-only value. The LSP always recognizes `MYUSERNAME` as pre-declared and flags any assignment to it as an error, regardless of settings.

---

## Summary Checklist

- [ ] Use Hungarian notation for all variables
- [ ] Use PascalCase for procedures and classes
- [ ] Terminate all statements with semicolon
- [ ] Use colon-prefixed UPPERCASE keywords
- [ ] Use `.AND.`, `.OR.`, `.NOT.` (with periods)
- [ ] Use `DoProc`/`ExecFunction` for procedure calls
- [ ] Include `:EXITCASE` in every CASE block
- [ ] Use 1-based array indexing
- [ ] Use parameterized queries for all SQL
- [ ] Add documentation headers to procedures
