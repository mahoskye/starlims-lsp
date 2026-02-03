# Common SSL Gotchas

This document highlights common pitfalls and mistakes when writing SSL code.

> **See also:** [AGENTS.md](../../AGENTS.md) for quick reference on avoiding these issues.

---

## Gotcha #1: Direct Procedure Calls Don't Work

**Problem:** SSL does not support calling custom procedures directly.

```ssl
/* WRONG - will not work!;
CalculateTotal(5, 10);
MyProcedure();
```

**Solution:** Use `DoProc` for same-file calls, `ExecFunction` for cross-file calls.

```ssl
/* CORRECT - same file;
result := DoProc("CalculateTotal", {5, 10});

/* CORRECT - different file;
result := ExecFunction("Module.CalculateTotal", {5, 10});

/* Skip parameters with empty array positions;
result := DoProc("MyProc", {param1,, param3});  /* Skips param2;
```

**LSP Support:** Yes — detects direct calls to procedures defined in the same file.

---

## Gotcha #2: Missing :EXITCASE Causes Fall-Through

**Problem:** Without `:EXITCASE`, execution falls through to the next case.

```ssl
/* WRONG - falls through!;
:BEGINCASE;
:CASE nVal == 1;
    DoOne();
    /* Missing :EXITCASE - DoTwo() will also execute!;
:CASE nVal == 2;
    DoTwo();
    :EXITCASE;
:ENDCASE;
```

**Solution:** Always include `:EXITCASE` at the end of every case block.

```ssl
/* CORRECT;
:BEGINCASE;
:CASE nVal == 1;
    DoOne();
    :EXITCASE;  /* Required!;
:CASE nVal == 2;
    DoTwo();
    :EXITCASE;  /* Required!;
:OTHERWISE;
    DoDefault();
    :EXITCASE;  /* Required!;
:ENDCASE;
```

**LSP Support:** Yes — warns about missing `:EXITCASE`.

---

## Gotcha #3: :DEFAULT Only Works with :PARAMETERS

**Problem:** Using `:DEFAULT` with `:DECLARE` has no effect.

```ssl
/* WRONG - DEFAULT is ignored!;
:DECLARE x;
:DEFAULT x, 10;
/* x is still empty string "", not 10;
```

**Solution:** Use `:DEFAULT` only after `:PARAMETERS`, or assign after declare.

```ssl
/* CORRECT - with parameters;
:PARAMETERS x;
:DEFAULT x, 10;

/* CORRECT - assign after declare;
:DECLARE x;
x := 10;
```

**LSP Support:** Yes — warns about this pattern.

---

## Gotcha #4: Logical Operators Need Periods

**Problem:** Using `AND`, `OR`, `NOT` without periods treats them as identifiers.

```ssl
/* WRONG - these don't work as expected!;
:IF condition1 AND condition2;
:IF condition1 OR condition2;
:IF NOT condition;
```

**Solution:** Wrap logical operators in periods.

```ssl
/* CORRECT;
:IF condition1 .AND. condition2;
:IF condition1 .OR. condition2;
:IF .NOT. condition;
```

**LSP Support:** Yes — reports bare logical operators as errors.

---

## Gotcha #5: Arrays Are 1-Based

**Problem:** SSL arrays start at index 1, not 0.

```ssl
aItems := {"first", "second", "third"};

/* WRONG - index 0 is invalid!;
sFirst := aItems[0];  /* Error or unexpected behavior;

/* CORRECT;
sFirst := aItems[1];  /* "first";
sLast := aItems[Len(aItems)];  /* "third";
```

**LSP Support:** Yes — detects `[0]` array access patterns.

---

## Gotcha #6: Comments End with Semicolon

**Problem:** A semicolon inside a comment terminates it.

```ssl
/* This comment ends here; this is CODE now!
DoSomething();  /* This line is actually outside the comment;
```

**Solution:** Be careful with semicolons in comments.

```ssl
/* This is a proper comment without embedded semicolons
   that spans multiple lines
;
```

**LSP Support:** Not currently detected (Issue #52 - deferred).

---

## Gotcha #7: SQLExecute vs Other Database Functions

**Problem:** Only `SQLExecute` supports `?varName?` syntax.

```ssl
/* WRONG - RunSQL doesn't support named parameters;
RunSQL("UPDATE Table SET Field = ?sValue?", "", {});

/* WRONG - LSearch doesn't support named parameters;
sResult := LSearch("SELECT Name FROM T WHERE ID = ?nID?", "");
```

**Solution:** Use positional `?` with value arrays for RunSQL, LSearch, etc.

```ssl
/* CORRECT - SQLExecute with named params;
aResults := SQLExecute("SELECT * FROM T WHERE Field = ?sValue?");

/* CORRECT - RunSQL with positional params;
RunSQL("UPDATE T SET Field = ? WHERE ID = ?", "", {sValue, nID});

/* CORRECT - LSearch with positional params;
sResult := LSearch("SELECT Name FROM T WHERE ID = ?", "",, {nID});
```

**LSP Support:** Yes — warns about `?varName?` syntax in functions that don't support it.

---

## Gotcha #8: Property Access Uses Colon, Not Dot

**Problem:** SSL uses colon `:` for property access, not dot `.`.

```ssl
/* WRONG - dot notation doesn't work;
oEmail.Subject := "Test";
nCount := oDataset.RowCount;
```

**Solution:** Use colon notation.

```ssl
/* CORRECT;
oEmail:Subject := "Test";
nCount := oDataset:RowCount;
sValue := oDataset:GetValue(1, "FieldName");
```

**LSP Support:** Yes — detects dot notation and suggests colon notation.

---

## Gotcha #9: Assignment vs Equality

**Problem:** `:=` is assignment, `=` is comparison. Using wrong one in conditions.

```ssl
/* WRONG - this assigns, not compares!;
:IF x := 5;
    /* Always executes because x becomes 5 (truthy);
:ENDIF;
```

**Solution:** Use `=` or `==` for comparison.

```ssl
/* CORRECT;
:IF x = 5;
    /* Executes only if x equals 5;
:ENDIF;

/* Strict equality;
:IF sName == "Test";
    /* Exact match only;
:ENDIF;
```

**Note:** For strings, `=` is loose (true if left starts with right or right is empty), `==` is strict.

**LSP Support:** Yes — warns when `:=` is used in IF/WHILE/CASE conditions.

---

## Gotcha #10: String Equality Is Loose

**Problem:** The `=` operator for strings has unexpected behavior.

```ssl
sName := "Hello";

sName = "Hello"    /* .T. - exact match;
sName = "Hell"     /* .T. - left starts with right!;
sName = ""         /* .T. - right is empty!;
```

**Solution:** Use `==` for strict string comparison.

```ssl
sName == "Hello"   /* .T. - exact match only;
sName == "Hell"    /* .F. - not exact;
sName == ""        /* .F. - not exact;
```

**LSP Support:** Not currently detected.

---

## Gotcha #11: NIL vs Empty

**Problem:** NIL and empty string are different but both considered "empty".

```ssl
:DECLARE x;
/* x is "" (empty string), not NIL;

x := NIL;
/* Now x is NIL;

/* Testing;
Empty(x);      /* .T. for both NIL and "";
x = NIL;       /* Only true for NIL;
x = "";        /* Only true for empty string;
```

**Solution:** Use `Empty()` to check for both, or explicit comparisons for specific cases.

```ssl
:IF Empty(sValue);
    /* Handles NIL, "", and 0;
:ENDIF;

:IF sValue = NIL;
    /* Only handles NIL;
:ENDIF;
```

**LSP Support:** Not currently detected.

---

## Gotcha #12: Keywords Are Case-Sensitive (UPPERCASE)

**Problem:** Keywords must be uppercase.

```ssl
/* WRONG - lowercase keywords don't work;
:if condition;
:while x < 10;
:procedure MyProc;
```

**Solution:** Always use UPPERCASE keywords.

```ssl
/* CORRECT;
:IF condition;
:WHILE x < 10;
:PROCEDURE MyProc;
```

**LSP Support:** Handled by lexer/parser — lowercase keywords will cause parse errors, not explicit diagnostic warnings.

---

## Gotcha #13: Object Property Assignment Looks Like Undeclared Variable

**Problem:** `oObject:Property := value` might look like an undeclared variable to tools.

```ssl
oLogger:LogError := GetLastSQLError();
/* LogError is a property, not a variable that needs declaring;
```

**Solution:** This is correct SSL syntax. Object property assignments don't require declaration.

**LSP Support:** Yes — property assignments are recognized.

---

## Gotcha #14: Str() vs LimsString()

**Problem:** `Str()` and `LimsString()` have different purposes and may be confused.

```ssl
/* Str() - formats numbers with length and decimal places;
sNum := Str(123, 6, 2);      /* "123.00" (6 chars, 2 decimals);

/* LimsString() - general value-to-string conversion;
sNum := LimsString(123);     /* "123";
sDate := LimsString(dDate);  /* Converts date to string;
```

**Solution:** Use `Str()` for number formatting with specific length/decimals, use `LimsString()` for general conversion.

**LSP Support:** Not currently detected.

---

## Gotcha #15: Built-in Class Instantiation Uses Curly Braces

**Problem:** Built-in SSL classes use `{}` not `()` for instantiation.

```ssl
/* WRONG - parentheses for class instantiation;
oEmail := Email();
oRegex := SSLRegex('\d+');

/* CORRECT - curly braces for built-in classes;
oEmail := Email{};
oRegex := SSLRegex{'\d+'};

/* User-defined classes use CreateUdObject;
oCustom := CreateUdObject("MyClass", {params});
```

**LSP Support:** Yes — detects `ClassName()` patterns for built-in classes.

---

## Summary Table

| # | Gotcha | LSP Detects |
|---|--------|-------------|
| 1 | Direct procedure calls | Yes |
| 2 | Missing `:EXITCASE` | Yes |
| 3 | `:DEFAULT` with `:DECLARE` | Yes |
| 4 | Bare logical operators | Yes |
| 5 | 0-based array indexing | Yes |
| 6 | Semicolon in comments | Partial |
| 7 | Named params with RunSQL | Yes |
| 8 | Dot property notation | Yes |
| 9 | `:=` in conditions | Yes |
| 10 | Loose string equality | No |
| 11 | NIL vs Empty | No |
| 12 | Lowercase keywords | Yes |
| 13 | Property as undeclared | Yes |
| 14 | Str() vs LimsString() | No |
| 15 | Parentheses for class instantiation | Yes |
