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

**Solution:** Use `DoProc` for same-file calls, `ExecFunction` for cross-file calls. Inside `:CLASS` methods, `DoProc` is a **compile-time error** (all forms, not just same-class calls) — use `Me:Method()` / `Base:Method()` instead.

```ssl
/* CORRECT - same file;
result := DoProc("CalculateTotal", {5, 10});

/* CORRECT - different file entry point;
result := ExecFunction("Category.Script", {5, 10});

/* CORRECT - different file specific procedure;
result := ExecFunction("Category.Script.CalculateTotal", {5, 10});

/* Skip parameters with empty array positions;
result := DoProc("MyProc", {param1,, param3});  /* Skips param2;

/* CORRECT - inside a :CLASS method;
Me:CalculateTotal(5, 10);
Base:InheritedMethod();
```

**LSP Support:** Yes — flags direct custom procedure calls and points users to `DoProc(...)` / `ExecFunction(...)`.

---

## Gotcha #2: Missing :EXITCASE Can Trigger Multiple Matching Cases

**Problem:** Without `:EXITCASE`, later `:CASE` expressions are still evaluated and their bodies run if they also match. This is multi-match behavior, not C-style fall-through.

```ssl
/* WRONG - both matching CASE blocks may run!;
:BEGINCASE;
:CASE nVal == 1;
    sResult := "one";
    /* Missing :EXITCASE - later CASE blocks can still run!;
:CASE nVal == 2;
    sResult := "two";
    :EXITCASE;
:ENDCASE;
```

**Solution:** Include `:EXITCASE` at the end of every `:CASE` and `:OTHERWISE` block. Note: `:OTHERWISE` is always skipped once any earlier `:CASE` body has run — even if that case omitted `:EXITCASE;`.

```ssl
/* CORRECT;
:BEGINCASE;
:CASE nVal == 1;
    sResult := "one";
    :EXITCASE;
:CASE nVal == 2;
    sResult := "two";
    :EXITCASE;
:OTHERWISE;
    sResult := "default";
    :EXITCASE;
:ENDCASE;
```

**LSP Support:** Yes — warns about missing `:EXITCASE`.

---

## Gotcha #3: :DEFAULT Only Works with :PARAMETERS (and Must Immediately Follow It)

**Severity:** Error (compile-time)

**Problem:** Using `:DEFAULT` with `:DECLARE` has no effect. Additionally, `:DEFAULT` must appear immediately after its corresponding `:PARAMETERS` block — placing any other statement between them is a compile error.

```ssl
/* WRONG - DEFAULT is ignored with :DECLARE!;
:DECLARE x;
:DEFAULT x, 10;
/* x is still empty string "", not 10;

/* WRONG - compile error! :DEFAULT separated from :PARAMETERS;
:PARAMETERS x;
:DECLARE sTemp;
:DEFAULT x, 10;
```

**Solution:** Use `:DEFAULT` only after `:PARAMETERS`, and place it immediately after.

```ssl
/* CORRECT - with parameters, immediately after;
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
x := 1;  /* This line is actually outside the comment;
```

**Solution:** Be careful with semicolons in comments.

```ssl
/* This is a proper comment without embedded semicolons
   that spans multiple lines
;
```

**LSP Support:** Partial. The LSP now warns when a comment token ends and more tokens continue on the same line, which catches the common accidental embedded-semicolon case.

---

## Gotcha #7: SQLExecute vs Other Database Functions

**Problem:** Only `SQLExecute` supports `?varName?` syntax.

```ssl
/* WRONG - RunSQL doesn't support named parameters;
RunSQL("UPDATE Table SET Field = ?sValue?", "", {});

/* WRONG - LSearch doesn't support named parameters;
sResult := LSearch("SELECT Name FROM T WHERE ID = ?nID?", "");
```

**Solution:** Use positional `?` with value arrays for `RunSQL`, `LSearch`, `LSelect`, `LSelect1`, `LSelectC`, and `GetDataSet`.

```ssl
/* CORRECT - SQLExecute with named params;
aResults := SQLExecute("SELECT * FROM T WHERE Field = ?sValue?");

/* CORRECT - RunSQL with positional params;
RunSQL("UPDATE T SET Field = ? WHERE ID = ?",, {sValue, nID});

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

**LSP Support:** Implemented conservatively. The LSP warns when either side can be inferred to be a string from literals, declared prefixes, straightforward assignments, constructors, or known built-in function returns. It does not attempt whole-program type propagation.

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

**LSP Support:** Implemented conservatively. The LSP warns on `NIL` comparisons against `""`, `0`, and `.F.` when the operands are directly visible or can be inferred from local declarations, assignments, constructors, or known built-in function returns. It does not attempt full semantic analysis of every `Empty()`-related case.

---

## Gotcha #12: Keywords Must Be Colon-Prefixed And Uppercase

**Problem:** SSL keywords are only valid in colon-prefixed uppercase form.

```ssl
/* WRONG - lowercase or bare keywords don't work;
:if condition;
while x < 10;
procedure MyProc;
```

**Solution:** Always use colon-prefixed UPPERCASE keywords.

```ssl
/* CORRECT;
:IF condition;
:WHILE x < 10;
:PROCEDURE MyProc;
```

**LSP Support:** Yes — reports explicit keyword-form diagnostics.

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

## Gotcha #14: `Str()` vs `LimsString()`

**Problem:** `Str()` and `LimsString()` serve different purposes and are easy to mix up.

```ssl
/* Str() - numeric formatting with width and decimals;
sNum := Str(123, 6, 2);      /* "123.00";

/* LimsString() - general value-to-string conversion;
sNum := LimsString(123);     /* "123";
sDate := LimsString(dDate);  /* Converts date to string;
```

**Solution:** Use `Str()` when you need numeric formatting with width and decimal precision. Use `LimsString()` when you need general value-to-string conversion.

**LSP Support:** Reference-only. The LSP recognizes both built-ins, but it does not currently diagnose choosing the wrong one.

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

## Gotcha #16: `!=` Is Not The Opposite of `=` For Strings

**Problem:** `!=` negates `==` (exact match), **not** `=` (prefix/loose match). This means `=` and `!=` are not logical opposites for strings.

```ssl
sVal := "Logged";

sVal = "Log"    /* .T. - "Logged" starts with "Log";
sVal != "Log"   /* .T. - "Logged" does not exactly equal "Log";
/* Both are .T. for the same operands! They are NOT opposites.;

sVal == "Log"   /* .F. - not an exact match;
.NOT. (sVal = "Log")  /* .F. - this is the true opposite of =;
```

**Solution:** To negate the loose `=` comparison, use `.NOT. (expr = value)` rather than `expr != value`. The `<>` and `#` operators behave identically to `!=` but `!=` is preferred.

```ssl
/* To check "does NOT start with 'Log'":;
:IF .NOT. (sVal = "Log");
    /* correct;
:ENDIF;

/* NOT this (incorrect):;
:IF sVal != "Log";
    /* wrong - != negates ==, not =;
:ENDIF;
```

**LSP Support:** Yes — the LSP warns when `!=` is used with string literals, since `!=` negates `==` (exact match) rather than `=` (prefix match).

---

## Gotcha #17: Function Name Casing

**Problem:** SSL function names are case-insensitive at runtime, but the canonical form uses PascalCase and certain names have fixed exceptions that tools must preserve.

```ssl
/* Inconsistent casing causes confusion and style violations;
sqLexecute("SELECT 1 FROM DUAL");
createudobject();
AEVAL(aArr, {|x| x});
```

**Solution:** Use PascalCase for built-in functions: `SQLExecute`, `CreateUdObject`, `AEval`, `AScan`. Preserve canonical exceptions exactly: `_AND`, `_OR`, `_XOR`, `_NOT`, `DOW`, `DOY`, `LIMSDate`.

```ssl
/* CORRECT;
SQLExecute("SELECT 1 FROM DUAL");
oObj := CreateUdObject();
AEval(aArr, {|x| x});
nDay := DOW(dDate);
```

**LSP Support:** Reference-only. The LSP uses canonical casing in completion and hover but does not flag wrong-cased function calls.

---

## Gotcha #18: SQL Parameter Names Are Case-Insensitive

**Problem:** `?varName?` placeholders in `SQLExecute` are case-insensitive, but this can cause confusion when variable names differ only in case.

```ssl
/* Both of these reference the same variable;
SQLExecute("SELECT * FROM T WHERE ID = ?sID?");
SQLExecute("SELECT * FROM T WHERE ID = ?SID?");
```

**Solution:** Always match the placeholder name to the exact variable name declared in the script for clarity. `?sID?` and `?SID?` both bind to `sID`.

**LSP Support:** Yes — the LSP matches SQL parameter placeholders case-insensitively.

---

## Gotcha #19: Default Variable Value Is Empty String, Not NIL

**Problem:** A variable declared with `:DECLARE` starts as empty string `""`, not `NIL`.

```ssl
:DECLARE sValue;
/* sValue is "" (empty string);

:IF sValue = NIL;
    /* This branch never executes! sValue is "", not NIL;
:ENDIF;

:IF Empty(sValue);
    /* This is correct - Empty() returns .T. for "" and NIL;
:ENDIF;
```

**Solution:** Use `Empty()` to test for uninitialized variables. Reserve `= NIL` only when you have explicitly set something to `NIL`.

**LSP Support:** Implemented conservatively. Related to Gotcha #11 (NIL vs Empty).

---

## Gotcha #20: SQLExecute Complex Expression Warning

**Problem:** Using expressions inside `?...?` placeholders in `SQLExecute` is legal but triggers a performance warning. The expression is evaluated on every query execution.

```ssl
/* BAD — expression computed inside placeholder each call;
aResult := SQLExecute("SELECT * FROM T WHERE Code = ?sPrefix + sSuffix?");
```

**Solution:** Pre-compute the value into a variable before passing it to `SQLExecute`.

```ssl
/* CORRECT — pre-computed;
sCode := sPrefix + sSuffix;
aResult := SQLExecute("SELECT * FROM T WHERE Code = ?sCode?");
```

**LSP Support:** Yes — the LSP warns when complex expressions appear inside `?...?` placeholders.

---

## Gotcha #21: `:FOR` Loop `:STEP` Keyword Spacing

**Problem:** `:STEP` in a `FOR` loop must be preceded by a space. Omitting it creates a parse ambiguity.

```ssl
/* WRONG - no space before :STEP;
:FOR i := 1 :TO 10:STEP 2;
```

**Solution:** Always place a space before `:STEP`.

```ssl
/* CORRECT;
:FOR i := 1 :TO 10 :STEP 2;
```

**LSP Support:** Yes — the formatter enforces correct spacing.

---

## Gotcha #22: `:REGION` Is a Functional Construct, Not a Folding Marker

**Problem:** `:REGION`/`:ENDREGION` looks like an IDE folding construct (similar to `// #region` in other languages) but it is a **functional body-capture construct**: the body text is stored and retrievable at runtime via `GetRegion()`.

```ssl
/* WRONG intent — if you just want code folding;
:REGION "MySection";
    /* ... code ...;
:ENDREGION;
```

**Solution:** For IDE code folding and procedure grouping, use comment-style markers instead. Reserve `:REGION`/`:ENDREGION` only when you actually need to call `GetRegion()` to retrieve the captured text at runtime.

```ssl
/* CORRECT — code folding only;
/* region MySection;
    /* ... code ...;
/* endregion;

/* CORRECT — functional body capture;
:REGION "ReportTemplate";
    [SELECT * FROM results WHERE status = 'Complete']
:ENDREGION;
nRegionContent := GetRegion("ReportTemplate");
```

**LSP Support:** Yes — the LSP emits an Info diagnostic when `:REGION` or `:ENDREGION` is used, noting it is a legacy functional construct and recommending `/* region` / `/* endregion` comments for code organization.

---

## Gotcha #23: `:ENDFOR` Is Not Valid

**Problem:** Although `:ENDFOR` exists as a recognized token in the lexer, it is **not a usable keyword**. FOR loops must be terminated with `:NEXT`, not `:ENDFOR` — using `:ENDFOR` causes a parse error.

```ssl
/* WRONG - :ENDFOR is not valid;
:FOR i := 1 :TO 10;
    nTotal += i;
:ENDFOR;

/* CORRECT - use :NEXT;
:FOR i := 1 :TO 10;
    nTotal += i;
:NEXT;
```

**Solution:** Always use `:NEXT` to close FOR loops.

**LSP Support:** Yes — the parser rejects `:ENDFOR`.

---

## Gotcha #24: `:FINALLY` Block Restrictions

**Problem:** Certain control-flow statements are **compile-time errors** inside a `:FINALLY` block: `:RETURN`, `:EXITFOR`, `:EXITWHILE`, and `:LOOP`.

```ssl
/* WRONG - compile error;
:TRY;
    DoProc("RiskyOperation");
:CATCH;
    oErr := GetLastSSLError();
:FINALLY;
    :RETURN NIL;  /* Compile error! :RETURN not allowed in :FINALLY;
:ENDTRY;
```

**Solution:** Move control-flow statements outside the `:FINALLY` block. Use `:FINALLY` only for cleanup actions like releasing resources.

```ssl
/* CORRECT;
:DECLARE bSuccess;
bSuccess := .F.;

:TRY;
    DoProc("RiskyOperation");
    bSuccess := .T.;
:CATCH;
    oErr := GetLastSSLError();
:FINALLY;
    oConnection := NIL;  /* Cleanup only;
:ENDTRY;

:IF .NOT. bSuccess;
    :RETURN NIL;
:ENDIF;
```

**LSP Support:** Yes — the parser reports `:RETURN`, `:EXITFOR`, `:EXITWHILE`, and `:LOOP` inside `:FINALLY` as compile-time errors.

---

## Gotcha #25: Visibility Annotations Have No Effect on Class Methods

**Severity:** Warning

**Problem:** The `/*@private;` and `/*@protected;` annotations restrict access for script-level procedures but have **no effect on class methods**. Class methods are always public regardless of any annotation.

```ssl
/* In a :CLASS file — annotation is silently ignored;
/*@private;
:PROCEDURE InternalHelper;
    /* This method is still accessible externally via Me:InternalHelper();
:ENDPROC;
```

**Solution:** Do not use `/*@private;` or `/*@protected;` on class methods. If you need to indicate a method is internal, use the underscore-prefix private convention (`_MethodName`), which excludes it from reflection.

```ssl
/* CORRECT - underscore convention signals private intent;
:PROCEDURE _InternalHelper;
    /* Excluded from reflection — the SSL private convention;
:ENDPROC;
```

**LSP Support:** Yes — the LSP warns when `/*@private;` or `/*@protected;` visibility annotations appear inside a class, since they have no effect on class methods (Info severity).

---

## Gotcha #26: :INCLUDE Placement

**Severity:** Info

**Problem:** `:INCLUDE` directives work anywhere in the file, but placing them in the middle of code makes dependencies hard to find.

```ssl
/* WRONG - include buried in the middle of the file;
:DECLARE sValue;
sValue := "test";
:INCLUDE File_Helpers.FileWork;
```

**Solution:** Place `:INCLUDE` directives at the top of the file, after any header comments, so dependencies are immediately visible.

```ssl
/* CORRECT - include at the top;
:INCLUDE File_Helpers.FileWork;

:DECLARE sValue;
sValue := "test";
```

**LSP Support:** Yes — the LSP flags `:INCLUDE` directives that appear after other significant statements (Info severity).

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
| 10 | Loose string equality | Implemented conservatively |
| 11 | NIL vs Empty | Implemented conservatively |
| 12 | Lowercase keywords | Yes |
| 13 | Property as undeclared | Yes |
| 14 | `Str()` vs `LimsString()` | Reference only |
| 15 | Parentheses for class instantiation | Yes |
| 16 | `!=` asymmetry with `=` for strings | Yes |
| 17 | Function name casing (PascalCase) | Reference only |
| 18 | SQL parameter names are case-insensitive | Yes |
| 19 | Default variable value is `""`, not `NIL` | Implemented conservatively |
| 20 | SQLExecute complex expression in placeholder | Yes |
| 21 | `:FOR` loop `:STEP` keyword spacing | Yes |
| 22 | `:REGION` is functional, not a folding marker | Yes (info diagnostic) |
| 23 | `:ENDFOR` is not valid — use `:NEXT` | Yes |
| 24 | `:FINALLY` block restrictions (no `:RETURN`, `:EXITFOR`, `:EXITWHILE`, `:LOOP`) | Yes |
| 25 | Visibility annotations (`/*@private;`, `/*@protected;`) have no effect on class methods | Yes |
| 26 | `:INCLUDE` placement — keep at top of file | Yes |
