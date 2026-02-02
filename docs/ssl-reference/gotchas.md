# Common SSL Gotchas

This document highlights common pitfalls and mistakes when writing SSL code.

**Primary Source:** [`ssl-style-guide/reference/ssl_agent_instructions.md`](../../ssl-style-guide/reference/ssl_agent_instructions.md) (Gotchas section)

---

## Gotcha #1: `:DEFAULT` Only Works with `:PARAMETERS`

**Problem:** Using `:DEFAULT` with `:DECLARE` has no effect.

```ssl
/* WRONG - DEFAULT is ignored! ;
:DECLARE x :DEFAULT 10;
/* x is NIL, not 10 ;

/* CORRECT - Use with PARAMETERS ;
:PARAMETERS x :DEFAULT 10;

/* CORRECT - Or assign after declare ;
:DECLARE x;
x := 10;
```

**LSP Support:** The diagnostics provider warns about this pattern.

---

## Gotcha #2: Logical Operators Need Periods

**Problem:** Using `AND`, `OR`, `NOT` without periods creates logic errors.

```ssl
/* WRONG - These are treated as identifiers! ;
:IF condition1 AND condition2;
:IF condition1 OR condition2;
:IF NOT condition;

/* CORRECT - Wrap in periods ;
:IF condition1 .AND. condition2;
:IF condition1 .OR. condition2;
:IF .NOT. condition;
```

**LSP Support:** The diagnostics provider reports bare logical operators as errors.

---

## Gotcha #3: Comments End with Semicolon

**Problem:** SSL uses `/* ... ;` for comments, not `/* ... */`.

```ssl
/* WRONG - This doesn't end the comment! */
More code here;

/* CORRECT - Semicolon ends the comment ;
More code here;
```

**Important:** A semicolon inside a comment terminates it:

```ssl
/* This comment has a semicolon; this is CODE now!
```

---

## Gotcha #4: Case Fallthrough in BEGINCASE

**Problem:** Unlike C/C++, SSL `CASE` blocks DO fall through without `:EXITCASE`.

```ssl
/* WRONG - Falls through to next case! ;
:BEGINCASE;
:CASE x = 1;
    DoOne();
    /* Missing :EXITCASE causes fallthrough! ;
:CASE x = 2;
    DoTwo();
    :EXITCASE;
:ENDCASE;

/* CORRECT - Always use :EXITCASE ;
:BEGINCASE;
:CASE x = 1;
    DoOne();
    :EXITCASE;
:CASE x = 2;
    DoTwo();
    :EXITCASE;
:ENDCASE;
```

**LSP Support:** The diagnostics provider warns about missing `:EXITCASE`.

---

## Gotcha #5: Assignment vs Equality

**Problem:** `:=` is assignment, `=` is comparison.

```ssl
/* WRONG - This assigns, not compares! ;
:IF x := 5;
    /* Always executes because x is now 5 (truthy) ;
:ENDIF;

/* CORRECT - Use = for comparison ;
:IF x = 5;
    /* Only executes if x equals 5 ;
:ENDIF;
```

---

## Gotcha #6: No Backslash Escapes in Strings

**Problem:** SSL strings do NOT use backslash escapes.

```ssl
/* WRONG - Backslash is literal! ;
path := "C:\Users\Name";  /* This is fine, \ is just a character ;
newline := "\n";           /* This is literally backslash-n, not a newline ;

/* Use Chr() for special characters ;
newline := Chr(10);        /* ASCII 10 = newline ;
tab := Chr(9);             /* ASCII 9 = tab ;
```

---

## Gotcha #7: String Concatenation with +

**Problem:** Type coercion issues with `+` operator.

```ssl
/* Works fine ;
sResult := "Hello " + "World";

/* Numeric + String may cause issues ;
sResult := "Count: " + nCount;  /* May need explicit conversion ;

/* SAFER - Use Str() for numbers ;
sResult := "Count: " + Str(nCount);
```

---

## Gotcha #8: Variable Scope

**Problem:** Variables are accessible in ways you might not expect.

```ssl
/* Variables in a procedure are local ;
:PROCEDURE ProcA;
:DECLARE x;
x := 10;
:ENDPROC;

:PROCEDURE ProcB;
x := 20;  /* This creates a NEW local variable x, not the one from ProcA ;
:ENDPROC;

/* PUBLIC makes it global ;
:PUBLIC gCounter;  /* Visible everywhere ;
```

---

## Gotcha #9: NIL vs Empty

**Problem:** NIL and empty are different.

```ssl
:DECLARE x;
/* x is NIL (uninitialized) ;

x := "";
/* x is now empty string, NOT NIL ;

:IF x = NIL;  /* True before assignment, false after ;
:IF Empty(x); /* True for both NIL and "" ;
```

---

## Gotcha #10: Array Indexing Starts at 1

**Problem:** Arrays are 1-indexed, not 0-indexed.

```ssl
:DECLARE a;
a := {10, 20, 30};

a[1];  /* 10 - First element ;
a[0];  /* ERROR - Invalid index! ;
```

---

## Gotcha #11: `Me` in Classes

**Problem:** `Me` is the self-reference in classes.

```ssl
:CLASS MyClass;
:PROCEDURE Initialize;
    /* Use Me to refer to instance properties ;
    Me:Counter := 0;
    Me:Name := "";
:ENDPROC;

:PROCEDURE Increment;
    Me:Counter := Me:Counter + 1;
    :RETURN Me:Counter;
:ENDPROC;
:ENDCLASS;
```

**LSP Support:** `Me` should never be flagged as undeclared.

---

## Gotcha #12: SQL Parameter Case

**Problem:** SQL `?param?` placeholders are case-insensitive.

```ssl
:PARAMETERS sCustomerID;

/* All of these work ;
sSQL := "SELECT * FROM t WHERE id = ?sCustomerID?";
sSQL := "SELECT * FROM t WHERE id = ?SCUSTOMERID?";
sSQL := "SELECT * FROM t WHERE id = ?scustomerid?";
```

**Recommendation:** Use consistent casing for readability.

---

## Summary Table

| Gotcha | Issue | LSP Detects |
|--------|-------|-------------|
| #1 | `:DEFAULT` with `:DECLARE` | Yes |
| #2 | Bare logical operators | Yes |
| #3 | Comment syntax | Partial |
| #4 | Missing `:EXITCASE` | Yes |
| #5 | `:=` vs `=` confusion | No |
| #6 | Backslash in strings | No |
| #7 | String concatenation | No |
| #8 | Variable scope | No |
| #9 | NIL vs Empty | No |
| #10 | Array indexing | No |
| #11 | `Me` in classes | Yes (planned) |
| #12 | SQL param case | Planned |
