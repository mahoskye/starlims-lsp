# SSL Syntax Reference

This document provides a comprehensive reference for SSL (STARLIMS Scripting Language) syntax.

> **See also:** [AGENTS.md](../../AGENTS.md) for quick coding conventions and anti-patterns.

---

## Statement Termination

All statements end with a semicolon, including comments:

```ssl
x := 10;
DoProc("MyProcedure");
/* This comment also ends with semicolon;
```

---

## Keywords

Keywords are colon-prefixed and UPPERCASE:

### Control Flow Keywords

| Keyword | Purpose |
|---------|---------|
| `:IF`, `:ELSE`, `:ENDIF` | Conditional branching |
| `:WHILE`, `:ENDWHILE` | While loop |
| `:FOR`, `:TO`, `:STEP`, `:NEXT` | For loop (`:ENDFOR` exists as a token but causes a parse error — always use `:NEXT`) |
| `:BEGINCASE`, `:CASE`, `:OTHERWISE`, `:EXITCASE`, `:ENDCASE` | Multi-branch conditional block |
| `:EXITFOR`, `:EXITWHILE`, `:LOOP` | Loop control |

### Error Handling Keywords

| Keyword | Purpose |
|---------|---------|
| `:TRY`, `:CATCH`, `:FINALLY`, `:ENDTRY` | Structured exception handling |
| `:ERROR` | Legacy error handling marker |
| `:RESUME` | Legacy resume-mode error handling keyword |

### Declaration Keywords

| Keyword | Purpose |
|---------|---------|
| `:DECLARE` | Declare local variables |
| `:PARAMETERS` | Declare procedure parameters |
| `:DEFAULT` | Set parameter default values |
| `:PUBLIC` | Declare global/public variables — **warning:** persists across all procedures and risks namespace pollution; prefer `:DECLARE` with parameter passing |

### Procedure & Class Keywords

| Keyword | Purpose |
|---------|---------|
| `:PROCEDURE`, `:ENDPROC` | Define a procedure |
| `:RETURN` | Return value from procedure |
| `:CLASS`, `:INHERIT` | Define a class with inheritance |

**Note:** There is NO `:ENDCLASS` keyword. A file can contain only one `:CLASS` declaration, and the class scope extends from `:CLASS` to the end of the file. All procedures defined after `:CLASS` become methods of that class.

### Other Keywords

| Keyword | Purpose |
|---------|---------|
| `:INCLUDE` | Include external script; keep includes at the top of the file |
| `:REGION`, `:ENDREGION` | Legacy functional text-capture regions; body text is stored for runtime retrieval via `GetRegion(sValue, vSrc, vDst)` |
| `:BEGININLINECODE`, `:ENDINLINECODE` | Legacy named inline-code storage blocks; `:BEGININLINECODE` **requires** an identifier (bare or quoted form) |
| `:LABEL` | Legacy `Branch()` target label; two forms: `:LABEL Name;` (spaced, preferred) and `:LABELName;` (compact) |

---

## Comments

SSL uses block comments that terminate with a semicolon:

```ssl
/* Single-line comment;

/* Multi-line comment
   spanning multiple lines
   ends with semicolon
;

/*
 * Documentation header style
 * Procedure: CalculateTotal
 * Parameters: nPrice, nQuantity
 * Returns: Numeric total
;
```

**Important:** A semicolon inside a comment terminates it:
```ssl
/* This ends here; this is CODE not comment!
```

---

## Variables

### Declaration

```ssl
:DECLARE sValue;               /* Declare local variable;
:DECLARE sName, nCount, aRows; /* Multiple variables;

:PARAMETERS sInput, nCount;    /* Procedure parameters;
:DEFAULT sInput, "default";    /* Default value (after :PARAMETERS only);

:PUBLIC gSharedValue;          /* Global/public scope (discouraged in new code);
```

Placement rules:

- Script-level `:PARAMETERS` must appear before top-level statements, though leading `:PROCEDURE` blocks may come first
- Inside a procedure, `:PARAMETERS` must immediately follow `:PROCEDURE`
- `:DEFAULT` must immediately follow `:PARAMETERS` (placing any statement between them is a compile error)

### Assignment

```ssl
sValue := "value";             /* Basic assignment;
nCount += 1;                   /* Add and assign;
nCount -= 1;                   /* Subtract and assign;
nCount *= 2;                   /* Multiply and assign;
nCount /= 2;                   /* Divide and assign;
nCount ^= 2;                   /* Power and assign;
nCount %= 3;                   /* Modulo and assign;
```

---

## Operators

### Assignment Operators

| Operator | Description |
|----------|-------------|
| `:=` | Assignment |
| `+=` | Add and assign |
| `-=` | Subtract and assign |
| `*=` | Multiply and assign |
| `/=` | Divide and assign |
| `^=` | Power and assign |
| `%=` | Modulo and assign |

### Comparison Operators

| Operator | Description |
|----------|-------------|
| `=` | Equality (loose; prefix-style for strings — see note below) |
| `==` | Strict equality (use for exact string equality) |
| `!=` | Not equal — negates `==`, **not** `=` (see note below) |
| `<>` | Not equal (not preferred — use `!=`) |
| `#` | Not equal (not preferred — use `!=`) |
| `<` | Less than |
| `>` | Greater than |
| `<=` | Less than or equal |
| `>=` | Greater than or equal |
| `$` | Containment — returns `.T.` if left string is found in right string (see also String Operators) |

**`!=` asymmetry:** `!=` negates `==` (exact match), not `=` (prefix match), so `=` and `!=` are **not logical opposites** for strings. For example, `"Logged" = "Log"` is `.T.` (prefix match) AND `"Logged" != "Log"` is also `.T.` (not an exact match). The `<>` and `#` operators behave identically to `!=`. To negate the loose `=` comparison, use `.NOT. (expr = value)` rather than `expr != value`.

### Arithmetic Operators

| Operator | Description |
|----------|-------------|
| `+` | Addition / String concatenation |
| `-` | Subtraction |
| `*` | Multiplication |
| `/` | Division |
| `^` | Power (exponentiation) |
| `**` | Power (alias for `^`) |
| `%` | Modulo |
| `++` | Increment |
| `--` | Decrement |
| `<<` | Bitwise left shift |
| `>>` | Bitwise right shift |

### Logical Operators (Must Include Periods!)

| Operator | Description |
|----------|-------------|
| `.AND.` | Logical AND (short-circuit) |
| `.OR.` | Logical OR (short-circuit) |
| `.NOT.` | Logical NOT |
| `!` | Negation (alternative) |

`.AND.` and `.OR.` use **short-circuit evaluation**: if the first operand determines the result, the second operand is not evaluated.

```ssl
/* CORRECT;
:IF bCondA .AND. bCondB;
:IF bCondA .OR. bCondB;
:IF .NOT. bCondA;

/* WRONG - periods are required!;
:IF bCondA AND bCondB;  /* Will not work;
```

### Operator Precedence (Low to High)

| Level | Operators | Associativity |
|-------|-----------|---------------|
| 1 | `.OR.` | Left |
| 2 | `.AND.` | Left |
| 3 | `=`, `==`, `!=`, `<>`, `#`, `$` | Left |
| 4 | `<`, `>`, `<=`, `>=` | Left |
| 5 | `<<`, `>>` | Left |
| 6 | `+`, `-` | Left |
| 7 | `*`, `/`, `%` | Left |
| 8 | `^`, `**` | **Right** |
| 9 | `-` (unary), `!`, `.NOT.` | Unary |

Level 3 groups equality, inequality, and containment together. Level 8 (power) is right-associative: `2^3^2` evaluates as `2^(3^2)` = 512.

### String Operators

| Operator | Description |
|----------|-------------|
| `+` | Concatenation |
| `-` | Trim trailing spaces from left operand, then concatenate |
| `$` | Contains (returns `.T.` if left string is found in right) |
| `[n]` | Character at position n (1-based) |

```ssl
sResult := "Hello " + "World";
sResult := "Hello   " - "World";  /* "HelloWorld" (trims trailing spaces first);
bFound := "needle" $ "haystack";  /* .F.;
bFound := "hay" $ "haystack";     /* .T.;
cFirst := "Hello"[1];             /* "H";
```

---

## Literals

### String Literals

Three quote styles are available:

```ssl
s1 := "double quotes";
s2 := 'single quotes';
s3 := [bracket quotes];  /* Useful for SQL with embedded quotes;
```

SSL does **not** have C-style escape sequences. Backslashes are literal characters, and a quote still closes the string:

```ssl
sPath := "C:\Temp\file.txt";
sLiteral := "Backslash \";  /* Ends at the quote after the backslash;
```

Multi-line strings are supported:
```ssl
sSQL := "SELECT *
         FROM Customers
         WHERE Status = 'A'";
```

### Numeric Literals

```ssl
n1 := 123;       /* Integer;
n2 := 3.14;      /* Decimal;
n3 := -5;        /* Negative;
n4 := 1.2e-3;    /* Scientific notation;
```

Scientific notation requires a decimal point before the exponent and must not use an explicit plus sign. Valid forms: `1.2e-3`, `4.56E-3`, `0.5e1`. Invalid forms: `9E+1` (plus sign not allowed), `7e2` (no decimal point), `.5e1` (leading decimal without zero — use `0.5e1`).

Division always produces a floating-point result: `5 / 2` yields `2.5`, not `2`. Use `Integer(n)` for explicit truncation.

Bitwise built-in functions (`_AND`, `_OR`, `_NOT`, `_XOR`) require integer-valued operands and raise errors on fractional values.

### Boolean Literals

```ssl
bTrue := .T.;    /* True;
bFalse := .F.;   /* False;
```

### Null Literal

```ssl
xValue := NIL;   /* Null/undefined;
```

### Array Literals (1-Based Indexing!)

```ssl
aItems := {1, 2, 3};
aNames := {"Alice", "Bob", "Charlie"};
aMatrix := {{1, 2}, {3, 4}};  /* 2D array;

/* Access (first element is index 1, not 0);
sFirst := aNames[1];  /* "Alice";
n := aMatrix[2, 1];   /* 3;
```

### Date Values

SSL has **no date literal syntax**. The brace form `{2024, 12, 25}` is an array literal, not a date. Dates are created via functions:

```ssl
dToday := Today();                              /* Current date;
dNow := Now();                                 /* Current date and time;
dParsed := CToD("12/25/2024");                 /* Parse from string;
dFrom := DateFromNumbers(2024, 12, 25, 14, 30, 0);  /* From components;
```

---

## Control Flow

### IF/ELSE

```ssl
:IF condition;
    /* then block;
:ELSE;
    /* else block;
:ENDIF;

/* Without ELSE;
:IF condition;
    /* then block;
:ENDIF;
```

### WHILE Loop

```ssl
:WHILE condition;
    /* body;
    :IF shouldExit;
        :EXITWHILE;
    :ENDIF;
:ENDWHILE;
```

### FOR Loop

```ssl
/* Basic FOR loop;
:FOR i := 1 :TO 10;
    /* body;
:NEXT;

/* With STEP;
:FOR i := 10 :TO 1 :STEP -1;
    /* body;
:NEXT;

/* Exit early;
:FOR i := 1 :TO 100;
    :IF i = 50;
        :EXITFOR;
    :ENDIF;
:NEXT;

/* Continue to next iteration;
:FOR i := 1 :TO 100;
    :IF i % 2 = 0;
        :LOOP;  /* Skip even numbers;
    :ENDIF;
    /* Process odd numbers;
:NEXT;
```

The `:FOR` start value, `:TO` limit, and optional `:STEP` value should be numeric. The LSP warns when those values can be inferred as non-numeric from literals, declared prefixes, straightforward assignments, constructors, or known built-in function returns.

### CASE Statement (`:EXITCASE` Recommended)

Without `:EXITCASE;`, later `:CASE` expressions are still evaluated and their bodies run if they also match. This is not C-style fall-through, but it can still execute multiple case blocks. `:OTHERWISE` is always skipped once any earlier `:CASE` body has run — even if that earlier case omitted `:EXITCASE;`.

```ssl
:BEGINCASE;
:CASE nVal == 1;
    sResult := "one";
    :EXITCASE;
:CASE nVal == 2;
    sResult := "two";
    :EXITCASE;
:OTHERWISE;
    sResult := "other";
    :EXITCASE;
:ENDCASE;
```

---

## Procedures

### Declaration

```ssl
:PROCEDURE MyProcedure;
:PARAMETERS sParam1, nParam2;
:DEFAULT sParam1, "default";
:DEFAULT nParam2, 0;
:DECLARE sLocalValue;

/* Procedure body;
sLocalValue := sParam1 + LimsString(nParam2);

:RETURN sLocalValue;
:ENDPROC;
```

### Calling Procedures

**Important:** Direct procedure calls are NOT supported. Use `DoProc` or `ExecFunction`:

```ssl
/* WRONG - will not work;
MyProcedure("test", 123);

/* CORRECT - same file;
result := DoProc("MyProcedure", {"test", 123});

/* CORRECT - different file;
result := ExecFunction("Category.Script.Proc", {"test", 123});

/* Preferred when there are no arguments;
result := DoProc("MyProcedure");
result := ExecFunction("Category.Script.Proc");

/* Skip parameters with empty array slots;
result := DoProc("MyProc", {sFirst,,nThird});  /* Skips param2;
```

`DoProc(...)` is a **compile-time error** inside class methods. Use `Me:MethodName()` / `Base:MethodName()` for sibling and inherited methods instead.

---

## Error Handling

### TRY/CATCH/FINALLY

```ssl
:TRY;
    /* Risky code;
    aResults := SQLExecute(sSQL);
:CATCH;
    /* Handle error;
    oErr := GetLastSSLError();
    UsrMes("Error: " + oErr:Description, "Error");
:FINALLY;
    /* Cleanup (always runs);
    oConnection := NIL;
:ENDTRY;
```

`:TRY` bodies must contain at least one statement before `:CATCH` or `:FINALLY`, and `:FINALLY` blocks must not be empty.
At least one of `:CATCH` or `:FINALLY` must be present. Only one `:CATCH` block is allowed, `:CATCH` does not name an exception variable, and `:RETURN`, `:EXITWHILE`, `:EXITFOR`, and `:LOOP` are compile-time errors inside `:FINALLY`.
`:CATCH` must appear before `:FINALLY`.

```ssl
/* WRONG - :CATCH does not bind an exception variable;
:CATCH oErr;

/* CORRECT;
:CATCH;
    oErr := GetLastSSLError();
```

### ERROR/RESUME (Legacy)

```ssl
:ERROR;
    oErr := GetLastSSLError();
    UsrMes(oErr:Description, "Error");
:RESUME;
```

Legacy `:ERROR` handlers apply to subsequent code in the current scope and must contain at least one statement. `:RESUME` switches execution into resume mode, which has significant runtime cost; prefer targeted `:TRY` / `:CATCH` / `:FINALLY` blocks in new or refactored code.

### Error Functions

| Function | Description |
|----------|-------------|
| `GetLastSSLError()` | Get last SSL error object |
| `GetLastSQLError()` | Get last SQL error |
| `RaiseError(msg)` | Raise custom error |
| `FormatErrorMessage(err)` | Format error for display |

---

## Property Access

SSL uses colon notation for object properties and methods:

Spaced member access forms such as `oObject : PropertyName` are accepted by the language, but style should prefer `oObject:PropertyName` with no spaces around the colon.

```ssl
/* Read property;
sName := oObject:PropertyName;

/* Write property;
oObject:PropertyName := "value";

/* Call method;
result := oObject:MethodName(arg1, arg2);

/* Chained access;
sValue := oDataset:Fields("Name"):Value;
```

---

## Branch Labels

`Branch()` is a legacy control-flow function. Its string target must include the label token text, including the word `LABEL`:

```ssl
/* CORRECT - spaced label token text;
:LABEL SKIP;
Branch("LABEL SKIP");

/* CORRECT - compact label token text;
:LABELSKIP;
Branch("LABELSKIP");

/* WRONG - omitting LABEL causes a runtime failure;
Branch("SKIP");
```

---

## Classes

### Class Definition

**Important:**
- A file can contain only ONE `:CLASS` declaration
- There is NO `:ENDCLASS` keyword
- The class scope extends from `:CLASS` to the end of the file
- All procedures defined after `:CLASS` become methods of that class
- Member order is `:INHERIT`, `:DECLARE`, regular methods, then `Constructor` — tooling enforces this order
- Bare and qualified `:INHERIT` names are both accepted
- Without `:INHERIT`, a class inherits from `SSLObject` by default
- `Constructor` is the reserved constructor method name inside a class, and `:RETURN` inside `Constructor` cannot return a value
- Inside class methods, use `Me:MethodName()` / `Base:MethodName()` for sibling and inherited method calls
- `Me` is only meaningful inside a `:CLASS` definition
- `Base` must always be used as `Base:MemberName` and is only meaningful when the class declares `:INHERIT`
- `Me`, `Base`, and `Constructor` are **case-insensitive** — `me`, `base`, `constructor` are all valid forms, though PascalCase is canonical
- Underscore-prefixed members such as `_sInternal` follow the SSL private convention and are excluded from reflection
- `/*@private;` and `/*@protected;` annotations apply to script procedures only; they do not change class-method visibility

```ssl
:CLASS MyClass;
:INHERIT BaseClass;  /* Optional inheritance;

:DECLARE sProperty, nValue;

:PROCEDURE Initialize;
:PARAMETERS sName;
    Me:sProperty := sName;
    Me:nValue := 0;
:ENDPROC;

:PROCEDURE Increment;
    Me:nValue := Me:nValue + 1;
    :RETURN Me:nValue;
:ENDPROC;
/* No :ENDCLASS - class extends to end of file;
```

### Object Creation

```ssl
/* Built-in classes use curly braces;
oEmail := Email{};
oRegex := SSLRegex{'\d+'};

/* User-defined classes use CreateUdObject;
oCustom := CreateUdObject("MyClass");
oCustom := CreateUdObject("MyClass", {param1, param2});

/* Empty dynamic object;
oAnon := CreateUdObject();
oAnon:Property := "value";

/* Anonymous object with named properties;
oSeeded := CreateUdObject({{"Property", "value"}});
```

---

## Code Organization

### Regions

```ssl
/* Functional text-capture region (legacy);
:REGION DataBlock;
    Raw text content;
:ENDREGION;

/* IDE folding (comment-based);
/* region Validation;
    /* Code here;
/* endregion;
```


### Includes

```ssl
:INCLUDE File_Helpers.FileWork;
```

Place `:INCLUDE` directives at the top of the file, after any header comments, so dependencies are easy to find.
