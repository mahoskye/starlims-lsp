# SSL Syntax Reference

This document provides a comprehensive reference for SSL (STARLIMS Scripting Language) syntax.

> **See also:** [AGENTS.md](../../AGENTS.md) for quick coding conventions and anti-patterns.

---

## Statement Termination

All statements end with a semicolon, including comments:

```ssl
x := 10;
DoProc("MyProcedure", {});
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
| `:FOR`, `:TO`, `:STEP`, `:NEXT` | For loop |
| `:BEGINCASE`, `:CASE`, `:OTHERWISE`, `:EXITCASE`, `:ENDCASE` | Switch statement |
| `:EXITFOR`, `:EXITWHILE`, `:LOOP` | Loop control |

### Error Handling Keywords

| Keyword | Purpose |
|---------|---------|
| `:TRY`, `:CATCH`, `:FINALLY`, `:ENDTRY` | Structured exception handling |
| `:ERROR` | Legacy error handling marker |

### Declaration Keywords

| Keyword | Purpose |
|---------|---------|
| `:DECLARE` | Declare local variables |
| `:PARAMETERS` | Declare procedure parameters |
| `:DEFAULT` | Set parameter default values |
| `:PUBLIC` | Declare global/public variables |

### Procedure & Class Keywords

| Keyword | Purpose |
|---------|---------|
| `:PROCEDURE`, `:ENDPROC` | Define a procedure |
| `:RETURN` | Return value from procedure |
| `:CLASS`, `:INHERIT` | Define a class with inheritance |

### Other Keywords

| Keyword | Purpose |
|---------|---------|
| `:INCLUDE` | Include external script |
| `:REGION`, `:ENDREGION` | Code organization regions |
| `:LABEL` | Define a label |

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
:DECLARE myVariable;           /* Declare local variable;
:DECLARE var1, var2, var3;     /* Multiple variables;

:PARAMETERS param1, param2;    /* Procedure parameters;
:DEFAULT param1, "default";    /* Default value (after :PARAMETERS only);

:PUBLIC globalVar;             /* Global/public scope;
```

### Assignment

```ssl
myVariable := "value";         /* Basic assignment;
nCount += 1;                   /* Add and assign;
nCount -= 1;                   /* Subtract and assign;
nCount *= 2;                   /* Multiply and assign;
nCount /= 2;                   /* Divide and assign;
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
| `=` | Equality (loose for strings) |
| `==` | Strict equality |
| `!=` | Not equal |
| `<>` | Not equal (alternative) |
| `#` | Not equal (alternative) |
| `<` | Less than |
| `>` | Greater than |
| `<=` | Less than or equal |
| `>=` | Greater than or equal |

### Arithmetic Operators

| Operator | Description |
|----------|-------------|
| `+` | Addition / String concatenation |
| `-` | Subtraction |
| `*` | Multiplication |
| `/` | Division |
| `^` | Power (exponentiation) |
| `%` | Modulo |

### Logical Operators (Must Include Periods!)

| Operator | Description |
|----------|-------------|
| `.AND.` | Logical AND |
| `.OR.` | Logical OR |
| `.NOT.` | Logical NOT |
| `!` | Negation (alternative) |

```ssl
/* CORRECT;
:IF bCondA .AND. bCondB;
:IF bCondA .OR. bCondB;
:IF .NOT. bCondA;

/* WRONG - periods are required!;
:IF bCondA AND bCondB;  /* Will not work;
```

### String Operators

| Operator | Description |
|----------|-------------|
| `+` | Concatenation |
| `$` | Contains (returns `.T.` if left string is found in right) |

```ssl
sResult := "Hello " + "World";
bFound := "needle" $ "haystack";  /* .F.;
bFound := "hay" $ "haystack";     /* .T.;
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

### Date Literals

```ssl
dDate := {2024, 12, 25, 14, 30, 0};  /* year, month, day, hour, min, sec;
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

### CASE Statement (EXITCASE Required!)

```ssl
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

---

## Procedures

### Declaration

```ssl
:PROCEDURE MyProcedure;
:PARAMETERS param1, param2;
:DEFAULT param1, "default";
:DEFAULT param2, 0;
:DECLARE localVar;

/* Procedure body;
localVar := param1 + LimsString(param2);

:RETURN localVar;
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
result := ExecFunction("Module.MyProcedure", {"test", 123});

/* Skip parameters with empty array slots;
result := DoProc("MyProc", {param1,, param3});  /* Skips param2;
```

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

## Classes

### Class Definition

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
```

### Object Creation

```ssl
/* Built-in classes use curly braces;
oEmail := Email{};
oRegex := SSLRegex{'\d+'};

/* User-defined classes use CreateUdObject;
oCustom := CreateUdObject("MyClass");
oCustom := CreateUdObject("MyClass", {param1, param2});

/* Anonymous object;
oAnon := CreateUdObject();
oAnon:Property := "value";
```

---

## Code Organization

### Regions

```ssl
:REGION Database Operations;
    /* Code here;
:ENDREGION;

/* IDE folding (comment-based);
/* region Validation;
    /* Code here;
/* endregion;
```

### Includes

```ssl
:INCLUDE "library-name";
```
