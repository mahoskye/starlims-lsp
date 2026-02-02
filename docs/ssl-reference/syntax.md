# SSL Syntax Reference

This document provides an overview of SSL syntax. For the complete reference, see:

**Primary Source:** [`ssl-style-guide/reference/ssl_agent_instructions.md`](../../ssl-style-guide/reference/ssl_agent_instructions.md)

---

## Quick Syntax Reference

### Statement Termination

All statements end with a semicolon:

```ssl
x := 10;
DoSomething();
```

### Keywords

Keywords are prefixed with a colon:

```ssl
:PROCEDURE, :IF, :WHILE, :FOR, :DECLARE, :RETURN, :ENDPROC
```

### Comments

SSL uses block comments with semicolon termination:

```ssl
/* This is a single-line comment ;

/* This is a
   multi-line
   comment
;
```

### Variables

Declaration and assignment:

```ssl
:DECLARE myVariable;           /* Explicit declaration ;
myVariable := "value";         /* Assignment ;

:PARAMETERS param1, param2;    /* Procedure parameters ;
:PUBLIC globalVar;             /* Public/global scope ;
```

### Operators

| Type | Operators |
|------|-----------|
| Assignment | `:=` |
| Comparison | `=`, `<>`, `<`, `>`, `<=`, `>=` |
| Arithmetic | `+`, `-`, `*`, `/` |
| Logical | `.AND.`, `.OR.`, `.NOT.` |
| String | `+` (concatenation) |

### Literals

| Type | Examples |
|------|----------|
| String | `"text"`, `'text'` |
| Numeric | `123`, `3.14`, `-5` |
| Boolean | `.T.` (true), `.F.` (false) |
| Null | `NIL` |
| Array | `{1, 2, 3}`, `{"a", "b"}` |

### Control Flow

```ssl
/* IF/ELSE ;
:IF condition;
    /* then block ;
:ELSE;
    /* else block ;
:ENDIF;

/* WHILE loop ;
:WHILE condition;
    /* body ;
:ENDWHILE;

/* FOR loop ;
:FOR i := 1 :TO 10 :STEP 1;
    /* body ;
:NEXT;

/* CASE statement ;
:BEGINCASE;
:CASE condition1;
    /* action ;
    :EXITCASE;
:OTHERWISE;
    /* default ;
    :EXITCASE;
:ENDCASE;
```

### Procedures

```ssl
:PROCEDURE MyProcedure;
:PARAMETERS param1, param2 :DEFAULT "";
:DECLARE localVar;

/* procedure body ;

:RETURN result;
:ENDPROC;
```

### Error Handling

```ssl
:TRY;
    /* risky code ;
:CATCH;
    /* handle error ;
:FINALLY;
    /* cleanup ;
:ENDTRY;
```

### Property Access

```ssl
object:PropertyName          /* Read property ;
object:PropertyName := value /* Write property ;
object:Method()              /* Call method ;
```

---

## Complete Reference

For comprehensive syntax documentation including:
- All keywords and their usage
- Built-in functions
- Database operations
- File handling
- Advanced patterns

See: [`ssl-style-guide/reference/ssl_agent_instructions.md`](../../ssl-style-guide/reference/ssl_agent_instructions.md)
