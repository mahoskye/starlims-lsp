# SSL Grammar Reference

This document provides a summary of the formal SSL grammar. For the complete EBNF specification, see:

**Primary Source:** [`ssl-style-guide/reference/ssl-ebnf-grammar-complete.md`](../../ssl-style-guide/reference/ssl-ebnf-grammar-complete.md)

---

## Grammar Overview

SSL grammar follows a structured format with distinct productions for statements, expressions, and declarations.

### Top-Level Structure

```ebnf
program = { statement } ;
statement = procedure_declaration
          | variable_declaration
          | control_statement
          | expression_statement
          | return_statement ;
```

### Procedure Declaration

```ebnf
procedure_declaration = ":PROCEDURE" identifier ";"
                       [ parameters_clause ]
                       { statement }
                       ":ENDPROC" ";" ;

parameters_clause = ":PARAMETERS" parameter_list ";" ;
parameter_list = parameter { "," parameter } ;
parameter = identifier [ ":DEFAULT" expression ] ;
```

### Variable Declaration

```ebnf
variable_declaration = ":DECLARE" identifier_list ";"
                     | ":PUBLIC" identifier_list ";" ;

identifier_list = identifier { "," identifier } ;
```

### Control Statements

```ebnf
if_statement = ":IF" expression ";"
               { statement }
               [ ":ELSE" ";" { statement } ]
               ":ENDIF" ";" ;

while_statement = ":WHILE" expression ";"
                  { statement }
                  ":ENDWHILE" ";" ;

for_statement = ":FOR" identifier ":=" expression
                ":TO" expression
                [ ":STEP" expression ]
                ";"
                { statement }
                ":NEXT" ";" ;

case_statement = ":BEGINCASE" ";"
                 { case_clause }
                 [ otherwise_clause ]
                 ":ENDCASE" ";" ;

case_clause = ":CASE" expression ";"
              { statement }
              ":EXITCASE" ";" ;

otherwise_clause = ":OTHERWISE" ";"
                   { statement }
                   ":EXITCASE" ";" ;
```

### Expressions

```ebnf
expression = logical_or ;
logical_or = logical_and { ".OR." logical_and } ;
logical_and = comparison { ".AND." comparison } ;
comparison = addition { comparison_op addition } ;
addition = multiplication { ("+" | "-") multiplication } ;
multiplication = unary { ("*" | "/") unary } ;
unary = [ ".NOT." | "-" ] primary ;
primary = literal
        | identifier
        | function_call
        | property_access
        | "(" expression ")"
        | array_literal ;
```

### Literals

```ebnf
literal = string_literal
        | numeric_literal
        | boolean_literal
        | nil_literal ;

string_literal = '"' { character } '"'
               | "'" { character } "'" ;
numeric_literal = digit { digit } [ "." digit { digit } ] ;
boolean_literal = ".T." | ".F." ;
nil_literal = "NIL" ;
array_literal = "{" [ expression { "," expression } ] "}" ;
```

---

## Tokens

### Keywords

```
BEGINCASE, BEGININLINECODE, CASE, CATCH, CLASS, DECLARE, DEFAULT,
ELSE, ENDCASE, ENDIF, ENDINLINECODE, ENDPROC, ENDREGION, ENDTRY,
ENDWHILE, ERROR, EXITCASE, EXITFOR, EXITWHILE, FINALLY, FOR, IF,
INCLUDE, INHERIT, LABEL, LOOP, NEXT, OTHERWISE, PARAMETERS,
PROCEDURE, PUBLIC, REGION, RETURN, STEP, TO, TRY, WHILE
```

### Operators

```
:=   Assignment
=    Equality
<>   Not equal
<    Less than
>    Greater than
<=   Less or equal
>=   Greater or equal
+    Addition/concatenation
-    Subtraction/negation
*    Multiplication
/    Division
```

### Logical Operators

```
.AND.   Logical AND
.OR.    Logical OR
.NOT.   Logical NOT
```

---

## Machine-Readable Grammars

For tooling integration, additional grammar formats are available:

- **Tree-sitter:** [`ssl-style-guide/ssl-style-guide/tree-sitter-ssl/`](../../ssl-style-guide/ssl-style-guide/tree-sitter-ssl/)
- **Schema YAML:** [`ssl-style-guide/ssl-style-guide/ssl-style-guide.schema.yaml`](../../ssl-style-guide/ssl-style-guide/ssl-style-guide.schema.yaml)

---

## Complete Reference

See: [`ssl-style-guide/reference/ssl-ebnf-grammar-complete.md`](../../ssl-style-guide/reference/ssl-ebnf-grammar-complete.md)
