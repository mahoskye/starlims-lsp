# SSL Grammar Reference

This document provides the formal SSL grammar specification in EBNF notation.

---

## Grammar Overview

SSL grammar follows a structured format with distinct productions for statements, expressions, and declarations.

### Top-Level Structure

```ebnf
program = { statement } ;

statement = procedure_declaration
          | class_declaration
          | variable_declaration
          | control_statement
          | expression_statement
          | return_statement ;
```

---

## Declarations

### Procedure Declaration

```ebnf
procedure_declaration = ":PROCEDURE" identifier ";"
                       [ parameters_clause ]
                       [ defaults_clause ]
                       [ declare_clause ]
                       { statement }
                       [ return_statement ]
                       ":ENDPROC" ";" ;

parameters_clause = ":PARAMETERS" parameter_list ";" ;
parameter_list = identifier { "," identifier } ;

defaults_clause = { ":DEFAULT" identifier "," expression ";" } ;

declare_clause = ":DECLARE" identifier_list ";" ;
identifier_list = identifier { "," identifier } ;
```

### Class Declaration

```ebnf
class_declaration = ":CLASS" identifier ";"
                   [ ":INHERIT" identifier ";" ]
                   { declare_clause }
                   { procedure_declaration } ;
```

### Variable Declaration

```ebnf
variable_declaration = ":DECLARE" identifier_list ";"
                     | ":PUBLIC" identifier_list ";" ;
```

---

## Control Statements

### Conditional

```ebnf
if_statement = ":IF" expression ";"
               { statement }
               [ ":ELSE" ";" { statement } ]
               ":ENDIF" ";" ;
```

### Loops

```ebnf
while_statement = ":WHILE" expression ";"
                  { statement }
                  ":ENDWHILE" ";" ;

for_statement = ":FOR" identifier ":=" expression
                ":TO" expression
                [ ":STEP" expression ]
                ";"
                { statement }
                ":NEXT" ";" ;
```

### Case Statement

```ebnf
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

### Error Handling

```ebnf
try_statement = ":TRY" ";"
                { statement }
                ":CATCH" ";"
                { statement }
                [ ":FINALLY" ";" { statement } ]
                ":ENDTRY" ";" ;
```

### Loop Control

```ebnf
loop_control = ":EXITFOR" ";"
             | ":EXITWHILE" ";"
             | ":LOOP" ";" ;
```

---

## Expressions

### Operator Precedence (Lowest to Highest)

```ebnf
expression = logical_or ;

logical_or = logical_and { ".OR." logical_and } ;

logical_and = comparison { ".AND." comparison } ;

comparison = addition { comparison_op addition } ;

comparison_op = "=" | "==" | "!=" | "<>" | "#" | "<" | ">" | "<=" | ">=" ;

addition = multiplication { ("+" | "-") multiplication } ;

multiplication = power { ("*" | "/" | "%") power } ;

power = unary { "^" unary } ;

unary = [ ".NOT." | "!" | "-" ] postfix ;

postfix = primary { index_access | property_access | method_call } ;

index_access = "[" expression { "," expression } "]" ;

property_access = ":" identifier ;

method_call = ":" identifier "(" [ argument_list ] ")" ;
```

### Primary Expressions

```ebnf
primary = literal
        | identifier
        | function_call
        | "(" expression ")"
        | array_literal
        | code_block ;

function_call = identifier "(" [ argument_list ] ")" ;

argument_list = expression { "," expression } ;

array_literal = "{" [ expression { "," expression } ] "}" ;

code_block = "{" "|" parameter_list "|" expression "}" ;
```

---

## Literals

```ebnf
literal = string_literal
        | numeric_literal
        | boolean_literal
        | nil_literal
        | date_literal ;

string_literal = '"' { character } '"'
               | "'" { character } "'"
               | "[" { character } "]" ;

numeric_literal = digit { digit } [ "." digit { digit } ]
                | digit { digit } "." digit { digit } "e" [ "+" | "-" ] digit { digit } ;

boolean_literal = ".T." | ".F." ;

nil_literal = "NIL" ;

date_literal = "{" year "," month "," day [ "," hour "," minute "," second ] "}" ;
```

---

## Tokens

### Keywords (32 Total)

All keywords are colon-prefixed and case-sensitive (UPPERCASE):

**Conditional:**
```
:IF, :ELSE, :ENDIF
```

**Loops:**
```
:FOR, :NEXT, :TO, :STEP
:WHILE, :ENDWHILE
:EXITFOR, :EXITWHILE, :LOOP
```

**Switch/Case:**
```
:BEGINCASE, :CASE, :ENDCASE, :OTHERWISE, :EXITCASE
```

**Exception Handling:**
```
:TRY, :CATCH, :FINALLY, :ENDTRY
:ERROR
```

**Procedures:**
```
:PROCEDURE, :ENDPROC
:RETURN
```

**Declarations:**
```
:DECLARE, :PARAMETERS, :DEFAULT, :PUBLIC
```

**Code Organization:**
```
:LABEL, :INCLUDE
:REGION, :ENDREGION
```

**Object-Oriented:**
```
:CLASS, :INHERIT
```

### Operators

**Assignment:**
```
:=   Assignment
+=   Add and assign
-=   Subtract and assign
*=   Multiply and assign
/=   Divide and assign
^=   Power and assign
%=   Modulo and assign
```

**Comparison:**
```
=    Equality (loose for strings)
==   Strict equality
!=   Not equal
<>   Not equal
#    Not equal
<    Less than
>    Greater than
<=   Less or equal
>=   Greater or equal
```

**Arithmetic:**
```
+    Addition / concatenation
-    Subtraction / negation
*    Multiplication
/    Division
^    Power
%    Modulo
```

**Logical (must include periods):**
```
.AND.   Logical AND
.OR.    Logical OR
.NOT.   Logical NOT
!       Negation (alternative)
```

**String:**
```
+    Concatenation
$    Contains
```

**Other:**
```
:    Property/method access
[]   Array indexing
{}   Array literal / object instantiation
()   Function call / grouping
```

---

## Identifier Rules

```ebnf
identifier = letter { letter | digit | "_" } ;

letter = "A" | "B" | ... | "Z" | "a" | "b" | ... | "z" ;

digit = "0" | "1" | ... | "9" ;
```

- Identifiers are case-insensitive
- Cannot start with a digit
- Cannot be a reserved keyword
- Recommended: Hungarian notation prefixes (`s`, `n`, `b`, `d`, `a`, `o`)

---

## Comments

```ebnf
comment = "/*" { any_character_except_semicolon } ";" ;
```

Comments start with `/*` and end at the first semicolon.

---

## Statement Termination

All statements (including comments) must end with a semicolon:

```ebnf
statement_terminator = ";" ;
```

---

## Special Syntax

### SQL Parameter Placeholders

**Named (SQLExecute only):**
```
?variableName?
```

**Positional (RunSQL, LSearch, etc.):**
```
?
```

### Object Instantiation

**Built-in classes:**
```ebnf
class_instantiation = class_name "{" [ argument_list ] "}" ;
```

**User-defined objects:**
```ebnf
udo_creation = "CreateUdObject" "(" [ string_literal [ "," array_literal ] ] ")" ;
```

---

## Machine-Readable Grammar

For parser implementation, see:

**LSP Parser:** `internal/parser/parser.go`
