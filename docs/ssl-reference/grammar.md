# SSL Grammar Reference

This document is a source-aligned summary of the SSL v11 grammar used by the LSP.

**Authority:** `dev/ssl-style-guide/ssl-style-guide/ssl-ebnf-grammar.md` is the canonical grammar. When this summary lags, the source file wins.

---

## Top-Level Structure

```ebnf
Program ::= ClassDefinition | {Statement}
(* A script can be a class definition or a series of statements *)

Statement ::= (
    ProcedureStatement |
    ConditionalStatement |
    LoopStatement |
    SwitchStatement |
    ErrorHandlingStatement |
    ErrorBlockStanza |
    DeclarationStatement |
    LogicStatement |
    CommentStatement |
    LabelStatement |
    RegionBlock |
    InlineCodeBlock |
    BranchStatement |
    DatabaseStatement
) ";"
```

Key implications:
- A file is either a `:CLASS` file or a script.
- There is no `:ENDCLASS`; class scope extends to end of file.
- Legacy constructs such as `:REGION`, `:BEGININLINECODE`, `:ERROR`, and `:RESUME` still exist in the broader syntax surface even though new code should usually avoid them.

---

## Classes And Procedures

```ebnf
ClassDefinition ::= ClassDeclaration [InheritStatement] {ClassMember}
ClassDeclaration ::= ":" "CLASS" [Identifier]
InheritStatement ::= ":" "INHERIT" Identifier
ClassMember ::= ClassFieldDeclaration | MethodDeclaration
ClassFieldDeclaration ::= ":" "DECLARE" IdentifierList
MethodDeclaration ::= ProcedureStatement

ProcedureStatement ::= ProcedureStart [ParameterDeclaration] [DefaultParameterDeclaration] {Statement} ProcedureEnd
ProcedureStart ::= ":" "PROCEDURE" Identifier
ProcedureEnd ::= ":" "ENDPROC"

ParameterDeclaration ::= ":" "PARAMETERS" ParameterList
DefaultParameterDeclaration ::= ":" "DEFAULT" DefaultParameterList
ParameterList ::= Identifier {"," Identifier}
DefaultParameterList ::= Identifier "," Expression
```

Source-aligned notes:
- Inside a procedure, `:PARAMETERS` must appear immediately after `:PROCEDURE`.
- `:DEFAULT` must immediately follow `:PARAMETERS`.
- Inside `:CLASS`, `Constructor` is the reserved constructor procedure name.

---

## Control Flow

```ebnf
ConditionalStatement ::= IfStatement | ElseStatement | EndIfStatement
IfStatement ::= ":" "IF" Expression
ElseStatement ::= ":" "ELSE"
EndIfStatement ::= ":" "ENDIF"

LoopStatement ::= WhileLoop | ForLoop | ExitWhileStatement | ExitForStatement | LoopContinue
WhileLoop ::= WhileStatement {Statement} EndWhileStatement
WhileStatement ::= ":" "WHILE" Expression
EndWhileStatement ::= ":" "ENDWHILE"
ExitWhileStatement ::= ":" "EXITWHILE"

ForLoop ::= ForStatement {Statement} NextStatement
ForStatement ::= ":" "FOR" Identifier ":=" Expression ":" "TO" Expression [":" "STEP" Expression]
NextStatement ::= ":" "NEXT"
ExitForStatement ::= ":" "EXITFOR"
LoopContinue ::= ":" "LOOP"

ResumeStatement ::= ":" "RESUME"
StepStatement   ::= ":" "STEP"
(* ResumeStatement is the legacy resume-mode marker; StepStatement appears inside ForStatement. *)
```

---

## CASE Blocks

```ebnf
SwitchStatement ::= BeginCaseStatement CaseBlock {CaseBlock} [OtherwiseBlock] EndCaseStatement
BeginCaseStatement ::= ":" "BEGINCASE"
CaseBlock ::= CaseStatement {Statement} [ExitCaseStatement]
CaseStatement ::= ":" "CASE" Expression
OtherwiseBlock ::= OtherwiseStatement {Statement}
OtherwiseStatement ::= ":" "OTHERWISE"
EndCaseStatement ::= ":" "ENDCASE"
ExitCaseStatement ::= ":" "EXITCASE"
```

Important behavior:
- `:BEGINCASE` is not a value-switch; each `:CASE` evaluates its own boolean expression.
- Without `:EXITCASE;`, later matching `:CASE` bodies may also run.
- `:BEGINCASE` requires at least one `:CASE`.

---

## TRY, CATCH, FINALLY

```ebnf
ErrorHandlingStatement ::= TryBlock
TryBlock ::= TryStatement Statement {Statement} (CatchBlock [FinallyBlock] | FinallyBlock) EndTryStatement
TryStatement ::= ":" "TRY"
CatchBlock ::= CatchStatement {Statement}
CatchStatement ::= ":" "CATCH"
FinallyBlock ::= FinallyStatement Statement {Statement}
FinallyStatement ::= ":" "FINALLY"
EndTryStatement ::= ":" "ENDTRY"

ErrorBlockStanza ::= ErrorMarker {Statement}
ErrorMarker ::= ":" "ERROR"
```

Important behavior:
- `:TRY` must contain at least one statement before `:CATCH` or `:FINALLY`.
- At least one of `:CATCH` or `:FINALLY` is required.
- `:CATCH` does not declare an exception variable; use `GetLastSSLError()` in the catch body.
- `:FINALLY` must contain at least one statement.
- `:RETURN`, `:EXITFOR`, `:EXITWHILE`, and `:LOOP` are compile-time errors inside `:FINALLY`.
- `:ERROR` is a legacy handler form and must contain at least one statement before `:RESUME` or the end of the current scope.

---

## Legacy Text-Capture Constructs

```ebnf
RegionBlock ::= RegionStart {Character} RegionEnd
RegionStart ::= ":" "REGION" Identifier ";"
RegionEnd ::= ":" "ENDREGION" ";"

InlineCodeBlock ::= InlineCodeStart {Statement} InlineCodeEnd
InlineCodeStart ::= ":" "BEGININLINECODE" [StringLiteral | Identifier] ";"
InlineCodeEnd ::= ":" "ENDINLINECODE" ";"

LabelStatement ::= ":" "LABEL" Identifier
BranchStatement ::= Identifier "(" StringLiteral ")"
```

Source-aligned notes:
- `:REGION` / `:ENDREGION` store raw text and are not modern editor-folding markers.
- `:BEGININLINECODE` must include a name.
- `Branch()` targets must include the label token text, such as `"LABEL SKIP"` or `"LABELSKIP"`.

---

## Expressions

```ebnf
LogicStatement ::= Assignment | FunctionCall | Expression | ReturnStatement
ReturnStatement ::= ":" "RETURN" [Expression]

Assignment ::= (VariableAccess | PropertyAccess) AssignmentOperator Expression
AssignmentOperator ::= ":=" | "+=" | "-=" | "*=" | "/=" | "^=" | "%="

FunctionCall ::= DirectFunctionCall | IndirectFunctionCall
DirectFunctionCall ::= Identifier "(" [ArgumentList] ")"
IndirectFunctionCall ::= Identifier "(" StringLiteral ["," ArrayLiteral] ")"
(* The array argument is optional when there are no parameters:
   DoProc("Name") is preferred over DoProc("Name", {}) when no arguments. *)
ArgumentList ::= Expression {"," Expression}

IncrementExpression ::= Identifier ("++" | "--") | ("++" | "--") Identifier

ObjectCreation     ::= Identifier "{" [ArgumentList] "}"
MethodCall         ::= Expression ":" Identifier "(" [ArgumentList] ")"
ObjectPropertyAccess ::= Expression ":" Identifier
DynamicCodeExecution ::= Identifier "(" StringLiteral ["," ArrayLiteral] ")"
(* DoProc and ExecFunction are the canonical dynamic-call functions. *)

DatabaseStatement     ::= DatabaseFunctionCall
DatabaseFunctionCall  ::= Identifier "(" [ArgumentList] ")"
(* SQL functions such as SQLExecute, RunSQL, LSearch; distinguished by context. *)
DatabaseParameter ::= "?" Identifier "?" | "?"
```

Source-aligned notes:
- Built-in functions use normal call syntax.
- Custom procedures are not called directly; use `DoProc(...)` / `ExecFunction(...)`, or `Me:Method()` / `Base:Method()` inside classes.
- Property and method access use colon notation, not dot notation.
- `DoProc("Name")` (no second argument) is preferred when there are no parameters.

---

## Literals And Core Tokens

```ebnf
Literal ::= NumberLiteral | StringLiteral | BooleanLiteral | ArrayLiteral | NilLiteral | DateLiteral | CodeBlockLiteral

NumberLiteral ::= Digit {Digit} ["." {Digit}] [("e" | "E") ["-"] Digit {Digit}]
(* Valid: 123, 3.14, 1.2e-3, 0.5e1 — Invalid: 9E+1 (plus sign), .5e1 (no leading zero), 7e2 (no decimal) *)

StringLiteral ::= '"' {Character} '"' | "'" {Character} "'" | "[" {Character} "]"

BooleanLiteral ::= ".T." | ".F."

NilLiteral ::= "NIL"

ArrayLiteral ::= "{" [Expression {"," Expression}] "}"

DateLiteral ::= "{" year "," month "," day [ "," hour "," minute "," second ] "}"

CodeBlockLiteral ::= "{|" [IdentifierList] "|" ExpressionList "}"
(* Anonymous function / code block: {|x| x * x} *)

DatabaseParameter ::= "?" Identifier "?" | "?"
```

Key language facts:
- Arrays are 1-based.
- Comments use `/* ... ;` and end at the first semicolon.
- Keywords are colon-prefixed and case-sensitive uppercase.
- Identifiers and function names are case-insensitive.
- Division always produces a floating-point result (`5 / 2` yields `2.5`, not `2`).
- Bitwise built-ins (`_AND`, `_OR`, `_NOT`, `_XOR`) require integer-valued operands.
