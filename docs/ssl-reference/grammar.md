# SSL Grammar Reference

This document is a canonical summary of the SSL v11 grammar used by the LSP.

**Authority:** `dev/ssl-style-guide/ssl-style-guide/ssl-ebnf-grammar.md` is the canonical grammar. When this summary lags, the source file wins.

---

## Top-Level Structure

```ebnf
Program ::= ClassDefinition | {Statement}
(* A script is either a class definition or a series of statements. :INCLUDE appears inside DeclarationStatement, not at program level. *)

Statement ::= CommentStatement | SimpleStatement ";" | BlockStatement | ExitWhileStatement | ExitForStatement | LoopContinue
(* Loop-control statements include their own ";". :EXITFOR, :EXITWHILE, and :LOOP are compile errors outside their respective loop contexts. *)
SimpleStatement ::= DeclarationStatement | LogicStatement | LabelStatement | BranchStatement | DatabaseStatement
BlockStatement ::=
    ProcedureStatement |
    ConditionalStatement |
    LoopStatement |
    SwitchStatement |
    ErrorHandlingStatement |
    ErrorBlockStanza |
    RegionBlock |
    InlineCodeBlock
```

Key implications:
- A file is either a `:CLASS` file or a script — never both.
- There is no `:ENDCLASS`; class scope extends to end of file.
- `:INCLUDE` is a `DeclarationStatement`, not a top-level program directive.
- Legacy constructs such as `:REGION`, `:BEGININLINECODE`, `:ERROR`, and `:RESUME` still exist in the broader syntax surface even though new code should usually avoid them.

---

## Classes And Procedures

```ebnf
ClassDefinition ::= ClassDeclaration [InheritStatement] {ClassFieldDeclaration} {MethodDeclaration} [ConstructorDeclaration]
(* Compiler enforces ordering: INHERIT, fields, methods, then Constructor *)
ClassDeclaration ::= ":" "CLASS" [Identifier] ";"
InheritStatement ::= ":" "INHERIT" (Identifier | QualifiedIdentifier) ";"
(* QualifiedIdentifier supports qualified names like "Category.ClassName" *)
QualifiedIdentifier ::= Identifier {"." Identifier}
ClassFieldDeclaration ::= ":" "DECLARE" IdentifierList ";"
MethodDeclaration ::= ProcedureStatement
ConstructorDeclaration ::= ProcedureStatement
(* Constructor procedure must be named "Constructor" (case-insensitive); if omitted an empty zero-arg constructor is auto-generated *)

ProcedureStatement ::= ProcedureStart [ParameterDeclaration] {DefaultParameterDeclaration} {Statement} ProcedureEnd
ProcedureStart ::= ":" "PROCEDURE" Identifier ";"
ProcedureEnd ::= ":" "ENDPROC" ";"

ParameterDeclaration ::= ":" "PARAMETERS" IdentifierList ";"
DefaultParameterDeclaration ::= ":" "DEFAULT" DefaultParameterPair ";"
(* One identifier/expression pair per :DEFAULT line *)
IdentifierList ::= Identifier {"," Identifier}
DefaultParameterPair ::= Identifier "," Expression

DeclarationStatement ::= ParametersStatement | DeclareStatement | DefaultStatement | PublicStatement | IncludeStatement
ParametersStatement ::= ":" "PARAMETERS" IdentifierList
DeclareStatement ::= ":" "DECLARE" IdentifierList
DefaultStatement ::= ":" "DEFAULT" DefaultParameterPair
PublicStatement ::= ":" "PUBLIC" IdentifierList
IncludeStatement ::= ":" "INCLUDE" IncludeTarget
IncludeTarget ::= Identifier | QualifiedIdentifier
```

Source-aligned notes:
- Inside a procedure, `:PARAMETERS` must appear immediately after `:PROCEDURE`.
- `:DEFAULT` must immediately follow `:PARAMETERS`.
- Inside `:CLASS`, `Constructor` is the reserved constructor procedure name.
- Inside `Constructor`, `:RETURN` must be bare (no expression); returning a value from a constructor is a compile-time error.

---

## Control Flow

```ebnf
ConditionalStatement ::= IfStatement {Statement} [ElseBlock] EndIfStatement
IfStatement ::= ":" "IF" Expression ";"
ElseBlock ::= ":" "ELSE" ";" {Statement}
EndIfStatement ::= ":" "ENDIF" ";"

LoopStatement ::= WhileLoop | ForLoop
WhileLoop ::= WhileStatement {Statement} EndWhileStatement
WhileStatement ::= ":" "WHILE" Expression ";"
EndWhileStatement ::= ":" "ENDWHILE" ";"
ExitWhileStatement ::= ":" "EXITWHILE" ";"

ForLoop ::= ForStatement {Statement} NextStatement
ForStatement ::= ":" "FOR" Identifier ":=" Expression ":" "TO" Expression [":" "STEP" Expression] ";"
NextStatement ::= ":" "NEXT" ";"
ExitForStatement ::= ":" "EXITFOR" ";"
LoopContinue ::= ":" "LOOP" ";"
(* :LOOP continues to next iteration; works in both :FOR and :WHILE loops *)
(* :EXITFOR, :EXITWHILE, and :LOOP are compile errors outside their respective loop contexts *)
```

---

## CASE Blocks

```ebnf
SwitchStatement ::= BeginCaseStatement CaseBlock {CaseBlock} [OtherwiseBlock] EndCaseStatement
BeginCaseStatement ::= ":" "BEGINCASE" ";"
CaseBlock ::= CaseStatement {Statement} [ExitCaseStatement]
CaseStatement ::= ":" "CASE" Expression ";"
OtherwiseBlock ::= OtherwiseStatement {Statement} [ExitCaseStatement]
OtherwiseStatement ::= ":" "OTHERWISE" ";"
EndCaseStatement ::= ":" "ENDCASE" ";"
ExitCaseStatement ::= ":" "EXITCASE" ";"
```

Important behavior:
- `:BEGINCASE` is not a value-switch; each `:CASE` evaluates its own boolean expression.
- Without `:EXITCASE;`, later matching `:CASE` bodies may also run.
- `:OTHERWISE` is always skipped once any earlier `:CASE` body has run — even if that earlier case omitted `:EXITCASE;`.
- `:BEGINCASE` requires at least one `:CASE`.

---

## TRY, CATCH, FINALLY

```ebnf
ErrorHandlingStatement ::= TryBlock
TryBlock ::= TryStatement Statement {Statement} (CatchBlock [FinallyBlock] | FinallyBlock) EndTryStatement
TryStatement ::= ":" "TRY" ";"
CatchBlock ::= CatchStatement {Statement}
CatchStatement ::= ":" "CATCH" ";"
FinallyBlock ::= FinallyStatement Statement {Statement}
FinallyStatement ::= ":" "FINALLY" ";"
EndTryStatement ::= ":" "ENDTRY" ";"

ErrorBlockStanza ::= ErrorMarker Statement {Statement} [ResumeStatement]
(* :ERROR body requires >=1 statement; optional :RESUME switches to resume mode *)
ErrorMarker ::= ":" "ERROR" ";"
ResumeStatement ::= ":" "RESUME" ";"
(* :RESUME inside :ERROR handler switches to resume mode, wrapping each subsequent statement in individual try/catch *)
```

Important behavior:
- `:TRY` must contain at least one statement before `:CATCH` or `:FINALLY`.
- At least one of `:CATCH` or `:FINALLY` is required.
- `:CATCH` does not declare an exception variable; use `GetLastSSLError()` in the catch body to retrieve an `SSLError` object (members: `:Message`, `:Description`, `:Operation`, `:Code`, `:GenCode`, `:FullDescription`, `:FullDescriptionEx`, `:InnerException`, `:NETException`).
- `:FINALLY` must contain at least one statement.
- `:RETURN`, `:EXITFOR`, `:EXITWHILE`, and `:LOOP` are compile-time errors inside `:FINALLY`.
- An empty `:CATCH` block (zero statements) is valid.
- Only one `:CATCH` block is allowed per `:TRY` — there is no multi-catch.
- `:ERROR` is a legacy handler form and must contain at least one statement before `:RESUME` or the end of the current scope.

---

## Legacy Text-Capture Constructs

```ebnf
RegionBlock ::= RegionStart {Character} RegionEnd
RegionStart ::= ":" "REGION" Identifier ";"
RegionEnd ::= ":" "ENDREGION" ";"

InlineCodeBlock ::= InlineCodeStart [Program] InlineCodeEnd
(* Body is re-parsed as a complete SSL unit — may contain procedures, parameters, classes, etc. *)
InlineCodeStart ::= ":" "BEGININLINECODE" (Identifier | QuotedIdentifier) ";"
(* Name is REQUIRED — bare identifier or double-quoted identifier; StringLiteral is not accepted *)
QuotedIdentifier ::= '"' Identifier '"'
InlineCodeEnd ::= ":" "ENDINLINECODE" ";"

LabelStatement ::= ":" ("LABEL" Identifier {Identifier} | MashedLabelName)
(* Accepted forms include :LABEL Name; and :LABELName; *)
MashedLabelName ::= "LABEL" Identifier
BranchStatement ::= Identifier "(" StringLiteral ")"
```

Source-aligned notes:
- `:REGION` / `:ENDREGION` store raw text and are not modern editor-folding markers.
- `:BEGININLINECODE` name is **required** — bare identifier or double-quoted identifier only.
- `Branch()` targets must include the label token text, such as `"LABEL SKIP"` or `"LABELSKIP"`.

---

## Expressions

```ebnf
LogicStatement ::= Assignment | FunctionCall | Expression | ReturnStatement
ReturnStatement ::= ":" "RETURN" [Expression]
CommentStatement ::= "/*" {Character} ";"
(* All comments use the same syntax; the lexer does not distinguish single-line from multi-line *)
ConstructorReturnStatement ::= ":" "RETURN"
(* Inside Constructor, only bare :RETURN is permitted; [Expression] form is a compile-time error *)

Assignment ::= (VariableAccess | PropertyAccess) AssignmentOperator Expression
AssignmentOperator ::= ":=" | "+=" | "-=" | "*=" | "/=" | "^=" | "%="

FunctionCall ::= DirectFunctionCall | IndirectFunctionCall
DirectFunctionCall ::= Identifier "(" [ArgumentList] ")"
IndirectFunctionCall ::= Identifier "(" StringLiteral ["," ArrayLiteral] ")"
(* Generic pattern for DoProc, ExecFunction, etc. — array argument is optional *)
ArgumentList ::= Expression {"," Expression}

IncrementExpression ::= IncrementTarget ("++" | "--") | ("++" | "--") IncrementTarget
IncrementTarget ::= Identifier | PropertyAccess | ArrayAccess
(* Applies to variables, array elements, and object properties *)

ObjectCreation ::= BuiltInClassInstantiation | DynamicObjectCreation | UserDefinedObjectCreation | AnonymousObjectCreation
BuiltInClassInstantiation ::= Identifier "{" [ArgumentList] "}"
DynamicObjectCreation ::= "CreateUdObject" "(" ")"
UserDefinedObjectCreation ::= "CreateUdObject" "(" StringLiteral ["," ArrayLiteral] ")"
AnonymousObjectCreation ::= "CreateUdObject" "(" ArrayLiteral ")"

MemberReceiver ::= Identifier | PropertyAccess | MethodCall | ArrayAccess | FunctionCall | MeLiteral | BaseAccess | "(" Expression ")"
(* Any postfix expression that can appear before ":" in member access — supports chaining like Me:GetConfig():Value *)
MethodCall ::= MemberReceiver ":" Identifier "(" [ArgumentList] ")"
PropertyAccess ::= MemberReceiver ":" Identifier

DatabaseStatement ::= DatabaseFunctionCall
DatabaseFunctionCall ::= Identifier "(" StringLiteral ["," Expression] {"," Expression} ")"
(* Database functions take a SQL string plus optional additional arguments such as friendly names, flags, and parameter arrays *)
DatabaseParameter ::= "?" Identifier "?" | "?"
(* Named: ?Name? for SQLExecute; Positional: ? for RunSQL/LSearch/etc. *)
```

Source-aligned notes:
- Built-in functions use normal call syntax.
- Custom procedures are not called directly; use `DoProc(...)` / `ExecFunction(...)`. `DoProc(...)` is a **compile-time error** inside class methods — use `Me:Method()` / `Base:Method()` instead.
- Property and method access use colon notation, not dot notation.
- `DoProc("Name")` (no second argument) is preferred over `DoProc("Name", {})` when there are no parameters; the grammar rule requires the array argument when present, but the runtime accepts its omission.

---

## Expression Precedence

```ebnf
Expression ::= OrExpression
OrExpression ::= AndExpression {".OR." AndExpression}
AndExpression ::= ComparisonExpression {".AND." ComparisonExpression}
ComparisonExpression ::= RelationalExpression {(EqualityOperator | ContainmentOperator) RelationalExpression}
EqualityOperator ::= "=" | "==" | "!=" | "<>" | "#"
(* "=" is loose equality (prefix match for strings); "==" is strict; "#", "<>", "!=" negate == not = *)
ContainmentOperator ::= "$"
(* Containment: left $ right is .T. if left found inside right *)
RelationalExpression ::= ShiftExpression {RelationalOperator ShiftExpression}
RelationalOperator ::= "<" | ">" | "<=" | ">="
ShiftExpression ::= ArithmeticExpression {ShiftOperator ArithmeticExpression}
ShiftOperator ::= "<<" | ">>"
ArithmeticExpression ::= Term {AdditiveOperator Term}
AdditiveOperator ::= "+" | "-"
Term ::= Factor {MultiplicativeOperator Factor}
MultiplicativeOperator ::= "*" | "/" | "%"
Factor ::= PowerOperand [PowerOperator Factor]
(* Right-associative: 2^3^2 = 2^(3^2) = 512 *)
PowerOperator ::= "^" | "**"
PowerOperand ::= [UnaryOperator] Primary
UnaryOperator ::= "-" | "!" | ".NOT."

Primary ::=
    Literal | VariableAccess | PropertyAccess | ArrayAccess |
    FunctionCall | BitwiseOperation |
    "(" Expression ")" | IncrementExpression |
    MeLiteral | BaseAccess | MethodCall | ObjectCreation

(* Bitwise operations use function call syntax, not infix operators *)
BitwiseOperation ::= "_AND" "(" Expression "," Expression ")" |
                     "_OR" "(" Expression "," Expression ")" |
                     "_XOR" "(" Expression "," Expression ")" |
                     "_NOT" "(" Expression ")"

VariableAccess ::= Identifier
MeLiteral ::= "Me"
(* Case-insensitive; reference to the current class instance *)
BaseAccess ::= "Base" ":" Identifier ["(" [ArgumentList] ")"]
(* Case-insensitive; Base must always be followed by a member name *)
ArrayAccess ::= Identifier ArraySubscript
ArraySubscript ::= "[" Expression {"," Expression} "]" | "[" Expression "]" {("[" Expression "]")}
(* Supports arr[1,2] and arr[1][2] *)
```

---

## Literals And Core Tokens

```ebnf
Literal ::= NumberLiteral | StringLiteral | BooleanLiteral | ArrayLiteral | NilLiteral | CodeBlockLiteral
(* There is NO date literal syntax. Dates are created via functions: Today(), Now(), CToD(), DateFromNumbers(). *)
(* Brace forms like {2024, 12, 25} are array literals, not dates. *)

NumberLiteral ::= IntegerPart [DecimalPart [Exponent]]
(* Scientific notation requires a decimal part: '7.0e2' works, '7e2' does not *)
IntegerPart ::= Digit {Digit}
DecimalPart ::= "." Digit {Digit}
(* At least one digit after the decimal point is required *)
Exponent ::= ("e" | "E") ["-"] Digit {Digit}
(* Valid: 123, 3.14, 1.2e-3, 0.5e1 — Invalid: 9E+1 (plus sign), .5e1 (no leading zero), 7e2 (no decimal), 7. (no digit after decimal) *)

StringLiteral ::= '"' {Character} '"' | "'" {Character} "'" | BracketString
(* No escape sequences — backslashes are literal *)
BracketString ::= "[" {BracketChar} "]"
(* One level of nested brackets supported: [[a]b] yields [a]b. Deeper nesting is not supported. *)

BooleanLiteral ::= ".T." | ".F."
(* Case-insensitive: .t. and .f. are also valid; .T./.F. are canonical *)

NilLiteral ::= "NIL"
(* Case-insensitive: nil, Nil, NIL are all valid *)

ArrayLiteral ::= "{" [ExpressionList] "}"
(* Mixed content and nested arrays are supported; e.g. {1, {2,3}, "x"} *)
ExpressionList ::= Expression {"," Expression}

CodeBlockLiteral ::= "{|" IdentifierList "|" Expression "}"
(* At least one parameter required; e.g. {|x| x*x} *)

Identifier ::= (Letter | "_") {Letter | Digit | "_"}
Letter ::= "A"..."Z" | "a"..."z"
Digit ::= "0"..."9"
Character ::= Letter | Digit | Symbol
(* Context-dependent *)
Newline ::= "\n" | "\r\n" | "\r"
```

Key language facts:
- Arrays are 1-based.
- Comments use `/* ... ;` and end at the first semicolon.
- Keywords are colon-prefixed and case-sensitive uppercase.
- Identifiers and function names are case-insensitive.
- SSL literals (`.T.`, `.F.`, `NIL`) and class-context forms (`Me`, `Base`, `Constructor`) are case-insensitive.
- Division always produces a floating-point result (`5 / 2` yields `2.5`, not `2`).
- Bitwise built-ins (`_AND`, `_OR`, `_NOT`, `_XOR`) require integer-valued operands.
- **No date literal syntax.** Dates are created via `Today()`, `Now()`, `CToD()`, or `DateFromNumbers()`. Brace notation like `{2024, 12, 25}` is an array, not a date.
- `=` is loose (prefix match for strings); `==` is strict equality. `=` and `!=` are **not logical opposites** for strings because `=` uses prefix matching.

---

## Implementation Considerations for Formatting Tools

When implementing a formatter for SSL, consider the following specifics that may not be explicitly defined in the grammar but affect code readability:

1. **Indentation**: Code blocks within control structures (`:IF`/`:ENDIF`, `:WHILE`/`:ENDWHILE`, etc.) should be indented consistently. Tabs are preferred; spaces are also accepted (4-space width when used).

2. **Alignment**: Parameters in multi-line function calls or array declarations are often aligned for readability.

3. **Empty Lines**: Use empty lines to separate logical sections of code, particularly between procedure definitions.

4. **Line Length**: Break long lines at logical points (typically around 90 characters), especially for:

    - Long parameter lists in function calls
    - Complex logical expressions
    - SQL queries
    - Array declarations

5. **SQL Formatting**: SQL statements embedded in strings should follow SQL formatting conventions, with clauses (SELECT, FROM, WHERE) aligned and properly indented.

6. **Comment Alignment**: End-of-line comments should be aligned at a consistent column position when appearing on consecutive lines.

7. **Section Headers**: Consider preserving region markers and section headers as structural elements of the code.

8. **Special Case Handling**: Take special care with:
    - String concatenation operators
    - Spacing around property access colons
    - Logical operators preceded by dots (`.AND.`, `.OR.`, `.T.`, `.F.`)
    - Increment/decrement operators which should not have spaces
