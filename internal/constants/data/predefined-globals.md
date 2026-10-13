# Predefined Globals

Every server script can read some names that it never declares, such as `MYUSERNAME` and `CRLF`. These names come from two places:

- **The platform** provides four objects by name: `Request`, `Response`, `Session` and `PublicConsts`.
- **The application** declares the rest. They are [`:PUBLIC`](../reference/keywords/PUBLIC.md) variables that the application's server-start scripts create before your script runs. In Designer, these scripts are in the `SystemInit` server-script category.

The second group belongs to the installation, not to the language. A base STARLIMS installation declares the names on this page. Customer installations often extend the server-start scripts, so yours may declare more names, fewer names or different values. The values also depend on the installation's data and configuration.

## How they behave

Predefined globals are ordinary public variables, so the rules in [Variable Scope](variable-scope.md#public-variables) apply:

- **Publics are found last.** A local, a [`:PARAMETERS`](../reference/keywords/PARAMETERS.md) entry or a caller's variable with the same name hides the global. A routine that declares its own `CRLF` no longer sees the global one.
- **Names ignore case.** `MYUSERNAME`, `MyUserName` and `myusername` are the same variable.
- **Nothing makes them read-only.** An assignment to `MYUSERNAME` succeeds and changes the value for every routine that reads it afterwards.
- **A missing name raises an error.** Reading a name that the installation does not declare raises `Variable [<name>] is undefined!`. Use [`IsDefined`](../reference/functions/IsDefined.md) to test for a name before relying on it.

## Provided by the platform

| Name | Holds |
|------|-------|
| [`Request`](../reference/special-forms/request.md) | The incoming HTTP request. It is meaningful only inside an endpoint script. |
| [`Response`](../reference/special-forms/response.md) | The outgoing HTTP response. It is meaningful only inside an endpoint script. |
| `Session` | The current user's session, the store that [`AddToSession`](../reference/functions/AddToSession.md) and [`GetFromSession`](../reference/functions/GetFromSession.md) work with. |
| `PublicConsts` | An application-wide store of values shared between calls. |

All four can be read in any server script, including one run from Designer. [`LimsType`](../reference/functions/LimsType.md) reports `UI` for each of them.

## Declared by the application

### User and session

| Name | Type | Holds |
|------|------|-------|
| `MYUSERNAME` | string | The current user's user name. It equals [`GetUserData()`](../reference/functions/GetUserData.md). |
| `UserLang` | string | The current user's language code, such as `ENG`. |
| `STARLIMSDEPT` | string | The current user's department code. |
| `STARLIMSSITECODE` | string | The current site code. |
| `METHODDEVELOPER` | string | The current user's method-developer flag. |

### Character constants

| Name | Value | Character |
|------|-------|-----------|
| `CRLF` | `Chr(13) + Chr(10)` | carriage return and line feed |
| `sCC` | `Chr(58)` | `:` |
| `sSC` | `Chr(59)` | `;` |
| `sTK` | `Chr(39)` | `'` |
| `sPK` | `Chr(44)` | `,` |
| `DQ` | `Chr(39)` | `'` (a single quote, despite the name) |

### Environment

| Name | Type | Holds |
|------|------|-------|
| `PLATFORMA` | string | The database platform, `"MSSQL"` on SQL Server. |
| `StationName` | string | The client machine name. |
| `TRANSACTION_ID` | string | An identifier for the current call, in GUID form. |
| `glb_In_Batch` | boolean | [`.T.`](../reference/literals/true.md) when the code runs as a batch job. See [In batch jobs](#in-batch-jobs). |
| `CALDATEFORMAT` | string | `"MM/DD/YYYY"` or `"DD/MM/YYYY"`, depending on the server's date settings. |
| `GlbDefaultWorkPath`, `GlbDefaultTempDirectory`, `GlbImpExpPath` | string | Folders on the application server. |

### Status names

The application declares a public for each status defined on the installation. Each one is named after its status and holds the status text the installation uses. `Done`, `Logged`, `Released` and `Draft` are typical examples. The set of names and their values come from your installation's data, and a value may not match its variable name exactly.

```ssl
:PROCEDURE IsEditable;
:PARAMETERS sStatus;

:RETURN sStatus == Draft .OR. sStatus == Logged;
:ENDPROC;
```

Comparing against the global keeps code correct when an installation stores the text differently. On one installation, for example, `OKTOSTART` holds different text from the literal `"Ok to Start"`.

### Others

The server-start scripts declare many more publics, mostly for specific modules. Some are declared but never assigned, so they hold an empty string. Read the `SystemInit` scripts on your installation before relying on any name not listed here.

## In batch jobs

A job submitted with [`SubmitToBatch`](../reference/functions/SubmitToBatch.md) sees the same predefined globals as the call that submitted it. It runs as the submitting user, so `MYUSERNAME` and the session-based names (`UserLang`, `STARLIMSDEPT`, `METHODDEVELOPER`) keep their values. Messages from [`UsrMes`](../reference/functions/UsrMes.md) go to that user's log.

A few values depend on the batch mode:

| Name | `"internal"` mode | `"queue"` mode |
|------|-------------------|----------------|
| `glb_In_Batch` | [`.F.`](../reference/literals/false.md) | [`.T.`](../reference/literals/true.md) |
| `StationName`, `cGlobalStationID` | the submitting client's machine name | `""` |

An `"internal"` job looks like an ordinary call, both to these globals and to [`InBatchProcess`](../reference/functions/InBatchProcess.md). A `"queue"` job reports itself as a batch.

## Names that are not predefined

These names look like globals but a base installation does not declare them. Reading one there raises `Variable [<name>] is undefined!`. Some customised installations do declare `sTab`, `sSQ` or `sDQ`, so they appear in existing code. Don't rely on them in code that has to run on other installations.

| Name | Use instead |
|------|-------------|
| `CR`, `LF` | `Chr(13)`, `Chr(10)` |
| `TAB`, `sTab` | `Chr(9)` |
| `sDQ` | `Chr(34)` |
| `sSQ` | `sTK`, or `Chr(39)` |
| `MYUSERID` | `MYUSERNAME` |
| `MYLANGID` | `UserLang` |
| `MYDEPT` | `STARLIMSDEPT` |
| `MYSITE` | `STARLIMSSITECODE` |

The null value is the literal [`NIL`](../reference/literals/nil.md), not a variable.

## Rules

!!! success "Do"
    - Read `MYUSERNAME` for the current user's name and `CRLF` for a line break.
    - Compare and query statuses through their globals, such as `Released`, rather than string literals.
    - Test with [`IsDefined`](../reference/functions/IsDefined.md) before reading a global that your installation may not declare.

!!! failure "Don't"
    - Assign to a predefined global. Every routine that reads it afterwards sees your value.
    - Declare a local with the same name as a global you need. The local hides the global.
    - Use `DQ` for a double quote. It holds a single quote; use `Chr(34)`.

## Related

- [Variable Scope](variable-scope.md) — how public variables are found and shadowed
- [`:PUBLIC`](../reference/keywords/PUBLIC.md) — declare a public variable
- [`CreatePublic`](../reference/functions/CreatePublic.md) — create a public variable by name
- [`IsDefined`](../reference/functions/IsDefined.md) — test whether a name resolves
- [`GetByName`](../reference/functions/GetByName.md) — read a variable whose name is data
- [`GetUserData`](../reference/functions/GetUserData.md) — the current user's name
