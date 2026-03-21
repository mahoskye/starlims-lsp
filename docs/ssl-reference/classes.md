# SSL Built-in Classes

This document lists the developer-facing built-in classes available in SSL.

**Primary Sources:** `dev/ssl-style-guide/README.md`, `dev/ssl-style-guide/agent-guides/ssl_agent_instructions.md`, `internal/constants/source_alignment.go`, `internal/constants/constants.go`

---

## Built-in Classes (21)

The starlims-lsp provides completion and hover support for these built-in classes:

### Core Classes

| Class | Description |
|-------|-------------|
| `SSLExpando` | Dynamic object with arbitrary properties |
| `SSLDataset` | In-memory data table |
| `SSLBaseDictionary` | Base dictionary class |
| `SSLIntDictionary` | Integer-keyed dictionary |
| `SSLStringDictionary` | String-keyed dictionary |
| `SSLRegex` | Regular expression operations |
| `SSLCodeProvider` | Code execution utilities |
| `SSLCompilerError` | Compiler error information |
| `SSLCompilerErrorList` | Collection of compiler errors |



### Integration Classes

| Class | Description |
|-------|-------------|
| `Email` | Email sending functionality |
| `WebServices` | Web service client |
| `AzureStorage` | Azure storage integration |
| `FtpsClient` | FTPS file transfer |

### Document Classes

| Class | Description |
|-------|-------------|
| `PdfSupport` | PDF generation and manipulation |
| `HtmlConverter` | HTML conversion utilities |
| `SDMS` | Scientific Data Management |
| `SDMSDocUploader` | SDMS document upload |

### System Classes

| Class | Description |
|-------|-------------|
| `BatchSupport` | Batch processing utilities |
| `PatcherSupport` | System patching utilities |
| `RegSetup` | Registry/setup utilities |
| `Sequence` | Sequence generation |



### Import/Export Classes

| Class | Description |
|-------|-------------|
| `EnterpriseExporter` | Enterprise data export |
| `TablesImport` | Table import functionality |

---

## Usage

### Creating Objects

```ssl
/* Built-in classes use curly-brace construction;
oExpando := SSLExpando{};
oExpando:PropertyName := "value";

/* Built-in dataset class;
oDataset := SSLDataset{};

/* User-defined classes use CreateUdObject;
oCustom := CreateUdObject("MyClass", {sName, nCount});

/* Anonymous property bag;
oAnon := CreateUdObject({{"Name", "value"}, {"Count", 1}});
```

### Accessing Properties and Methods

```ssl
/* Property access;
value := oObject:PropertyName;
oObject:PropertyName := newValue;

/* Method calls;
vResult := oObject:MethodName(sName, nCount);
```

---

## SSLExpando Details

The most commonly used class for dynamic objects:

```ssl
:DECLARE oData;

oData := SSLExpando{};
oData:Name := "John Doe";
oData:Age := 30;
oData:Items := {1, 2, 3};

/* Add property dynamically;
oData:AddProperty("NewProperty");
oData:NewProperty := "value";

/* Check if property exists;
bExists := oData:HasProperty("Name");
```

---

## SSLDataset Details

For tabular data manipulation:

```ssl
:DECLARE oDS;

/* Create an empty dataset object;
oDS := SSLDataset{};

/* Or retrieve one from SQL;
oDS := GetSSLDataset("SELECT * FROM customers");

/* Navigate;
oDS:First();
:WHILE .NOT. oDS:Eof();
    sName := oDS:Fields("customer_name"):Value;
    oDS:Next();
:ENDWHILE;

/* Modify;
oDS:Edit();
oDS:Fields("status"):Value := "active";
oDS:Post();
```

---

## Class Support in LSP

Class-style notes from the guide:
- User-defined class files prefer `:INHERIT`, then `:DECLARE`, then regular methods, then `Constructor`
- Bare and qualified `:INHERIT` names are both accepted
- Without `:INHERIT`, classes inherit from `SSLObject` by default
- `Me` is only meaningful inside a `:CLASS`; `Base` must be used as `Base:MemberName` and requires `:INHERIT`
- Underscore-prefixed members such as `_sInternal` follow the SSL private convention and are excluded from reflection
- `/*@private;` and `/*@protected;` annotations do not affect class-method visibility

The LSP provides:
- **Completion:** Class names when creating objects
- **Hover:** Class descriptions

**Not yet supported:**
- Method completion after `:` on typed objects
- Property suggestions based on class type
- Dynamic property tracking for SSLExpando
