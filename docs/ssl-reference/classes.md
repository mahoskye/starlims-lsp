# SSL Built-in Classes

This document lists the built-in classes available in SSL.

**LSP Source:** `internal/constants/constants.go`

---

## Built-in Classes (30)

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

### Data Classes

| Class | Description |
|-------|-------------|
| `CDataTable` | Data table representation |
| `CDataRow` | Single row of data |
| `CDataColumn` | Column definition |
| `CDataColumns` | Collection of columns |
| `CDataField` | Data field representation |

### Integration Classes

| Class | Description |
|-------|-------------|
| `SQLConnection` | Database connection management |
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
| `EnterpriseImpExBase` | Import/export base class |
| `TablesImport` | Table import functionality |

---

## Usage

### Creating Objects

```ssl
/* Using CreateUDObject;
oExpando := CreateUDObject("SSLExpando");
oExpando:PropertyName := "value";

/* Direct instantiation (where supported);
oDataset := CreateUDObject("SSLDataset");
```

### Accessing Properties and Methods

```ssl
/* Property access;
value := oObject:PropertyName;
oObject:PropertyName := newValue;

/* Method calls;
result := oObject:MethodName(param1, param2);
```

---

## SSLExpando Details

The most commonly used class for dynamic objects:

```ssl
:DECLARE oData;

oData := CreateUDObject("SSLExpando");
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

/* Create from SQL;
oDS := GetDataSet("SELECT * FROM customers", "dsCustomers");

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

The LSP provides:
- **Completion:** Class names when creating objects
- **Hover:** Class descriptions

**Not yet supported:**
- Method completion after `:` on typed objects
- Property suggestions based on class type
- Dynamic property tracking for SSLExpando
