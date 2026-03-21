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

The most commonly used class for dynamic objects. Supports arbitrary named properties added at runtime.

**Methods:**

| Method | Description |
|--------|-------------|
| `AddProperty(name)` | Add a new dynamic property |
| `clone()` | Return a shallow copy |
| `Deserialize(s)` | Populate from serialized string |
| `Destroy()` | Release the object |
| `GetDynPropList()` | Return array of dynamic property names |
| `GetMethods()` | Return array of method names |
| `GetProperties()` | Return array of all property names |
| `GetProperty(name)` | Get property value by name |
| `GetPropList()` | Return array of property names (alias) |
| `InvokeMethod(name, args)` | Call method by name |
| `IsEmpty()` | Return `.T.` if no dynamic properties |
| `IsMethod(name)` | Return `.T.` if named method exists |
| `IsProperty(name)` | Return `.T.` if named property exists |
| `Serialize()` | Serialize to string |
| `SetProperty(name, value)` | Set property value by name |
| `ToJson()` | Serialize to JSON string |

**Properties:**

| Property | Description |
|----------|-------------|
| `XmlType` | XML type hint for serialization |

**Note:** `HasProperty(obj, name)` is a **standalone built-in function**, not an instance method of SSLExpando. Use `oObj:IsProperty("Name")` for the method form or `HasProperty(oObj, "Name")` for the standalone form.

```ssl
:DECLARE oData;

oData := SSLExpando{};
oData:Name := "John Doe";
oData:Age := 30;
oData:Items := {1, 2, 3};

/* Add property dynamically;
oData:AddProperty("NewProperty");
oData:NewProperty := "value";

/* Check if property exists (method form);
bExists := oData:IsProperty("Name");

/* Check if property exists (standalone function form);
bExists := HasProperty(oData, "Name");

/* Serialize / deserialize;
sJson := oData:ToJson();
sCopy := oData:clone();
```

---

## SSLDataset Details

In-memory data table returned by `GetSSLDataset` or `RunDS`. Used for converting dataset results.

**Methods:**

| Method | Description |
|--------|-------------|
| `ToArray()` | Convert to 2D array |
| `ToDataSet()` | Convert to XML dataset string |
| `ToXml()` | Convert to XML string |

**Construction:**

```ssl
:DECLARE oDS;

/* Create from data;
oDS := GetSSLDataset("SELECT * FROM customers");

/* Create empty;
oDS := SSLDataset{};

/* Convert result;
sXml := oDS:ToXml();
aRows := oDS:ToArray();
```

---

## SSLBaseDictionary Details

Base dictionary class providing keyed storage.

**Methods:**

| Method | Description |
|--------|-------------|
| `AddValue(key, value)` | Add or update a key/value pair |
| `Clear()` | Remove all entries |
| `Contains(key)` | Return `.T.` if key exists |
| `GetValue(key)` | Return value for key |
| `Invoke(key, args)` | Invoke stored code block |
| `Remove(key)` | Remove a key/value pair |
| `TryGetValue(key, @outValue)` | Get value if key exists; return `.T.` on success |

**Properties:**

| Property | Description |
|----------|-------------|
| `Count` | Number of entries |
| `Keys` | Array of all keys |
| `Values` | Array of all values |

---

## SSLIntDictionary Details

Integer-keyed dictionary. Inherits from `SSLBaseDictionary`.

**Construction:** `SSLIntDictionary{}` or `SSLIntDictionary{nLength}`

**Methods:**

| Method | Description |
|--------|-------------|
| `AddValue(key, value)` | Add or update integer key |
| `Contains(key)` | Return `.T.` if integer key exists |
| `GetValue(key)` | Return value for integer key |
| `Remove(key)` | Remove integer key |
| `TryGetValue(key, @outValue)` | Get value if key exists |

---

## SSLStringDictionary Details

String-keyed dictionary. Inherits from `SSLBaseDictionary`.

**Construction:** `SSLStringDictionary{}` or `SSLStringDictionary{xCaseSensitive, nLength}`

**Methods:**

| Method | Description |
|--------|-------------|
| `AddValue(key, value)` | Add or update string key |
| `Contains(key)` | Return `.T.` if string key exists |
| `GetValue(key)` | Return value for string key |
| `Remove(key)` | Remove string key |
| `TryGetValue(key, @outValue)` | Get value if key exists |

---

## SSLRegex Details

Regular expression operations.

**Construction:** `SSLRegex{cPattern}` or `SSLRegex{cPattern, lCaseSensitive}`

**Methods:**

| Method | Description |
|--------|-------------|
| `IsMatch(s)` | Return `.T.` if string matches pattern |

**Properties:**

| Property | Description |
|----------|-------------|
| `CaseSensitive` | Whether matching is case-sensitive |

```ssl
oRe := SSLRegex{'\d{4}-\d{2}-\d{2}'};
bMatch := oRe:IsMatch("2024-12-25");
```

---

## SSLCodeProvider Details

Code execution and compilation utilities.

**Construction:** `SSLCodeProvider{}`

**Methods:**

| Method | Description |
|--------|-------------|
| `CompileAll()` | Compile all scripts and data sources |
| `CompileAllDataSources()` | Compile all data sources |
| `CompileAllServerScripts()` | Compile all server scripts |
| `CompileDataSource(name)` | Compile a single data source |
| `CompileDataSourceCategories(aCategories)` | Compile data source categories |
| `CompileDataSourceCategory(name)` | Compile a single data source category |
| `CompileDataSources(aNames)` | Compile a list of data sources |
| `CompileScript(name)` | Compile a single script |
| `CompileServerScript(name)` | Compile a single server script |
| `CompileServerScriptCategories(aCategories)` | Compile server script categories |
| `CompileServerScriptCategory(name)` | Compile a single server script category |
| `CompileServerScripts(aNames)` | Compile a list of server scripts |

**Returns:** `SSLCompilerErrorList` containing any compile errors.

---

## Email Details

Email sending functionality.

**Construction:** `Email{}` or `Email{lIgnoreExceptions}`

**Methods:**

| Method | Description |
|--------|-------------|
| `LoadMessage(path)` | Load email from file |
| `SaveMessage(path)` | Save email to file |
| `Send()` | Send the email |
| `SendToOutbox()` | Place in outbox queue |
| `SetEncryptCertificateFromPath(path)` | Set encryption certificate from file path |
| `SetEncryptCertificateFromStore(name)` | Set encryption certificate from store |
| `SetSignCertificateFromPath(path)` | Set signing certificate from file path |
| `SetSignCertificateFromStore(name)` | Set signing certificate from store |

**Properties:** `LogSMTP`, `From`, `To`, `CC`, `BCC`, `IgnoreExceptions`, `Subject`, `Body`, `IsHTMLBody`, `Attachments`, `SMTPServerName`, `SMTPServerPort`, `SMTPTimeout`, `SMTPSecureConnection`, `SMTPServerUserName`, `SMTPServerUserPassword`, `Exception`

---

## WebServices Details

Web service client.

**Construction:** `WebServices{}`

**Methods:**

| Method | Description |
|--------|-------------|
| `CreateHttpClient()` | Create an HTTP client object |
| `CreateSoapClient()` | Create a SOAP client object |

---

## AzureStorage Details

Azure storage integration.

**Construction:** `AzureStorage{}`, `AzureStorage{cConnectionName}`, `AzureStorage{cAccountName, cAccountKey}`, or `AzureStorage{cAccountName, cAccountKey, lUseHttps}`

**Methods:** `CreateContainer`, `CreateTable`, `DeleteBlob`, `DeleteContainer`, `DeleteEntities`, `DeleteEntity`, `DeleteTable`, `GetBlob`, `InsertEntities`, `InsertEntity`, `PutBlob`, `ReadBlobAsText`, `SelectEntities`, `SelectEntity`, `UpdateEntity`

---

## FtpsClient Details

FTPS file transfer.

**Construction:** `FtpsClient{}`

**Methods:** `CheckOnFtps`, `Connect`, `CopyToFtps`, `DeleteDirOnFtps`, `DeleteFromFtps`, `Disconnect`, `GetDirFromFtps`, `GetDirNamesFromFtps`, `GetFromFtps`, `Login`, `MakeDirOnFtps`, `MoveInFtps`, `ReadFromFtps`, `RenameOnFtps`, `Secure`, `SendToFtps`, `SetFtpsProxy`, `SetTlsParameters`, `WriteToFtps`

---

## PdfSupport Details

PDF generation and manipulation.

**Construction:** `PdfSupport{}`

**Methods:** `AddPageFromImage`, `AddPDFDocument`, `AddTextOnPage`, `Open`, `OpenProtectedDocument`, `Print`, `Protect`, `Save`, `SetTextStyle`

**Properties:** `UserPassword`, `OwnerPassword`, `DocumentSecurityLevel`, `PermitAccessibilityExtractContent`, `PermitAnnotations`, `PermitAssembleDocument`, `PermitExtractContent`, `PermitFormsFill`, `PermitFullQualityPrint`, `PermitModifyDocument`, `PermitPrint`, `PageCount`

---

## HtmlConverter Details

HTML conversion utilities.

**Construction:** `HtmlConverter{}`

**Methods:**

| Method | Description |
|--------|-------------|
| `ClearLog()` | Clear the conversion log |
| `Convert()` | Perform the conversion |

**Properties:** `OptionsXml`, `Log`, `SimplifiedLog`

---

## SDMS Details

Scientific Data Management System integration.

**Construction:** `SDMS{}` or `SDMS{oCredentials}`

**Methods:** `CheckOutDocument`, `CreateDocUploader`, `CreateUnifiedXmlDOM`, `DownloadDocument`, `DownloadDocument2`, `DownloadOriginalDocument`, `DownloadOriginalDocument2`, `DownloadUnifiedXmlDocument`, `DownloadUnifiedXmlDocument2`, `DownloadUnifiedXmlTemplate`, `GetHttpPassHash`, `GetSoapPassHash`, `SetSDMSConnection`, `UploadDocument`

**Properties:** `ErrorMessage`, `SessionId`, `IsSessionExpired`

---

## SDMSDocUploader Details

SDMS document upload helper.

**Construction:** `SDMSDocUploader{oCredentials}` or `SDMSDocUploader{}`

**Methods:** `AddHeader`, `AttachDocToWorkflow`, `AttachFileToDocument`, `CheckInDocument`, `CheckInWorkflowDocument`, `DoUpload`, `RemoveHeader`, `UploadELNDocument`, `UploadNewRevisionForWorkflowDocument`, `UploadOfficeTemplate`, `UploadOriginalDoc`

**Properties:** `FilePath`, `DocName`, `DocId`, `FileType`, `ProjectName`, `WorkflowId`, `StageId`, `ActionId`, `Metadata`, `UXmlTemplate`

---

## BatchSupport Details

Batch processing utilities.

**Construction:** `BatchSupport{}`

**Methods:**

| Method | Description |
|--------|-------------|
| `Dispose()` | Release resources |
| `IsRunning()` | Return `.T.` if a batch is currently running |

**Properties:** `ActiveBatchesNumber`, `PhysicalMemory`, `VirtualMemory`

**See also:** `InBatchProcess`, `SubmitToBatch`, `SubmitToBatchEx`

---

## PatcherSupport Details

System patching utilities.

**Construction:** `PatcherSupport{}`

**Methods:**

| Method | Description |
|--------|-------------|
| `Compare()` | Compare local and external system data |
| `ConnectToExternalSystem()` | Connect to the external system |
| `GetDataFromWholeDictionary()` | Retrieve all dictionary data |

**Properties:** `DiffDataTable`, `InternalErrors`, `LogFilePath`, `ResultTable`

---

## RegSetup Details

Registry/setup utilities.

**Construction:** `RegSetup{}`

**Methods:**

| Method | Description |
|--------|-------------|
| `RegCloseKey()` | Close an open registry key |
| `RegOpenKey(key)` | Open a registry key |
| `RegQueryValue(key, name)` | Query a registry value |

---

## Sequence Details

Sequence generation for unique identifiers.

**Construction:** `Sequence{cPlatforma, cTableName, cFieldName, cPrefix}`

**Methods:**

| Method | Description |
|--------|-------------|
| `Create()` | Create the sequence |
| `Drop()` | Drop/delete the sequence |
| `Reset()` | Reset the sequence value |
| `SetDatabase(db)` | Set the target database |

**Properties:** `StartWith`, `CacheSize`, `SequenceName`, `Exists`, `NextValue`

---

## EnterpriseExporter Details

Enterprise data export.

**Construction:** `EnterpriseExporter{aTables, lSysTables, cPath}`

**Methods:**

| Method | Description |
|--------|-------------|
| `DoExport()` | Perform the export |

**Properties:** `AbortOnError`, `LogFile`, `IsEnterpriseOnly`, `FromSQL`, `NullAsBlank`, `InvariantDateColumns`

---

## TablesImport Details

Table import functionality.

**Construction:** `TablesImport{cFolder}`

**Methods:**

| Method | Description |
|--------|-------------|
| `GetTable(name)` | Retrieve a table by name (returns `CDataTable`) |

**Properties:** `NullAsBlank`, `IncludeORIGREC`, `ErrMsg`

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
