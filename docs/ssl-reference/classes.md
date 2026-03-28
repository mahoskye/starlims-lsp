# SSL Built-in Classes

This document lists the developer-facing built-in classes available in SSL.

**Primary Sources:** `dev/ssl-style-guide/agent-guides/ssl_agent_instructions.md` (authoritative), `internal/constants/canonical.go`, `internal/constants/constants.go`

---

## Built-in Classes (22)

The starlims-lsp provides completion and hover support for these 22 directly-instantiable built-in classes. The authoritative `ssl-element-list.json` documents 26 classes total; 4 `CData*` return-value-only types (`CDataColumn`, `CDataColumns`, `CDataField`, `CDataRow`) are excluded from LSP completion because they are not directly instantiated — they are obtained exclusively as return values from `TablesImport:GetTable(name)`. `CDataTable` is included because it supports direct construction via `CDataTable{}`. See `internal/constants/canonical.go` for the full exclusion rationale.

### Core Classes

| Class | Description |
|-------|-------------|
| `CDataTable` | In-memory data table (directly instantiable via `CDataTable{}`) |
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
| `clone()` | Return a deep copy (recursively clones all elements) |
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

Dataset wrapper for query results. Obtain via `GetSSLDataset(...)` or `RunDS(..., "ssldataset")`.

**Construction:** `SSLDataset{}` or `SSLDataset{vData, vNullAsBlank}`

**Methods:**

| Method | Returns | Description |
|--------|---------|-------------|
| `ToArray()` | Array | Convert to 2D array |
| `ToDataSet()` | Object | Convert to XML dataset object |
| `ToXml()` | String | Convert to XML string |

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
| `GetValue(key, vDefault)` | Return value for key, or `vDefault` if not found |
| `Invoke(key, args)` | Invoke stored code block |
| `Remove(key)` | Remove a key/value pair |
| `TryGetValue(key)` | Return `{Exists, Value}` object; `Exists` is `.T.` if key found |

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
| `GetValue(key, vDefault)` | Return value for integer key, or `vDefault` if not found |
| `Remove(key)` | Remove integer key |
| `TryGetValue(key)` | Return `{Exists, Value}` object; `Exists` is `.T.` if key found |

---

## SSLStringDictionary Details

String-keyed dictionary. Inherits from `SSLBaseDictionary`.

**Construction:** `SSLStringDictionary{}` or `SSLStringDictionary{vCaseSensitive, nLength}`

**Methods:**

| Method | Description |
|--------|-------------|
| `AddValue(key, value)` | Add or update string key |
| `Contains(key)` | Return `.T.` if string key exists |
| `GetValue(key, vDefault)` | Return value for string key, or `vDefault` if not found |
| `Remove(key)` | Remove string key |
| `TryGetValue(key)` | Return `{Exists, Value}` object; `Exists` is `.T.` if key found |

---

## SSLRegex Details

Regular expression operations.

**Construction:** `SSLRegex{cPattern}` or `SSLRegex{cPattern, lCaseSensitive}`

**Methods:**

| Method | Description |
|--------|-------------|
| `IsMatch(sInput, nStartAt)` | Return `.T.` if string matches pattern starting at `nStartAt` |

**Properties:**

| Property | Description |
|----------|-------------|
| `CaseSensitive` | Whether matching is case-sensitive |

```ssl
oRe := SSLRegex{'\d{4}-\d{2}-\d{2}'};
bMatch := oRe:IsMatch("2024-12-25", 1);
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

| Method | Returns | Description |
|--------|---------|-------------|
| `Send()` | Boolean | Send the email |
| `SendToOutbox()` | Boolean | Place in outbox queue |
| `SaveMessage(sPath)` | Boolean | Save email to file |
| `LoadMessage(sPath)` | Boolean | Load email from file |
| `SetSignCertificateFromStore(sEmail, sStoreName)` | Boolean | Set signing certificate from store |
| `SetEncryptCertificateFromStore(sEmail, sStoreName)` | Boolean | Set encryption certificate from store |
| `SetSignCertificateFromPath(sCertPath, sPassword)` | Boolean | Set signing certificate from file path |
| `SetEncryptCertificateFromPath(sCertPath, sPassword)` | Boolean | Set encryption certificate from file path |

**Properties:**

| Property | Type | Access |
|----------|------|--------|
| `LogSMTP` | Boolean | read/write |
| `From` | String | read/write |
| `To` | Array | read/write |
| `CC` | Array | read/write |
| `BCC` | Array | read/write |
| `IgnoreExceptions` | Boolean | read/write |
| `Subject` | String | read/write |
| `Body` | String | read/write |
| `IsHTMLBody` | Boolean | read/write |
| `Attachments` | Array | read/write |
| `SMTPServerName` | String | read/write |
| `SMTPServerPort` | Number | read/write |
| `SMTPTimeout` | Number | read/write |
| `SMTPSecureConnection` | Boolean | read/write |
| `SMTPServerUserName` | String | read/write |
| `SMTPServerUserPassword` | String | read/write |
| `Exception` | Object | read-only |

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

**Methods:**

| Method | Returns | Description |
|--------|---------|-------------|
| `CreateTable(sTableName)` | | Create a table |
| `DeleteTable(sTableName)` | | Delete a table |
| `InsertEntity(sTableName, oEntity)` | | Insert a single entity |
| `InsertEntities(sTableName, aEntities)` | | Insert multiple entities |
| `SelectEntity(sTableName, sPartitionKey, sRowKey)` | Object | Select a single entity |
| `SelectEntities(sTableName, oAttributes)` | Array | Select multiple entities |
| `DeleteEntity(sTableName, sPartitionKey, sRowKey)` | | Delete a single entity |
| `DeleteEntities(sTableName, aEntities)` | | Delete multiple entities |
| `UpdateEntity(sTableName, oEntity)` | Boolean | Update an entity |
| `CreateContainer(sContainerName)` | | Create a blob container |
| `DeleteContainer(sContainerName)` | | Delete a blob container |
| `PutBlob(sContainerName, sLocalPath, sBlobName)` | | Upload a blob |
| `GetBlob(sContainerName, sBlobName, sDestPath)` | String | Download a blob |
| `DeleteBlob(sContainerName, sBlobName)` | | Delete a blob |
| `ReadBlobAsText(sContainerName, sBlobName)` | String | Read blob as text |

---

## FtpsClient Details

FTPS file transfer.

**Construction:** `FtpsClient{}`

**Methods:**

| Method | Returns | Description |
|--------|---------|-------------|
| `SetFtpsProxy(sProxyType, sProxy, nPort, sUser, sPassword)` | | Configure proxy |
| `SetTlsParameters(sAllowedSuites, sCommonName, sVersion, sCertLocation, sCertPath, sCertPassword)` | | Configure TLS |
| `Connect(sServer, nPort, sSecurity)` | String | Connect to server |
| `Disconnect()` | String | Disconnect |
| `Login(sUserName, sPassword, sAccount)` | String | Authenticate |
| `Secure()` | | Enable security |
| `CheckOnFtps(sRemoteDir, sFileName)` | Boolean | Check if file exists |
| `CopyToFtps(sRemoteDir, aRemoteFileNames, sContents)` | Boolean | Copy content to remote |
| `DeleteDirOnFtps(sRemoteDir)` | Boolean | Delete remote directory |
| `DeleteFromFtps(sRemoteDir, sFileName)` | Boolean | Delete remote file |
| `GetDirFromFtps(sRemoteDir)` | Array | Get directory listing |
| `GetDirNamesFromFtps(sRemoteDir)` | Array | Get directory names |
| `GetFromFtps(sRemoteDir, sRemoteFile, sLocalFile)` | Boolean | Download file |
| `MakeDirOnFtps(sRemoteDir)` | Boolean | Create remote directory |
| `MoveInFtps(sFromDir, sToDir, sFromFile, sToFile)` | Boolean | Move remote file |
| `ReadFromFtps(sRemoteDir, sFileName, nMaxSize)` | String | Read remote file content |
| `RenameOnFtps(sRemoteDir, sOldName, sNewName)` | Boolean | Rename remote file |
| `SendToFtps(sRemoteDir, sRemoteFile, sLocalFile)` | Boolean | Upload file |
| `WriteToFtps(sRemoteDir, sRemoteFile, sContents)` | Boolean | Write content to remote |

---

## PdfSupport Details

PDF generation and manipulation.

**Construction:** `PdfSupport{}`

**Methods:**

| Method | Description |
|--------|-------------|
| `Open(sFileName)` | Open a PDF file |
| `OpenProtectedDocument(sFileName, sPassword)` | Open a password-protected PDF |
| `Save(sFileName)` | Save the PDF to file |
| `AddPageFromImage(sImagePath)` | Add a page from an image file |
| `AddPDFDocument(sPdfPath)` | Append another PDF document |
| `SetTextStyle(sFontName, nFontSize, sFontStyle, sFontColor)` | Set text rendering style |
| `AddTextOnPage(sText, nPageNum, nX, nY)` | Add text at a position on a page |
| `Print(sAdobePath, sFileName, sPrinterName)` | Print the PDF |
| `Protect(sPassword)` | Password-protect the PDF |

**Properties:**

| Property | Type | Access |
|----------|------|--------|
| `UserPassword` | String | write-only |
| `OwnerPassword` | String | write-only |
| `DocumentSecurityLevel` | String | read/write |
| `PermitAccessibilityExtractContent` | Boolean | read/write |
| `PermitAnnotations` | Boolean | read/write |
| `PermitAssembleDocument` | Boolean | read/write |
| `PermitExtractContent` | Boolean | read/write |
| `PermitFormsFill` | Boolean | read/write |
| `PermitFullQualityPrint` | Boolean | read/write |
| `PermitModifyDocument` | Boolean | read/write |
| `PermitPrint` | Boolean | read/write |
| `PageCount` | Number | read-only |

---

## HtmlConverter Details

HTML conversion utilities.

**Construction:** `HtmlConverter{}`

**Methods:**

| Method | Description |
|--------|-------------|
| `ClearLog()` | Clear the conversion log |
| `Convert()` | Perform the conversion |

**Properties:**

| Property | Type | Access |
|----------|------|--------|
| `OptionsXml` | String | write-only |
| `Log` | String | read-only |
| `SimplifiedLog` | String | read-only |

---

## SDMS Details

Scientific Data Management System integration.

**Construction:** `SDMS{}` or `SDMS{oCredentials}`

**Methods:**

| Method | Returns | Description |
|--------|---------|-------------|
| `CreateUnifiedXmlDOM()` | Object | Create unified XML DOM |
| `GetSoapPassHash(sDictPass)` | String | Get SOAP password hash |
| `GetHttpPassHash(sDictPass)` | String | Get HTTP password hash |
| `CreateDocUploader(oCredentials)` | SDMSDocUploader | Create a document uploader |
| `DownloadDocument2(sDocId, sDocType, sPath)` | Boolean | Download document |
| `DownloadOriginalDocument2(sDocId, sPath)` | Boolean | Download original document |
| `DownloadUnifiedXmlDocument2(sDocId, sPath)` | Boolean | Download unified XML document |
| `DownloadUnifiedXmlTemplate(sTemplateId, sPath)` | Boolean | Download unified XML template |
| `CheckOutDocument(sDocId, sPath)` | Boolean | Check out document |

**Properties:**

| Property | Type | Access |
|----------|------|--------|
| `SessionId` | String | read/write |
| `ErrorMessage` | String | read-only |
| `IsSessionExpired` | Boolean | read-only |

---

## SDMSDocUploader Details

SDMS document upload helper.

**Construction:** `SDMSDocUploader{oCredentials}` or `SDMSDocUploader{}`

**Methods:**

| Method | Returns | Description |
|--------|---------|-------------|
| `UploadOriginalDoc()` | Boolean | Upload original document |
| `AttachDocToWorkflow()` | Boolean | Attach document to workflow |
| `CheckInDocument(sRevision, sStatus)` | Boolean | Check in document |
| `AttachFileToDocument()` | Boolean | Attach file to document |
| `UploadOfficeTemplate()` | Boolean | Upload Office template |
| `UploadELNDocument()` | Boolean | Upload ELN document |
| `AddHeader(sKey, sValue)` | | Add HTTP header |
| `RemoveHeader(sKey)` | | Remove HTTP header |
| `DoUpload(sFilePath, sSdmsUrl)` | Number | Upload file to SDMS URL |
| `CheckInWorkflowDocument(sRevision, sStatus, nEntryPoint)` | Boolean | Check in workflow document |
| `UploadNewRevisionForWorkflowDocument(sMessage)` | Boolean | Upload new revision for workflow document |

**Properties:**

| Property | Type |
|----------|------|
| `FilePath` | String |
| `DocName` | String |
| `DocId` | Number |
| `FileType` | String |
| `ProjectName` | String |
| `WorkflowId` | Number |
| `StageId` | Number |
| `ActionId` | Number |
| `Metadata` | Array |
| `UXmlTemplate` | String |

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

**Properties:**

| Property | Type | Access |
|----------|------|--------|
| `LogFilePath` | String | read/write |
| `ResultTable` | Object | read-only |
| `InternalErrors` | Object | read-only |
| `DiffDataTable` | Object | read-only |

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
| `Reset(nNewValue)` | Reset the sequence to a new value |
| `SetDatabase(sDatabase)` | Set the target database |

**Properties:**

| Property | Type | Access |
|----------|------|--------|
| `StartWith` | Number | read/write |
| `CacheSize` | Number | read/write |
| `SequenceName` | String | read-only |
| `Exists` | Boolean | read-only |
| `NextValue` | Number | read-only |

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

## SSLError Details

Error object returned by `GetLastSSLError()` inside a `:CATCH` or `:ERROR` block. Not directly instantiated.

**Properties:**

| Property | Description |
|----------|-------------|
| `Message` | Short error message |
| `Description` | Detailed error description |
| `Operation` | Operation that caused the error |
| `Code` | Error code |
| `GenCode` | General error code |
| `FullDescription` | Full error description |
| `FullDescriptionEx` | Extended full description |
| `InnerException` | Nested inner exception object |
| `NETException` | Underlying .NET exception object |

```ssl
:TRY;
    /* Code that might error;
:CATCH;
    oErr := GetLastSSLError();
    UsrMes(oErr:Message);
    UsrMes(oErr:FullDescription);
:ENDTRY;
```

**Note:** `SSLError` is not in the 22 directly-instantiable classes. It is always obtained via `GetLastSSLError()` or `ClearLastSSLError()`.

---

## Class Support in LSP

Class-style notes from the guide:
- User-defined class files must follow the order: `:INHERIT`, then `:DECLARE`, then regular methods, then `Constructor` — tooling enforces this
- Bare and qualified `:INHERIT` names are both accepted
- Without `:INHERIT`, classes inherit from `SSLObject` by default
- `Me` is only meaningful inside a `:CLASS`; `Base` must be used as `Base:MemberName` and requires `:INHERIT`
- Underscore-prefixed members such as `_sInternal` follow the SSL private convention and are excluded from reflection
- `DoProc(...)` is a **compile-time error** inside class methods — use `Me:MethodName()` / `Base:MethodName()` instead
- `/*@private;` and `/*@protected;` annotations do not affect class-method visibility

The LSP provides:
- **Completion:** Class names when creating objects
- **Hover:** Class descriptions

**Not yet supported:**
- Method completion after `:` on typed objects
- Property suggestions based on class type
- Dynamic property tracking for SSLExpando
