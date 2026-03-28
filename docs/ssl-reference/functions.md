# SSL Built-in Functions

This document lists all **354 developer-facing built-in functions** exposed by the LSP. Signatures are sourced from `dev/ssl-style-guide/agent-guides/ssl_agent_instructions.md` (authoritative).

**Primary Sources:** `dev/ssl-style-guide/agent-guides/ssl_agent_instructions.md` (authoritative), `internal/constants/canonical.go`, `internal/constants/signatures.go`

Function names are case-insensitive at runtime, but the LSP presents the canonical casing documented in the style-guide materials. Parameter prefixes: `s` = string, `n` = number, `b` = boolean, `d` = date, `a` = array, `o` = object, `v` = any/variant, `fn` = code block.

---

## Function Categories

### String Functions

| Function | Signature |
|----------|-----------|
| `Len` | `Len(vSource)` |
| `SubStr` | `SubStr(sSource, nStart, nLength)` |
| `Upper` | `Upper(sSource)` |
| `Lower` | `Lower(sSource)` |
| `LLower` | `LLower(sSource)` |
| `AllTrim` | `AllTrim(sSource)` |
| `Trim` | `Trim(sSource)` |
| `LTrim` | `LTrim(sSource)` |
| `Left` | `Left(sSource, nLength)` |
| `Right` | `Right(sSource, nLength)` |
| `StrTran` | `StrTran(sSource, sSearchFor, sReplaceWith)` |
| `Replace` | `Replace(sSource, sSearchFor, sReplaceWith)` |
| `At` | `At(sSubString, sSource)` |
| `LimsAt` | `LimsAt(sSubString, sSource, nOffset)` |
| `Rat` | `Rat(sSubStr, sSource)` |
| `StrSrch` | `StrSrch(sSubStr, sSource, nIndexOrOccurence, bFlag)` |
| `LimsString` | `LimsString(vSource)` |
| `Val` | `Val(sSNumber)` |
| `Chr` | `Chr(nAsciiCode)` |
| `Asc` | `Asc(sSource)` |
| `Replicate` | `Replicate(sSource, nCount)` |
| `Str` | `Str(nNumber, nLength, nDecimals)` |
| `LStr` | `LStr(vNumber)` |
| `StrZero` | `StrZero(nNumber, nLength, nDecimals)` |
| `LCase` | `LCase(bCondition, sTrueValue, sFalseValue)` |
| `LFromHex` | `LFromHex(sSource)` |
| `LToHex` | `LToHex(sSource)` |
| `LHex2Dec` | `LHex2Dec(sSource)` |
| `LTransform` | `LTransform(vExpression, sPicture)` |
| `AddColDelimiters` | `AddColDelimiters(sDSN, aCols, sTable)` |
| `AddNameDelimiters` | `AddNameDelimiters([sDSN[, sName]])` |
| `BuildString2` | `BuildString2(aTarget[, sLineDelimiter[, sColDelimiter]])` |
| `PrepareArrayForIn` | `PrepareArrayForIn(vArray, vItemType)` |

### Numeric Functions

| Function | Signature |
|----------|-----------|
| `Abs` | `Abs(nValue)` |
| `Round` | `Round(vValue, vDigits[, sMidPointRounding])` |
| `Integer` | `Integer(nDecimalValue)` |
| `Max` | `Max(vValue1, vValue2)` |
| `Min` | `Min(vValue1, vValue2)` |
| `Sqrt` | `Sqrt(nNumber)` |
| `_AND` | `_AND(nValue1, nValue2)` — bitwise AND (function call syntax, not infix) |
| `_OR` | `_OR(nValue1, nValue2)` — bitwise OR (function call syntax, not infix) |
| `_XOR` | `_XOR(nValue1, nValue2)` — bitwise XOR (function call syntax, not infix) |
| `_NOT` | `_NOT(nValue)` — bitwise NOT (function call syntax, not infix) |
| `Rand` | `Rand([nSeed])` |
| `RoundPoint5` | `RoundPoint5(nNumber)` |
| `StdRound` | `StdRound(sStandard, nNrDigits, nNumber)` |
| `SigFig` | `SigFig(sStandard, nNrDigits, nNumber)` |
| `Scient` | `Scient(nDoubleValue)` |
| `ToScientific` | `ToScientific(vNumber, vDecimalPlaces)` |
| `MatFunc` | `MatFunc(sFunctionName, nNumber)` |
| `ToNumeric` | `ToNumeric(vSNumber, vAllowHex)` |
| `IsNumeric` | `IsNumeric(vSNumber, vAllowHex)` |
| `ValidateNumeric` | `ValidateNumeric(sSNumber)` |
| `IsHex` | `IsHex(sSource)` |
| `IsGuid` | `IsGuid(sGuid)` |
| `LimsXOr` | `LimsXOr(nVal1, nVal2)` |

### Date/Time Functions

| Function | Signature |
|----------|-----------|
| `Today` | `Today()` |
| `Now` | `Now()` |
| `Time` | `Time()` |
| `LimsTime` | `LimsTime()` |
| `Year` | `Year(dDate)` |
| `Month` | `Month(dDate)` |
| `Day` | `Day(dDate)` |
| `Hour` | `Hour(dDate)` |
| `Minute` | `Minute(dDate)` |
| `Second` | `Second(dDate)` |
| `Seconds` | `Seconds()` |
| `DOW` | `DOW(dDate)` |
| `DOY` | `DOY(dDate)` |
| `JDay` | `JDay(vDate)` |
| `CMonth` | `CMonth(dDate)` |
| `NoOfDays` | `NoOfDays(dDate)` |
| `CToD` | `CToD(sDateString)` |
| `DToC` | `DToC(dDate)` |
| `DToS` | `DToS(dDate)` |
| `DateAdd` | `DateAdd(vDate, vNumber, vDatepart)` |
| `DateDiff` | `DateDiff(vStartDate, vEndDate, vDatepart)` |
| `DateDiffEx` | `DateDiffEx(vStartDate, vEndDate)` |
| `DateFormat` | `DateFormat(sNewFormat)` |
| `DateFromNumbers` | `DateFromNumbers([vYear[, vMonth[, vDay[, vHour[, vMinute[, vSecond[, vMillisecond[, vMakeInvariant]]]]]]]])` |
| `DateFromString` | `DateFromString(vDateAsString[, vFormat[, vUseLocalCulture[, vMakeInvariant]]])` |
| `DateToString` | `DateToString(vDate, sFormat)` |
| `LIMSDate` | `LIMSDate(vDate, sFormat)` |
| `LimsGetDateFormat` | `LimsGetDateFormat()` |
| `ValidateDate` | `ValidateDate(sStringDate, vUseDateFormat)` |
| `IsInvariantDate` | `IsInvariantDate(vDateValue)` |
| `MakeDateInvariant` | `MakeDateInvariant(vDateValue, vColumnsIndex)` |
| `MakeDateLocal` | `MakeDateLocal(vDateValue, vColumnsIndex)` |
| `StringToDate` | `StringToDate(sDateString, sDateFormat)` |

### Array Functions

| Function | Signature |
|----------|-----------|
| `AAdd` | `AAdd(aTarget, vElement)` |
| `ALen` | `ALen(aTarget)` |
| `AScan` | `AScan(aTarget, vValueOrBlock[, nStart[, nCount]])` |
| `AScanExact` | `AScanExact(aTarget, vValueOrBlock[, nStart[, nCount]])` |
| `AEval` | `AEval(aTarget, fnBlock[, nStart[, nCount]])` |
| `AEvalA` | `AEvalA(aTarget, fnBlock[, nStart[, nCount]])` |
| `AFill` | `AFill(aTarget, vValue[, nStart[, nCount]])` |
| `DelArray` | `DelArray(aTarget, nIndex)` |
| `ArrayNew` | `ArrayNew([nDim1[, nDim2[, nDim3]]])` |
| `ArrayCalc` | `ArrayCalc(aTarget[, sOperation[, vValue[, nStart[, nCount]]]])` |
| `ArrayToTVP` | `ArrayToTVP(vValues, vDataType, sConnectionName)` |
| `BuildArray` | `BuildArray(sText[, bCrlfOk[, sDelimiter[, bUnique[, bTrimSpaces]]]])` |
| `BuildArray2` | `BuildArray2(sText[, sLineDelimiter[, sColDelimiter[, bCrlfOk[, bTrimSpaces]]]])` |
| `BuildString` | `BuildString(aTarget[, nStart[, nCount[, sDelimiter]]])` |
| `BuildStringForIn` | `BuildStringForIn(aTarget)` |
| `CompArray` | `CompArray(aA1, aA2)` |
| `ExtractCol` | `ExtractCol(aTarget, nColumn)` |
| `SortArray` | `SortArray(aTarget, vNumeric)` |

### Database Functions

| Function | Signature |
|----------|-----------|
| `SQLExecute` | `SQLExecute(vCommandString[, vFriendlyName[, vRollbackExistingTransaction[, vNullAsBlank[, vInvariantDateColumns[, vReturnType[, sTableName[, vIncludeSchema[, vIncludeHeader]]]]]]]])` |
| `RunSQL` | `RunSQL(sCommandString[, sFriendlyName[, vValues]])` |
| `RunDS` | `RunDS(vDataSourceName[, vParameters[, vReturnType]])` |
| `LSearch` | `LSearch(sCommandString, vDefaultValue[, sFriendlyName[, aArrayOfValues]])` |
| `LSelect` | `LSelect(sCommandString[, aFieldList[, sFriendlyName[, aArrayOfValues[, bNullAsBlank[, aInvariantDateColumns]]]]])` |
| `LSelect1` | `LSelect1(sCommandString[, sFriendlyName[, aArrayOfValues[, bNullAsBlank[, aInvariantDateColumns]]]])` |
| `LSelectC` | `LSelectC(sCommandString[, aFieldList[, sFriendlyName[, aArrayOfValues[, bNullAsBlank[, aInvariantDateColumns]]]]])` |
| `GetDataSet` | `GetDataSet(sCommandString[, aArrayOfValues[, bIncludeSchema[, sTableName[, bNullAsBlank[, aInvariantDateColumns]]]]])` |
| `GetDataSetEx` | `GetDataSetEx(sCommandString[, sFriendlyName[, aArrayOfValues[, bIncludeSchema[, bIncludeHeader[, sTableName[, bNullAsBlank[, aInvariantDateColumns]]]]]]])` |
| `GetDataSetFromArray` | `GetDataSetFromArray(aArrayOfValues, aArrayFields)` |
| `GetDataSetFromArrayEx` | `GetDataSetFromArrayEx(aArrayOfValues[, aArrayFields[, sTableName[, bIncludeHeader[, bIncludeSchema]]]])` |
| `GetDataSetWithSchemaFromSelect` | `GetDataSetWithSchemaFromSelect(sCommandString, sFriendlyName, aArrayOfValues, aArrayOfPrimaryKeys, aArrayOfUniqueConstraints)` |
| `GetDataSetXMLFromArray` | `GetDataSetXMLFromArray(aArrayOfValues, aArrayFields, sTableName, bIncludeHeader, bIncludeSchema)` |
| `GetDataSetXMLFromSelect` | `GetDataSetXMLFromSelect(sCommandString[, sFriendlyName[, bIncludeHeader[, aArrayOfValues[, bIncludeSchema[, sTableName[, bNullAsBlank[, aInvariantDateColumns]]]]]]])` |
| `GetSSLDataset` | `GetSSLDataset(sSql[, sDSN[, aParamNames[, aParamValues[, sTableName[, bNullAsBlank[, aInvariantDateColumns]]]]]])` |
| `GetNETDataSet` | `GetNETDataSet(vCommandString, vFriendlyName, vArrayOfValues, sTableName, vReturnXml, vR1Compatible)` |
| `GetDSParameters` | `GetDSParameters(sDsName)` |
| `GetTables` | `GetTables(sSql)` |
| `XmlExportSql` | `XmlExportSql(sSql, sFile[, sDb[, aSqlParams[, sTable]]])` |
| `BeginLimsTransaction` | `BeginLimsTransaction(vFriendlyName, vIsoLevel)` |
| `EndLimsTransaction` | `EndLimsTransaction(sFriendlyName, bCommit)` |
| `IsInTransaction` | `IsInTransaction(vConnection)` |
| `GetTransactionsCount` | `GetTransactionsCount(vConnection)` |
| `LimsRecordsAffected` | `LimsRecordsAffected()` |
| `GetLastSQLError` | `GetLastSQLError()` |
| `ReturnLastSQLError` | `ReturnLastSQLError()` |
| `ShowSqlErrors` | `ShowSqlErrors(bFlag)` |
| `IgnoreSqlErrors` | `IgnoreSqlErrors(bFlag)` |
| `DetectSqlInjections` | `DetectSqlInjections(vOnOff, sConnectionName)` |
| `SQLRemoveComments` | `SQLRemoveComments(vStatement)` |
| `GetNoLock` | `GetNoLock(sConnectionName)` |
| `GetRdbmsDelimiter` | `GetRdbmsDelimiter(sDSN, bOpen)` |
| `IsTable` | `IsTable(sFriendlyName, sTableName)` |
| `IsTableFld` | `IsTableFld(sFriendlyName, sTableName, sFieldName)` |
| `TableFldLst` | `TableFldLst(sFriendlyName, sTableName)` |
| `RetrieveLong` | `RetrieveLong(sFriendlyName, sTableName, sColumnName, sWhereCondition, sOutputFilePath, bIsCompressed)` |
| `UpdLong` | `UpdLong(sFriendlyName, sTableName, sColumnName, sWhereCondition, sInputFilePath, bIsCompressed)` |
| `LimsSetCounter` | `LimsSetCounter(sTableName, sFieldName, sPrefix, aArrayOfFields, aArrayOfValues, vNull)` |

### Database Connection

| Function | Signature |
|----------|-----------|
| `GetDBMSName` | `GetDBMSName(sFriendlyName)` |
| `GetDBMSProviderName` | `GetDBMSProviderName(sFriendlyName)` |
| `GetDefaultConnection` | `GetDefaultConnection()` |
| `SetDefaultConnection` | `SetDefaultConnection(vDefaultConnection)` |
| `GetConnectionByName` | `GetConnectionByName(sFriendlyName)` |
| `GetConnectionStrings` | `GetConnectionStrings()` |
| `IsDBConnected` | `IsDBConnected(vFriendlyName)` |
| `LimsSqlConnect` | `LimsSqlConnect(sFriendlyName)` |
| `LimsSqlDisconnect` | `LimsSqlDisconnect(sFriendlyName)` |
| `SetLocationOracle` | `SetLocationOracle(sFile, sServer, sUser, sPassword, bEncrypted)` |
| `SetLocationSQLServer` | `SetLocationSQLServer(sFile, sServer, sDatabase, sOwner, sUser, sPassword, bEncrypted)` |
| `SetSqlTimeout` | `SetSqlTimeout(nTimeout, vConnection)` |
| `SqlTraceOn` | `SqlTraceOn()` |
| `SqlTraceOff` | `SqlTraceOff()` |
| `LimsOleConnect` | `LimsOleConnect(vV)` |
| `EndLimsOleConnect` | `EndLimsOleConnect(vV)` |

### Type & Validation Functions

| Function | Signature |
|----------|-----------|
| `Empty` | `Empty(vValue)` |
| `LimsType` | `LimsType(vParam)` |
| `LimsTypeEx` | `LimsTypeEx(vValue)` |
| `IsDefined` | `IsDefined(vVarName)` |
| `Nothing` | `Nothing(vValue)` |
| `MakeNETObject` | `MakeNETObject(vValue)` |

### Object Functions

| Function | Signature |
|----------|-----------|
| `CreateUdObject` | `CreateUdObject()` / `CreateUdObject(sClassName)` / `CreateUdObject(sClassName, aArgs)` / `CreateUdObject(aPropertyDefs)` |
| `AddProperty` | `AddProperty(oO, vPropName)` |
| `HasProperty` | `HasProperty(vO, sPropName)` |
| `GetInternal` | `GetInternal(vO, sPropName)` |
| `SetInternal` | `SetInternal(vO, sPropName, vPropValue)` |
| `GetInternalC` | `GetInternalC(vO, sCollectionName, vArg1, vArg2, vArg3, vArg4, vArg5, vArg6)` |
| `SetInternalC` | `SetInternalC(vO, sCollectionName, vValue, vArg1, vArg2, vArg3, vArg4, vArg5, vArg6)` |
| `GetByName` | `GetByName(sName)` |
| `SetByName` | `SetByName(sName, vValue)` |
| `CreateLocal` | `CreateLocal(vVarName, vVarValue)` |
| `CreatePublic` | `CreatePublic(vVarName, vVarValue)` |
| `CreateORMSession` | `CreateORMSession()` |
| `LKill` | `LKill(sVarName)` |
| `UndeclaredVars` | `UndeclaredVars(bAllowUndeclaredVars)` |

### Procedure Functions

| Function | Signature |
|----------|-----------|
| `DoProc` | `DoProc(sProcedureName, aArguments)` |
| `ExecFunction` | `ExecFunction(sName[, aParameters])` |
| `ExecUdf` | `ExecUdf(vCode[, aArgs[, bCacheCode]])` |
| `ExecInternal` | `ExecInternal(vO, sMethodName, vArg01, vArg02, vArg03, vArg04, vArg05, vArg06, vArg07, vArg08, vArg09, vArg10, vArg11, vArg12, vArg13, vArg14, vArg15, vArg16, vArg17, vArg18, vArg19, vArg20, vArg21)` |
| `Eval` | `Eval(vCode[, vArg1[, vArg2 ...]])` |
| `Branch` | `Branch(vTarget)` |
| `PrmCount` | `PrmCount()` |
| `GetInlineCode` | `GetInlineCode(sValue, aVariables)` |
| `DeleteInlineCode` | `DeleteInlineCode(sValue)` |
| `GetRegion` | `GetRegion(sValue, vSrc, vDst)` |
| `GetRegionEx` | `GetRegionEx(vValue, vSrc, vDst, vLocalRegions)` |
| `RunApp` | `RunApp(sApplication, sArguments)` |
| `LimsExec` | `LimsExec(sApplication, bShow, sArguments)` |

When there are no arguments, prefer `DoProc("Name")` over `DoProc("Name", {})`, and likewise for `ExecFunction`.
`DoProc(...)` is a **compile-time error** inside class methods. Use `Me:MethodName()` / `Base:MethodName()` for sibling and inherited methods instead.

Only `SQLExecute` supports named `?varName?` substitution. `RunSQL`, `LSearch`, `LSelect`, `LSelect1`, `LSelectC`, `GetDataSet`, `GetDataSetEx`, `GetDataSetWithSchemaFromSelect`, `GetDataSetXMLFromSelect`, `GetNETDataSet`, `XmlExportSql`, and `GetTables` use positional `?` placeholders with explicit value arrays.

`SQLExecute` named placeholder patterns (all resolved at query time):

| Pattern | Description | Example |
|---------|-------------|---------|
| `?name?` | Simple variable binding | `?sCustomerID?` |
| `?obj:Prop?` | Object-property access | `?oUser:ID?` |
| `?obj:method()?` | Parameterless object method | `?oSeq:GetNext()?` |
| `?arr[i]?` | Array element access | `?aValues[1]?` |
| `?Func()?` | Parameterless function call | `?Today()?` |
| `?'value'?` / `?123?` | Constant literal | `?'ACTIVE'?` |
| `?expr + expr?` | Complex expression (evaluated each execution; prefer pre-computed variable) | `?sPrefix + sCode?` |
| `?aValues?` | Array expansion (for IN clauses) | `WHERE id IN (?aIDs?)` |

Built-in classes such as `Email`, `SSLDataset`, and `SSLRegex` must use curly-brace construction (`Email{}`), not `CreateUdObject("Email")`.

`GetRegionEx(vValue, vSrc, vDst, vLocalRegions)` extracts text between two marker strings in an arbitrary string. It is a pure string utility — it does not require a compiled `:REGION` block. Use it in preference to `GetRegion` when working with string content rather than named code regions.

### Error Handling Functions

| Function | Signature |
|----------|-----------|
| `GetLastSSLError` | `GetLastSSLError()` |
| `ClearLastSSLError` | `ClearLastSSLError()` |
| `RaiseError` | `RaiseError(sMessage[, sLocation[, nErrorCode[, oInnerException]]])` |
| `FormatErrorMessage` | `FormatErrorMessage(vV)` |
| `FormatSqlErrorMessage` | `FormatSqlErrorMessage(vV)` |

### Message Functions

| Function | Signature |
|----------|-----------|
| `UsrMes` | `UsrMes(vArg1[, vArg2])` |
| `InfoMes` | `InfoMes(vArg1[, vArg2])` |
| `ErrorMes` | `ErrorMes(vArg1[, vArg2])` |
| `SendLimsEmail` | `SendLimsEmail(sSMTP, aRecipients, sFromWho, sSubject, sMessageBody, aAttachList, aCClist, aBCClist, sReplyTo, nNPort, sUName, sUPass, bIgnoreErrors, bUseCDO, nTimeout, bUseSSL, bIsBodyHTML, sEncryptedData)` |
| `SendToOutbox` | `SendToOutbox(sSMTP, aRecipients, sFromWho, sSubject, sMessageBody, aAttachList, aCClist, aBCClist, sReplyTo, nNPort, sUName, sUPass, bIgnoreErrors, bUseSSL, bIsBodyHTML, sEncryptedData)` |
| `SendFromOutbox` | `SendFromOutbox(bIgnoreErrors, bUseCDO, nTimeout)` |
| `SendOutlookReminder` | `SendOutlookReminder(sSMTP, nStart, dEnd, sSubject, sSummary, sLocation, sOrganizerName, sOrganizerEmail, sAttendeeName, sAttendeeEmail, nNPort, sUName, sUPass, bIgnoreErrors, bUseSSL)` |

`UsrMes`, `ErrorMes`, and `InfoMes` all write to the server log. The only functional difference is the `forceWrite` flag: `ErrorMes` passes `forceWrite=true`, so it writes even when `UsrMes` logging is globally disabled. `InfoMes` delegates directly to `UsrMes` (identical behavior). Use `ErrorMes` for messages that must always be recorded; use `UsrMes`/`InfoMes` for messages that administrators may suppress.

### System Functions

| Function | Signature |
|----------|-----------|
| `IIf` | `IIf(bCondition, vTrueValue, vFalseValue)` |
| `CreateGUID` | `CreateGUID()` |
| `LWait` | `LWait(nSeconds)` |
| `DosSupport` | `DosSupport(sCmd, sPrm, vDbg)` |

### File Functions

| Function | Signature |
|----------|-----------|
| `ReadText` | `ReadText(sFileName[, nCharsToRead[, sEncoding]])` |
| `WriteText` | `WriteText(sFileName, sCharsToWrite, sConfirmRequired[, sAppend[, sEncoding]])` |
| `ReadBytesBase64` | `ReadBytesBase64(sFileName)` |
| `WriteBytesBase64` | `WriteBytesBase64(sFileName, sBase64Data)` |
| `Directory` | `Directory(sFilePattern, oAttributes)` |
| `LDir` | `LDir(sFilePattern, oAttributes)` |
| `FileSupport` | `FileSupport(vFileIdentifier, vRequest, vArg1, vArg2, sEncoding)` |
| `CombineFiles` | `CombineFiles(aArFileNames, sSOutFile)` |
| `GetFileVersion` | `GetFileVersion(sFileName)` |
| `LPrint` | `LPrint(sSource)` |
| `GetPrinters` | `GetPrinters()` |
| `ConvertReport` | `ConvertReport(sFile)` |
| `GetAppBaseFolder` | `GetAppBaseFolder()` |
| `GetAppWorkPathFolder` | `GetAppWorkPathFolder()` |
| `GetLogsFolder` | `GetLogsFolder()` |
| `GetWebFolder` | `GetWebFolder()` |

### Web/XML Functions

| Function | Signature |
|----------|-----------|
| `ToXml` | `ToXml(vO, sTypeName)` |
| `FromXml` | `FromXml(sXml)` |
| `ToJson` | `ToJson(vValue)` |
| `FromJson` | `FromJson(vValue)` |
| `HtmlEncode` | `HtmlEncode(vData)` |
| `HtmlDecode` | `HtmlDecode(vData)` |
| `UrlEncode` | `UrlEncode(vData)` |
| `UrlDecode` | `UrlDecode(vData)` |
| `XmlDomToUdObject` | `XmlDomToUdObject(vXml, vPreserveWhitespace)` |

### Session & Application Functions

| Function | Signature |
|----------|-----------|
| `AddToSession` | `AddToSession(sKey, vValue)` |
| `GetFromSession` | `GetFromSession(sKey)` |
| `ClearSession` | `ClearSession()` |
| `GetFromApplication` | `GetFromApplication(sKey)` |
| `GetSetting` | `GetSetting(sName)` |
| `GetSettings` | `GetSettings(aNames)` |
| `GetUserData` | `GetUserData()` |
| `SetUserData` | `SetUserData(vUserName)` |
| `StationName` | `StationName()` |
| `GetInstallationKey` | `GetInstallationKey()` |
| `GetExecutionTrace` | `GetExecutionTrace()` |
| `IsProductionModeOn` | `IsProductionModeOn()` |
| `GetForbiddenAppIDs` | `GetForbiddenAppIDs()` |
| `GetForbiddenDesignerAppIDs` | `GetForbiddenDesignerAppIDs()` |

### .NET Integration

| Function | Signature |
|----------|-----------|
| `LimsNETConnect` | `LimsNETConnect(sAssembly, sTypeName, aArgs, vAsStatic)` |
| `LimsNETCast` | `LimsNETCast(vValue, sNewType)` |
| `LimsNETTypeOf` | `LimsNETTypeOf(vTypeName)` |
| `In64BitMode` | `In64BitMode()` |
| `NetFrameworkVersion` | `NetFrameworkVersion()` |

### FTP Operations

| Function | Signature |
|----------|-----------|
| `CheckOnFtp` | `CheckOnFtp(sServerNameOrIP, sRemoteDirectory, sRemoteFileName, sUserName, sPassword, nPort, sProxy, bIsSFTP, sPrivateKeyFilePath)` |
| `CopyToFtp` | `CopyToFtp(sServerNameOrIP, sRemoteDirectory, aRemoteFileNames, sFileContents, sUserName, sPassword, nPort, sProxy, bIsSFTP, sPrivateKeyFilePath)` |
| `DeleteDirOnFtp` | `DeleteDirOnFtp(sServerNameOrIP, sRemoteDirectory, sUserName, sPassword, nPort, sProxy, bIsSFTP, sPrivateKeyFilePath)` |
| `DeleteFromFtp` | `DeleteFromFtp(sServerNameOrIP, sRemoteDirectory, sRemoteFileName, sUserName, sPassword, nPort, sProxy, bIsSFTP, sPrivateKeyFilePath)` |
| `GetDirFromFtp` | `GetDirFromFtp(sServerNameOrIP, sRemoteDirectory, sFilePattern, sUserName, sPassword, nPort, sProxy, bUsePassive, bIsSFTP, sPrivateKeyFilePath)` |
| `GetFromFtp` | `GetFromFtp(sServerNameOrIP, sRemoteDirectory, sRemoteFileName, sLocalFileName, sUserName, sPassword, nPort, sProxy, bIsSFTP, sPrivateKeyFilePath)` |
| `MakeDirOnFtp` | `MakeDirOnFtp(sServerNameOrIP, sRemoteDirectory, sUserName, sPassword, nPort, sProxy, bIsSFTP, sPrivateKeyFilePath)` |
| `MoveInFtp` | `MoveInFtp(sServerNameOrIP, sRemoteDirectoryFrom, sRemoteDirectoryTo, sRemoteFileFrom, sRemoteFileTo, sUserName, sPassword, nPort, sProxy, bIsSFTP, sPrivateKeyFilePath)` |
| `ReadFromFtp` | `ReadFromFtp(sServerNameOrIP, sRemoteDirectory, sRemoteFileName, nMaxSize, sUserName, sPassword, nPort, sProxy, bIsSFTP, sPrivateKeyFilePath)` |
| `RenameOnFtp` | `RenameOnFtp(sServerNameOrIP, sRemoteDirectory, sFileNameOld, sFileNameNew, sUserName, sPassword, nPort, sProxy, bIsSFTP, sPrivateKeyFilePath)` |
| `SendToFtp` | `SendToFtp(sServerNameOrIP, sRemoteDirectory, sRemoteFileName, sLocalFileName, sUserName, sPassword, nPort, sProxy, bUsePassive, bIsSFTP, sPrivateKeyFilePath)` |
| `WriteToFtp` | `WriteToFtp(sServerNameOrIP, sRemoteDirectory, sRemoteFileName, sFileContents, sUserName, sPassword, nPort, sProxy, bIsSFTP, sPrivateKeyFilePath)` |

### Security & Encryption

| Function | Signature |
|----------|-----------|
| `EncryptData` | `EncryptData(sInputData, sPassword, sAlgorithm, sKey, sRetType)` |
| `DecryptData` | `DecryptData(sInputData, sPassword)` |
| `HashData` | `HashData(sInputData, sAlgorithm)` |
| `VerifySignature` | `VerifySignature(sCertificateString, vData, sSignature)` |
| `MimeEncode` | `MimeEncode(vV)` |
| `MimeDecode` | `MimeDecode(vV)` |
| `ChkPassword` | `ChkPassword(sUserName, sPassword)` |
| `ChkNewPassword` | `ChkNewPassword(sPassword, vPrevPasswords)` |
| `SetUserPassword` | `SetUserPassword(sUserName, sPassword)` |
| `LDAPAuth` | `LDAPAuth(sLdapHost, nLdapPort, sLdapUserName, sLdapPassword, sLdapDistinctiveName, bSecure)` |
| `LDAPAuthEX` | `LDAPAuthEX(sLdapHost, nLdapPort, sBindUserName, sBindUserPassword, sSearchUserName, sSearchUserPassword, sLdapDistinguishedName, sLdapDistinguishedNameStartSearch, sSearchFilter, sAuthAttribName, bSecure)` |
| `SearchLDAPUser` | `SearchLDAPUser(sLdapHost, nLdapPort, sBindUserName, sBindUserPassword, sSearchUserName, sLdapDistinguishedNameStartSearch, sSearchFilter, bSecure)` |

### Compression

| Function | Signature |
|----------|-----------|
| `Compress` | `Compress(sSource, vToFile)` |
| `Decompress` | `Decompress(sSource, vFromFile)` |
| `CreateZip` | `CreateZip(sZipFileName, sSourceDirectory, bRecurse, sFileFilter, sPassword)` |
| `ExtractZip` | `ExtractZip(sZipFileName, sTargetDirectory, sFileFilter, sPassword)` |

### Batch & System

| Function | Signature |
|----------|-----------|
| `SubmitToBatch` | `SubmitToBatch(sCode, vParameters, sMode, sUserName, sPassword)` |
| `SubmitToBatchEx` | `SubmitToBatchEx(sCode)` |
| `InBatchProcess` | `InBatchProcess()` |
| `TraceOn` | `TraceOn()` |
| `TraceOff` | `TraceOff()` |
| `SetDecimalSeparator` | `SetDecimalSeparator(sDecimalSep)` |
| `GetDecimalSep` | `GetDecimalSep()` |
| `GetDecimalSeparator` | `GetDecimalSeparator()` |
| `SetGroupSeparator` | `SetGroupSeparator(sGroupSep)` |
| `GetGroupSeparator` | `GetGroupSeparator()` |

`TraceOn()` and `TraceOff()` toggle `InvokeMethodPerformanceLog` and return the previous `Enabled` state. They silently no-op if `AppConfig.InvokeMethodPerformanceLogEnabled` is `false`. `SqlTraceOn()` and `SqlTraceOff()` work identically but toggle `SQLPerformanceLog`.

### Formatting & Locale

| Function | Signature |
|----------|-----------|
| `LimsGetDateFormat` | `LimsGetDateFormat()` |
| `GetDecimalSep` | `GetDecimalSep()` |
| `GetDecimalSeparator` | `GetDecimalSeparator()` |
| `SetDecimalSeparator` | `SetDecimalSeparator(sDecimalSep)` |
| `GetGroupSeparator` | `GetGroupSeparator()` |
| `SetGroupSeparator` | `SetGroupSeparator(sGroupSep)` |
| `LStr` | `LStr(vNumber)` |
| `LTransform` | `LTransform(vExpression, sPicture)` |

### Server Time

| Function | Signature |
|----------|-----------|
| `ServerStartOfDay` | `ServerStartOfDay(vDate)` |
| `ServerEndOfDay` | `ServerEndOfDay(vDate)` |
| `ClientStartOfDay` | `ClientStartOfDay(vDate)` |
| `ClientEndOfDay` | `ClientEndOfDay(vDate)` |
| `ServerTimeZone` | `ServerTimeZone()` |
| `UserTimeZone` | `UserTimeZone()` |

### Licensing & Features

| Function | Signature |
|----------|-----------|
| `GetFeaturesAndNumbers` | `GetFeaturesAndNumbers()` |
| `GetInstallationKey` | `GetInstallationKey()` |
| `GetLicenseInfoAsText` | `GetLicenseInfoAsText(bBHtml)` |
| `GetNumberOfInstrumentConnections` | `GetNumberOfInstrumentConnections()` |
| `GetNumberOfNamedConcurrentUsers` | `GetNumberOfNamedConcurrentUsers()` |
| `GetNumberOfNamedUsers` | `GetNumberOfNamedUsers()` |
| `In64BitMode` | `In64BitMode()` |
| `IsDemoLicense` | `IsDemoLicense()` |
| `IsFeatureAuthorized` | `IsFeatureAuthorized(sAppGuid)` |
| `IsFeatureBasedLicense` | `IsFeatureBasedLicense()` |
| `IsProductionModeOn` | `IsProductionModeOn()` |
| `NetFrameworkVersion` | `NetFrameworkVersion()` |
| `ResetFeatures` | `ResetFeatures()` |

### Reporting

| Function | Signature |
|----------|-----------|
| `ConvertReport` | `ConvertReport(sFile)` |
| `GetPrinters` | `GetPrinters()` |

### Documentum Integration

| Function | Signature |
|----------|-----------|
| `DocInitDocumentumInterface` | `DocInitDocumentumInterface()` |
| `DocEndDocumentumInterface` | `DocEndDocumentumInterface()` |
| `DocLoginToDocumentum` | `DocLoginToDocumentum(sDocBase, sUser, sPassword)` |
| `DocCommandFailed` | `DocCommandFailed()` |
| `DocGetErrorMessage` | `DocGetErrorMessage()` |
| `DocCreateCabinet` | `DocCreateCabinet(sName, sCabinetType, sAcl)` |
| `DocDeleteCabinet` | `DocDeleteCabinet(sCabinetId, bDeepDelete)` |
| `DocGetCabinets` | `DocGetCabinets()` |
| `DocCreateFolder` | `DocCreateFolder(sPath, sName, sAcl)` |
| `DocDeleteFolder` | `DocDeleteFolder(sFolderId, bDeepDelete)` |
| `DocGetFolders` | `DocGetFolders(sParentPath)` |
| `DocImportDocument` | `DocImportDocument(sDocFile, sDestinationPath, sDocName, sDocType, sAppCode, sAclName)` |
| `DocExportDocument` | `DocExportDocument(sDocumentId, sFormat)` |
| `DocGetDocuments` | `DocGetDocuments(sFolderPath, sDocTypes)` |
| `DocExists` | `DocExists(sObjId)` |
| `DocDelete` | `DocDelete(sObjId, bAllVersions)` |
| `DocCheckinDocument` | `DocCheckinDocument(sFilePath, sDocumentId, sVersion, bReplaceContent, bMajorVersion)` |
| `DocCheckoutDocument` | `DocCheckoutDocument(sDocumentId)` |
| `DocCancelCheckout` | `DocCancelCheckout(sDocumentId)` |
| `DocGetMetadata` | `DocGetMetadata(sObjId, oAttributes)` |
| `DocSetMetadata` | `DocSetMetadata(sObjId, oAttributes)` |
| `DocGetTypeAttributes` | `DocGetTypeAttributes(sTypeName)` |
| `DocGetTypeAttributesAsDataset` | `DocGetTypeAttributesAsDataset(sTypeName)` |
| `DocCreateACL` | `DocCreateACL(sName, sDescription, aGroups)` |
| `DocCreateUser` | `DocCreateUser(sLoginName, sPassword, sUserName, sEMail, sDefaultFolder, sGroupName, sPermissionSet, nUserPrivileges)` |
| `DocUpdateUser` | `DocUpdateUser(sLoginName, sPassword, sUserName, sEMail, sDefaultFolder, sGroupName, sPermissionSet, nUserPrivileges)` |
| `DocDeleteUser` | `DocDeleteUser(sName)` |
| `DocExistsUser` | `DocExistsUser(sLoginName, sUserName)` |
| `DocCreateGroup` | `DocCreateGroup(sName, sDescription)` |
| `DocAddUsersToGroup` | `DocAddUsersToGroup(sGroupName, aUsers)` |
| `DocRemoveUsersFromGroup` | `DocRemoveUsersFromGroup(sGroupName, aUsers)` |
| `DocRemoveAllUsersFromGroup` | `DocRemoveAllUsersFromGroup(sGroupName)` |
| `DocStartWorkflow` | `DocStartWorkflow(sWorkflowId, aDocumentIds, sPackageName)` |
| `DocStopWorkflow` | `DocStopWorkflow(sWorkflowId)` |
| `DocPauseWorkflow` | `DocPauseWorkflow(sWorkflowId)` |
| `DocResumeWorkflow` | `DocResumeWorkflow(sWorkflowId)` |
| `DocGetWorkflowStatus` | `DocGetWorkflowStatus(sWorkflowId)` |
| `DocGetTasks` | `DocGetTasks(sWorkflowId)` |
| `DocGetTasksCount` | `DocGetTasksCount()` |
| `DocAcquireWorkitem` | `DocAcquireWorkitem(sWorkitemId)` |
| `DocCompleteWorkitem` | `DocCompleteWorkitem(sWorkitemId, sSignOffUser, sSignOffPass, sSignOffReason)` |
| `DocDelegateWorkitem` | `DocDelegateWorkitem(sWorkitemId, sUser)` |
| `DocRepeatWorkitem` | `DocRepeatWorkitem(sWorkitemId, aUsers, sSignOffUser, sSignOffPass, sSignOffReason)` |
| `DocGetWorkitemProperties` | `DocGetWorkitemProperties(sWorkitemId)` |
| `DocSearchFullText` | `DocSearchFullText(sTextToSearch, sStartLocation, nResultSetSize)` |
| `DocSearchAsDataset` | `DocSearchAsDataset(sContains, sStartLocation, sObjectType, sWhere, bAllVersions, nResultSetSize)` |
| `DocSearchUsingDql` | `DocSearchUsingDql(sDql, nResultSetSize)` |

---

## Top 30 Most-Used Functions

Based on production code analysis:

| Rank | Function | Usage Count |
|------|----------|-------------|
| 1 | `SQLExecute` | 32,822 |
| 2 | `Empty` | 14,372 |
| 3 | `DoProc` | 11,413 |
| 4 | `Len` | 11,058 |
| 5 | `LimsString` | 10,340 |
| 6 | `ExecFunction` | 8,914 |
| 7 | `UsrMes` | 8,638 |
| 8 | `Upper` | 8,119 |
| 9 | `AAdd` | 5,804 |
| 10 | `Chr` | 4,634 |
| 11 | `AllTrim` | 4,486 |
| 12 | `RunSQL` | 2,873 |
| 13 | `SubStr` | 2,785 |
| 14 | `Now` | 2,750 |
| 15 | `GetSetting` | 2,720 |
| 16 | `Left` | 2,324 |
| 17 | `Val` | 2,233 |
| 18 | `LSearch` | 2,211 |
| 19 | `ExtractCol` | 2,130 |
| 20 | `At` | 2,002 |
| 21 | `Trim` | 1,958 |
| 22 | `Max` | 1,802 |
| 23 | `Time` | 1,705 |
| 24 | `IIf` | 1,661 |
| 25 | `StrTran` | 1,612 |
| 26 | `CreateUdObject` | 1,573 |
| 27 | `RaiseError` | 1,571 |
| 28 | `Today` | 1,412 |
| 29 | `GetDataSet` | 1,303 |
| 30 | `BuildString` | 1,280 |

---

## Function Signatures in LSP

The LSP provides function signatures with:
- Parameter names and types
- Optional parameter indicators
- Return type
- Description

Example hover/signature for `SQLExecute`:

```
SQLExecute(vCommandString, [vFriendlyName], [vRollbackExistingTransaction],
           [vNullAsBlank], [vInvariantDateColumns], [vReturnType],
           [sTableName], [vIncludeSchema], [vIncludeHeader]) → variant

Universal database function. Supports ?varName? variable substitution.
Routes SELECT to array/XML, DML to RunSQL internally.

Parameters:
- vCommandString: The SQL query or command to execute
- vFriendlyName: Friendly/logging name (optional)
- ... additional optional parameters

Returns: Variant result routed by SQLExecute
```

---

## Function Casing

SSL functions are case-insensitive but should use documented casing:

### Source-Aligned Canonical Spellings
```ssl
AAdd(aArray, value);
ALen(aArray);
AScan(aArray, value);
```

### PascalCase (Most Functions)
```ssl
AllTrim(sString);
SQLExecute(sSQL);
CreateUdObject("ClassName");
LimsString(nValue);
Str(nValue, 6, 2);   /* Numeric formatting with width/decimals;
```

---

## Complete Reference

For the full list of 354 canonical built-in functions with detailed signatures, see:

**LSP Sources:** `internal/constants/canonical.go`, `internal/constants/signatures.go`
