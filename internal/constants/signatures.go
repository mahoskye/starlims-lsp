// Package constants defines SSL language keywords, operators, functions, and classes.
package constants

import "strings"

// FunctionParameter represents a parameter in a function signature.
type FunctionParameter struct {
	Name        string
	Type        string
	Required    bool
	Description string
}

// FunctionSignature represents a function's signature with parameters.
type FunctionSignature struct {
	Name        string
	Description string
	ReturnType  string
	Parameters  []FunctionParameter
}

// SSLFunctionSignatures maps function names (lowercase) to their signatures.
var SSLFunctionSignatures = map[string]FunctionSignature{
	"aadd": {
		Name: "aadd", Description: "Appends an element to an array and returns the appended element.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "target", Type: "array", Required: true, Description: "Array to which the element will be added."},
			{Name: "element", Type: "variant", Required: true, Description: "Item to be added to the target array."},
		},
	},
	"abs": {
		Name: "Abs", Description: "Computes the absolute value of a numeric value and returns it as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "numericValue", Type: "double", Required: true, Description: "Numeric value for which the absolute value is to be calculated."},
		},
	},
	"addcoldelimiters": {
		Name: "AddColDelimiters", Description: "Adds RDBMS-specific name delimiters to column names in an array for use in SQL queries.", ReturnType: "void",
		Parameters: []FunctionParameter{
			{Name: "dsn", Type: "string", Required: true, Description: "Data source name used to determine the appropriate RDBMS-specific delimiter style. Can be null or empty string for default behavior."},
			{Name: "cols", Type: "array", Required: true, Description: "An array of column names to wrap with delimiters. Modified in place with the delimited values."},
			{Name: "table", Type: "string", Required: true, Description: "Name of the table the columns belong to."},
		},
	},
	"addnamedelimiters": {
		Name: "AddNameDelimiters", Description: "Wraps a database object name with RDBMS-specific delimiters for use in SQL queries.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "dsn", Type: "string", Required: true, Description: "Data source name used to determine the appropriate RDBMS-specific delimiter style. Can be null or empty for default behavior."},
			{Name: "name", Type: "string", Required: true, Description: "The database object name to wrap with delimiters. Can be null (defaults to empty string)."},
		},
	},
	"addproperty": {
		Name: "AddProperty", Description: "Adds a property to an object and returns null.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "o", Type: "object", Required: true, Description: "Object to which properties will be added."},
			{Name: "propName", Type: "variant", Required: true, Description: "Name of the property to be added to an object."},
		},
	},
	"addtoapplication": {
		Name: "AddToApplication", Description: "Stub function that always returns null. Does not actually store data.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "key", Type: "string", Required: true, Description: "Unique identifier for the data being added to the application."},
			{Name: "value", Type: "variant", Required: true, Description: "Value to add to the application under the specified key."},
		},
	},
	"addtosession": {
		Name: "AddToSession", Description: "Adds a key-value pair to the session and returns null.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "key", Type: "string", Required: true, Description: "Unique identifier used to store and retrieve values in the session."},
			{Name: "value", Type: "variant", Required: true, Description: "Is the data that will be stored in the session under the specified key."},
		},
	},
	"aeval": {
		Name: "aeval", Description: "Applies a code block to each element in an array for side effects; returns the original array unchanged.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "target", Type: "array", Required: true, Description: "Array on which the code block will be applied to each element."},
			{Name: "codeBlock", Type: "sslfunction", Required: true, Description: "Function to apply to each element of the target array during iteration."},
			{Name: "start", Type: "double", Required: false, Description: "Specifies the index at which to begin evaluating elements in the array."},
			{Name: "count", Type: "double", Required: false, Description: "Specifies the number of items to process in the array from the starting index."},
		},
	},
	"aevala": {
		Name: "aevala", Description: "Applies a code block to each element in an array and stores the results back into the array; returns the modified array.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "target", Type: "array", Required: true, Description: "Array on which the code block will be applied to each element."},
			{Name: "codeBlock", Type: "sslfunction", Required: true, Description: "A function that takes an array element as input and returns a computed value, which is then used to update or replace elements in the target array during iteration."},
			{Name: "start", Type: "double", Required: false, Description: "Specifies the index at which to begin evaluating elements in the array."},
			{Name: "count", Type: "double", Required: false, Description: "Specifies the number of items to process in the array."},
		},
	},
	"afill": {
		Name: "afill", Description: "Fills a portion of an array with a specified value and returns the modified array.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "target", Type: "array", Required: true, Description: "Array to be filled with values."},
			{Name: "value", Type: "variant", Required: true, Description: "Value to fill the array with."},
			{Name: "start", Type: "double", Required: false, Description: "1-based index at which to begin filling the array. Defaults to 1. Must be an integer value."},
			{Name: "count", Type: "double", Required: false, Description: "Number of elements to fill starting from the start index. Defaults to array length. Must be an integer value."},
		},
	},
	"alen": {
		Name: "alen", Description: "Computes and returns the length of the given array as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "target", Type: "array", Required: true, Description: "Array for which the length is to be determined."},
		},
	},
	"alltrim": {
		Name: "AllTrim", Description: "Returns a new string with leading and trailing whitespace removed from the input string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String that will have leading and trailing whitespace removed."},
		},
	},
	"arraycalc": {
		Name: "arraycalc", Description: "Computes and returns various statistical values or modified arrays based on specified operations.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "target", Type: "array", Required: true, Description: "Array on which operations will be performed."},
			{Name: "operation", Type: "string", Required: true, Description: "Specifies the mathematical or logical operation to perform on an array, such as \"SUM\", \"AVG\", or \"MERGE\"."},
			{Name: "value", Type: "variant", Required: false, Description: "Data or value to be used in operations like MERGE, SORT, ADD, FILL, INS. Not needed for statistical operations."},
			{Name: "start", Type: "double", Required: false, Description: "1-based index at which to begin processing. Defaults to 1. Used by COPY, DEL, FILL, INS operations."},
			{Name: "count", Type: "double", Required: false, Description: "Number of elements to process. Defaults to array length. Used by COPY, DEL, FILL operations."},
		},
	},
	"arraynew": {
		Name: "arraynew", Description: "Creates a new array of specified dimensions and returns it as an SSLArray object.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "dim1", Type: "double", Required: false, Description: "Size of the first dimension. Defaults to 0 if null. Must be a non-negative integer."},
			{Name: "dim2", Type: "double", Required: false, Description: "Size of the second dimension. If used, dim1 must be > 0. Must be a non-negative integer."},
			{Name: "dim3", Type: "double", Required: false, Description: "Size of the third dimension. If used, dim2 must be > 0. Must be a non-negative integer."},
		},
	},
	"arraytotvp": {
		Name: "ArrayToTVP", Description: "Converts an array to a TVP and returns it as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "values", Type: "variant", Required: true, Description: "Array of data to be converted into a Table Value Parameter (TVP)."},
			{Name: "dataType", Type: "variant", Required: true, Description: "Specifies the data type for TVP elements. Valid values are \"INT\", \"DOUBLE\", \"DATE\", or any other value defaults to string."},
			{Name: "connectionName", Type: "variant", Required: false, Description: "Optional database connection name used to determine the database platform. If null, uses the default connection."},
		},
	},
	"asc": {
		Name: "Asc", Description: "Converts the first character of a string to its ASCII code and returns it as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String from which the ASCII value of the first character will be determined."},
		},
	},
	"ascan": {
		Name: "ascan", Description: "Returns the 1-based index of the first occurrence of a specified value in an array, or 0 if not found.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "target", Type: "array", Required: false, Description: "Array in which to search for elements that match a specified value or function."},
			{Name: "value", Type: "variant", Required: false, Description: "Value to search for within the array."},
			{Name: "start", Type: "double", Required: false, Description: "Specifies the index at which to begin searching within the array for the specified value or condition."},
			{Name: "count", Type: "double", Required: false, Description: "Specifies the number of items to process in the array starting from the specified index."},
		},
	},
	"ascanexact": {
		Name: "ascanexact", Description: "Returns the 1-based index of an exact value match in an array, or 0 if not found.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "target", Type: "array", Required: false, Description: "Array in which to search for the specified value(s)."},
			{Name: "value", Type: "variant", Required: false, Description: "Item to search for within the array."},
			{Name: "start", Type: "double", Required: false, Description: "Specifies the index at which to begin searching for the specified value within the target array."},
			{Name: "count", Type: "double", Required: false, Description: "Number of items to process from the start index in the array."},
		},
	},
	"at": {
		Name: "At", Description: "Returns the 1-based starting position of a substring within a string, or 0 if not found.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "subString", Type: "string", Required: true, Description: "Substring to search for within the source string."},
			{Name: "source", Type: "string", Required: true, Description: "String in which to search for the specified substring."},
		},
	},
	"beginlimstransaction": {
		Name: "BeginLimsTransaction", Description: "Starts a database transaction and returns a boolean indicating success.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "friendlyName", Type: "variant", Required: false, Description: "Name given to the transaction for easy identification."},
			{Name: "isoLevel", Type: "variant", Required: false, Description: "Specifies the isolation level for the database transaction. If null or empty, uses the default isolation level from GlobalSettings.SqlDefaultIsolationLevel."},
		},
	},
	"break": {
		Name: "Break", Description: "Triggers a debugger break and returns null.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"buildarray": {
		Name: "buildarray", Description: "Converts a string into an array based on specified delimiters and options, returning the resulting array.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "text", Type: "string", Required: false, Description: "String that will be split into an array based on specified delimiters and other options."},
			{Name: "crlfOk", Type: "boolean", Required: false, Description: "Determines whether carriage return and line feed characters are allowed in the input text."},
			{Name: "delimiter", Type: "string", Required: false, Description: "Specifies the character or string used to separate values in the input text when building an array."},
			{Name: "unique", Type: "boolean", Required: false, Description: "Parameter is accepted but NOT IMPLEMENTED - does not actually filter duplicates. Defaults to false."},
			{Name: "trimSpaces", Type: "boolean", Required: false, Description: "TrimSpaces determines whether leading and trailing spaces should be removed from each item in the resulting array."},
		},
	},
	"buildarray2": {
		Name: "buildarray2", Description: "Converts a string into a 2D array using specified delimiters and options. Returns an array.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "text", Type: "string", Required: false, Description: "String that will be parsed into an array based on specified delimiters and options for handling carriage returns, line feeds, and trimming spaces."},
			{Name: "lineDelimiter", Type: "string", Required: false, Description: "Specifies the string used to delimit lines in the input text when building an array."},
			{Name: "colDelimiter", Type: "string", Required: false, Description: "Specifies the character used to separate columns in the input text."},
			{Name: "crlfOk", Type: "boolean", Required: false, Description: "Indicates whether carriage return and line feed characters are allowed in the input text."},
			{Name: "trimSpaces", Type: "boolean", Required: false, Description: "TrimSpaces determines whether leading and trailing spaces should be removed from text before processing."},
		},
	},
	"buildstring": {
		Name: "buildstring", Description: "Builds a string from array elements, starting at a specified index and count, using a delimiter. Returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "target", Type: "array", Required: true, Description: "Array from which elements will be concatenated into a string."},
			{Name: "start", Type: "double", Required: false, Description: "Index at which to begin building the string from the target array."},
			{Name: "count", Type: "double", Required: false, Description: "Number of elements from the array to include in the resulting string, starting from the specified index."},
			{Name: "delimiter", Type: "string", Required: false, Description: "String used to separate elements in the resulting concatenated string."},
		},
	},
	"buildstring2": {
		Name: "buildstring2", Description: "Builds a string from an array, using specified delimiters; returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "target", Type: "array", Required: true, Description: "Is the array of data to be processed and converted into a string format."},
			{Name: "lineDelimiter", Type: "string", Required: false, Description: "Specifies the string used to separate lines in the resulting concatenated string."},
			{Name: "colDelimiter", Type: "string", Required: false, Description: "String used to separate columns in the output string."},
		},
	},
	"buildstringforin": {
		Name: "BuildStringForIn", Description: "Converts an array to a string suitable for SQL IN clause, returning a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "target", Type: "array", Required: false, Description: "An array containing the elements that will be converted into a string format suitable for use in an SQL IN clause."},
		},
	},
	"callbuiltinfunction": {
		Name: "CallBuiltInFunction", Description: "Calls a parameterless built-in SSL function by name and returns its result.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "functionName", Type: "string", Required: true, Description: "Name of the built-in function to be called."},
		},
	},
	"checkonftp": {
		Name: "CheckOnFtp", Description: "Checks the existence of a file on an FTP server and returns `true` if the file exists, otherwise `false`.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "serverNameOrIP", Type: "string", Required: true, Description: "IP address or hostname of the FTP server."},
			{Name: "remoteDirectory", Type: "string", Required: true, Description: "RemoteDirectory specifies the directory on the FTP server where the file is located."},
			{Name: "remoteFileName", Type: "string", Required: true, Description: "Name of the file to check on the FTP server."},
			{Name: "userName", Type: "string", Required: true, Description: "Username used for authentication with the FTP server."},
			{Name: "password", Type: "string", Required: true, Description: "Represents the user's password for authenticating with an FTP server."},
			{Name: "port", Type: "double", Required: false, Description: "Port number on which the FTP server is listening."},
			{Name: "proxy", Type: "string", Required: false, Description: "Specifies the proxy server to use for FTP operations. Can be null if no proxy is needed."},
			{Name: "isSFTP", Type: "boolean", Required: false, Description: "Indicates whether to use SFTP (true) or FTP (false)."},
			{Name: "privateKeyFilePath", Type: "string", Required: false, Description: "Path to the private key file used for SFTP authentication. Only used when isSFTP is true."},
		},
	},
	"chknewpassword": {
		Name: "ChkNewPassword", Description: "Validates a new password against previous passwords; returns true if valid.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "password", Type: "string", Required: false, Description: "Represents a new password entered by the user for validation."},
			{Name: "prevPasswords", Type: "variant", Required: false, Description: "An array containing previous passwords used by the user."},
		},
	},
	"chkpassword": {
		Name: "ChkPassword", Description: "Validates user credentials and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "userName", Type: "string", Required: false, Description: "Username of the user whose password is being checked."},
			{Name: "password", Type: "string", Required: false, Description: "Represents the user's password for authentication."},
		},
	},
	"chr": {
		Name: "Chr", Description: "Converts an ASCII code to its corresponding character. Returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "asciiCode", Type: "double", Required: false, Description: "ASCII code for which the corresponding character is to be returned."},
		},
	},
	"clearlastsslerror": {
		Name: "ClearLastSSLError", Description: "Clears any stored SSL error message and returns true.", ReturnType: "boolean",
		Parameters: []FunctionParameter{},
	},
	"clearsession": {
		Name: "ClearSession", Description: "Clears the current user session and returns null.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"clientendofday": {
		Name: "ClientEndOfDay", Description: "Computes the end of day for a given date and returns it as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "variant", Required: true, Description: "Date and time for which the end-of-day time is calculated."},
		},
	},
	"clientstartofday": {
		Name: "ClientStartOfDay", Description: "Computes the start of the day for a given date in the client's time zone, returning an SSLDate object.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "variant", Required: true, Description: "Date for which the start of the day is calculated, adjusted for user time zone differences."},
		},
	},
	"cmonth": {
		Name: "CMonth", Description: "Returns the full month name from a given date as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "date", Required: true, Description: "Date for which the month name is to be retrieved."},
		},
	},
	"combinefiles": {
		Name: "CombineFiles", Description: "Combines multiple files into a single output file and returns an empty string on success.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "arFileNames", Type: "array", Required: true, Description: "An array containing the names of files to be combined."},
			{Name: "sOutFile", Type: "string", Required: true, Description: "Destination file path where the combined content will be saved."},
		},
	},
	"comparray": {
		Name: "comparray", Description: "Compares two arrays and returns true if they are equal, false otherwise.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "a1", Type: "array", Required: true, Description: "First array to compare."},
			{Name: "a2", Type: "array", Required: true, Description: "A2: The second array to compare with the first array."},
		},
	},
	"compress": {
		Name: "Compress", Description: "Compresses a string and returns the compressed data as a string or writes it to a file based on the provided parameters.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "variant", Required: true, Description: "String to compress."},
			{Name: "toFile", Type: "variant", Required: false, Description: "Indicates whether the compressed data should be written to a file. Defaults to false if null."},
		},
	},
	"convertreport": {
		Name: "ConvertReport", Description: "Converts a report file to another format; returns true on success or throws an exception on failure.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "file", Type: "string", Required: true, Description: "Path to the report file that needs to be converted."},
		},
	},
	"copytoftp": {
		Name: "CopyToFtp", Description: "Copies file contents to an FTP server and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "serverNameOrIP", Type: "string", Required: true, Description: "Specifies the name or IP address of the FTP server where files will be copied."},
			{Name: "remoteDirectory", Type: "string", Required: true, Description: "Directory on the FTP server where files will be copied."},
			{Name: "remoteFileNames", Type: "array", Required: false, Description: "An array of strings representing the names of files to copy to the remote FTP server."},
			{Name: "fileContents", Type: "string", Required: true, Description: "Content of the file to be uploaded to the FTP server."},
			{Name: "userName", Type: "string", Required: true, Description: "Username used to authenticate with the FTP server or SFTP service for file transfer operations."},
			{Name: "password", Type: "string", Required: true, Description: "Password for authenticating with the FTP server."},
			{Name: "port", Type: "double", Required: false, Description: "Specifies the port number for the FTP server to which files will be copied."},
			{Name: "proxy", Type: "string", Required: false, Description: "Specifies the proxy server to use for the FTP connection. Can be null if no proxy is needed."},
			{Name: "isSFTP", Type: "boolean", Required: false, Description: "Indicates whether to use SFTP instead of FTP for the file transfer."},
			{Name: "privateKeyFilePath", Type: "string", Required: false, Description: "Path to the private key file used for SFTP authentication. Only used when isSFTP is true."},
		},
	},
	"createguid": {
		Name: "CreateGUID", Description: "Generates a unique globally unique identifier (GUID) as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"createlocal": {
		Name: "CreateLocal", Description: "Creates a local variable and assigns it a value, returning the assigned value as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "varName", Type: "variant", Required: true, Description: "Name of the local variable to be created."},
			{Name: "varValue", Type: "variant", Required: false, Description: "Value to assign to the local variable specified by varName."},
		},
	},
	"createormsession": {
		Name: "CreateORMSession", Description: "Creates and returns an ORM session object for database operations.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"createpublic": {
		Name: "CreatePublic", Description: "Creates a public variable with the specified name and assigns it a value, returning the value.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "varName", Type: "variant", Required: true, Description: "Name of the public variable to create."},
			{Name: "varValue", Type: "variant", Required: false, Description: "Value to assign to the public variable being created."},
		},
	},
	"createudobject": {
		Name: "CreateUdObject", Description: "Creates user-defined objects with dynamic properties and returns a `sslexpando` object.", ReturnType: "sslexpando",
		Parameters: []FunctionParameter{
			{Name: "args", Type: "sslvalue[]", Required: true, Description: "An array of SSLValue objects. First element can be a string (object type name for late binding) or an array of property definitions. Property definitions can be strings (property names) or arrays of [name, value] pairs."},
		},
	},
	"createzip": {
		Name: "CreateZip", Description: "Creates a zip file from a directory and returns null.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "zipFileName", Type: "string", Required: true, Description: "Name of the zip file to be created."},
			{Name: "sourceDirectory", Type: "string", Required: true, Description: "Directory containing the files to be zipped."},
			{Name: "recurse", Type: "boolean", Required: false, Description: "Indicates whether the zip creation should include subdirectories and their contents."},
			{Name: "fileFilter", Type: "string", Required: false, Description: "FileFilter specifies the pattern used to filter files included in the zip archive."},
			{Name: "password", Type: "string", Required: false, Description: "Specifies the encryption password for the created zip file."},
		},
	},
	"ctod": {
		Name: "CToD", Description: "Converts a date string to a date object using the system's default date format. Returns a date object.", ReturnType: "date",
		Parameters: []FunctionParameter{
			{Name: "dateString", Type: "string", Required: true, Description: "A string that contains a date in a specific format, which will be converted into an SSLDate object."},
		},
	},
	"dateadd": {
		Name: "DateAdd", Description: "Adds a specified number of units to a date and returns the new date.", ReturnType: "date",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "variant", Required: true, Description: "Initial date or datetime value to which the specified number of units will be added."},
			{Name: "number", Type: "variant", Required: true, Description: "Specifies the amount by which to add or subtract from the date, based on the unit of time specified in the datepart parameter."},
			{Name: "datepart", Type: "variant", Required: false, Description: "Datepart specifies the unit of time (year, month, day, etc.) by which the date should be incremented or decremented."},
		},
	},
	"datediff": {
		Name: "DateDiff", Description: "Computes the difference between two dates in specified units and returns a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "startDate", Type: "variant", Required: true, Description: "Beginning date for calculating the difference between two dates using the DateDiff function."},
			{Name: "endDate", Type: "variant", Required: true, Description: "End date for calculating the difference from the start date in the DateDiff function."},
			{Name: "datepart", Type: "variant", Required: false, Description: "Specifies the unit of time (e.g., day, hour) for which the difference between two dates should be calculated."},
		},
	},
	"datediffex": {
		Name: "DateDiffEx", Description: "Computes the difference between two dates and returns a TimeSpan object.", ReturnType: "sslnetobject",
		Parameters: []FunctionParameter{
			{Name: "startDate", Type: "variant", Required: true, Description: "Beginning date for calculating the difference between two dates using the DateDiffEx function."},
			{Name: "endDate", Type: "variant", Required: true, Description: "End date for calculating the difference with startDate."},
		},
	},
	"dateformat": {
		Name: "DateFormat", Description: "Sets the global date format and returns an empty string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "newFormat", Type: "string", Required: true, Description: "The date format string to set as the global date format. Characters Y, m, D are translated to y, M, d."},
		},
	},
	"datefromnumbers": {
		Name: "DateFromNumbers", Description: "Converts numbers to a date object. Returns a date.", ReturnType: "date",
		Parameters: []FunctionParameter{
			{Name: "year", Type: "variant", Required: false, Description: "Year component of a date, specified as an integer value."},
			{Name: "month", Type: "variant", Required: false, Description: "Month of the year as an integer, where January is 1 and December is 12."},
			{Name: "day", Type: "variant", Required: false, Description: "Day of the month, ranging from 1 to 31."},
			{Name: "hour", Type: "variant", Required: false, Description: "Hour of the day in a 24-hour format, ranging from 0 to 23."},
			{Name: "minute", Type: "variant", Required: false, Description: "Represents the minute component of a date and time."},
			{Name: "second", Type: "variant", Required: false, Description: "Second component of a date and time, ranging from 0 to 59."},
			{Name: "millisecond", Type: "variant", Required: false, Description: "Number of milliseconds within a second, ranging from 0 to 999."},
			{Name: "makeInvariant", Type: "variant", Required: false, Description: "MakeInvariant determines whether the resulting date is local (default) or unspecified."},
		},
	},
	"datefromstring": {
		Name: "DateFromString", Description: "Converts a date string to a Date object using specified format and culture settings. Returns a Date object or null if conversion fails.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "dateAsString", Type: "variant", Required: true, Description: "String representation of the date to be parsed."},
			{Name: "format", Type: "variant", Required: false, Description: "Specifies the date string format(s) used for parsing. Can be a string, array of strings, or null (uses general parsing)."},
			{Name: "useLocalCulture", Type: "variant", Required: false, Description: "Indicates whether to use the local culture settings for parsing the date string."},
			{Name: "makeInvariant", Type: "variant", Required: false, Description: "A boolean parameter that determines whether the resulting DateTime object should be invariant (unspecified kind) or local (specified kind)."},
		},
	},
	"datetostring": {
		Name: "DateToString", Description: "Converts a date to a string using a specified format or default format. Returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "variant", Required: true, Description: "Date and time value that will be converted to a string using the specified format."},
			{Name: "format", Type: "variant", Required: false, Description: "Specifies the string pattern used to convert a date into a readable string."},
		},
	},
	"day": {
		Name: "Day", Description: "Returns the day of the month as a double from a given date.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "date", Required: true, Description: "A date value from which the day component is extracted."},
		},
	},
	"decompress": {
		Name: "Decompress", Description: "Returns a decompressed string from the provided source or file path.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "variant", Required: true, Description: "Is a string containing compressed data that needs to be decompressed."},
			{Name: "fromFile", Type: "variant", Required: false, Description: "A boolean indicating whether the input string is compressed in a file. Defaults to false if null."},
		},
	},
	"decryptdata": {
		Name: "DecryptData", Description: "Decrypts data using a password and returns the decrypted string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "inputData", Type: "string", Required: false, Description: "Encrypted data string to be decrypted."},
			{Name: "password", Type: "string", Required: false, Description: "Is used to decrypt the inputData string."},
		},
	},
	"delarray": {
		Name: "delarray", Description: "Removes an element from an array at a specified index and returns the modified array.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "target", Type: "array", Required: true, Description: "Array from which an element will be deleted."},
			{Name: "index", Type: "double", Required: true, Description: "1-based position of the element within the array that will be deleted. Must be an integer value."},
		},
	},
	"deletedironftp": {
		Name: "DeleteDirOnFtp", Description: "Deletes a directory on an FTP server and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "serverNameOrIP", Type: "string", Required: true, Description: "Represents the address or hostname of the FTP server from which the directory will be deleted."},
			{Name: "remoteDirectory", Type: "string", Required: true, Description: "Specifies the directory on the FTP server that needs to be deleted."},
			{Name: "userName", Type: "string", Required: true, Description: "Username used for authentication when connecting to the FTP server."},
			{Name: "password", Type: "string", Required: true, Description: "Represents the user's password for authenticating with the FTP server."},
			{Name: "port", Type: "double", Required: false, Description: "Specifies the port number on which the FTP server is listening."},
			{Name: "proxy", Type: "string", Required: false, Description: "Proxy server to route FTP requests through. Can be null if no proxy is needed."},
			{Name: "isSFTP", Type: "boolean", Required: false, Description: "IsSFTP determines whether the operation should use SFTP (true) or FTP (false)."},
			{Name: "privateKeyFilePath", Type: "string", Required: false, Description: "Path to the private key file used for SFTP authentication. Only used when isSFTP is true."},
		},
	},
	"deletefromftp": {
		Name: "DeleteFromFtp", Description: "Deletes a file from an FTP or SFTP server and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "serverNameOrIP", Type: "string", Required: true, Description: "Represents the address or hostname of the FTP server from which a file will be deleted."},
			{Name: "remoteDirectory", Type: "string", Required: true, Description: "Directory on the FTP server where the file is located."},
			{Name: "remoteFileName", Type: "string", Required: true, Description: "Name of the file to delete from the FTP server."},
			{Name: "userName", Type: "string", Required: true, Description: "Username used for authentication when deleting a file from an FTP server."},
			{Name: "password", Type: "string", Required: true, Description: "Password for the FTP user."},
			{Name: "port", Type: "double", Required: false, Description: "Specifies the FTP server's port number for the connection."},
			{Name: "proxy", Type: "string", Required: false, Description: "Proxy server address used for the FTP connection. Can be null if no proxy is needed."},
			{Name: "isSFTP", Type: "boolean", Required: false, Description: "Indicates whether the FTP operation should use SFTP instead of standard FTP."},
			{Name: "privateKeyFilePath", Type: "string", Required: false, Description: "Path to the private key file used for SFTP authentication. Only used when isSFTP is true."},
		},
	},
	"deleteinlinecode": {
		Name: "deleteinlinecode", Description: "Deletes inline code and always returns true.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "s", Type: "variant", Required: true, Description: "Inline code to be deleted."},
		},
	},
	"detectsqlinjections": {
		Name: "DetectSqlInjections", Description: "Enables or disables SQL injection detection for a database connection and returns the current state.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "onOff", Type: "variant", Required: true, Description: "Determines whether SQL injection detection is enabled or disabled for the specified database connection."},
			{Name: "connectionName", Type: "variant", Required: false, Description: "Name of the database connection to configure. If not provided or not a string, uses the default connection."},
		},
	},
	"directory": {
		Name: "Directory", Description: "Returns a list of files and directories matching the specified pattern.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "filePattern", Type: "string", Required: true, Description: "Specifies the pattern used to filter files in a directory."},
			{Name: "attributes", Type: "string", Required: false, Description: "Optional string specifying attributes to filter directory entries."},
		},
	},
	"docacquireworkitem": {
		Name: "DocAcquireWorkitem", Description: "Acquires a workitem by ID and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "workitemId", Type: "string", Required: true, Description: "Represents the unique identifier of the document or item to acquire."},
		},
	},
	"docadduserstogroup": {
		Name: "DocAddUsersToGroup", Description: "Adds users to a specified group and returns true if successful.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "groupName", Type: "string", Required: true, Description: "Identifies the group to which users will be added."},
			{Name: "users", Type: "array", Required: true, Description: "An array of user names to be added to a group."},
		},
	},
	"doccancelcheckout": {
		Name: "DocCancelCheckout", Description: "Cancels a document checkout and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "documentId", Type: "string", Required: true, Description: "Unique identifier for the document that needs its checkout canceled."},
		},
	},
	"doccheckindocument": {
		Name: "DocCheckinDocument", Description: "Returns a string indicating whether a document was successfully checked in.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "filePath", Type: "string", Required: true, Description: "Path to the document file being checked in."},
			{Name: "documentId", Type: "string", Required: true, Description: "Unique identifier for the document being checked in."},
			{Name: "version", Type: "string", Required: false, Description: "Represents the version number of the document being checked in."},
			{Name: "replaceContent", Type: "boolean", Required: false, Description: "Indicates whether to replace the existing content of the document with the new content being checked in."},
			{Name: "majorVersion", Type: "boolean", Required: false, Description: "Indicates whether the document should be checked in with a major version increment."},
		},
	},
	"doccheckoutdocument": {
		Name: "DocCheckoutDocument", Description: "Returns a string indicating whether a document checkout was successful or not.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "documentId", Type: "string", Required: true, Description: "Document ID of the document to check out."},
		},
	},
	"doccommandfailed": {
		Name: "DocCommandFailed", Description: "Returns a boolean indicating whether the last Documentum command failed.", ReturnType: "boolean",
		Parameters: []FunctionParameter{},
	},
	"doccompleteworkitem": {
		Name: "DocCompleteWorkitem", Description: "Completes a workitem in Documentum and returns true if successful.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "workitemId", Type: "string", Required: true, Description: "Unique identifier for the work item to be completed."},
			{Name: "signOffUser", Type: "string", Required: false, Description: "Username of the user signing off on the workitem."},
			{Name: "signOffPass", Type: "string", Required: false, Description: "Password used for signing off on a workitem."},
			{Name: "signOffReason", Type: "string", Required: false, Description: "Reason for completing the work item."},
		},
	},
	"doccreateacl": {
		Name: "DocCreateACL", Description: "Creates a new Access Control List (ACL) and returns its name as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "name", Type: "string", Required: true, Description: "Identifier for the access control list (ACL) being created."},
			{Name: "description", Type: "string", Required: false, Description: "Represents a string that provides additional information or context for the Access Control List (ACL) being created."},
			{Name: "groups", Type: "array", Required: false, Description: "An array of strings representing the names of groups to which access control is being granted or modified."},
		},
	},
	"doccreatecabinet": {
		Name: "DocCreateCabinet", Description: "Creates a new document cabinet with the specified name and optional type and ACL, returning the cabinet's ID as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "name", Type: "string", Required: true, Description: "Specifies the name of the cabinet to be created."},
			{Name: "cabinetType", Type: "string", Required: false, Description: "Specifies the type of cabinet to create when using the DocCreateCabinet function."},
			{Name: "acl", Type: "string", Required: false, Description: "Specifies the access control list for the document cabinet, defining permissions and roles."},
		},
	},
	"doccreatefolder": {
		Name: "DocCreateFolder", Description: "Creates a new folder at the specified path with the given name and ACL, returning the folder's ID as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "path", Type: "string", Required: true, Description: "Specifies the directory path where the new folder will be created."},
			{Name: "name", Type: "string", Required: true, Description: "Specifies the name of the folder to be created."},
			{Name: "acl", Type: "string", Required: false, Description: "Specifies the access control list for the new folder, defining permissions for users and groups."},
		},
	},
	"doccreategroup": {
		Name: "DocCreateGroup", Description: "Creates a new document group and returns its name as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "name", Type: "string", Required: true, Description: "Name of the group to be created."},
			{Name: "description", Type: "string", Required: false, Description: "Is a string that provides additional information or details about the group being created."},
		},
	},
	"doccreateuser": {
		Name: "DocCreateUser", Description: "Creates a new user in Documentum and returns the user's login name as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "loginName", Type: "string", Required: true, Description: "Username used for logging into the system."},
			{Name: "password", Type: "string", Required: true, Description: "Represents the user's login password."},
			{Name: "userName", Type: "string", Required: false, Description: "Username for the new user being created."},
			{Name: "eMail", Type: "string", Required: false, Description: "Email address associated with the user being created."},
			{Name: "defaultFolder", Type: "string", Required: false, Description: "Default folder where the user's documents will be stored."},
			{Name: "groupName", Type: "string", Required: false, Description: "Specifies the group to which the user should be added."},
			{Name: "permissionSet", Type: "string", Required: false, Description: "Specifies the set of permissions to assign to the new user."},
			{Name: "userPrivileges", Type: "double", Required: false, Description: "Numeric value of user privileges to be assigned to the newly created user."},
		},
	},
	"docdelegateworkitem": {
		Name: "DocDelegateWorkitem", Description: "Delegates a work item to a specified user and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "workitemId", Type: "string", Required: true, Description: "Workitem identifier for the document to delegate."},
			{Name: "user", Type: "string", Required: true, Description: "Username of the person to whom the workitem is being delegated."},
		},
	},
	"docdelete": {
		Name: "DocDelete", Description: "Deletes a document by object ID, optionally all versions. Returns true on success, false otherwise.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "objId", Type: "string", Required: true, Description: "Unique identifier of the document to be deleted."},
			{Name: "allVersions", Type: "boolean", Required: false, Description: "A boolean indicating whether to delete all versions of the document or just the current version."},
		},
	},
	"docdeletecabinet": {
		Name: "DocDeleteCabinet", Description: "Deletes a document cabinet by ID, optionally deleting all contained documents; returns true on success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "cabinetId", Type: "string", Required: true, Description: "Unique identifier for the cabinet to be deleted."},
			{Name: "deepDelete", Type: "boolean", Required: false, Description: "Determines whether to delete all documents within the cabinet, not just the cabinet itself."},
		},
	},
	"docdeletefolder": {
		Name: "DocDeleteFolder", Description: "Deletes a folder by ID; returns true on success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "folderId", Type: "string", Required: true, Description: "Is a string representing the unique identifier of the folder to be deleted."},
			{Name: "deepDelete", Type: "boolean", Required: false, Description: "DeepDelete determines whether to delete subfolders and documents within the specified folder."},
		},
	},
	"docdeleteuser": {
		Name: "DocDeleteUser", Description: "Deletes a user document and returns true if successful.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "name", Type: "string", Required: true, Description: "Username of the user to be deleted."},
		},
	},
	"docenddocumentuminterface": {
		Name: "DocEndDocumentumInterface", Description: "Returns null after ending Documentum interface connection.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"docexists": {
		Name: "DocExists", Description: "Returns true if a document exists with the specified ID; otherwise, false.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "objId", Type: "string", Required: true, Description: "Unique identifier for the document to check."},
		},
	},
	"docexistsuser": {
		Name: "DocExistsUser", Description: "Checks if a user exists for a given login name and returns a boolean indicating its existence.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "loginName", Type: "string", Required: true, Description: "Username used for authentication in the system."},
			{Name: "userName", Type: "string", Required: true, Description: "Name of the user for whom document existence is being checked."},
		},
	},
	"docexportdocument": {
		Name: "DocExportDocument", Description: "Exports a document to a specified format and returns its content as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "documentId", Type: "string", Required: true, Description: "Represents the unique identifier of the document to be exported."},
			{Name: "format", Type: "string", Required: false, Description: "Specifies the output format for the document, such as 'PDF' or 'DOCX'."},
		},
	},
	"docgetcabinets": {
		Name: "DocGetCabinets", Description: "Returns an array of cabinet names as strings.", ReturnType: "array",
		Parameters: []FunctionParameter{},
	},
	"docgetdocuments": {
		Name: "DocGetDocuments", Description: "Returns an array of documents in a specified folder, optionally filtering by document types.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "folderPath", Type: "string", Required: true, Description: "Path to the folder from which documents will be retrieved."},
			{Name: "docTypes", Type: "string", Required: false, Description: "DocTypes specifies the types of documents to retrieve from the specified folder."},
		},
	},
	"docgeterrormessage": {
		Name: "DocGetErrorMessage", Description: "Returns the last error message as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"docgetfolders": {
		Name: "DocGetFolders", Description: "Returns an array of folder names within a specified parent path.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "parentPath", Type: "string", Required: true, Description: "ParentPath specifies the directory from which to retrieve subfolders."},
		},
	},
	"docgetmetadata": {
		Name: "DocGetMetadata", Description: "Returns metadata attributes for a document as an array.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "objId", Type: "string", Required: true, Description: "Identifier of the document for which metadata is being retrieved."},
			{Name: "attributes", Type: "string", Required: false, Description: "A string containing a comma-separated list of metadata attribute names to retrieve for the specified document."},
		},
	},
	"docgettasks": {
		Name: "DocGetTasks", Description: "Returns an array of tasks for a given workflow ID.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "workflowId", Type: "string", Required: false, Description: "Unique identifier for a workflow in STARLIMS, used to retrieve tasks associated with that workflow."},
		},
	},
	"docgettaskscount": {
		Name: "DocGetTasksCount", Description: "Returns the count of tasks as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{},
	},
	"docgettypeattributes": {
		Name: "DocGetTypeAttributes", Description: "Returns an array of attributes for a specified document type.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "typeName", Type: "string", Required: true, Description: "Name of the document type for which attributes are being requested."},
		},
	},
	"docgettypeattributesasdataset": {
		Name: "DocGetTypeAttributesAsDataset", Description: "Returns a dataset of attributes for a specified document type as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "typeName", Type: "string", Required: true, Description: "Name of the document type for which attributes are being retrieved."},
		},
	},
	"docgetworkflowstatus": {
		Name: "DocGetWorkflowStatus", Description: "Retrieves the status of a workflow by ID and returns it as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "workflowId", Type: "string", Required: true, Description: "Specifies the unique identifier for the workflow whose status is being retrieved."},
		},
	},
	"docgetworkitemproperties": {
		Name: "DocGetWorkitemProperties", Description: "Returns an array of workitem properties for a given workitem ID.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "workitemId", Type: "string", Required: true, Description: "Unique identifier for a workitem in the STARLIMS system."},
		},
	},
	"docimportdocument": {
		Name: "DocImportDocument", Description: "Imports a document from a file to a specified path and returns the document's name as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "docFile", Type: "string", Required: true, Description: "Path to the document file that will be imported into STARLIMS."},
			{Name: "destinationPath", Type: "string", Required: true, Description: "Directory where the document will be imported."},
			{Name: "docName", Type: "string", Required: false, Description: "Name of the document to be imported into STARLIMS."},
			{Name: "docType", Type: "string", Required: false, Description: "DocType specifies the type of document being imported."},
			{Name: "appCode", Type: "string", Required: false, Description: "Specifies the application code associated with the document being imported, ensuring that the document is processed according to the rules and configurations defined for that specific application."},
			{Name: "aclName", Type: "string", Required: false, Description: "Specifies the access control list (ACL) to apply to the imported document, controlling who can view and interact with it within the system."},
		},
	},
	"docinitdocumentuminterface": {
		Name: "DocInitDocumentumInterface", Description: "Initializes the Documentum interface and returns a variant object.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"doclogintodocumentum": {
		Name: "DocLoginToDocumentum", Description: "Attempts to log in to Documentum and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "docBase", Type: "string", Required: true, Description: "Base URL or identifier of the Documentum repository to which the user intends to log in."},
			{Name: "user", Type: "string", Required: true, Description: "Username used for logging into the Documentum system."},
			{Name: "password", Type: "string", Required: true, Description: "Is a string that represents the user's login password for accessing the Documentum system."},
		},
	},
	"docpauseworkflow": {
		Name: "DocPauseWorkflow", Description: "Pauses a workflow by its ID and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "workflowId", Type: "string", Required: true, Description: "Represents the unique identifier for the workflow to be paused."},
		},
	},
	"docremoveallusersfromgroup": {
		Name: "DocRemoveAllUsersFromGroup", Description: "Removes all users from a specified group and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "groupName", Type: "string", Required: true, Description: "O: The group from which all users will be removed."},
		},
	},
	"docremoveusersfromgroup": {
		Name: "DocRemoveUsersFromGroup", Description: "Removes users from a specified group and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "groupName", Type: "string", Required: true, Description: "Name of the group from which users will be removed."},
			{Name: "users", Type: "array", Required: true, Description: "An array of user names to remove from a group."},
		},
	},
	"docrepeatworkitem": {
		Name: "DocRepeatWorkitem", Description: "Repeats a workitem for specified users and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "workitemId", Type: "string", Required: true, Description: "Unique identifier of the workitem to be repeated in STARLIMS."},
			{Name: "users", Type: "array", Required: true, Description: "An array of user identifiers representing the individuals who will repeat a workitem."},
			{Name: "signOffUser", Type: "string", Required: false, Description: "Username of the user who is signing off on the workitem."},
			{Name: "signOffPass", Type: "string", Required: false, Description: "Password used for authentication when signing off a work item in STARLIMS."},
			{Name: "signOffReason", Type: "string", Required: false, Description: "Specifies the reason for repeating a workitem."},
		},
	},
	"docresumeworkflow": {
		Name: "DocResumeWorkflow", Description: "Resumes a workflow by ID and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "workflowId", Type: "string", Required: true, Description: "Workflow ID of the document to resume."},
		},
	},
	"docsearchasdataset": {
		Name: "DocSearchAsDataset", Description: "Converts a document search query into a dataset string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "contains", Type: "string", Required: false, Description: "Contains a string that filters documents based on their content."},
			{Name: "startLocation", Type: "string", Required: false, Description: "Specifies the starting point for the document search within the Documentum repository."},
			{Name: "objectType", Type: "string", Required: false, Description: "Type of document objects to search for."},
			{Name: "where", Type: "string", Required: false, Description: "Specifies the search criteria for documents in the dataset, allowing for filtering based on specific conditions."},
			{Name: "allVersions", Type: "boolean", Required: false, Description: "AllVersions determines whether to include all versions of documents in the search results."},
			{Name: "resultSetSize", Type: "double", Required: false, Description: "Specifies the maximum number of documents to return in the search result dataset."},
		},
	},
	"docsearchfulltext": {
		Name: "DocSearchFullText", Description: "Returns an array of documents matching a full-text search query.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "textToSearch", Type: "string", Required: true, Description: "Text to search for within documents."},
			{Name: "startLocation", Type: "string", Required: false, Description: "StartLocation specifies the starting point for the document search within the documentum system."},
			{Name: "resultSetSize", Type: "double", Required: false, Description: "Specifies the maximum number of search results to return."},
		},
	},
	"docsearchusingdql": {
		Name: "DocSearchUsingDql", Description: "Returns an array of documents based on a DQL query.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "dql", Type: "string", Required: true, Description: "Documentum Query Language (DQL) query used to search for documents in the system."},
			{Name: "resultSetSize", Type: "double", Required: false, Description: "Specifies the maximum number of documents to return in the search results when using the DocSearchUsingDql function."},
		},
	},
	"docsetmetadata": {
		Name: "DocSetMetadata", Description: "Sets document metadata and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "objId", Type: "string", Required: true, Description: "Identifier of the document on which metadata will be set."},
			{Name: "attributes", Type: "array", Required: true, Description: "An array of attribute objects containing metadata key-value pairs to set."},
		},
	},
	"docstartworkflow": {
		Name: "DocStartWorkflow", Description: "Starts a workflow for specified documents and returns workflow ID and performers as an array.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "workflowId", Type: "string", Required: true, Description: "WorkflowId identifies the specific workflow to start."},
			{Name: "documentIds", Type: "array", Required: false, Description: "An array of document IDs representing the documents to start the workflow on."},
			{Name: "packageName", Type: "string", Required: false, Description: "Name of the package associated with the workflow."},
		},
	},
	"docstopworkflow": {
		Name: "DocStopWorkflow", Description: "Stops a workflow by ID and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "workflowId", Type: "string", Required: true, Description: "Unique identifier of the workflow to be stopped."},
		},
	},
	"docupdateuser": {
		Name: "DocUpdateUser", Description: "Updates user details in Documentum and returns a success message or error string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "loginName", Type: "string", Required: true, Description: "Username used for authentication in the system."},
			{Name: "password", Type: "string", Required: true, Description: "Represents the user's new password for authentication."},
			{Name: "userName", Type: "string", Required: false, Description: "Username of the user whose details are being updated."},
			{Name: "eMail", Type: "string", Required: false, Description: "Represents the email address associated with the user being updated."},
			{Name: "defaultFolder", Type: "string", Required: false, Description: "Default folder where the user's documents will be stored."},
			{Name: "groupName", Type: "string", Required: false, Description: "Name of the group to which the user should be added."},
			{Name: "permissionSet", Type: "string", Required: false, Description: "PermissionSet specifies the set of permissions to assign to the user."},
			{Name: "userPrivileges", Type: "double", Required: false, Description: "Represents the set of permissions assigned to a user."},
		},
	},
	"doproc": {
		Name: "DoProc", Description: "Executes a procedure based on the provided name and arguments, returning the result as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "args", Type: "sslvalue[]", Required: true, Description: "An array containing the parameters passed to the DoProc function for processing."},
		},
	},
	"dossupport": {
		Name: "DosSupport", Description: "Returns a variant containing the result of executing a DOS command or retrieving directory information.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "cmd", Type: "string", Required: true, Description: "Command to execute."},
			{Name: "prm", Type: "string", Required: false, Description: "Additional parameters for the command. Required for all commands except WORK, WORKDIR, and CURRENTDRIVE."},
			{Name: "dbg", Type: "variant", Required: false, Description: "A boolean indicating whether debugging information should be displayed."},
		},
	},
	"dow": {
		Name: "DOW", Description: "Computes the day of the week as a number (1 for Sunday to 7 for Saturday). Returns a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "date", Required: true, Description: "Date for which the day of the week is to be determined."},
		},
	},
	"doy": {
		Name: "DOY", Description: "Returns the day of the year for a given date as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "date", Required: true, Description: "A date value for which the day of the year (DOY) is calculated. Throws ArgumentNullException if null."},
		},
	},
	"dtoc": {
		Name: "DToC", Description: "Converts a date to a string using the default format. Returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "date", Required: true, Description: "A date value that will be converted to a string format using the current global settings."},
		},
	},
	"dtos": {
		Name: "DToS", Description: "Converts a date to a string in \"yyyyMMdd\" format. Returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "date", Required: true, Description: "A date value that will be converted to a string in the format \"yyyyMMdd\"."},
		},
	},
	"empty": {
		Name: "Empty", Description: "Returns true if the provided value is empty or null; otherwise, false.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "value", Type: "variant", Required: false, Description: "Value to check if it is empty."},
		},
	},
	"encryptdata": {
		Name: "EncryptData", Description: "Encrypts data using a specified algorithm and returns the encrypted string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "inputData", Type: "string", Required: false, Description: "Data to be encrypted."},
			{Name: "password", Type: "string", Required: false, Description: "Is used to encrypt the input data."},
			{Name: "algorithm", Type: "string", Required: false, Description: "Specifies the encryption algorithm to use for encrypting the data."},
			{Name: "key", Type: "string", Required: false, Description: "Encryption key used to encrypt the data."},
			{Name: "retType", Type: "string", Required: false, Description: "RetType specifies the format of the returned encryption data, such as 'base64' or 'hexadecimal'."},
		},
	},
	"endlimsoleconnect": {
		Name: "endlimsoleconnect", Description: "Closes a connection to the STARLIMS server and returns an empty string.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "v", Type: "variant", Required: true, Description: "SSLNetObject that needs to be disconnected."},
		},
	},
	"endlimstransaction": {
		Name: "EndLimsTransaction", Description: "Ends a LIMS transaction and commits changes if specified; returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "friendlyName", Type: "string", Required: false, Description: "A descriptive name for the transaction, used for logging and auditing purposes."},
			{Name: "commit", Type: "boolean", Required: false, Description: "A boolean indicating whether to commit the transaction."},
		},
	},
	"errormes": {
		Name: "ErrorMes", Description: "Constructs and returns a formatted error message from two input values.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "a", Type: "variant", Required: true, Description: "Error code or identifier."},
			{Name: "b", Type: "variant", Required: true, Description: "Error message or description."},
		},
	},
	"execfunction": {
		Name: "ExecFunction", Description: "Executes a named SSL function with parameters and returns the result as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "name", Type: "string", Required: true, Description: "Name of the function to execute."},
			{Name: "parameters", Type: "object[]", Required: true, Description: "An array of objects representing the parameters to pass to the function."},
		},
	},
	"execinternal": {
		Name: "ExecInternal", Description: "Invokes a method on an object and returns the result as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "o", Type: "variant", Required: true, Description: "Object on which the method will be invoked."},
			{Name: "methodName", Type: "string", Required: true, Description: "Name of the method to invoke."},
			{Name: "arg01", Type: "variant", Required: false, Description: "First argument to pass to the method."},
			{Name: "arg02", Type: "variant", Required: false, Description: "Second argument to pass to the method."},
			{Name: "Arg03", Type: "variant", Required: false, Description: "Third argument to pass to the method."},
			{Name: "arg04", Type: "variant", Required: false, Description: "Fourth argument to pass to the method."},
			{Name: "arg05", Type: "variant", Required: false, Description: "Fifth argument to pass to the method."},
			{Name: "arg06", Type: "variant", Required: false, Description: "Sixth argument to pass to the method."},
			{Name: "arg07", Type: "variant", Required: false, Description: "Seventh argument to pass to the method."},
			{Name: "arg08", Type: "variant", Required: false, Description: "Eighth argument to pass to the method."},
			{Name: "arg09", Type: "variant", Required: false, Description: "Ninth argument to pass to the method."},
			{Name: "arg10", Type: "variant", Required: false, Description: "Tenth argument to pass to the method."},
			{Name: "arg11", Type: "variant", Required: false, Description: "Eleventh argument to pass to the method."},
			{Name: "arg12", Type: "variant", Required: false, Description: "Twelfth argument to pass to the method."},
			{Name: "arg13", Type: "variant", Required: false, Description: "Thirteenth argument to pass to the method."},
			{Name: "arg14", Type: "variant", Required: false, Description: "Fourteenth argument to pass to the method."},
			{Name: "arg15", Type: "variant", Required: false, Description: "Fifteenth argument to pass to the method."},
			{Name: "arg16", Type: "variant", Required: false, Description: "Sixteenth argument to pass to the method."},
			{Name: "arg17", Type: "variant", Required: false, Description: "Seventeenth argument to pass to the method."},
			{Name: "arg18", Type: "variant", Required: false, Description: "Eighteenth argument to pass to the method."},
			{Name: "arg19", Type: "variant", Required: false, Description: "Nineteenth argument to pass to the method."},
			{Name: "arg20", Type: "variant", Required: false, Description: "Twentieth argument to pass to the method."},
			{Name: "arg21", Type: "variant", Required: false, Description: "Twenty-first argument to pass to the method."},
		},
	},
	"execudf": {
		Name: "ExecUdf", Description: "Executes a user-defined function and returns its result as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "code", Type: "string", Required: false, Description: "Script or code snippet to be executed dynamically."},
			{Name: "args", Type: "sslvalue[]", Required: false, Description: "Array of arguments passed to the user-defined function."},
			{Name: "cacheCode", Type: "boolean", Required: true, Description: "CacheCode indicates whether to cache the generated code for performance improvement."},
		},
	},
	"executedatasource": {
		Name: "ExecuteDataSource", Description: "Executes a data source by name and returns its result as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "dsName", Type: "string", Required: true, Description: "Data source name to execute."},
			{Name: "parameters", Type: "object[]", Required: true, Description: "An array of objects representing the parameters to pass to the data source."},
		},
	},
	"extractcol": {
		Name: "extractcol", Description: "Extracts a specified column from a 2D array and returns it as a new array.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "target", Type: "array", Required: true, Description: "Two-dimensional array from which a specific column of data will be extracted."},
			{Name: "column", Type: "double", Required: true, Description: "1-based index of the column to extract from the 2D array. Must be an integer value >= 1."},
		},
	},
	"extractzip": {
		Name: "ExtractZip", Description: "Extracts files from a ZIP archive to a specified directory and returns null.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "zipFileName", Type: "string", Required: true, Description: "Name of the zip file to extract."},
			{Name: "targetDirectory", Type: "string", Required: true, Description: "Target directory where the files extracted from the zip archive will be saved."},
			{Name: "fileFilter", Type: "string", Required: false, Description: "FileFilter specifies the pattern used to filter files when extracting a zip archive."},
			{Name: "password", Type: "string", Required: false, Description: "Is used to provide a secure extraction key for encrypted zip files."},
		},
	},
	"filesupport": {
		Name: "FileSupport", Description: "Returns a variant containing file support operations based on the request and arguments provided.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "fileIdentifier", Type: "variant", Required: true, Description: "Identifier for the file to perform operations on."},
			{Name: "request", Type: "string", Required: true, Description: "Specifies the operation to perform. Valid values include CHECK, SETATTR, GETATTR, CREATE, OPEN, CLOSE, READ, READBLK, WRITE, BOF, EOF, TELL, SEEK, RESIZE, COPY, DELETE, MOVE, RENAME, PATH, FOLDERNAME, FILENAME, NAME, EXT, SIZE, DATE, TIME, DIR."},
			{Name: "arg1", Type: "variant", Required: false, Description: "Operation or action to perform on the file, such as read, write, delete, etc."},
			{Name: "arg2", Type: "variant", Required: false, Description: "Additional parameters or options for the file operation."},
			{Name: "encoding", Type: "string", Required: false, Description: "Specifies the character encoding used for reading or writing files. Defaults to UTF8."},
		},
	},
	"formaterrormessage": {
		Name: "FormatErrorMessage", Description: "Converts a variant to its full error description string.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "v", Type: "variant", Required: true, Description: "Error object that needs formatting."},
		},
	},
	"formatsqlerrormessage": {
		Name: "FormatSqlErrorMessage", Description: "Formats a SQL error message into a human-readable string and returns it as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "v", Type: "variant", Required: true, Description: "Error object that needs formatting into a SQL error message."},
		},
	},
	"fromjson": {
		Name: "FromJson", Description: "Converts a JSON string to an SSLValue object.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "value", Type: "variant", Required: true, Description: "JSON string that will be converted into an SSLValue object."},
		},
	},
	"fromxml": {
		Name: "FromXml", Description: "Converts XML string to SSLValue object. Returns variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "xml", Type: "string", Required: true, Description: "A string containing XML data that will be parsed and converted into an SSLValue object."},
		},
	},
	"getallclientscripts": {
		Name: "GetAllClientScripts", Description: "Returns a string containing all client scripts for a given encoded XFD.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "encodedXFD", Type: "string", Required: true, Description: "Encoded XFD string that identifies the client scripts to retrieve."},
		},
	},
	"getappbasefolder": {
		Name: "GetAppBaseFolder", Description: "Returns the base folder path of the application as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"getappworkpathfolder": {
		Name: "GetAppWorkPathFolder", Description: "Returns the application's working path folder as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"getbyname": {
		Name: "GetByName", Description: "Returns a variant by name from the local context.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "name", Type: "string", Required: true, Description: "Variable name whose value is being retrieved."},
		},
	},
	"getclientscriptreferences": {
		Name: "GetClientScriptReferences", Description: "Computes and returns a list of client script references from the provided code.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "csCode", Type: "variant", Required: true, Description: "A string containing client script code from which references will be extracted and returned as an array of client script IDs."},
		},
	},
	"getconnectionbyname": {
		Name: "GetConnectionByName", Description: "Returns an SQLConnection object for a database connection identified by its friendly name.", ReturnType: "sqlconnection",
		Parameters: []FunctionParameter{
			{Name: "friendlyName", Type: "string", Required: false, Description: "FriendlyName identifies the connection by a user-friendly name."},
		},
	},
	"getconnectionstrings": {
		Name: "GetConnectionStrings", Description: "Returns an array of database connection strings.", ReturnType: "array",
		Parameters: []FunctionParameter{},
	},
	"getdataset": {
		Name: "GetDataSet", Description: "Returns a string containing dataset data based on a SQL command and optional parameters.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "commandString", Type: "string", Required: true, Description: "SQL query or command string used to retrieve data from the database."},
			{Name: "arrayOfValues", Type: "array", Required: false, Description: "An array of values to be used in the SQL query."},
			{Name: "includeSchema", Type: "boolean", Required: false, Description: "Indicates whether the schema information should be included in the data set returned by the GetDataSet function."},
			{Name: "tableName", Type: "string", Required: false, Description: "Specifies the name of the table from which to retrieve data."},
			{Name: "nullAsBlank", Type: "boolean", Required: false, Description: "NullAsBlank determines whether null values in the dataset should be represented as blank strings."},
			{Name: "invariantDateColumns", Type: "array", Required: false, Description: "An array of column names that should not be affected by date format changes in the dataset."},
		},
	},
	"getdatasetex": {
		Name: "GetDataSetEx", Description: "Returns a string containing XML data from a database query based on provided parameters.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "commandString", Type: "string", Required: true, Description: "CommandString contains the SQL query or command used to retrieve data from the database."},
			{Name: "friendlyName", Type: "string", Required: false, Description: "FriendlyName identifies the dataset by a user-friendly name."},
			{Name: "arrayOfValues", Type: "array", Required: false, Description: "An array of values that specifies the data to retrieve."},
			{Name: "includeSchema", Type: "boolean", Required: false, Description: "Indicates whether the schema information should be included in the dataset."},
			{Name: "includeHeader", Type: "boolean", Required: false, Description: "IncludeHeader determines whether the header row should be included in the dataset."},
			{Name: "tableName", Type: "string", Required: false, Description: "Name of the database table from which data is to be retrieved."},
			{Name: "nullAsBlank", Type: "boolean", Required: false, Description: "NullAsBlank determines whether null values in the dataset should be represented as blank strings."},
			{Name: "invariantDateColumns", Type: "array", Required: false, Description: "Specifies an array of column names that should remain in their original date format when retrieving data, rather than being converted to a different format."},
		},
	},
	"getdatasetfromarray": {
		Name: "GetDataSetFromArray", Description: "Converts an array of values and fields into a dataset string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "arrayOfValues", Type: "array", Required: true, Description: "Array containing the values to be included in the dataset."},
			{Name: "arrayFields", Type: "array", Required: false, Description: "Names of the fields to include in the resulting dataset."},
		},
	},
	"getdatasetfromarrayex": {
		Name: "GetDataSetFromArrayEx", Description: "Converts array data to a dataset string, optionally including header and schema.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "arrayOfValues", Type: "array", Required: true, Description: "An array containing the data values to be included in the dataset."},
			{Name: "arrayFields", Type: "array", Required: false, Description: "An array containing the names of the fields to include in the dataset."},
			{Name: "tableName", Type: "string", Required: false, Description: "Name of the table to store the data set."},
			{Name: "includeHeader", Type: "boolean", Required: false, Description: "Determines whether to include a header row in the resulting dataset."},
			{Name: "includeSchema", Type: "boolean", Required: false, Description: "IncludeSchema determines whether the schema information (column names) should be included in the returned dataset."},
		},
	},
	"getdatasetwithschemafromselect": {
		Name: "GetDataSetWithSchemaFromSelect", Description: "Computes a dataset with schema from a select command and returns it as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "commandString", Type: "string", Required: true, Description: "SQL query used to retrieve data from the database."},
			{Name: "friendlyName", Type: "string", Required: false, Description: "\"friendlyName\": The name used to identify or reference a specific dataset within the system."},
			{Name: "arrayOfValues", Type: "array", Required: false, Description: "An array containing values to be used in the SQL query."},
			{Name: "arrayOfPrimaryKeys", Type: "array", Required: true, Description: "Primary keys of the data to be retrieved in the dataset."},
			{Name: "arrayOfUniqueConstraints", Type: "array", Required: true, Description: "An array containing unique constraints for the dataset."},
		},
	},
	"getdatasetxmlfromarray": {
		Name: "GetDataSetXMLFromArray", Description: "Converts an array of values into XML format, optionally including headers and schema. Returns a string containing the XML data.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "arrayOfValues", Type: "array", Required: true, Description: "Data to be converted into XML format."},
			{Name: "arrayFields", Type: "array", Required: false, Description: "An array of field names to include in the resulting XML dataset."},
			{Name: "tableName", Type: "string", Required: false, Description: "Name of the table for which the dataset XML is being generated."},
			{Name: "includeHeader", Type: "boolean", Required: false, Description: "Determines whether the XML output should include a header row."},
			{Name: "includeSchema", Type: "boolean", Required: false, Description: "Indicates whether to include the schema information in the XML output generated from an array of values."},
		},
	},
	"getdatasetxmlfromselect": {
		Name: "GetDataSetXMLFromSelect", Description: "Converts a database query result to XML format and returns it as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "commandString", Type: "string", Required: true, Description: "SQL query used to retrieve data from the database."},
			{Name: "friendlyName", Type: "string", Required: false, Description: "FriendlyName\": \"A human-readable identifier for the dataset being retrieved."},
			{Name: "includeHeader", Type: "boolean", Required: false, Description: "Indicates whether the XML output should include a header row."},
			{Name: "arrayOfValues", Type: "array", Required: false, Description: "An array containing values to filter or select data in a dataset."},
			{Name: "includeSchema", Type: "boolean", Required: false, Description: "Determines whether to include schema information in the returned XML dataset."},
			{Name: "tableName", Type: "string", Required: false, Description: "Specifies the name of the database table from which to retrieve data for the dataset XML."},
			{Name: "nullAsBlank", Type: "boolean", Required: false, Description: "When nullAsBlank is set to true, null values in the dataset are replaced with blank strings when generating XML output."},
			{Name: "invariantDateColumns", Type: "array", Required: false, Description: "Array of column indices that should remain invariant during data transformations or updates."},
		},
	},
	"getdbmsname": {
		Name: "GetDBMSName", Description: "Returns the name of the database management system as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "friendlyName", Type: "string", Required: false, Description: "\"friendlyName\": The name of the database management system (DBMS) to retrieve."},
		},
	},
	"getdbmsprovidername": {
		Name: "GetDBMSProviderName", Description: "Returns the name of the database management system provider as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "friendlyName", Type: "string", Required: false, Description: "User-friendly name of a database management system provider."},
		},
	},
	"getdecimalsep": {
		Name: "GetDecimalSep", Description: "Returns the decimal separator as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{},
	},
	"getdecimalseparator": {
		Name: "GetDecimalSeparator", Description: "Returns the decimal separator used in numeric formats as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"getdefaultconnection": {
		Name: "GetDefaultConnection", Description: "Returns the default database connection string as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"getdirfromftp": {
		Name: "GetDirFromFtp", Description: "Retrieves files from an FTP server matching a pattern and returns their details as an array.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "serverNameOrIP", Type: "string", Required: true, Description: "Hostname or IP address of the FTP server from which to retrieve directory contents."},
			{Name: "remoteDirectory", Type: "string", Required: true, Description: "RemoteDirectory specifies the directory on the FTP server from which files will be retrieved."},
			{Name: "filePattern", Type: "string", Required: true, Description: "Specifies the pattern to match files in the remote directory during an FTP operation."},
			{Name: "userName", Type: "string", Required: true, Description: "Username used for authentication when connecting to an FTP server."},
			{Name: "password", Type: "string", Required: true, Description: "Is used to authenticate the user when connecting to the FTP server."},
			{Name: "port", Type: "double", Required: false, Description: "Specifies the port number for connecting to the FTP server."},
			{Name: "proxy", Type: "string", Required: false, Description: "Specifies the proxy server to use for the FTP connection."},
			{Name: "usePassive", Type: "boolean", Required: false, Description: "Enables or disables passive mode for FTP operations."},
			{Name: "isSFTP", Type: "boolean", Required: false, Description: "Determines whether to use SFTP (true) or FTP (false)."},
			{Name: "privateKeyFilePath", Type: "string", Required: false, Description: "Path to the private key file used for SFTP authentication."},
		},
	},
	"getdsparameters": {
		Name: "GetDSParameters", Description: "Retrieves and returns an array of parameters for a specified data source.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "dsName", Type: "string", Required: true, Description: "Identifier for the dataset whose parameters are to be retrieved."},
		},
	},
	"getexecutiontrace": {
		Name: "GetExecutionTrace", Description: "Returns a string containing the execution trace of the current thread's call stack.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"getfeaturesandnumbers": {
		Name: "GetFeaturesAndNumbers", Description: "Returns an array of feature IDs and their corresponding license counts.", ReturnType: "array",
		Parameters: []FunctionParameter{},
	},
	"getfileversion": {
		Name: "GetFileVersion", Description: "Returns the file version as a string for the specified file name.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "fileName", Type: "string", Required: true, Description: "Path to the file for which the version information is to be retrieved."},
		},
	},
	"getforbiddenappids": {
		Name: "GetForbiddenAppIDs", Description: "Returns a string containing IDs of forbidden applications.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"getforbiddendesignerappids": {
		Name: "GetForbiddenDesignerAppIDs", Description: "Returns a string of forbidden designer application IDs.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"getformreferences": {
		Name: "GetFormReferences", Description: "Returns a list of client script references from an XFD document.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "formId", Type: "variant", Required: true, Description: "FormId identifies the form for which references are being retrieved. Must be a non-empty string."},
			{Name: "xfdDocument", Type: "variant", Required: true, Description: "Represents the XML-formatted document containing form references. Must be a non-empty string."},
		},
	},
	"getfromapplication": {
		Name: "GetFromApplication", Description: "Retrieves connected users from the application and returns them as a string.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "key", Type: "string", Required: false, Description: "Identifier used to retrieve data from the application's session, specifically targeting \"STARLIMSUSERS\" to get connected users."},
		},
	},
	"getfromftp": {
		Name: "GetFromFtp", Description: "Downloads a file from an FTP server and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "serverNameOrIP", Type: "string", Required: true, Description: "Hostname or IP address of the FTP server from which to retrieve a file."},
			{Name: "remoteDirectory", Type: "string", Required: true, Description: "RemoteDirectory specifies the directory on the FTP server from which the file will be retrieved."},
			{Name: "remoteFileName", Type: "string", Required: true, Description: "Specifies the name of the file to retrieve from the FTP server."},
			{Name: "localFileName", Type: "string", Required: true, Description: "Local file path where the downloaded file will be saved."},
			{Name: "userName", Type: "string", Required: true, Description: "Username used to authenticate with the FTP server."},
			{Name: "password", Type: "string", Required: true, Description: "Represents the user's authentication credentials for accessing the FTP server."},
			{Name: "port", Type: "double", Required: false, Description: "Specifies the port number on which the FTP server is listening."},
			{Name: "proxy", Type: "string", Required: false, Description: "Specifies the proxy server to use for the FTP connection, allowing the script to route requests through an intermediary server."},
			{Name: "isSFTP", Type: "boolean", Required: false, Description: "Determines whether to use SFTP (True) or FTP (False)."},
			{Name: "privateKeyFilePath", Type: "string", Required: false, Description: "Path to the private key file used for SFTP authentication."},
		},
	},
	"getfromsession": {
		Name: "GetFromSession", Description: "Retrieves a value from the current session using a key and returns it as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "key", Type: "string", Required: true, Description: "Key used to retrieve a value from the session."},
		},
	},
	"getgroupseparator": {
		Name: "GetGroupSeparator", Description: "Returns the group separator used in numeric formats as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"getinlinecode": {
		Name: "getinlinecode", Description: "Returns a string containing inline code based on provided SSLValue and variables.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "s", Type: "variant", Required: true, Description: "Inline code or script that will be executed."},
			{Name: "variables", Type: "array", Required: true, Description: "An array containing variables that can be referenced within the inline code."},
		},
	},
	"getinstallationkey": {
		Name: "GetInstallationKey", Description: "Returns the installation key as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"getinternal": {
		Name: "GetInternal", Description: "Retrieves a property value from an object and returns it as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "o", Type: "variant", Required: true, Description: "Object from which to get the internal value."},
			{Name: "propName", Type: "string", Required: true, Description: "Name of the property to retrieve from the object."},
		},
	},
	"getinternalc": {
		Name: "GetInternalC", Description: "Returns a nested index from an object based on provided arguments.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "o", Type: "variant", Required: true, Description: "Object from which to get the internal value."},
			{Name: "collectionName", Type: "string", Required: true, Description: "Name of the collection within an object on which the method will be invoked."},
			{Name: "arg1", Type: "variant", Required: true, Description: "Key or index used to access a specific element within an object or collection."},
			{Name: "arg2", Type: "variant", Required: false, Description: "Arg2: The second index value used to access nested elements within an object or collection."},
			{Name: "arg3", Type: "variant", Required: false, Description: "Third index used to access nested elements within the object."},
			{Name: "arg4", Type: "variant", Required: false, Description: "Fourth index value used to access a nested element within an object structure."},
			{Name: "arg5", Type: "variant", Required: false, Description: "Fifth index or key used to access nested elements within an object structure."},
			{Name: "arg6", Type: "variant", Required: false, Description: "Sixth index value used to access a nested property or element within an object structure."},
		},
	},
	"getlastsqlerror": {
		Name: "GetLastSQLError", Description: "Returns the last SQL error as an SSLSQLError object or null if no error occurred.", ReturnType: "sslsqlerror",
		Parameters: []FunctionParameter{},
	},
	"getlastsslerror": {
		Name: "GetLastSSLError", Description: "Returns the last SSL error encountered as an sslerror object.", ReturnType: "sslerror",
		Parameters: []FunctionParameter{},
	},
	"getlicenseinfoastext": {
		Name: "GetLicenseInfoAsText", Description: "Returns license information as a text string in HTML format if specified.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "bHtml", Type: "boolean", Required: true, Description: "BHtml determines whether the license information is returned in HTML format or plain text."},
		},
	},
	"getlogsfolder": {
		Name: "GetLogsFolder", Description: "Returns the path to the logs folder as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"getnetdataset": {
		Name: "GetNETDataSet", Description: "Returns a dataset based on a command string and parameters, optionally in XML format.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "commandString", Type: "variant", Required: true, Description: "SQL command or query string to execute."},
			{Name: "friendlyName", Type: "variant", Required: true, Description: "FriendlyName specifies the friendly name or identifier for the dataset to retrieve."},
			{Name: "arrayOfValues", Type: "variant", Required: true, Description: "An array of values that will be used in the database query."},
			{Name: "tableName", Type: "variant", Required: true, Description: "TableName specifies the name of the database table from which data is retrieved."},
			{Name: "returnXml", Type: "variant", Required: true, Description: "Whether to return the dataset as an XML string or a native object."},
			{Name: "r1Compatible", Type: "variant", Required: true, Description: "R1Compatible determines whether the returned dataset is compatible with version 1."},
		},
	},
	"getnolock": {
		Name: "GetNoLock", Description: "Returns a variant representing the result of executing a no-lock query on the specified database connection.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "connectionName", Type: "variant", Required: true, Description: "Specifies the database connection to use for retrieving data without locking."},
		},
	},
	"getnumberofinstrumentconnections": {
		Name: "GetNumberOfInstrumentConnections", Description: "Returns the number of instrument connections as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{},
	},
	"getnumberofnamedconcurrentusers": {
		Name: "GetNumberOfNamedConcurrentUsers", Description: "Returns the number of named concurrent users as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{},
	},
	"getnumberofnamedusers": {
		Name: "GetNumberOfNamedUsers", Description: "Returns the number of named users as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{},
	},
	"getprinters": {
		Name: "GetPrinters", Description: "Returns a list of available printers as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"getrdbmsdelimiter": {
		Name: "GetRdbmsDelimiter", Description: "Returns the delimiter string for a specified RDBMS connection.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "dsn", Type: "string", Required: false, Description: "Data source name (DSN) identifying the database."},
			{Name: "open", Type: "boolean", Required: true, Description: "Indicates whether the database connection should be opened before retrieving the delimiter."},
		},
	},
	"getregion": {
		Name: "getregion", Description: "Returns the region between two specified values as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "s", Type: "variant", Required: true, Description: "Source object from which the region will be retrieved."},
			{Name: "src", Type: "variant", Required: true, Description: "Source data or object from which the region information is to be extracted."},
			{Name: "dst", Type: "variant", Required: true, Description: "Destination object where the result of the getregion operation will be stored."},
		},
	},
	"getregionex": {
		Name: "getregionex", Description: "Extracts a region from a source string between specified delimiters using optional local regions support and returns it as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "s", Type: "variant", Required: true, Description: "Source string in which the region will be searched."},
			{Name: "src", Type: "variant", Required: true, Description: "Starting delimiter/marker used to identify the beginning of the region to extract."},
			{Name: "dst", Type: "variant", Required: true, Description: "Ending delimiter/marker used to identify the end of the region to extract."},
			{Name: "localRegions", Type: "variant", Required: true, Description: "Configuration for handling local regions in the extraction process."},
		},
	},
	"getsetting": {
		Name: "GetSetting", Description: "Returns a setting value as a variant based on its name.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "name", Type: "string", Required: true, Description: "Name of the setting to retrieve."},
		},
	},
	"getsettings": {
		Name: "GetSettings", Description: "Returns an array of system settings based on provided names.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "names", Type: "array", Required: true, Description: "An array of setting names to retrieve."},
		},
	},
	"getssldataset": {
		Name: "GetSSLDataset", Description: "Returns a dataset based on an SQL query and optional parameters.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "sql", Type: "string", Required: true, Description: "SQL query to execute and retrieve data from the database."},
			{Name: "dsn", Type: "string", Required: false, Description: "Specifies the data source name, which identifies the database from which to retrieve data."},
			{Name: "paramNames", Type: "array", Required: false, Description: "An array of parameter names used in the SQL query."},
			{Name: "paramValues", Type: "array", Required: false, Description: "ParamValues array contains the values to be substituted for the parameters specified in paramNames when executing a SQL query."},
			{Name: "tableName", Type: "string", Required: false, Description: "Name of the database table from which to retrieve data."},
			{Name: "nullAsBlank", Type: "boolean", Required: false, Description: "When nullAsBlank is set to true, null values in the dataset are converted to blank strings; otherwise, they remain as null."},
			{Name: "invariantDateColumns", Type: "array", Required: false, Description: "Is an array of strings that specifies which date columns should remain invariant during data processing, ensuring their values do not change."},
		},
	},
	"gettables": {
		Name: "GetTables", Description: "Returns an array of table names from the database based on a provided SQL query or all tables if no query is given.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "sql", Type: "string", Required: false, Description: "SQL query used to filter the tables to retrieve."},
		},
	},
	"gettransactionscount": {
		Name: "GetTransactionsCount", Description: "Counts transactions and returns a double.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "connection", Type: "variant", Required: false, Description: "Is a string representing the database connection details."},
		},
	},
	"getuserdata": {
		Name: "GetUserData", Description: "Returns user data as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"getwebfolder": {
		Name: "GetWebFolder", Description: "Returns the current web folder path as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"hashdata": {
		Name: "HashData", Description: "Computes a hash of input data using a specified algorithm and returns it as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "inputData", Type: "string", Required: false, Description: "Data string to be hashed."},
			{Name: "algorithm", Type: "string", Required: false, Description: "Specifies the hashing algorithm to use for the input data."},
		},
	},
	"hasproperty": {
		Name: "HasProperty", Description: "Returns a boolean indicating whether the specified object has a property with the given name.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "o", Type: "variant", Required: true, Description: "Object that may have properties, on which the HasProperty function will check for the existence of a specified property."},
			{Name: "propName", Type: "string", Required: true, Description: "Name of the property to check for on the object."},
		},
	},
	"hour": {
		Name: "Hour", Description: "Returns the hour component of a date as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "date", Required: true, Description: "Date from which the hour component will be extracted."},
		},
	},
	"htmldecode": {
		Name: "HtmlDecode", Description: "Converts HTML-encoded text back to plain text. Returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "data", Type: "string", Required: false, Description: "Is a string containing HTML-encoded text that needs to be decoded."},
		},
	},
	"htmlencode": {
		Name: "HtmlEncode", Description: "Converts a string to its HTML-encoded equivalent and returns it as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "data", Type: "string", Required: false, Description: "String that will be encoded for HTML output."},
		},
	},
	"ignoresqlerrors": {
		Name: "IgnoreSqlErrors", Description: "Sets whether SQL errors are ignored and returns the previous setting as a boolean.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "flag", Type: "boolean", Required: true, Description: "Indicates whether SQL errors should be ignored."},
		},
	},
	"iif": {
		Name: "IIf", Description: "Returns a value based on whether a condition is true or false.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "condition", Type: "boolean", Required: true, Description: "A boolean value that determines whether to return the trueValue or falseValue."},
			{Name: "trueValue", Type: "variant", Required: false, Description: "To return if the condition is true."},
			{Name: "falseValue", Type: "variant", Required: false, Description: "To return when the condition is false."},
		},
	},
	"in64bitmode": {
		Name: "In64BitMode", Description: "Determines if the system is running in 64-bit mode and returns a boolean value.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"inbatchprocess": {
		Name: "InBatchProcess", Description: "Determines if the current process is running in a batch mode and returns a boolean value.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"infomes": {
		Name: "InfoMes", Description: "Constructs and returns a user message from two input values.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "a", Type: "variant", Required: true, Description: "First argument passed to the InfoMes function, representing a message or object to be displayed."},
			{Name: "b", Type: "variant", Required: true, Description: "Message to display."},
		},
	},
	"integer": {
		Name: "Integer", Description: "Converts a decimal number to its integer part and returns it as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "decimalValue", Type: "double", Required: true, Description: "Double precision floating-point number from which an integer value is derived by truncating the decimal part."},
		},
	},
	"isdbconnected": {
		Name: "IsDBConnected", Description: "Checks if a database connection is established and returns a boolean indicating the connection status.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "friendlyName", Type: "variant", Required: false, Description: "\"friendlyName\": The name of the database connection to check for connectivity."},
		},
	},
	"isdefined": {
		Name: "IsDefined", Description: "Determines if a variable is defined and returns a boolean.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "varName", Type: "string", Required: true, Description: "String containing the name of the variable to check for existence."},
		},
	},
	"isdemolicense": {
		Name: "IsDemoLicense", Description: "Determines if the current license is a demo version and returns `true` if it is.", ReturnType: "boolean",
		Parameters: []FunctionParameter{},
	},
	"isfeatureauthorized": {
		Name: "IsFeatureAuthorized", Description: "Validates if a feature is authorized based on application GUID and returns a boolean.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "appGuid", Type: "string", Required: true, Description: "A string containing application GUIDs used to check feature authorization."},
		},
	},
	"isfeaturebasedlicense": {
		Name: "IsFeatureBasedLicense", Description: "Determines if the license is feature-based and returns a boolean value.", ReturnType: "boolean",
		Parameters: []FunctionParameter{},
	},
	"isguid": {
		Name: "IsGuid", Description: "Validates whether a string is a valid GUID and returns a boolean result.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "guid", Type: "string", Required: false, Description: "String representation of a globally unique identifier (GUID) to validate."},
		},
	},
	"ishex": {
		Name: "IsHex", Description: "Determines if a string is a valid hexadecimal number and returns a boolean.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String to check if it is a hexadecimal value."},
		},
	},
	"isintransaction": {
		Name: "IsInTransaction", Description: "Determines whether a database connection is currently in a transaction and returns a boolean value.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "connection", Type: "variant", Required: true, Description: "Represents the database connection object used to check if a transaction is currently in progress."},
		},
	},
	"isinvariantdate": {
		Name: "IsInvariantDate", Description: "Determines if a date is invariant and returns a boolean.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "dateValue", Type: "variant", Required: true, Description: "Date to check for invariance."},
		},
	},
	"isnumeric": {
		Name: "IsNumeric", Description: "Determines if a value is numeric, optionally allowing hexadecimal format; returns boolean.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "sNumber", Type: "string", Required: true, Description: "String to be checked for numeric validity."},
			{Name: "allowHex", Type: "boolean", Required: false, Description: "When true, hexadecimal values are considered numeric. Defaults to false."},
		},
	},
	"isproductionmodeon": {
		Name: "IsProductionModeOn", Description: "Determines if the system is in production mode and returns a boolean value.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"istable": {
		Name: "IsTable", Description: "Determines if a table exists in the database and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "friendlyName", Type: "string", Required: false, Description: "\"friendlyName\": The name used to identify or reference a table in the database."},
			{Name: "tableName", Type: "string", Required: true, Description: "Name of the table to check for existence."},
		},
	},
	"istablefld": {
		Name: "IsTableFld", Description: "Determines if a field exists in a table and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "friendlyName", Type: "string", Required: false, Description: "\"friendlyName\": The human-readable name of the field within a table."},
			{Name: "tableName", Type: "string", Required: false, Description: "Table_name\": \"The name of the table in which to check for the field."},
			{Name: "fieldName", Type: "string", Required: true, Description: "Name of the field within a table that needs to be checked for existence."},
		},
	},
	"jday": {
		Name: "JDay", Description: "Computes and returns the Julian day number for a given date as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "variant", Required: false, Description: "A date value used to calculate the Julian day, which is the continuous count of days since January 1, 4713 BC."},
		},
	},
	"lcase": {
		Name: "LCase", Description: "Evaluates and executes SSL expressions conditionally based on a boolean condition.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "condition", Type: "boolean", Required: true, Description: "Indicates whether to return the value of trueValue or falseValue."},
			{Name: "trueValue", Type: "string", Required: true, Description: "SSL expression string to execute if the condition is true."},
			{Name: "falseValue", Type: "string", Required: false, Description: "SSL expression string to execute if the condition is false. Returns empty string if null or empty."},
		},
	},
	"ldapauth": {
		Name: "LDAPAuth", Description: "Authenticates a user against an LDAP server and returns the authentication result as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "ldapHost", Type: "string", Required: true, Description: "Specifies the hostname of the LDAP server to authenticate against."},
			{Name: "ldapPort", Type: "double", Required: false, Description: "Specifies the port number on which the LDAP server is listening for connections. Defaults to 389 if not provided."},
			{Name: "ldapUserName", Type: "string", Required: true, Description: "Username used for authentication in an LDAP (Lightweight Directory Access Protocol) directory service."},
			{Name: "ldapPassword", Type: "string", Required: false, Description: "Password used to authenticate with an LDAP server during authentication."},
			{Name: "ldapDistinctiveName", Type: "string", Required: false, Description: "Unique identifier for a user in an LDAP directory, used to authenticate the user during the LDAP authentication process."},
			{Name: "secure", Type: "boolean", Required: false, Description: "Indicates whether the LDAP connection should use SSL/TLS encryption."},
		},
	},
	"ldapauthex": {
		Name: "LDAPAuthEX", Description: "Computes LDAP authentication and returns a string result.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "ldapHost", Type: "string", Required: true, Description: "Specifies the hostname or IP address of the LDAP server to authenticate against."},
			{Name: "ldapPort", Type: "double", Required: true, Description: "Specifies the port number on which to connect to the LDAP server for authentication."},
			{Name: "bindUserName", Type: "string", Required: true, Description: "Specifies the username used to bind to the LDAP server for authentication purposes."},
			{Name: "bindUserPassword", Type: "string", Required: false, Description: "Password used to authenticate the user specified by bindUserName when connecting to an LDAP server."},
			{Name: "searchUserName", Type: "string", Required: false, Description: "Specifies the username to search for in the LDAP directory."},
			{Name: "searchUserPassword", Type: "string", Required: false, Description: "Password for the user being searched in LDAP authentication."},
			{Name: "ldapDistinguishedName", Type: "string", Required: false, Description: "Distinguished name of the LDAP user to authenticate."},
			{Name: "ldapDistinguishedNameStartSearch", Type: "string", Required: false, Description: "LdapDistinguishedNameStartSearch specifies the starting point in the LDAP directory for searching user entries."},
			{Name: "searchFilter", Type: "string", Required: false, Description: "Specifies the LDAP query filter used to search for user accounts during authentication."},
			{Name: "authAttribName", Type: "string", Required: false, Description: "AuthAttribName specifies the attribute used for authentication in LDAP queries."},
			{Name: "secure", Type: "boolean", Required: true, Description: "Indicates whether the LDAP connection should be established using a secure protocol, such as LDAPS (LDAP over SSL)."},
		},
	},
	"ldir": {
		Name: "lDir", Description: "Returns an array of file names matching a specified pattern and attributes.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "filePattern", Type: "string", Required: true, Description: "Specifies the pattern used to filter files in a directory."},
			{Name: "attributes", Type: "string", Required: false, Description: "Optional string specifying attributes to filter the files returned."},
		},
	},
	"left": {
		Name: "Left", Description: "Returns the leftmost characters of a string based on a specified length.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String from which characters will be extracted."},
			{Name: "length", Type: "double", Required: true, Description: "Length specifies the number of characters to extract from the beginning of the string."},
		},
	},
	"len": {
		Name: "Len", Description: "Computes the length of a string or array and returns it as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "variant", Required: true, Description: "Object or collection whose length will be determined."},
		},
	},
	"lfromhex": {
		Name: "LFromHex", Description: "Converts a hexadecimal string to a regular string. Returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String containing hexadecimal values that will be converted to a different format."},
		},
	},
	"lhex2dec": {
		Name: "LHex2Dec", Description: "Converts a hexadecimal string to a decimal string. Returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "Hexadecimal string to convert to decimal."},
		},
	},
	"limsat": {
		Name: "LimsAt", Description: "Returns the 1-based position of a substring within a string, starting from an optional offset.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "subString", Type: "string", Required: true, Description: "Substring to search for within the source string."},
			{Name: "source", Type: "string", Required: true, Description: "String in which to search for a substring."},
			{Name: "offset", Type: "double", Required: false, Description: "Specifies the starting position within the source string from which to begin searching for the substring."},
		},
	},
	"limscleanup": {
		Name: "LimsCleanup", Description: "Returns null without performing any actual cleanup operations (stub function).", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"limsdate": {
		Name: "LIMSDate", Description: "Converts a date to a formatted string. Returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "variant", Required: true, Description: "Date value to be formatted, which can be a string or a date object."},
			{Name: "format", Type: "string", Required: false, Description: "Specifies the string pattern used to represent the date."},
		},
	},
	"limsexec": {
		Name: "LimsExec", Description: "Executes a specified application and returns whether it was successful as a boolean.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "application", Type: "string", Required: false, Description: "Represents the name of the executable or script to run."},
			{Name: "show", Type: "boolean", Required: false, Description: "Whether to display a window for the application being executed."},
			{Name: "arguments", Type: "string", Required: false, Description: "Represents a string containing command-line arguments for the application being executed by LimsExec."},
		},
	},
	"limsgetdateformat": {
		Name: "LimsGetDateFormat", Description: "Returns the current date format as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"limsnetcast": {
		Name: "LimsNETCast", Description: "Converts a value to a specified data type and returns the new value.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "val", Type: "variant", Required: false, Description: "To be cast or converted to a new data type specified by the newType parameter."},
			{Name: "newType", Type: "string", Required: true, Description: "Target data type for casting. Valid values include .NET type names, \"byref\", or \"enum:TypeName\" for enumerations."},
		},
	},
	"limsnetconnect": {
		Name: "LimsNETConnect", Description: "Connects to a .NET assembly and creates an instance of a specified type, returning the created object or null.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "assembly", Type: "string", Required: false, Description: "Name or path of the assembly containing the type to instantiate."},
			{Name: "typeName", Type: "string", Required: false, Description: "Fully qualified name of the type within the specified assembly that will be instantiated or invoked."},
			{Name: "args", Type: "array", Required: false, Description: "Array containing arguments to pass to the method being invoked."},
			{Name: "asStatic", Type: "variant", Required: false, Description: "AsStatic determines whether the method should be called as a static method."},
		},
	},
	"limsnettypeof": {
		Name: "LimsNETTypeOf", Description: "Returns the .NET type of a given name as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "typeName", Type: "variant", Required: true, Description: "Is a string that specifies the fully qualified name of the type for which information is required."},
		},
	},
	"limsoleconnect": {
		Name: "limsoleconnect", Description: "Creates a new SSLNetObject based on the provided application name or SOAP client.", ReturnType: "sslnetobject",
		Parameters: []FunctionParameter{
			{Name: "v", Type: "variant", Required: true, Description: "Application name or object to be connected to in the limsoleconnect function."},
		},
	},
	"limsrecordsaffected": {
		Name: "LimsRecordsAffected", Description: "Returns the number of records affected by recent database operations as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{},
	},
	"limssetcounter": {
		Name: "LimsSetCounter", Description: "Sets a counter value in the database and returns the new value as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "tableName", Type: "string", Required: false, Description: "\"tableName\": The name of the database table where the counter will be set or incremented."},
			{Name: "fieldName", Type: "string", Required: false, Description: "Name of the field within the specified table for which the counter value needs to be incremented or set."},
			{Name: "prefix", Type: "string", Required: false, Description: "\"prefix\": The prefix string that will be added to the counter value when setting it in the database."},
			{Name: "arrayOfFields", Type: "array", Required: false, Description: "An array of field names to update."},
			{Name: "arrayOfValues", Type: "array", Required: false, Description: "An array of values to be set for the specified fields in the database table."},
			{Name: "incrementWith", Type: "variant", Required: false, Description: "By which a counter should be incremented, allowing for custom increments beyond the default of 1."},
		},
	},
	"limssqlconnect": {
		Name: "LimsSqlConnect", Description: "Establishes a connection to a database using a friendly name and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "friendlyName", Type: "string", Required: false, Description: "\"friendlyName\": The name used to identify or label a database connection in the system."},
		},
	},
	"limssqldisconnect": {
		Name: "LimsSqlDisconnect", Description: "Disconnects from a database using a friendly name and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "friendlyName", Type: "string", Required: false, Description: "FriendlyName identifies the database connection to be disconnected."},
		},
	},
	"limsstring": {
		Name: "LimsString", Description: "Converts a value to a string; returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "variant", Required: false, Description: "Or object from which a string representation will be derived."},
		},
	},
	"limstime": {
		Name: "LimsTime", Description: "Returns the current system time as a formatted string.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"limstype": {
		Name: "LimsType", Description: "Determines and returns the data type of a given parameter as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "param", Type: "string", Required: true, Description: "String containing the variable name or expression whose type is to be determined."},
		},
	},
	"limstypeex": {
		Name: "LimsTypeEx", Description: "Determines and returns the data type of a given source as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "variant", Required: false, Description: "Whose data type is to be determined and returned as a string."},
		},
	},
	"limsxor": {
		Name: "LimsXOr", Description: "Computes the bitwise XOR of two integers and returns the result as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "val1", Type: "double", Required: true, Description: "First operand for the bitwise XOR operation."},
			{Name: "val2", Type: "double", Required: true, Description: "Second integer value used in the XOR operation performed by the LimsXOr function."},
		},
	},
	"lkill": {
		Name: "LKill", Description: "Deletes a public variable and returns an empty string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "varName", Type: "string", Required: true, Description: "Variable name to delete from public scope."},
		},
	},
	"llower": {
		Name: "LLower", Description: "Converts a string to lowercase and returns it as a new string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String that will be converted to lowercase."},
		},
	},
	"lower": {
		Name: "Lower", Description: "Converts a string to lowercase and returns it as a new string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String that will be converted to lowercase."},
		},
	},
	"lprint": {
		Name: "LPrint", Description: "Writes a line of text to the report output and returns null.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String to be printed or logged."},
		},
	},
	"lsearch": {
		Name: "LSearch", Description: "Computes a search based on a command string and returns the result as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "commandString", Type: "string", Required: true, Description: "Search query or command used to filter data in a database search operation."},
			{Name: "defaultValue", Type: "variant", Required: false, Description: "Default value to return if no matching item is found in the search array."},
			{Name: "friendlyName", Type: "string", Required: false, Description: "\"friendlyName\": The name of the field or column being searched."},
			{Name: "arrayOfValues", Type: "array", Required: false, Description: "An array of values to be searched against."},
		},
	},
	"lselect": {
		Name: "LSelect", Description: "Computes a database query result based on provided command string and parameters, returning an array of data.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "commandString", Type: "string", Required: true, Description: "CommandString contains the SQL query to execute."},
			{Name: "fieldList", Type: "array", Required: false, Description: "Specifies the columns to be selected from a database query."},
			{Name: "friendlyName", Type: "string", Required: false, Description: "FriendlyName identifies the database or data source from which data is selected."},
			{Name: "arrayOfValues", Type: "array", Required: false, Description: "An array of values to be used in the query or operation."},
			{Name: "nullAsBlank", Type: "boolean", Required: false, Description: "NullAsBlank determines whether null values in the result set should be treated as blank strings."},
			{Name: "invariantDateColumns", Type: "array", Required: false, Description: "Is an array of strings that specifies which columns should not have their date values converted from UTC to the local time zone."},
		},
	},
	"lselect1": {
		Name: "LSelect1", Description: "Executes a database query and returns the results as an array.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "commandString", Type: "string", Required: true, Description: "SQL command to execute."},
			{Name: "friendlyName", Type: "string", Required: false, Description: "Name of the database or data source to query."},
			{Name: "arrayOfValues", Type: "array", Required: false, Description: "An array containing values to filter or select data based on."},
			{Name: "nullAsBlank", Type: "boolean", Required: false, Description: "Determines whether null values in the result set should be treated as blank strings."},
			{Name: "invariantDateColumns", Type: "array", Required: false, Description: "Is an array of strings representing the column names that should remain invariant during date conversions, ensuring their values are not affected by time zone differences."},
		},
	},
	"lselectc": {
		Name: "LSelectC", Description: "Returns an array of records based on a SQL command string and field list.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "commandString", Type: "string", Required: true, Description: "SQL command string used to query the database."},
			{Name: "fieldList", Type: "array", Required: false, Description: "An array of field names to be selected in a database query."},
			{Name: "friendlyName", Type: "string", Required: false, Description: "Specifies the name of the database or data source from which to retrieve data."},
			{Name: "arrayOfValues", Type: "array", Required: false, Description: "Values to be selected in a database query."},
			{Name: "nullAsBlank", Type: "boolean", Required: false, Description: "NullAsBlank determines whether null values should be treated as blank strings in the output."},
			{Name: "invariantDateColumns", Type: "array", Required: false, Description: "An array of column names that should remain in their original date format when converting data from a database query result."},
		},
	},
	"lstr": {
		Name: "LStr", Description: "Converts a numeric value to a string and trims any leading/trailing whitespace. Returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "number", Type: "variant", Required: false, Description: "Numeric value that will be converted to a string and trimmed."},
		},
	},
	"ltohex": {
		Name: "LToHex", Description: "Converts a string or integer to its hexadecimal representation and returns it as a string.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "variant", Required: false, Description: "String or integer value that will be converted to a hexadecimal format."},
		},
	},
	"ltransform": {
		Name: "LTransform", Description: "Converts a numeric expression to a formatted string based on a picture format. Returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "expression", Type: "variant", Required: false, Description: "Represents the value or data that will be transformed by the LTransform function."},
			{Name: "picture", Type: "string", Required: false, Description: "Format string used to transform a numeric value into a formatted string representation."},
		},
	},
	"ltrim": {
		Name: "LTrim", Description: "Returns a new string with leading whitespace removed from the original string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String from which leading whitespace will be removed."},
		},
	},
	"lwait": {
		Name: "lWait", Description: "Returns a string indicating completion after waiting for a specified number of seconds.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "seconds", Type: "double", Required: false, Description: "Duration in seconds for which the script should pause execution before continuing."},
		},
	},
	"makedateinvariant": {
		Name: "MakeDateInvariant", Description: "Converts a date or array of dates to be invariant to time zone changes by removing kind information. Returns the modified date or array.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "dateValue", Type: "variant", Required: true, Description: "Date or array of dates to be converted to an invariant kind, regardless of their original time zone."},
			{Name: "columnsIndex", Type: "variant", Required: false, Description: "Required when dateValue is an array. Specifies the column indices containing dates to convert. Can be a number or array of numbers."},
		},
	},
	"makedatelocal": {
		Name: "MakeDateLocal", Description: "Converts a date to local time and returns it as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "dateValue", Type: "variant", Required: true, Description: "Date or array of dates to be converted to local time."},
			{Name: "columnsIndex", Type: "variant", Required: false, Description: "Required when dateValue is an array. Specifies the column indices containing dates to convert. Can be a number or array of numbers."},
		},
	},
	"makedironftp": {
		Name: "MakeDirOnFtp", Description: "Creates a directory on an FTP server and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "serverNameOrIP", Type: "string", Required: true, Description: "IP address or hostname of the FTP server."},
			{Name: "remoteDirectory", Type: "string", Required: true, Description: "Specifies the directory path where a new directory should be created on the FTP server."},
			{Name: "userName", Type: "string", Required: true, Description: "Username used for authentication on the FTP server to create a directory."},
			{Name: "password", Type: "string", Required: true, Description: "Is used to authenticate the user when connecting to an FTP server."},
			{Name: "port", Type: "double", Required: false, Description: "Specifies the port number for connecting to the FTP server."},
			{Name: "proxy", Type: "string", Required: false, Description: "Specifies the address of the FTP server's proxy server through which connections should be made."},
			{Name: "isSFTP", Type: "boolean", Required: false, Description: "Indicates whether to use SFTP instead of FTP for directory creation."},
			{Name: "privateKeyFilePath", Type: "string", Required: false, Description: "Path to the private key file used for SFTP operations."},
		},
	},
	"makenetobject": {
		Name: "MakeNETObject", Description: "Converts a variant to an SSLNetObject or returns null.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "value", Type: "variant", Required: false, Description: "Represents the object or data that will be converted into an SSLNetObject."},
		},
	},
	"matfunc": {
		Name: "MatFunc", Description: "Computes a mathematical result based on the provided function name and number, returning a double value.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "functionName", Type: "string", Required: true, Description: "Name of the mathematical function to be applied to the provided number."},
			{Name: "number", Type: "double", Required: true, Description: "Numeric value on which mathematical operations will be performed by the MatFunc function."},
		},
	},
	"max": {
		Name: "Max", Description: "Computes the maximum of two values and returns it as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "value1", Type: "variant", Required: true, Description: "First value to compare in determining the maximum value between two values."},
			{Name: "value2", Type: "variant", Required: true, Description: "Second value to compare and return the maximum of."},
		},
	},
	"mergeglobalresources": {
		Name: "MergeGlobalResources", Description: "Merges global resources for a specified language and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "packageRes", Type: "string", Required: true, Description: "String representing the resource package to merge."},
			{Name: "language", Type: "string", Required: true, Description: "Specifies the language for merging global resources."},
		},
	},
	"mergehtmlform": {
		Name: "MergeHtmlForm", Description: "Merges HTML form data into a single string based on provided parameters and returns it as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "formId", Type: "variant", Required: true, Description: "Unique identifier for the HTML form to be merged."},
			{Name: "langId", Type: "variant", Required: true, Description: "Language identifier for the HTML form being merged."},
			{Name: "formDefinition", Type: "variant", Required: true, Description: "HTML form definition to be merged."},
			{Name: "formType", Type: "variant", Required: true, Description: "Type of HTML form to be merged, such as \"sample\" or \"analysis\"."},
			{Name: "formFullName", Type: "variant", Required: true, Description: "Full name or type of the form, which is used to determine the form's type and resources during processing."},
			{Name: "offlineMode", Type: "variant", Required: false, Description: "A boolean parameter that indicates whether the form should be processed in offline mode."},
		},
	},
	"mergexfd": {
		Name: "MergeXfd", Description: "Merges XFD documents into a single document and returns it as a string.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "formId", Type: "variant", Required: true, Description: "FormId identifies the form whose XFD document is being merged."},
			{Name: "langId", Type: "string", Required: true, Description: "Specifies the language identifier for merging resources in an XFD document."},
			{Name: "xfdDocument", Type: "string", Required: true, Description: "A string containing the XML-format document to be merged."},
		},
	},
	"mimedecode": {
		Name: "MimeDecode", Description: "Converts a MIME-encoded string back to its original form and returns it as a string.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "v", Type: "string", Required: true, Description: "MIME-encoded string that needs to be decoded. Must be a string type."},
		},
	},
	"mimeencode": {
		Name: "MimeEncode", Description: "Converts a string to MIME-encoded format and returns it as a string.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "v", Type: "string", Required: true, Description: "String value to be MIME-encoded. Must be a string type."},
		},
	},
	"min": {
		Name: "Min", Description: "Returns the smaller of two values.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "value1", Type: "variant", Required: true, Description: "First value to compare in a minimum operation."},
			{Name: "value2", Type: "variant", Required: true, Description: "Value2: The second value to compare and return the minimum of."},
		},
	},
	"minute": {
		Name: "Minute", Description: "Returns the minute component of a date as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "date", Required: true, Description: "A date value from which the minute component will be extracted."},
		},
	},
	"month": {
		Name: "Month", Description: "Returns the month of a date as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "date", Required: true, Description: "A date object from which the month value will be extracted and returned as a double."},
		},
	},
	"moveinftp": {
		Name: "MoveInFtp", Description: "Moves a file from one FTP directory to another and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "serverNameOrIP", Type: "string", Required: true, Description: "Represents the hostname or IP address of the FTP server from which files will be moved."},
			{Name: "remoteDirectoryFrom", Type: "string", Required: true, Description: "Source directory from which files will be moved."},
			{Name: "remoteDirectoryTo", Type: "string", Required: true, Description: "RemoteDirectoryTo specifies the destination directory for the file transfer."},
			{Name: "remoteFileFrom", Type: "string", Required: true, Description: "File to be moved from a remote directory on an FTP server."},
			{Name: "remoteFileTo", Type: "string", Required: true, Description: "Destination filename for the file being moved."},
			{Name: "userName", Type: "string", Required: true, Description: "Username used for authentication when moving files via FTP or SFTP."},
			{Name: "password", Type: "string", Required: true, Description: "Represents the user's password for authenticating with an FTP server during a file transfer operation."},
			{Name: "port", Type: "double", Required: false, Description: "Specifies the port number for the FTP connection."},
			{Name: "proxy", Type: "string", Required: false, Description: "Specifies the address of the proxy server through which FTP requests should be routed."},
			{Name: "isSFTP", Type: "boolean", Required: false, Description: "Indicates whether the FTP operation should use SFTP instead of standard FTP."},
			{Name: "privateKeyFilePath", Type: "string", Required: false, Description: "Path to the private key file used for SFTP operations."},
		},
	},
	"netframeworkversion": {
		Name: "NetFrameworkVersion", Description: "Returns the version of the .NET Framework as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"noofdays": {
		Name: "NoOfDays", Description: "Computes the number of days in a given month and returns it as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "date", Required: true, Description: "A specific date for which the number of days in that month is calculated."},
		},
	},
	"nothing": {
		Name: "Nothing", Description: "Returns `true` if the input is `null`, empty, or \"0\"; otherwise, returns `false`.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "val", Type: "variant", Required: false, Description: "To be evaluated, determining if it is considered \"nothing\" based on its content."},
		},
	},
	"now": {
		Name: "Now", Description: "Returns the current date and time as a date object.", ReturnType: "date",
		Parameters: []FunctionParameter{},
	},
	"preparearrayforin": {
		Name: "PrepareArrayForIn", Description: "Converts an array to a format suitable for input operations, returning the modified array or null if invalid.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "array", Type: "variant", Required: true, Description: "Array to prepare for input operations."},
			{Name: "itemType", Type: "variant", Required: false, Description: "Specifies the data type for elements in the array being prepared, such as \"string\", \"numeric\", or \"date\". Defaults to \"string\" if not provided."},
		},
	},
	"prepareform": {
		Name: "PrepareForm", Description: "Returns a new form ID based on encoded XFD and provided new ID.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "encodedXFD", Type: "string", Required: true, Description: "EncodedXFD contains the encoded form definition data to be prepared."},
			{Name: "newId", Type: "string", Required: true, Description: "Unique identifier for the form being prepared."},
		},
	},
	"prepareformclientscript": {
		Name: "PrepareFormClientScript", Description: "Converts encoded XFD and client script pairs into a form client script. Returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "encodedXFD", Type: "string", Required: true, Description: "A string that represents the encoded XML Form Definition (XFD) used to prepare the client script."},
			{Name: "stringOfClientScriptPairs", Type: "string", Required: true, Description: "String of client script pairs that defines the scripts to be executed on the client side."},
		},
	},
	"prmcount": {
		Name: "PrmCount", Description: "Returns the number of parameters in the current procedure as a double.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"processxfdformforimport": {
		Name: "ProcessXfdFormForImport", Description: "Converts import settings to XFD format and returns a variant object.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "settings", Type: "variant", Required: true, Description: "An object containing configuration settings for processing XFD forms, including package and dictionary XFD values, mode of operation, and overwrite transaction settings."},
		},
	},
	"raiseerror": {
		Name: "RaiseError", Description: "Raises a custom SSL error with a message and optional details. Always throws an exception.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "message", Type: "string", Required: true, Description: "Error message to display when raising an exception."},
			{Name: "location", Type: "string", Required: false, Description: "Location where the error occurred."},
			{Name: "errorCode", Type: "double", Required: false, Description: "Error code associated with the exception being raised."},
			{Name: "innerException", Type: "sslerror", Required: false, Description: "Error that caused the current error, allowing for a nested error structure."},
		},
	},
	"rand": {
		Name: "Rand", Description: "Generates a random double between 0 and 1; returns a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "seed", Type: "double", Required: false, Description: "Initializes the random number generator, ensuring reproducible sequences when the same seed is used."},
		},
	},
	"rat": {
		Name: "Rat", Description: "Returns the 1-based position of the last occurrence of a substring within a string, or 0 if not found.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "subStr", Type: "string", Required: true, Description: "Substring to search for within the source string."},
			{Name: "source", Type: "string", Required: true, Description: "String in which to search for the substring."},
		},
	},
	"readbytesbase64": {
		Name: "ReadBytesBase64", Description: "Reads a file and returns its contents as a Base64-encoded string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "fileName", Type: "string", Required: true, Description: "Path to the file from which bytes will be read and returned as a Base64-encoded string."},
		},
	},
	"readfromftp": {
		Name: "ReadFromFtp", Description: "Reads a file from an FTP or SFTP server and returns its contents as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "serverNameOrIP", Type: "string", Required: true, Description: "Specifies the FTP server's hostname or IP address from which to read a file."},
			{Name: "remoteDirectory", Type: "string", Required: true, Description: "Specifies the directory on the FTP server from which the file will be read."},
			{Name: "remoteFileName", Type: "string", Required: true, Description: "Name of the file to be read from the FTP server."},
			{Name: "maxSize", Type: "double", Required: false, Description: "Specifies the maximum size, in bytes, of the file to be read from the FTP server."},
			{Name: "userName", Type: "string", Required: true, Description: "Username used for authentication when connecting to an FTP server."},
			{Name: "password", Type: "string", Required: true, Description: "Password for the FTP server used to authenticate the user."},
			{Name: "port", Type: "double", Required: false, Description: "Specifies the port number on which the FTP server is listening."},
			{Name: "proxy", Type: "string", Required: false, Description: "Specifies the proxy server to use for the FTP connection, allowing access through a network firewall or other intermediary."},
			{Name: "isSFTP", Type: "boolean", Required: false, Description: "Indicates whether to use SFTP (true) or FTP (false)."},
			{Name: "privateKeyFilePath", Type: "string", Required: false, Description: "Path to the private key file used for SFTP operations."},
		},
	},
	"readtext": {
		Name: "ReadText", Description: "Reads text from a file and returns it as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "fileName", Type: "string", Required: true, Description: "Path to the file from which text will be read."},
			{Name: "charsToRead", Type: "double", Required: false, Description: "CharsToRead specifies the maximum number of characters to read from the file."},
			{Name: "encoding", Type: "variant", Required: false, Description: "Encoding specifies the character encoding used to read the text file."},
		},
	},
	"renameonftp": {
		Name: "RenameOnFtp", Description: "Renames a file on an FTP server and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "serverNameOrIP", Type: "string", Required: true, Description: "Hostname or IP address of the FTP server where the file rename operation will be performed."},
			{Name: "remoteDirectory", Type: "string", Required: true, Description: "Specifies the directory on the FTP server where the file rename operation should occur."},
			{Name: "fileNameOld", Type: "string", Required: true, Description: "Current name of the file on the FTP server that you want to rename."},
			{Name: "fileNameNew", Type: "string", Required: true, Description: "New name for the file to be renamed on the FTP server."},
			{Name: "userName", Type: "string", Required: true, Description: "Username used for authentication on the FTP server when renaming a file."},
			{Name: "password", Type: "string", Required: true, Description: "Password for authenticating with the FTP server."},
			{Name: "port", Type: "double", Required: false, Description: "Represents the FTP server's port number for connecting and renaming files."},
			{Name: "proxy", Type: "string", Required: false, Description: "Specifies the address of the FTP proxy server through which the connection should be made."},
			{Name: "isSFTP", Type: "boolean", Required: false, Description: "Indicates whether the FTP operation should use SFTP protocol instead of regular FTP."},
			{Name: "privateKeyFilePath", Type: "string", Required: false, Description: "Path to the private key file used for SFTP operations."},
		},
	},
	"replace": {
		Name: "Replace", Description: "Replaces occurrences of a substring within a string and returns the modified string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String in which a specified value will be replaced."},
			{Name: "searchFor", Type: "string", Required: true, Description: "Substring to search for within the source string."},
			{Name: "replaceWith", Type: "string", Required: true, Description: "String that will replace occurrences of searchFor within source."},
		},
	},
	"replicate": {
		Name: "Replicate", Description: "Returns a new string by repeating the source string a specified number of times.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String that will be repeated multiple times."},
			{Name: "count", Type: "double", Required: true, Description: "Number of times the source string should be repeated."},
		},
	},
	"resetapplication": {
		Name: "ResetApplication", Description: "Resets the application and returns null.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"resetfeatures": {
		Name: "ResetFeatures", Description: "Resets system features and returns true on success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{},
	},
	"retrievelong": {
		Name: "RetrieveLong", Description: "Retrieves a long integer from the database and saves it to a file. Returns `true` on success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "friendlyName", Type: "string", Required: false, Description: "\"friendlyName\": The name of the item or entity for which data is being retrieved."},
			{Name: "tableName", Type: "string", Required: true, Description: "Name of the database table from which data will be retrieved."},
			{Name: "columnName", Type: "string", Required: true, Description: "Name of the column from which to retrieve data."},
			{Name: "whereCondition", Type: "string", Required: true, Description: "Specifies the condition used to filter records in the database table before retrieving the long value."},
			{Name: "outputFilePath", Type: "string", Required: true, Description: "Represents the path where the retrieved long value will be saved."},
			{Name: "isCompressed", Type: "boolean", Required: false, Description: "Indicates whether the output should be compressed."},
		},
	},
	"returnlastsqlerror": {
		Name: "ReturnLastSQLError", Description: "Returns the last SQL error encountered as an `SSLSQLError` object.", ReturnType: "sslsqlerror",
		Parameters: []FunctionParameter{},
	},
	"right": {
		Name: "Right", Description: "Returns a substring of the source string starting from the right side up to the specified length.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String from which the rightmost portion will be extracted."},
			{Name: "length", Type: "double", Required: true, Description: "Length specifies the number of characters to return from the right side of the string."},
		},
	},
	"round": {
		Name: "Round", Description: "Rounds a number to a specified number of decimal places using midpoint rounding mode and returns the result as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "value", Type: "variant", Required: true, Description: "Numeric value to be rounded."},
			{Name: "digits", Type: "variant", Required: true, Description: "Specifies the number of decimal places to which a numeric value should be rounded."},
			{Name: "midPointRounding", Type: "variant", Required: false, Description: "MidPointRounding determines how numbers are rounded when they are exactly halfway between two values. Accepts 'ToEven' (banker's rounding, the default) or 'AwayFromZero'."},
		},
	},
	"roundpoint5": {
		Name: "RoundPoint5", Description: "Rounds a number to the nearest half-integer and returns it as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "number", Type: "double", Required: true, Description: "Numeric value to be rounded."},
		},
	},
	"runapp": {
		Name: "RunApp", Description: "Runs a specified application with optional arguments and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "application", Type: "string", Required: false, Description: "Represents the name or path of the executable to run."},
			{Name: "arguments", Type: "string", Required: false, Description: "Arguments to pass to the application when running it."},
		},
	},
	"runds": {
		Name: "RunDS", Description: "Executes a database query using the specified data source and parameters, returning the results as an array or XML based on the specified return type.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "dataSourceName", Type: "variant", Required: true, Description: "Identifier for the data source to be queried or manipulated."},
			{Name: "parameters", Type: "variant", Required: false, Description: "An array of values that will be passed to the data source for processing, allowing for customization and filtering of the dataset returned by the RunDS function."},
			{Name: "returnType", Type: "variant", Required: false, Description: "Specifies the format of the data to be returned, either as an array or XML. Defaults to \"array\" if not specified."},
		},
	},
	"runsql": {
		Name: "RunSQL", Description: "Executes a SQL command and returns a boolean indicating success or failure.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "commandString", Type: "string", Required: true, Description: "SQL command to be executed."},
			{Name: "friendlyName", Type: "string", Required: false, Description: "FriendlyName identifies the SQL command for logging and error reporting purposes."},
			{Name: "arrayOfValues", Type: "variant", Required: false, Description: "Values to be used in the SQL command, allowing for dynamic data insertion into the query."},
		},
	},
	"scient": {
		Name: "Scient", Description: "Converts a double to its scientific notation representation as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "doubleValue", Type: "double", Required: true, Description: "Numeric value on which the Scient function will perform its operations."},
		},
	},
	"searchldapuser": {
		Name: "SearchLDAPUser", Description: "Searches LDAP user details and returns a string containing the search results.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "ldapHost", Type: "string", Required: true, Description: "Hostname or IP address of the LDAP server to search for user information."},
			{Name: "ldapPort", Type: "double", Required: false, Description: "Port number used to connect to the LDAP server for user search operations. Defaults to 389 if not specified."},
			{Name: "bindUserName", Type: "string", Required: true, Description: "Username used for binding to the LDAP server."},
			{Name: "bindUserPassword", Type: "string", Required: false, Description: "Password used to authenticate the user binding to the LDAP server for searching operations."},
			{Name: "searchUserName", Type: "string", Required: false, Description: "Username to search for in the LDAP directory."},
			{Name: "ldapDistinguishedNameStartSearch", Type: "string", Required: false, Description: "LdapDistinguishedNameStartSearch specifies the starting point in the LDAP directory for searching user entries."},
			{Name: "searchFilter", Type: "string", Required: false, Description: "Specifies the LDAP query filter to use when searching for user entries in the directory, allowing customization of the search criteria based on attributes like name or email."},
			{Name: "secure", Type: "boolean", Required: false, Description: "Indicates whether to use a secure connection when searching for an LDAP user. Defaults to false if not specified."},
		},
	},
	"second": {
		Name: "Second", Description: "Returns the second of a given date as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "date", Required: true, Description: "A date and time value."},
		},
	},
	"seconds": {
		Name: "Seconds", Description: "Returns the current time in seconds since midnight as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{},
	},
	"sendfromoutbox": {
		Name: "SendFromOutbox", Description: "Sends emails from the outbox and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "ignoreErrors", Type: "boolean", Required: false, Description: "Determines whether to ignore errors during the email sending process."},
			{Name: "useCDO", Type: "boolean", Required: false, Description: "Indicates whether to use CDO (Component Object Model) for sending emails."},
			{Name: "timeout", Type: "double", Required: false, Description: "Specifies the maximum time in seconds to wait for a response when sending an email from the outbox."},
		},
	},
	"sendlimsemail": {
		Name: "SendLimsEmail", Description: "Sends an email using SMTP and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "SMTP", Type: "string", Required: true, Description: "Represents the server address used to send an email."},
			{Name: "recipients", Type: "array", Required: true, Description: "Recipients array contains the email addresses of the individuals to whom the email will be sent."},
			{Name: "fromWho", Type: "string", Required: true, Description: "Specifies the email address of the sender."},
			{Name: "subject", Type: "string", Required: false, Description: "Represents the email's subject line."},
			{Name: "messageBody", Type: "string", Required: false, Description: "Body of the email message to be sent."},
			{Name: "attachList", Type: "array", Required: false, Description: "An array of file paths representing attachments to include in the email."},
			{Name: "cClist", Type: "array", Required: false, Description: "CClist array represents the carbon copy (CC) email addresses to be included in the email message sent by the SendLimsEmail function."},
			{Name: "bCClist", Type: "array", Required: false, Description: "An array of email addresses that should receive a blind carbon copy (BCC) of the email."},
			{Name: "replyTo", Type: "string", Required: false, Description: "Specifies the email address to which recipients can reply when they receive the email sent by this function."},
			{Name: "nPort", Type: "double", Required: false, Description: "Port number to use for sending emails through an SMTP server."},
			{Name: "uName", Type: "string", Required: false, Description: "Username used for authentication with the SMTP server."},
			{Name: "uPass", Type: "string", Required: false, Description: "Password used for authentication when sending an email through the SMTP server."},
			{Name: "ignoreErrors", Type: "boolean", Required: false, Description: "Determines whether to ignore errors during the email sending process and return a default value instead of throwing an exception."},
			{Name: "useCDO", Type: "boolean", Required: false, Description: "Indicates whether to use CDO (Component Object Model) for sending emails."},
			{Name: "timeout", Type: "double", Required: false, Description: "Specifies the maximum time, in seconds, that the function will wait for a response before timing out."},
			{Name: "useSSL", Type: "boolean", Required: false, Description: "Indicates whether to use SSL for the email connection."},
			{Name: "isBodyHTML", Type: "boolean", Required: false, Description: "Indicates whether the email message body is in HTML format."},
			{Name: "encryptedData", Type: "string", Required: false, Description: "Encrypted data to be included in the email message."},
		},
	},
	"sendoutlookreminder": {
		Name: "SendOutlookReminder", Description: "Sends an Outlook reminder via email and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "SMTP", Type: "string", Required: true, Description: "SMTP server address used to send the reminder email."},
			{Name: "start", Type: "date", Required: true, Description: "Date and time when the reminder should begin."},
			{Name: "end", Type: "date", Required: true, Description: "Date and time when the reminder should cease to be active."},
			{Name: "subject", Type: "string", Required: true, Description: "Represents the title or description of the reminder email."},
			{Name: "summary", Type: "string", Required: true, Description: "Represents a brief description of the reminder."},
			{Name: "location", Type: "string", Required: true, Description: "Location where the reminder is taking place, such as a conference room or office."},
			{Name: "organizerName", Type: "string", Required: true, Description: "Name of the organizer for the reminder."},
			{Name: "organizerEmail", Type: "string", Required: true, Description: "Email address of the organizer for the reminder."},
			{Name: "attendeeName", Type: "string", Required: true, Description: "Name of the person receiving the reminder."},
			{Name: "attendeeEmail", Type: "string", Required: true, Description: "Email address of the person who should receive the reminder."},
			{Name: "nPort", Type: "double", Required: false, Description: "Specifies the port number for sending an Outlook reminder email."},
			{Name: "uName", Type: "string", Required: false, Description: "Username for authentication when sending an Outlook reminder via email."},
			{Name: "uPass", Type: "string", Required: false, Description: "Password used for authentication when sending an Outlook reminder email."},
			{Name: "ignoreErrors", Type: "boolean", Required: true, Description: "Determines whether to ignore errors during the execution of the SendOutlookReminder function."},
			{Name: "useSSL", Type: "boolean", Required: false, Description: "Indicates whether SSL should be used for the email connection."},
		},
	},
	"sendtoftp": {
		Name: "SendToFtp", Description: "Sends a file to an FTP server and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "serverNameOrIP", Type: "string", Required: true, Description: "Specifies the hostname or IP address of the FTP server to which files will be sent."},
			{Name: "remoteDirectory", Type: "string", Required: true, Description: "Specifies the directory on the FTP server where the file will be uploaded."},
			{Name: "remoteFileName", Type: "string", Required: true, Description: "Represents the name of the file to send to the FTP server."},
			{Name: "localFileName", Type: "string", Required: true, Description: "Local file path to be uploaded to the FTP server."},
			{Name: "userName", Type: "string", Required: true, Description: "Username used for authentication when connecting to the FTP server."},
			{Name: "password", Type: "string", Required: true, Description: "Represents the user's password for authenticating with the FTP server."},
			{Name: "port", Type: "double", Required: false, Description: "FTP server port number to connect to."},
			{Name: "proxy", Type: "string", Required: false, Description: "Specifies the proxy server to use for sending files via FTP or SFTP."},
			{Name: "usePassive", Type: "boolean", Required: false, Description: "Indicates whether to use passive mode for the FTP connection when sending a file."},
			{Name: "isSFTP", Type: "boolean", Required: false, Description: "Indicates whether to use SFTP instead of FTP for sending files."},
			{Name: "privateKeyFilePath", Type: "string", Required: false, Description: "Path to the private key file used for SFTP operations."},
		},
	},
	"sendtooutbox": {
		Name: "SendToOutbox", Description: "Sends an email and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "SMTP", Type: "string", Required: true, Description: "SMTP server address used to send an email."},
			{Name: "recipients", Type: "array", Required: true, Description: "An array containing email addresses of recipients to whom the email will be sent."},
			{Name: "fromWho", Type: "string", Required: true, Description: "Email address of the sender."},
			{Name: "subject", Type: "string", Required: false, Description: "Subject line of the email message to be sent."},
			{Name: "messageBody", Type: "string", Required: false, Description: "Content of the email message to be sent."},
			{Name: "attachList", Type: "array", Required: false, Description: "An array of file paths representing the attachments to be sent with the email."},
			{Name: "cClist", Type: "array", Required: false, Description: "An array of email addresses representing the carbon copy recipients."},
			{Name: "bCClist", Type: "array", Required: false, Description: "An array of email addresses that should receive a blind carbon copy (BCC) of the email being sent."},
			{Name: "replyTo", Type: "string", Required: false, Description: "Specifies the email address to which replies should be sent."},
			{Name: "nPort", Type: "double", Required: false, Description: "Port number for the SMTP server connection."},
			{Name: "uName", Type: "string", Required: false, Description: "Username used for authentication when sending an email through the SMTP server."},
			{Name: "uPass", Type: "string", Required: false, Description: "Password used for authentication with the SMTP server when sending an email."},
			{Name: "ignoreErrors", Type: "boolean", Required: false, Description: "Determines whether to ignore errors during the email sending process and continue execution."},
			{Name: "useSSL", Type: "boolean", Required: false, Description: "Determines whether SSL should be used for secure communication with the SMTP server."},
			{Name: "isBodyHTML", Type: "boolean", Required: false, Description: "IsBodyHTML indicates whether the message body is in HTML format."},
			{Name: "encryptedData", Type: "string", Required: false, Description: "Encrypted data to be sent in the email."},
		},
	},
	"serverendofday": {
		Name: "ServerEndOfDay", Description: "Returns the end of day date for a given date or the current server date.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "variant", Required: true, Description: "A date value for which the function calculates the end of the day, specifically setting the time to 23:59:59.997."},
		},
	},
	"serverstartofday": {
		Name: "ServerStartOfDay", Description: "Computes the start of the day for a given date and returns it as a date variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "variant", Required: true, Description: "Date for which the start of the day is being calculated."},
		},
	},
	"servertimezone": {
		Name: "ServerTimeZone", Description: "Returns the server's time zone offset from UTC in minutes.", ReturnType: "double",
		Parameters: []FunctionParameter{},
	},
	"setampm": {
		Name: "SetAmPm", Description: "Sets or retrieves the AM/PM format flag for date and time values; returns a boolean indicating the current setting.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "flag", Type: "boolean", Required: false, Description: "Indicates whether to set the time format to AM/PM."},
		},
	},
	"setbyname": {
		Name: "SetByName", Description: "Sets a variable by name and returns the new value.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "name", Type: "string", Required: true, Description: "Identifier for the variable to be set."},
			{Name: "value", Type: "variant", Required: true, Description: "Value to assign to a variable specified by name."},
		},
	},
	"setdecimalseparator": {
		Name: "SetDecimalSeparator", Description: "Sets the decimal separator for numeric operations and returns the new separator as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "decimalSep", Type: "string", Required: true, Description: "Sets the decimal separator character used in numeric operations."},
		},
	},
	"setdefaultconnection": {
		Name: "SetDefaultConnection", Description: "Sets the default database connection and returns a confirmation message as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "defaultConnection", Type: "variant", Required: true, Description: "Specifies the database connection string that will be used by default in subsequent database operations."},
		},
	},
	"setgroupseparator": {
		Name: "SetGroupSeparator", Description: "Sets the group separator for numeric formatting and returns the new separator as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "groupSep", Type: "string", Required: true, Description: "Specifies the character used as a group separator for numeric values in STARLIMS."},
		},
	},
	"setinternal": {
		Name: "SetInternal", Description: "Sets a property on an object and returns null.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "o", Type: "variant", Required: true, Description: "Object on which to set the internal value."},
			{Name: "propName", Type: "string", Required: true, Description: "Name of the property to set on the object."},
			{Name: "propValue", Type: "variant", Required: true, Description: "Value to set for the specified property."},
		},
	},
	"setinternalc": {
		Name: "SetInternalC", Description: "Sets or updates a nested property in an object using specified indices and values; returns the updated object.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "o", Type: "variant", Required: true, Description: "Object on which to set the internal value."},
			{Name: "collectionName", Type: "string", Required: true, Description: "Name of the collection within an object where a value will be set or retrieved."},
			{Name: "val", Type: "variant", Required: true, Description: "Value to set at the specified index or indices within the object."},
			{Name: "arg1", Type: "variant", Required: true, Description: "Key or index used to access a specific element within an object or collection."},
			{Name: "arg2", Type: "variant", Required: false, Description: "Second index or key used to access a nested property or element within an object or collection."},
			{Name: "arg3", Type: "variant", Required: false, Description: "Third index or key in a multi-level collection structure, used to access or set a value within nested collections."},
			{Name: "arg4", Type: "variant", Required: false, Description: "Fourth index value to set in the specified collection within the object."},
			{Name: "arg5", Type: "variant", Required: false, Description: "Fifth value to set in the nested structure."},
			{Name: "arg6", Type: "variant", Required: false, Description: "Value to set at the specified index in the collection."},
		},
	},
	"setlocationoracle": {
		Name: "SetLocationOracle", Description: "Sets Oracle location for reporting and returns a string result.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "file", Type: "string", Required: true, Description: "Path to the file."},
			{Name: "server", Type: "string", Required: true, Description: "Represents the hostname or IP address of the Oracle database server."},
			{Name: "user", Type: "string", Required: true, Description: "Username used for authentication with the Oracle server."},
			{Name: "password", Type: "string", Required: true, Description: "Represents the user's password for accessing an Oracle database location."},
			{Name: "encrypted", Type: "boolean", Required: true, Description: "Indicates whether the connection to the Oracle server should be made using encryption."},
		},
	},
	"setlocationsqlserver": {
		Name: "SetLocationSQLServer", Description: "Sets SQL Server location for a file and returns a string indicating success or failure.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "file", Type: "string", Required: true, Description: "Path to the file being processed."},
			{Name: "server", Type: "string", Required: true, Description: "Specifies the SQL Server instance where the database is located."},
			{Name: "database", Type: "string", Required: true, Description: "Specifies the name of the SQL Server database where the data is stored."},
			{Name: "owner", Type: "string", Required: true, Description: "Specifies the owner of the SQL Server location being set."},
			{Name: "user", Type: "string", Required: true, Description: "Username used to authenticate with the SQL Server database."},
			{Name: "password", Type: "string", Required: true, Description: "Password for authenticating with the SQL Server database."},
			{Name: "encrypted", Type: "boolean", Required: true, Description: "Indicates whether the connection to the SQL Server should be encrypted."},
		},
	},
	"setsqltimeout": {
		Name: "SetSqlTimeout", Description: "Sets SQL query timeout and returns the result as a double.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "timeout", Type: "variant", Required: false, Description: "Maximum time (in seconds) that a SQL query is allowed to execute before timing out."},
			{Name: "connection", Type: "variant", Required: false, Description: "String representing the database connection to set the SQL timeout for."},
		},
	},
	"setuserdata": {
		Name: "SetUserData", Description: "Sets user data and returns null.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "userName", Type: "string", Required: true, Description: "Username for which user data is being set. Must be a non-empty string."},
		},
	},
	"setuserpassword": {
		Name: "SetUserPassword", Description: "Sets a user's password and returns the hashed version as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "userName", Type: "string", Required: false, Description: "Username of the user whose password is being set."},
			{Name: "password", Type: "string", Required: false, Description: "Represents the new password for a user."},
		},
	},
	"showsqlerrors": {
		Name: "ShowSqlErrors", Description: "Sets whether SQL errors are shown and returns the previous setting as a boolean.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "flag", Type: "boolean", Required: true, Description: "Indicates whether SQL errors should be shown."},
		},
	},
	"sigfig": {
		Name: "SigFig", Description: "Computes a number to a specified number of significant figures and returns it as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "standard", Type: "string", Required: true, Description: "Rounding standard to apply, such as 'Standard', 'Engineering', or 'Scientific'."},
			{Name: "nrDigits", Type: "double", Required: true, Description: "Specifies the number of significant figures to which a given number should be rounded."},
			{Name: "number", Type: "double", Required: true, Description: "Numeric value to which the specified number of significant figures will be applied."},
		},
	},
	"sortarray": {
		Name: "SortArray", Description: "Sorts an array in-place based on numeric or custom criteria and returns the sorted array.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "target", Type: "array", Required: true, Description: "Array that will be sorted."},
			{Name: "numeric", Type: "variant", Required: false, Description: "Controls sort behavior - 1/true for numeric sort, lambda for custom comparator, null for string sort."},
		},
	},
	"sqlexecute": {
		Name: "SQLExecute", Description: "Executes a SQL command and returns the result as a variant.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "commandString", Type: "variant", Required: true, Description: "SQL command to be executed."},
			{Name: "friendlyName", Type: "variant", Required: false, Description: "A descriptive name for the SQL command, used for logging and identification."},
			{Name: "rollbackExistingTransaction", Type: "variant", Required: false, Description: "RollbackExistingTransaction determines whether an existing transaction should be rolled back before executing the SQL command."},
			{Name: "nullAsBlank", Type: "variant", Required: false, Description: "NullAsBlank determines whether null values in the result set should be treated as blank strings."},
			{Name: "invariantDateColumns", Type: "variant", Required: false, Description: "Specifies which columns in the result set should have their date values treated as invariant, meaning they will not be affected by time zone differences or other locale-specific settings."},
			{Name: "returnType", Type: "variant", Required: false, Description: "Specifies the format of the data to be returned by the SQL query execution, such as \"array\" or \"object\"."},
			{Name: "tableName", Type: "variant", Required: false, Description: "Name of the database table to execute the SQL command against."},
			{Name: "includeSchema", Type: "variant", Required: false, Description: "Determines whether to include schema information in the result set."},
			{Name: "includeHeader", Type: "variant", Required: false, Description: "IncludeHeader determines whether the result set should include a header row."},
		},
	},
	"sqlremovecomments": {
		Name: "SQLRemoveComments", Description: "Removes SQL comments from a statement and returns the cleaned string.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "statement", Type: "variant", Required: false, Description: "SQL query or statement from which comments will be removed."},
		},
	},
	"sqltraceoff": {
		Name: "SqlTraceOff", Description: "Disables SQL tracing and returns a boolean indicating success or failure.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"sqltraceon": {
		Name: "SqlTraceOn", Description: "Enables SQL tracing and returns a boolean indicating success.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"sqrt": {
		Name: "Sqrt", Description: "Computes and returns the square root of a given number as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "number", Type: "double", Required: true, Description: "For which the square root is to be calculated."},
		},
	},
	"stationname": {
		Name: "StationName", Description: "Returns the name of the current station as a string.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"stdround": {
		Name: "StdRound", Description: "Rounds a number to a specified number of decimal places and returns the result as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "standard", Type: "string", Required: true, Description: "Specifies the rounding method to be used, such as \"Standard\", \"HalfUp\", or \"Floor\"."},
			{Name: "nrDigits", Type: "double", Required: true, Description: "Specifies the number of decimal places to which a given numeric value should be rounded."},
			{Name: "number", Type: "double", Required: true, Description: "Numeric value to be rounded."},
		},
	},
	"str": {
		Name: "Str", Description: "Converts a number to a string with specified length and decimal places. Returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "number", Type: "double", Required: true, Description: "Numeric value to be converted to a string."},
			{Name: "length", Type: "double", Required: false, Description: "Specifies the total length of the resulting string, including any decimal places if specified."},
			{Name: "decimals", Type: "double", Required: false, Description: "Specifies the number of decimal places to include in the formatted string representation of a numeric value."},
		},
	},
	"stringtodate": {
		Name: "StringToDate", Description: "Converts a date string to a date object using a specified format. Returns a date.", ReturnType: "date",
		Parameters: []FunctionParameter{
			{Name: "dateString", Type: "string", Required: true, Description: "Date value in string format that needs to be converted to a date object using the specified dateFormat."},
			{Name: "dateFormat", Type: "string", Required: true, Description: "Specifies the format in which the input date string is structured, allowing for accurate conversion to a date object."},
		},
	},
	"strsrch": {
		Name: "StrSrch", Description: "Returns the 1-based position of a substring within a string, or 0 if not found.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "subStr", Type: "string", Required: true, Description: "Substring to search for within a larger string."},
			{Name: "source", Type: "string", Required: true, Description: "String in which to search for a substring."},
			{Name: "indexOrOccurence", Type: "double", Required: false, Description: "IndexOrOccurence specifies the position or occurrence of the substring to search for within the source string."},
			{Name: "flag", Type: "boolean", Required: false, Description: "Controls the meaning of the indexOrOccurrence parameter. When false (default), indexOrOccurrence specifies which occurrence to find (1=first, 2=second, etc.). When true, indexOrOccurrence is treated as a 1-based starting index for the search."},
		},
	},
	"strtran": {
		Name: "StrTran", Description: "Replaces occurrences of a substring within a string and returns the modified string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String in which search and replace operations will be performed."},
			{Name: "searchFor", Type: "string", Required: true, Description: "String to search for within the source string during the replacement operation."},
			{Name: "replaceWith", Type: "string", Required: false, Description: "Specifies the string that will replace occurrences of the searchFor string within the source string."},
		},
	},
	"strzero": {
		Name: "StrZero", Description: "Formats a number as a zero-padded string of specified length and decimal places. Returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "number", Type: "double", Required: false, Description: "Represents the numeric value that will be formatted with leading zeros."},
			{Name: "length", Type: "double", Required: false, Description: "Specifies the total length of the resulting string, including any decimal places if specified."},
			{Name: "decimals", Type: "double", Required: false, Description: "Specifies the number of decimal places to include in the formatted string."},
		},
	},
	"submittobatch": {
		Name: "SubmitToBatch", Description: "Submits code to a batch process and returns the result as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "code", Type: "string", Required: false, Description: "Script or command to be executed in the batch process."},
			{Name: "parameters", Type: "variant", Required: false, Description: "An array or object containing additional data to be submitted with the batch request."},
			{Name: "mode", Type: "string", Required: false, Description: "Specifies the submission mode for the batch process, such as \"submit\" or \"preview\"."},
			{Name: "userName", Type: "string", Required: false, Description: "Username of the user submitting the batch job."},
			{Name: "password", Type: "string", Required: false, Description: "User's password required to submit code to a batch process."},
		},
	},
	"submittobatchex": {
		Name: "SubmitToBatchEx", Description: "Computes and submits a batch of code to STARLIMS, returning a string result.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "code", Type: "string", Required: false, Description: "Batch processing code to be executed."},
		},
	},
	"substr": {
		Name: "SubStr", Description: "Extracts a substring from a string based on start position and length. Returns a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String from which a substring will be extracted."},
			{Name: "startPos", Type: "double", Required: false, Description: "StartPos specifies the starting position within the source string from which to extract a substring."},
			{Name: "length", Type: "double", Required: false, Description: "Specifies the number of characters to extract from the source string, starting at the specified position."},
		},
	},
	"syncdesignresources": {
		Name: "SyncDesignResources", Description: "Synchronizes design resources for a given XFD document and optionally updates translations, returning a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "formID", Type: "string", Required: true, Description: "FormID string specifies the identifier for the design resource to synchronize."},
			{Name: "languageID", Type: "string", Required: true, Description: "LanguageID specifies the language for which design resources should be synchronized."},
			{Name: "xfdDocument", Type: "string", Required: true, Description: "XML document containing design resources to synchronize."},
			{Name: "translationOnly", Type: "boolean", Required: false, Description: "Indicates whether to perform a translation-only synchronization."},
		},
	},
	"syncprogramaticresources": {
		Name: "SyncProgramaticResources", Description: "Syncs programmatic resources based on form and language IDs, returning a boolean success status.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "formID", Type: "string", Required: true, Description: "Unique identifier for a form in STARLIMS, which is used to synchronize programmatic resources."},
			{Name: "languageID", Type: "string", Required: true, Description: "Language identifier for the resources being synchronized."},
			{Name: "resourceDocument", Type: "string", Required: true, Description: "Document containing programmatic resources to synchronize."},
			{Name: "translationOnly", Type: "boolean", Required: false, Description: "TranslationOnly determines whether to perform a translation-only synchronization."},
		},
	},
	"tablefldlst": {
		Name: "TableFldLst", Description: "Returns an array of field names for a specified table in STARLIMS.", ReturnType: "array",
		Parameters: []FunctionParameter{
			{Name: "friendlyName", Type: "string", Required: false, Description: "Name of the field to retrieve from the table."},
			{Name: "tableName", Type: "string", Required: true, Description: "Name of the table from which to retrieve field names."},
		},
	},
	"time": {
		Name: "Time", Description: "Returns the current time as a string formatted according to the global settings.", ReturnType: "string",
		Parameters: []FunctionParameter{},
	},
	"today": {
		Name: "Today", Description: "Returns the current date as a `date` object.", ReturnType: "date",
		Parameters: []FunctionParameter{},
	},
	"tojson": {
		Name: "ToJson", Description: "Converts a value to its JSON string representation. Returns a string.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "value", Type: "variant", Required: false, Description: "Data or object that will be converted into a JSON string using the ToJson method."},
		},
	},
	"tonumeric": {
		Name: "ToNumeric", Description: "Converts a string to a numeric value and returns it as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "sNumber", Type: "string", Required: true, Description: "String representation of a numeric value to convert to a double."},
			{Name: "allowHex", Type: "boolean", Required: false, Description: "When true, allows hexadecimal values to be converted. Defaults to false."},
		},
	},
	"toscientific": {
		Name: "ToScientific", Description: "Converts a number to scientific notation with specified decimal places and returns it as a string.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "number", Type: "variant", Required: true, Description: "Numeric value that will be formatted in scientific notation."},
			{Name: "decimalPlaces", Type: "variant", Required: false, Description: "Specifies the number of decimal places to include in the scientific notation representation of the number."},
		},
	},
	"toxml": {
		Name: "ToXml", Description: "Converts a value to XML format and returns it as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "o", Type: "variant", Required: false, Description: "Object that will be converted to XML."},
			{Name: "typeName", Type: "string", Required: false, Description: "TypeName specifies the type of XML schema to use for serialization."},
		},
	},
	"traceoff": {
		Name: "TraceOff", Description: "Disables process tracing and returns a boolean indicating success.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"traceon": {
		Name: "TraceOn", Description: "Enables tracing and returns a boolean indicating success or failure.", ReturnType: "variant",
		Parameters: []FunctionParameter{},
	},
	"trim": {
		Name: "Trim", Description: "Returns a new string with leading and trailing whitespace removed from the source string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String from which leading and trailing whitespace will be removed."},
		},
	},
	"tryconnect": {
		Name: "TryConnect", Description: "Attempts to establish a connection and returns a double indicating success or failure.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "userName", Type: "string", Required: true, Description: "Username used for authentication in a connection attempt."},
			{Name: "utcOffset", Type: "double", Required: true, Description: "Difference in hours between Coordinated Universal Time (UTC) and the local time zone, used to adjust timestamps for accurate synchronization across different geographical locations."},
			{Name: "userType", Type: "string", Required: true, Description: "Type of user attempting to connect, such as \"admin\" or \"guest\"."},
			{Name: "platforma", Type: "string", Required: true, Description: "Platform identifier used in the connection attempt."},
		},
	},
	"undeclaredvars": {
		Name: "UndeclaredVars", Description: "Sets whether undeclared variables are allowed and returns the previous setting as a boolean.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "allowUndeclaredVars", Type: "boolean", Required: false, Description: "AllowUndeclaredVars determines whether undeclared variables are allowed in the script."},
		},
	},
	"updlong": {
		Name: "UpdLong", Description: "Updates a long integer column in a database table based on a condition and returns whether the operation was successful.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "friendlyName", Type: "string", Required: false, Description: "User-friendly name of the database table or column being updated."},
			{Name: "tableName", Type: "string", Required: true, Description: "Name of the database table where the data update operation will be performed."},
			{Name: "columnName", Type: "string", Required: true, Description: "Name of the column in the database table where the data will be updated."},
			{Name: "whereCondition", Type: "string", Required: true, Description: "Specifies the condition used to filter records in the database table before updating them."},
			{Name: "inputFilePath", Type: "string", Required: true, Description: "Represents the path to the file containing the data to update."},
			{Name: "isCompressed", Type: "boolean", Required: false, Description: "IsCompressed indicates whether the input file is compressed and should be decompressed before processing."},
		},
	},
	"upper": {
		Name: "Upper", Description: "Converts a string to uppercase and returns it as a new string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "source", Type: "string", Required: true, Description: "String that will be converted to uppercase."},
		},
	},
	"urldecode": {
		Name: "UrlDecode", Description: "Converts URL-encoded data to its original form and returns a string.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "data", Type: "variant", Required: false, Description: "Represents the string that needs to be URL-decoded."},
		},
	},
	"urlencode": {
		Name: "UrlEncode", Description: "Encodes a string to be URL-safe and returns it as a string.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "data", Type: "variant", Required: false, Description: "Represents the string that will be URL-encoded."},
		},
	},
	"usertimezone": {
		Name: "UserTimeZone", Description: "Returns the user's time zone offset in minutes from UTC.", ReturnType: "double",
		Parameters: []FunctionParameter{},
	},
	"usrmes": {
		Name: "usrmes", Description: "Constructs a user message from two variants and returns it as a string.", ReturnType: "variant",
		Parameters: []FunctionParameter{
			{Name: "a", Type: "variant", Required: true, Description: "First parameter in the usrmes function, a, represents the object on which the method will be invoked."},
			{Name: "b", Type: "variant", Required: true, Description: "Message to display."},
		},
	},
	"val": {
		Name: "Val", Description: "Converts a string to a double. Returns a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "sNumber", Type: "string", Required: true, Description: "A string that contains a numeric value, which will be converted to a double for further processing. Cannot be null."},
		},
	},
	"validatedate": {
		Name: "ValidateDate", Description: "Validates a date string and returns true if valid, false otherwise.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "stringDate", Type: "string", Required: true, Description: "Date string to validate."},
			{Name: "useDateFormat", Type: "variant", Required: false, Description: "Indicates whether to validate the date string using a four-digit format."},
		},
	},
	"validatenumeric": {
		Name: "ValidateNumeric", Description: "Validates whether a string represents a numeric value and returns a boolean result.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "sNumber", Type: "string", Required: true, Description: "String representation of a numeric value to validate."},
		},
	},
	"verifysignature": {
		Name: "VerifySignature", Description: "Verifies a digital signature against provided data and certificate; returns true if valid.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "certificateString", Type: "string", Required: false, Description: "Certificate in string format used to verify a digital signature."},
			{Name: "data", Type: "string", Required: false, Description: "Content to be verified against a digital signature."},
			{Name: "signature", Type: "string", Required: false, Description: "Digital signature to be verified."},
		},
	},
	"writebytesbase64": {
		Name: "WriteBytesBase64", Description: "Converts base64 data to bytes and writes them to a file, returning an empty string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "fileName", Type: "string", Required: true, Description: "Path to the file where the base64-encoded data will be written."},
			{Name: "base64Data", Type: "string", Required: true, Description: "Represents the Base64-encoded string data to write to a file."},
		},
	},
	"writetext": {
		Name: "WriteText", Description: "Writes text to a file and returns a confirmation message as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "fileName", Type: "string", Required: true, Description: "Path to the file where text will be written."},
			{Name: "charsToWrite", Type: "string", Required: true, Description: "Text content to write to the specified file."},
			{Name: "confirmRequired", Type: "string", Required: false, Description: "ConfirmRequired indicates whether confirmation is required before writing text to a file."},
			{Name: "append", Type: "string", Required: false, Description: "Indicates whether to append text to an existing file ('Y') or overwrite it ('N'). Defaults to 'N'."},
			{Name: "encoding", Type: "string", Required: false, Description: "Specifies the character encoding used for writing text to a file. Defaults to UTF8."},
		},
	},
	"writetoftp": {
		Name: "WriteToFtp", Description: "Writes a file to an FTP server and returns a boolean indicating success.", ReturnType: "boolean",
		Parameters: []FunctionParameter{
			{Name: "serverNameOrIP", Type: "string", Required: true, Description: "Specifies the FTP or SFTP server's hostname or IP address where the file will be written."},
			{Name: "remoteDirectory", Type: "string", Required: true, Description: "Specifies the directory on the FTP server where the file will be uploaded."},
			{Name: "remoteFileName", Type: "string", Required: true, Description: "Specifies the name of the file to be written on the remote FTP server."},
			{Name: "fileContents", Type: "string", Required: true, Description: "Content of the file to be written to the FTP server."},
			{Name: "userName", Type: "string", Required: true, Description: "Username required for authentication when writing a file to an FTP server."},
			{Name: "password", Type: "string", Required: true, Description: "Password for authenticating with the FTP server."},
			{Name: "port", Type: "double", Required: false, Description: "Specifies the FTP server's port number for connecting and transferring files."},
			{Name: "proxy", Type: "string", Required: false, Description: "Specifies the proxy server to use for the FTP operation, allowing connections through a network gateway."},
			{Name: "isSFTP", Type: "boolean", Required: false, Description: "IsSFTP determines whether to use SFTP (true) or FTP (false) for file transfer."},
			{Name: "privateKeyFilePath", Type: "string", Required: false, Description: "Path to the private key file used for SFTP authentication."},
		},
	},
	"xmldomtoudobject": {
		Name: "XmlDomToUdObject", Description: "Converts XML string to a dynamic object while preserving whitespace. Returns an SSLExpando object.", ReturnType: "sslexpando",
		Parameters: []FunctionParameter{
			{Name: "xml", Type: "variant", Required: true, Description: "XML string that will be converted to a user-defined object."},
			{Name: "preserveWhitespace", Type: "variant", Required: false, Description: "Whether to preserve whitespace in the XML when converting it to a user-defined object."},
		},
	},
	"xmlexportsql": {
		Name: "XmlExportSql", Description: "Converts SQL query to XML format and exports it to a file. Returns the path of the exported file as a string.", ReturnType: "string",
		Parameters: []FunctionParameter{
			{Name: "sql", Type: "string", Required: true, Description: "SQL query to be executed for data extraction."},
			{Name: "file", Type: "string", Required: true, Description: "Path and name where the XML export of SQL data will be saved."},
			{Name: "db", Type: "string", Required: false, Description: "Database name for which the SQL query is being exported to XML."},
			{Name: "sqlParams", Type: "array", Required: false, Description: "An array of parameters to be used in the SQL query."},
			{Name: "table", Type: "string", Required: false, Description: "Name of the database table for which XML export is required."},
		},
	},
	"year": {
		Name: "Year", Description: "Returns the year from a given date as a double.", ReturnType: "double",
		Parameters: []FunctionParameter{
			{Name: "date", Type: "date", Required: true, Description: "Date from which the year is extracted."},
		},
	},
}

// GetFunctionSignature returns the signature for a function name (case-insensitive).
func GetFunctionSignature(name string) (FunctionSignature, bool) {
	sig, ok := SSLFunctionSignatures[strings.ToLower(name)]
	return sig, ok
}
