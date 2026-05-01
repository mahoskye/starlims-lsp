// Package providers implements LSP feature providers for SSL.
package providers

// SQLKeywords contains all SQL keywords for formatting.
var SQLKeywords = map[string]bool{
	"SELECT": true, "UPDATE": true, "DELETE": true, "INSERT": true,
	"MERGE": true, "USING": true, "WITH": true, "RECURSIVE": true,
	"VALUES": true, "SET": true, "WHERE": true, "FROM": true,
	"JOIN": true, "INTO": true, "ON": true, "AND": true, "OR": true,
	"AS": true, "IN": true, "DISTINCT": true, "TOP": true, "NOT": true,
	"BETWEEN": true, "LIKE": true, "IS": true, "NULL": true,
	"INNER": true, "LEFT": true, "RIGHT": true, "FULL": true, "CROSS": true,
	"GROUP": true, "BY": true, "ORDER": true, "HAVING": true,
	"UNION": true, "ALL": true, "EXCEPT": true, "INTERSECT": true, "MINUS": true,
	"ASC": true, "DESC": true, "LIMIT": true, "OFFSET": true,
	"EXISTS": true, "CASE": true, "WHEN": true, "THEN": true,
	"ELSE": true, "END": true, "MATCHED": true, "RETURNING": true,
	"OVER": true, "PARTITION": true, "ROWS": true, "RANGE": true,
	"PRECEDING": true, "FOLLOWING": true, "CURRENT": true, "ROW": true,
	"START": true, "CONNECT": true, "PRIOR": true, "SIBLINGS": true,
	"PIVOT": true, "UNPIVOT": true, "LATERAL": true, "OF": true,
	"NOWAIT": true, "WAIT": true, "FETCH": true, "NEXT": true, "ONLY": true,
	// DDL keywords
	"CREATE": true, "ALTER": true, "DROP": true, "TRUNCATE": true,
	"TABLE": true, "VIEW": true, "INDEX": true,
	"CONSTRAINT": true, "PRIMARY": true, "KEY": true, "FOREIGN": true,
	"REFERENCES": true, "UNIQUE": true, "CHECK": true, "DEFAULT": true,
	"ADD": true, "MODIFY": true,
	// Additional
	"OUTER": true, "FOR": true, "WITHIN": true, "UNBOUNDED": true,
	"OVERFLOW": true,
	// Oracle data types and pseudocolumns (uppercased in SQL context)
	"NUMBER": true, "VARCHAR2": true, "DATE": true, "INTERVAL": true,
	"LEVEL": true, "NEXTVAL": true, "CURRVAL": true,
	// ORDER BY modifiers
	"NULLS": true, "LAST": true, "FIRST": true,
	// DDL modifiers
	"PURGE": true, "REPLACE": true, "SEQUENCE": true,
	"CASCADE": true, "NATURAL": true, "PERCENT": true,
	// DML command keywords (also need uppercasing)
	"GRANT": true, "REVOKE": true,
}

// SQLBuiltinFunctions contains common SQL aggregate and scalar functions.
// These should be cased like keywords (uppercase by default).
var SQLBuiltinFunctions = map[string]bool{
	// Aggregate functions
	"COUNT": true, "SUM": true, "AVG": true, "MIN": true, "MAX": true,
	"STDEV": true, "STDEVP": true, "VAR": true, "VARP": true,
	// String functions
	"CONCAT": true, "SUBSTRING": true, "SUBSTR": true, "LEFT": true, "RIGHT": true,
	"UPPER": true, "LOWER": true, "TRIM": true, "LTRIM": true, "RTRIM": true,
	"LEN": true, "LENGTH": true, "CHARINDEX": true, "INSTR": true,
	"REPLACE": true, "REVERSE": true, "STUFF": true, "TRANSLATE": true,
	"COALESCE": true, "NULLIF": true, "ISNULL": true, "NVL": true, "IFNULL": true,
	// Date/Time functions
	"GETDATE": true, "GETUTCDATE": true, "SYSDATETIME": true,
	"DATEADD": true, "DATEDIFF": true, "DATEPART": true, "DATENAME": true,
	"YEAR": true, "MONTH": true, "DAY": true, "HOUR": true, "MINUTE": true, "SECOND": true,
	"NOW": true, "CURDATE": true, "CURTIME": true, "CURRENT_DATE": true, "CURRENT_TIME": true,
	// Conversion functions
	"CAST": true, "CONVERT": true, "TRY_CAST": true, "TRY_CONVERT": true,
	"STR": true, "FORMAT": true,
	// Math functions
	"ABS": true, "CEILING": true, "FLOOR": true, "ROUND": true,
	"POWER": true, "SQRT": true, "SIGN": true, "MOD": true,
	// Window / analytic functions
	"ROW_NUMBER": true, "RANK": true, "DENSE_RANK": true, "NTILE": true,
	"LAG": true, "LEAD": true, "FIRST_VALUE": true, "LAST_VALUE": true,
	"LISTAGG": true,
	"OVER":    true, "PARTITION": true,
	// Oracle functions
	"TO_DATE": true, "TO_CHAR": true, "TO_NUMBER": true,
	"DECODE": true, "NVL2": true,
	"TRUNC": true, "SYSDATE": true, "SYSTIMESTAMP": true,
	"LPAD": true, "RPAD": true,
	"ADD_MONTHS": true, "SYS_CONNECT_BY_PATH": true, "CONNECT_BY_ROOT": true,
	// Other common functions
	"IIF": true, "CHOOSE": true,
	// Oracle date functions
	"MONTHS_BETWEEN": true,
	// Oracle pseudocolumns
	"ROWNUM": true, "ROWID": true,
}

// SQLBreakBeforeKeywords are keywords that trigger line breaks before them.
var SQLBreakBeforeKeywords = map[string]bool{
	"FROM": true, "WHERE": true, "INNER": true, "LEFT": true,
	"RIGHT": true, "FULL": true, "CROSS": true, "ORDER": true,
	"GROUP": true, "HAVING": true, "UNION": true, "VALUES": true,
	"INTO": true, "USING": true, "ON": true, "CASE": true, "WHEN": true,
	"ELSE": true, "END": true, "FOR": true,
	// Set operations and Oracle-specific clauses
	"INTERSECT": true, "MINUS": true, "EXCEPT": true,
	"START": true, "CONNECT": true,
	"PIVOT": true, "UNPIVOT": true,
	"RETURNING": true,
}

// SQLJoinModifiers are keywords that modify JOIN and shouldn't break before JOIN.
var SQLJoinModifiers = map[string]bool{
	"INNER": true, "LEFT": true, "RIGHT": true, "FULL": true, "CROSS": true,
}

// SQLIndentedKeywords get extra indentation.
var SQLIndentedKeywords = map[string]bool{
	"AND": true, "OR": true, "ON": true, "HAVING": true, "WHEN": true, "ELSE": true,
}

// SQLFunctions that take SQL strings in SSL code.
var SQLFunctions = map[string]bool{
	"SQLEXECUTE":                     true,
	"GETDATASET":                     true,
	"GETDATASETWITHSCHEMAFROMSELECT": true,
	"GETDATASETXMLFROMSELECT":        true,
	"GETNETDATASET":                  true,
	"RUNSQL":                         true,
	"LSEARCH":                        true,
	"LSELECT":                        true,
	"LSELECT1":                       true,
	"LSELECTC":                       true,
	"GETDATASETEX":                   true,
	"GETTABLES":                      true,
	"XMLEXPORTSQL":                   true,
	"GETSSLDATASET":                  true,
}

// SQLCommandKeywords are SQL statement-starting keywords used for detection.
var SQLCommandKeywords = map[string]bool{
	"SELECT":   true,
	"INSERT":   true,
	"UPDATE":   true,
	"DELETE":   true,
	"MERGE":    true,
	"WITH":     true,
	"CREATE":   true,
	"ALTER":    true,
	"DROP":     true,
	"TRUNCATE": true,
	"EXEC":     true,
	"EXECUTE":  true,
	"CALL":     true,
	"GRANT":    true,
	"REVOKE":   true,
}

// SQLDDLObjects are object types used in DDL statements (CREATE, ALTER, DROP).
var SQLDDLObjects = map[string]bool{
	"TABLE":     true,
	"VIEW":      true,
	"INDEX":     true,
	"PROCEDURE": true,
	"FUNCTION":  true,
	"TRIGGER":   true,
	"SCHEMA":    true,
	"DATABASE":  true,
}
