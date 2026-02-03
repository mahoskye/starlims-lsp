// Package providers implements LSP feature providers for SSL.
package providers

// SQLKeywords contains all SQL keywords for formatting.
var SQLKeywords = map[string]bool{
	"SELECT": true, "UPDATE": true, "DELETE": true, "INSERT": true,
	"VALUES": true, "SET": true, "WHERE": true, "FROM": true,
	"JOIN": true, "INTO": true, "ON": true, "AND": true, "OR": true,
	"AS": true, "IN": true, "DISTINCT": true, "TOP": true, "NOT": true,
	"BETWEEN": true, "LIKE": true, "IS": true, "NULL": true,
	"INNER": true, "LEFT": true, "RIGHT": true, "FULL": true, "CROSS": true,
	"GROUP": true, "BY": true, "ORDER": true, "HAVING": true,
	"UNION": true, "ALL": true, "EXCEPT": true, "INTERSECT": true,
	"ASC": true, "DESC": true, "LIMIT": true, "OFFSET": true,
	"EXISTS": true, "CASE": true, "WHEN": true, "THEN": true,
	"ELSE": true, "END": true,
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
	// Other common functions
	"ROW_NUMBER": true, "RANK": true, "DENSE_RANK": true, "NTILE": true,
	"OVER": true, "PARTITION": true,
	"IIF": true, "CHOOSE": true,
}

// SQLBreakBeforeKeywords are keywords that trigger line breaks before them.
var SQLBreakBeforeKeywords = map[string]bool{
	"FROM": true, "WHERE": true, "INNER": true, "LEFT": true,
	"RIGHT": true, "FULL": true, "CROSS": true, "ORDER": true,
	"GROUP": true, "HAVING": true, "UNION": true, "VALUES": true,
	"INTO": true, "ON": true, "CASE": true, "WHEN": true,
	"ELSE": true, "END": true,
}

// SQLJoinModifiers are keywords that modify JOIN and shouldn't break before JOIN.
var SQLJoinModifiers = map[string]bool{
	"INNER": true, "LEFT": true, "RIGHT": true, "FULL": true, "CROSS": true,
}

// SQLIndentedKeywords get extra indentation.
var SQLIndentedKeywords = map[string]bool{
	"AND": true, "OR": true, "ON": true, "WHEN": true, "ELSE": true,
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
}
