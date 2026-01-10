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
