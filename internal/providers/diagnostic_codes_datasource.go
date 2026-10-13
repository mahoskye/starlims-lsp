package providers

// Data-source header and parameter-list rules from the reference's
// data-sources guide (issue #260). Kept in their own file so adding them
// does not realign the stable-code table in diagnostic_codes.go.
const (
	CodeEmptyParameters                  = "empty_parameters"
	CodeDatasourceSQLQuestionPlaceholder = "datasource_sql_question_placeholder"
	CodeNullAsBlankValue                 = "nullasblank_value"
	CodeInvariantDateColumnsQuoted       = "invariant_date_columns_quoted"
)
