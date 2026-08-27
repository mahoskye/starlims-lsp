package providers

// Diagnostic codes are stable, machine-readable identifiers for diagnostic
// rules. Clients use them to wire quick-fix code actions, suppression
// comments, and per-rule severity overrides.
//
// Where ssl-style-guide.schema.yaml defines a `lints` rule slug, the code
// here uses that slug verbatim (snake_case). Codes for parser/lexer-level
// findings — which the schema does not enumerate — are derived from the
// producing check function. Codes are namespaceless strings (no `ssl-`
// prefix) so they pair naturally with the diagnostic `Source: "ssl-lsp"`.
const (
	// Schema lints (ssl-style-guide.schema.yaml — compile_errors).
	CodeExitForInFinally       = "exitfor_in_finally"
	CodeExitWhileInFinally     = "exitwhile_in_finally"
	CodeLoopInFinally          = "loop_in_finally"
	CodeReturnInFinally        = "return_in_finally"
	CodeExitForOutsideLoop     = "exitfor_outside_loop"
	CodeExitWhileOutsideLoop   = "exitwhile_outside_loop"
	CodeLoopOutsideLoop        = "loop_outside_loop"
	CodeOneClassPerFile        = "one_class_per_file"
	CodeClassOrScript          = "class_or_script"
	CodeDoProcInClass          = "doproc_in_class"
	CodeConstructorReturnValue = "constructor_return_value"

	// Schema lints — type_safety.
	CodeForNumericValues     = "for_numeric_values"
	CodeDollarStringOnly     = "dollar_string_only"
	CodeNilInOperations      = "nil_in_operations"
	CodeCodeBlockComparison  = "code_block_comparison"
	CodeNilNotEmptyString    = "nil_not_empty_string"
	CodeEqualsVsStrictEquals = "equals_vs_strict_equals"

	// Schema lints — variable_behavior.
	CodeRedeclareIsNoop = "redeclare_is_noop"

	// Schema lints — class_rules / coding_standards.
	CodeClassMemberOrder        = "class_member_order"
	CodeMaxParamsWarning        = "max_params_warning"
	CodeLimitPublicVars         = "limit_public_vars"
	CodePreferExitCase          = "prefer_exitcase"
	CodeParametersFirst         = "parameters_first"
	CodeDefaultAfterParameters  = "default_after_parameters"
	CodeIncludeEarly            = "include_early"
	CodeConstructorOutsideClass = "constructor_outside_class"

	// Schema lints — datasource.
	CodeNoDefaultStatementsInDatasource = "no_default_statements_in_datasource"
	CodeDatasourceSQLSemicolon          = "datasource_sql_semicolon"
	CodeDatasourceUndeclaredPlaceholder = "datasource_undeclared_placeholder"

	// Schema lints — deprecated.
	CodeDeprecatedKeyword = "deprecated_keyword"

	// Schema lints — not preferred operators.
	CodeNotPreferredOperator = "not_preferred_operator"

	// Extension-named rules (no schema slug; derived from check name).
	CodeKeywordUppercase            = "keyword_uppercase"
	CodeLabelKeywordForm            = "label_keyword_form"
	CodeUnknownKeyword              = "unknown_keyword"
	CodeEndForInvalid               = "endfor_invalid"
	CodeUnknownToken                = "unknown_token"
	CodeCommentTermination          = "comment_termination"
	CodeCommentTextAfterTerminator  = "comment_text_after_terminator"
	CodeAssignmentInCondition       = "assignment_in_condition"
	CodeDotPropertyAccess           = "dot_property_access"
	CodeClassInstantiationCurly     = "class_instantiation_curly"
	CodeCreateUdObjectBuiltinMisuse = "createudobject_builtin_misuse"
	CodeZeroBasedArrayIndex         = "zero_based_array_index"
	CodeNamedSqlParamUnsupported    = "named_sql_param_unsupported"
	CodeComplexSqlPlaceholder       = "complex_sql_placeholder"
	CodeUdObjectArrayInClause       = "udobject_array_in_clause"
	CodeDirectProcedureCall         = "direct_procedure_call"
	CodeProcedureDeclarationSyntax  = "procedure_declaration_syntax"
	CodeExecFunctionMissingQuotes   = "execfunction_missing_quotes"
	CodeExecFunctionClassTarget     = "execfunction_class_target"
	CodeMeOutsideClass              = "me_outside_class"
	CodeBaseStandalone              = "base_standalone"
	CodeBaseOutsideClass            = "base_outside_class"
	CodeBaseRequiresInherit         = "base_requires_inherit"
	CodeUnmatchedDelimiter          = "unmatched_delimiter"
	CodeMismatchedDelimiter         = "mismatched_delimiter"
	CodeUnclosedDelimiter           = "unclosed_delimiter"
	CodeUnmatchedBlockEnd           = "unmatched_block_end"
	CodeUnclosedBlock               = "unclosed_block"
	CodeMismatchedBlockEnd          = "mismatched_block_end"
	CodeMaxBlockDepth               = "max_block_depth"
	CodeHungarianNotation           = "hungarian_notation"
	CodeMissingOtherwise            = "missing_otherwise"
	CodeBareLogicalOperator         = "bare_logical_operator"
	CodeInvalidOperatorSequence     = "invalid_operator_sequence"
	CodeIncludeInProcedure          = "include_in_procedure"
	CodeDefaultOnDeclareLine        = "default_on_declare_line"
	CodeDeclareInitializer          = "declare_initializer"
	CodeInlineCodeNaming            = "inline_code_naming"
	CodeBeginCaseRequiresCase       = "begincase_requires_case"
	CodeTryStructure                = "try_structure"
	CodeTryRequiresHandler          = "try_requires_handler"
	CodeSingleCatch                 = "single_catch"
	CodeSingleFinally               = "single_finally"
	CodeFinallyEmpty                = "finally_empty"
	CodeCatchOrderBeforeFinally     = "catch_order_before_finally"
	CodeErrorHandlerStructure       = "error_handler_structure"
	CodeCatchClauseForm             = "catch_clause_form"
	CodeRaiseErrorInCatch           = "raiseerror_in_catch"
	CodeScientificNotation          = "scientific_notation"
	CodeMixedTypeOperator           = "mixed_type_operator"
	CodeArithmeticTypeMismatch      = "arithmetic_type_mismatch"
	CodeEmptyOptionalParamArray     = "empty_optional_param_array"
	CodeBranchTargetLabel           = "branch_target_label"
	CodeVisibilityAnnotation        = "visibility_annotation"
	CodeNilMethodCall               = "nil_method_call"
	CodeGlobalAssignment            = "global_assignment"
	CodeUndeclaredVariable          = "undeclared_variable"
	CodeUnusedVariable              = "unused_variable"
	CodeInvalidSqlParam             = "invalid_sql_param"
	CodeNestedIif                   = "nested_iif"
	CodeNegativeLogic               = "negative_logic"
	CodeStepSpacing                 = "step_spacing"
	CodeRegionEndMismatch           = "region_end_mismatch"
	CodeCodeBlockStructure          = "code_block_structure"
	CodeBuilderDirectiveCase        = "builder_directive_case"
	CodeIdentifierTooLong           = "identifier_too_long"
	CodeSqlInjection                = "sql_injection"
	CodeClassNameCollision          = "class_name_collision"
	CodeUnqualifiedFieldAssignment  = "unqualified_field_assignment"
	CodeStepZeroLiteral             = "step_zero_literal"
	CodeExitCaseAfterReturn         = "exitcase_after_return"
	CodeMixedErrorHandlingFamilies  = "mixed_error_handling_families"
	CodeInvalidLimsTypeExComparison = "invalid_limstypeex_comparison"
	CodeRunSQLNonDML                = "runsql_non_dml"
	CodeUnicodeLiteralPrefix        = "unicode_literal_prefix"
	CodeUnjustifiedCollate          = "unjustified_collate"
	CodeTrailingSkipCommas          = "trailing_skip_commas"
	CodeSpacedSkipCommas            = "spaced_skip_commas"
	CodeFormatArgNotArray           = "format_arg_not_array"
	CodeVisibilityAnnotationUsage   = "visibility_annotation_usage"
)
