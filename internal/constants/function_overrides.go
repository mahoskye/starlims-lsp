package constants

// Deliberate, documented differences from the generated function signatures
// (issue #252). Everything else hover, signature help, and completion say
// about a built-in function comes straight from the vendored reference.
//
// An override may only fill a gap in what the reference JSON carries — it
// never renames a parameter or invents one. TestFunctionSignaturesMatchReference
// enforces that: an appended parameter must already be named in the
// function's published signature, and every override must say why it
// exists. When the upstream extraction is fixed and the generated data
// carries the content itself, delete the override.

type functionOverride struct {
	// why records the reason for the override. Required.
	why string
	// returnDescription replaces a return description the extraction cut
	// short.
	returnDescription string
	// appendParameters adds parameter rows the extraction dropped.
	appendParameters []FunctionParameter
	// forms describes alternative call forms.
	forms []FunctionForm
}

func (o functionOverride) apply(sig FunctionSignature) FunctionSignature {
	if o.returnDescription != "" {
		sig.ReturnDescription = o.returnDescription
	}
	if len(o.appendParameters) > 0 {
		params := make([]FunctionParameter, 0, len(sig.Parameters)+len(o.appendParameters))
		params = append(params, sig.Parameters...)
		sig.Parameters = append(params, o.appendParameters...)
	}
	if len(o.forms) > 0 {
		sig.Forms = o.forms
	}
	return sig
}

// The return descriptions below condense a reference Returns section whose
// first paragraph ends in a colon: the extraction keeps only that paragraph
// and drops the table or list that completes it.
var functionOverrides = map[string]functionOverride{
	"createudobject": {
		why: "The reference documents three call forms with one flat parameter table " +
			"(aPropertyDefs and sClassName are both the first argument, told apart by type); " +
			"rendering them as three positional parameters misdescribes every call. " +
			"The extraction also cuts the return description short.",
		forms: []FunctionForm{
			{Parameters: nil},
			{Parameters: []string{"aPropertyDefs"}},
			{Parameters: []string{"sClassName", "aArgs"}},
		},
		returnDescription: "Depends on the call form: CreateUdObject() returns an empty SSLExpando, " +
			"CreateUdObject(aPropertyDefs) returns an SSLExpando with those properties already created, " +
			"and CreateUdObject(sClassName, [aArgs]) returns an instance of the named user-defined class.",
	},
	"datediff": {
		why: "The extraction cuts the return description short.",
		returnDescription: "A whole-number difference between dStartDate and dEndDate in the sDatepart unit. " +
			"\"year\" and \"month\" subtract calendar fields; \"day\" through \"millisecond\" count elapsed " +
			"whole units, truncating a partial unit toward zero.",
	},
	"fromjson": {
		why: "The extraction cuts the return description short.",
		returnDescription: "Depends on vValue: an array for a JSON array, an SSLExpando object for a JSON object, " +
			"a number, boolean, or string for those JSON values (a string starting with SSLDate| becomes a date), " +
			"and NIL for NIL, an empty string, or JSON null. A vValue that is not a string is returned unchanged.",
	},
	"fromxml": {
		why: "The extraction cuts the return description short.",
		returnDescription: "Depends on the root element of sXml: a boolean for <bool>, a number for a numeric tag " +
			"such as <int> or <double>, a string for <string>, a date for <date>, an array for <complextype>, " +
			"an SSLExpando object for <dto>, and NIL for empty input, <null>, <dbnull>, or an empty date.",
	},
	"sortarray": {
		why: "The extraction drops the vNumeric row because its description contains a code block " +
			"literal ({|vLeft, vRight| ...}) whose pipes split the table cell; the published " +
			"signature SortArray(aTarget, [vNumeric]) names it.",
		appendParameters: []FunctionParameter{
			{
				Name: "vNumeric", Type: "any", Required: false,
				Description: "Sort mode selector. Pass 1 or .T. to compare string elements by their numeric value, " +
					"or pass a code block such as {|vLeft, vRight| ...} that returns a negative number, 0, or a " +
					"positive number. Other values use the default sort behavior.",
			},
		},
	},
}
