package constants

// Types modeling the structured fields of ssl-element-reference.json:
// function signatures, class constructors and methods, operator type-behavior
// tables, type members, and special-form syntax.

// FunctionParameter is one parameter of a built-in function or constructor.
type FunctionParameter struct {
	Name     string
	Type     string
	Required bool
	// Conditional marks a parameter the reference lists as required only
	// in the call form that uses it (Required is false).
	Conditional bool
	// Default is the published default for an optional parameter, as
	// prose (".T.", "current default connection"); empty when none is
	// documented.
	Default     string
	Description string
}

// FunctionSignature is a built-in function's published signature.
type FunctionSignature struct {
	Name              string
	Description       string
	ReturnType        string
	ReturnDescription string
	Parameters        []FunctionParameter
	// Variadic marks a function that accepts further arguments after its
	// last listed parameter (Eval).
	Variadic bool
	// Forms lists alternative call forms when one flat parameter list
	// would misdescribe the function (CreateUdObject). Empty for nearly
	// every function; renderers then build one form from Parameters.
	Forms []FunctionForm
}

// FunctionForm is one call form of a function with several, naming the
// parameters (from FunctionSignature.Parameters) that form takes, in order.
type FunctionForm struct {
	Parameters []string
}

// ConstructorSignature describes one constructor form for a built-in class.
type ConstructorSignature struct {
	Signature   string
	Description string
	Parameters  []FunctionParameter
}

// ClassMethod describes a member method on a built-in class.
type ClassMethod struct {
	Name        string
	Returns     string
	Description string
}

// ClassProperty describes a property exposed by a built-in class.
type ClassProperty struct {
	Name        string
	Type        string
	Access      string
	Description string
}

// ClassDetails holds the structured detail rows for a single class.
type ClassDetails struct {
	Title        string
	Summary      string
	BaseClass    string
	Constructors []ConstructorSignature
	Methods      []ClassMethod
	Properties   []ClassProperty
}

// ReturnsObjectDetails holds the structured detail rows for a returns-category
// object — an object surfaced only as a built-in class method's return value
// (HttpClient, SoapResponse, …) or ambient in endpoint scripts (SSLRequest,
// SSLResponse). Row shapes are shared with classes.
type ReturnsObjectDetails struct {
	Title      string
	Summary    string
	Properties []ClassProperty
	Methods    []ClassMethod
}

// OperatorTypeBehavior is one row of an operator's type-behavior table.
type OperatorTypeBehavior struct {
	Left     string
	Right    string
	Result   string
	Behavior string
}

// OperatorDetails holds the structured detail rows for a single operator.
type OperatorDetails struct {
	Symbol       string
	Title        string
	Summary      string
	Syntax       string
	TypeBehavior []OperatorTypeBehavior
}

// TypeMember describes one entry on a type's members table (property or method
// of the type itself, e.g. `Append` on `array`).
type TypeMember struct {
	Name        string
	Kind        string // "Method", "Property", "" if untyped
	Returns     string
	Description string
	Group       string // optional H3 sub-group label such as "properties" or "methods"
}

// TypeTask is one row of a value type's Task | Use table. Value types with
// no SSL-defined `:` members (array, boolean, codeblock, date, netobject,
// number, string) document the function or operator to use for each common
// task instead.
type TypeTask struct {
	Task string
	Use  string
}

// TypeOperatorRow describes one entry on a type's operators table (e.g. how
// `==` behaves on arrays).
type TypeOperatorRow struct {
	Operator string
	Symbol   string
	Returns  string
	Behavior string
}

// TypeDetails holds the structured detail rows for one of the 8 core SSL
// value types (array, boolean, codeblock, date, netobject, number, object,
// string).
type TypeDetails struct {
	Title       string
	Summary     string
	RuntimeType string
	Operators   []TypeOperatorRow
	Members     []TypeMember
	Tasks       []TypeTask
}

// SpecialFormDetails holds the canonical syntax block and summary for one of
// the 6 SSL special forms (access-modifiers, base, code-block,
// code-organization, constructor, me).
type SpecialFormDetails struct {
	Title   string
	Summary string
	Syntax  string
}

// LiteralDetails holds the structured detail for one of the 3 SSL literals
// (.T., .F., NIL).
type LiteralDetails struct {
	Title   string
	Summary string
	Syntax  string
}

// KeywordDetails holds the structured detail for one keyword: the published
// summary (also exposed as SSLKeywordDescriptions) and syntax block.
type KeywordDetails struct {
	Title   string
	Summary string
	Syntax  string
}
