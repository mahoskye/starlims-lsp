package constants

// Types modeling the structured fields of ssl-element-reference.json that the
// hand-maintained constants did not previously expose: class constructors and
// methods, operator type-behavior tables, type members, and special-form
// syntax. The existing FunctionSignature/FunctionParameter shapes (defined in
// signatures.go) remain the source of truth for function parameter detail and
// are unchanged.

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

// KeywordDetails holds the structured detail for one keyword. Hand-maintained
// rich descriptions still live in SSLKeywordDescriptions (constants.go) and
// take precedence; this carries the published syntax block for hover and
// completion.
type KeywordDetails struct {
	Title   string
	Summary string
	Syntax  string
}
