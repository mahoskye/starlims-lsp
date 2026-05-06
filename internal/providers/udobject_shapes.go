package providers

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/lexer"
)

// UDObjectShape captures the inferred property names of a CreateUDObject
// initialization, indexed by variable name (case-insensitive).
//
// Issue #7: SSL's CreateUDObject({...}) creates an anonymous object with no
// compile-time symbol. We approximate "type" by inferring property names from
// the initializer dict, then propagate that approximation through clone().
type UDObjectShape struct {
	Properties []UDObjectProperty
}

// UDObjectProperty is one property in an inferred UDObject shape.
type UDObjectProperty struct {
	Name string
	// Type is a coarse value-kind label ("string", "boolean", "number",
	// "array", "object", "unknown") inferred from the initializer expression.
	// Empty when the value couldn't be classified.
	Type string
}

// BuildUDObjectShapes walks the token stream and returns a map from
// variable name (lowercase) to its inferred UDObject shape. Two patterns
// are recognized:
//
//  1. <var> := CreateUDObject({{"key1", val1}, {"key2", val2}, ...})
//  2. <var> := <other>:clone()  — inherits shape from <other>.
//
// On reassignment, the last write wins. SSL has no static scoping rich
// enough to disambiguate inside a single file, so this is file-global.
func BuildUDObjectShapes(tokens []lexer.Token) map[string]UDObjectShape {
	shapes := make(map[string]UDObjectShape)

	for i := 0; i < len(tokens); i++ {
		if tokens[i].Type != lexer.TokenIdentifier {
			continue
		}
		lhs := tokens[i].Text
		opIdx := nextSignificantTokenIndex(tokens, i+1)
		if opIdx < 0 {
			continue
		}
		if tokens[opIdx].Type != lexer.TokenOperator || tokens[opIdx].Text != ":=" {
			continue
		}
		rhsIdx := nextSignificantTokenIndex(tokens, opIdx+1)
		if rhsIdx < 0 {
			continue
		}

		// Pattern 1: CreateUDObject({...})
		if tokens[rhsIdx].Type == lexer.TokenIdentifier && strings.EqualFold(tokens[rhsIdx].Text, "CreateUDObject") {
			if shape := parseCreateUDObjectShape(tokens, rhsIdx); shape != nil {
				shapes[strings.ToLower(lhs)] = *shape
			}
			continue
		}

		// Pattern 2: <other>:clone(...)
		if tokens[rhsIdx].Type == lexer.TokenIdentifier {
			other := tokens[rhsIdx].Text
			colonIdx := nextSignificantTokenIndex(tokens, rhsIdx+1)
			if colonIdx < 0 || tokens[colonIdx].Text != ":" {
				continue
			}
			methodIdx := nextSignificantTokenIndex(tokens, colonIdx+1)
			if methodIdx < 0 {
				continue
			}
			if tokens[methodIdx].Type == lexer.TokenIdentifier && strings.EqualFold(tokens[methodIdx].Text, "clone") {
				if src, ok := shapes[strings.ToLower(other)]; ok {
					// Shallow shape copy. Future mutations on the clone are
					// not tracked yet — see issue #7 task list.
					shapes[strings.ToLower(lhs)] = src
				}
			}
		}
	}

	return shapes
}

// parseCreateUDObjectShape parses the argument list of CreateUDObject(...)
// starting at idx (the CreateUDObject identifier). Expects the canonical
// SSL form documented in the language spec:
//
//	CreateUDObject({ {"key1", val1}, {"key2", val2}, ... })
//
// Returns nil when the call doesn't match this shape (e.g., dispatched by
// class name string, or built up dynamically).
func parseCreateUDObjectShape(tokens []lexer.Token, idx int) *UDObjectShape {
	parenIdx := nextSignificantTokenIndex(tokens, idx+1)
	if parenIdx < 0 || tokens[parenIdx].Text != "(" {
		return nil
	}
	outerBraceIdx := nextSignificantTokenIndex(tokens, parenIdx+1)
	if outerBraceIdx < 0 || tokens[outerBraceIdx].Text != "{" {
		return nil
	}

	var props []UDObjectProperty

	// Walk pairs at depth 1 (inside the outer brace). When we see a `{` at
	// that depth, it opens a {key, value} pair — read the first string
	// literal as the key, classify the value, then skip to the pair's `}`.
	j := outerBraceIdx + 1
	depth := 1
	for j < len(tokens) && depth > 0 {
		tok := tokens[j]
		if tok.Type == lexer.TokenWhitespace || tok.Type == lexer.TokenComment {
			j++
			continue
		}
		if tok.Text == "{" && depth == 1 {
			prop := parseShapePair(tokens, j)
			if prop != nil {
				props = append(props, *prop)
			}
			// Skip past the matching `}` of this pair.
			j = skipBracedGroup(tokens, j)
			continue
		}
		if tok.Text == "}" {
			depth--
			j++
			continue
		}
		j++
	}

	if len(props) == 0 {
		return nil
	}
	return &UDObjectShape{Properties: props}
}

// parseShapePair reads the {key, value} pair starting at openIdx (the `{`).
// Returns nil if the first significant token isn't a string literal.
func parseShapePair(tokens []lexer.Token, openIdx int) *UDObjectProperty {
	keyIdx := nextSignificantTokenIndex(tokens, openIdx+1)
	if keyIdx < 0 || tokens[keyIdx].Type != lexer.TokenString {
		return nil
	}
	key := unquoteSSLString(tokens[keyIdx].Text)
	if key == "" {
		return nil
	}
	prop := &UDObjectProperty{Name: key}

	commaIdx := nextSignificantTokenIndex(tokens, keyIdx+1)
	if commaIdx >= 0 && tokens[commaIdx].Text == "," {
		valIdx := nextSignificantTokenIndex(tokens, commaIdx+1)
		if valIdx >= 0 {
			prop.Type = classifyShapeValue(tokens[valIdx])
		}
	}
	return prop
}

// skipBracedGroup returns the index just after the `}` matching the `{` at
// openIdx. If braces are unbalanced, returns len(tokens).
func skipBracedGroup(tokens []lexer.Token, openIdx int) int {
	depth := 0
	for j := openIdx; j < len(tokens); j++ {
		switch tokens[j].Text {
		case "{":
			depth++
		case "}":
			depth--
			if depth == 0 {
				return j + 1
			}
		}
	}
	return len(tokens)
}

// classifyShapeValue maps a single value-token to a coarse type label.
// Composite values (lists, nested objects, function calls) are not deeply
// inspected here — that would require a real expression analyzer.
func classifyShapeValue(tok lexer.Token) string {
	switch tok.Type {
	case lexer.TokenString:
		return "string"
	case lexer.TokenNumber:
		return "number"
	case lexer.TokenKeyword:
		// .T. / .F. / NIL are emitted as keywords by the lexer (see
		// readDotOperatorOrBoolean and readIdentifier in lexer.go).
		upper := strings.ToUpper(tok.Text)
		switch upper {
		case ".T.", ".F.":
			return "boolean"
		case "NIL":
			return "unknown"
		}
	}
	if tok.Text == "{" {
		return "array"
	}
	return "unknown"
}

// GetUDObjectShapeCompletions returns property completions for a variable
// whose UDObject shape was inferred. Returns nil when the variable has no
// known shape.
func GetUDObjectShapeCompletions(varName string, shapes map[string]UDObjectShape) []CompletionItem {
	shape, ok := shapes[strings.ToLower(varName)]
	if !ok {
		return nil
	}
	items := make([]CompletionItem, 0, len(shape.Properties))
	for _, prop := range shape.Properties {
		detail := "UDObject property"
		if prop.Type != "" {
			detail = fmt.Sprintf("UDObject property (%s)", prop.Type)
		}
		items = append(items, CompletionItem{
			Label:            prop.Name,
			Kind:             CompletionKindProperty,
			Detail:           detail,
			InsertText:       prop.Name,
			InsertTextFormat: InsertTextFormatPlainText,
		})
	}
	return items
}
