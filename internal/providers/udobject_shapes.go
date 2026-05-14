package providers

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
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
	return BuildUDObjectShapesWithProcedures(tokens, nil)
}

// BuildUDObjectShapesWithProcedures extends shape inference with two extra
// passes beyond the initializer scan (vs-code-ssl-formatter#73):
//
//  1. Property augmentation — `oVar:propName := …` adds `propName` to oVar's
//     shape (or creates a minimal shape for previously-unknown variables).
//  2. Cross-procedure propagation — `DoProc("Bar", {oFoo, …})` binds oFoo's
//     shape to the procedure's first parameter name, so completions inside
//     the callee see the same property set the caller built up.
//
// Passes are run to a fixpoint so a procedure that itself builds out an
// object and passes it onward gets fully propagated.
func BuildUDObjectShapesWithProcedures(tokens []lexer.Token, procedures []parser.ProcedureInfo) map[string]UDObjectShape {
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

	// Iterate property augmentation and cross-procedure propagation passes
	// until no new property/binding is added. Bound by a small iteration cap
	// to avoid pathological loops on malformed input.
	for iter := 0; iter < 8; iter++ {
		changed := augmentShapesFromPropertyAssignments(tokens, shapes)
		if propagateShapesAcrossProcedures(tokens, shapes, procedures) {
			changed = true
		}
		if !changed {
			break
		}
	}

	return shapes
}

// augmentShapesFromPropertyAssignments scans for `oVar:prop := …` and adds
// `prop` to oVar's shape. Returns true if any shape gained a property (or a
// new shape was created), so the caller can iterate to a fixpoint.
func augmentShapesFromPropertyAssignments(tokens []lexer.Token, shapes map[string]UDObjectShape) bool {
	changed := false
	for i := 0; i < len(tokens); i++ {
		if tokens[i].Type != lexer.TokenIdentifier {
			continue
		}
		// Only treat as an assignment LHS at the start of a statement: the
		// previous significant token should be a semicolon, the start of the
		// file, or a block keyword. This avoids re-counting the same LHS when
		// it appears as part of a larger expression.
		if !atStatementStart(tokens, i) {
			continue
		}
		colonIdx := nextSignificantTokenIndex(tokens, i+1)
		if colonIdx < 0 || tokens[colonIdx].Type != lexer.TokenPunctuation || tokens[colonIdx].Text != ":" {
			continue
		}
		propIdx := nextSignificantTokenIndex(tokens, colonIdx+1)
		if propIdx < 0 || tokens[propIdx].Type != lexer.TokenIdentifier {
			continue
		}
		opIdx := nextSignificantTokenIndex(tokens, propIdx+1)
		if opIdx < 0 || tokens[opIdx].Type != lexer.TokenOperator || tokens[opIdx].Text != ":=" {
			continue
		}

		lhs := strings.ToLower(tokens[i].Text)
		propName := tokens[propIdx].Text

		shape := shapes[lhs]
		if hasProperty(shape, propName) {
			continue
		}

		rhsIdx := nextSignificantTokenIndex(tokens, opIdx+1)
		propType := "unknown"
		if rhsIdx >= 0 {
			propType = classifyShapeValue(tokens[rhsIdx])
		}
		shape.Properties = append(shape.Properties, UDObjectProperty{Name: propName, Type: propType})
		shapes[lhs] = shape
		changed = true
	}
	return changed
}

// propagateShapesAcrossProcedures looks for DoProc/ExecFunction calls and
// binds caller-side shapes to the callee's parameter names. Argument list
// shape: `DoProc("Name", {arg1, arg2, ...})`. The function-name token may be
// either an identifier or a punctuation chain — only the canonical SSL form
// is handled.
func propagateShapesAcrossProcedures(tokens []lexer.Token, shapes map[string]UDObjectShape, procedures []parser.ProcedureInfo) bool {
	if len(procedures) == 0 {
		return false
	}

	procIndex := map[string]parser.ProcedureInfo{}
	for _, p := range procedures {
		procIndex[strings.ToLower(p.Name)] = p
	}

	changed := false
	for i := 0; i < len(tokens); i++ {
		if tokens[i].Type != lexer.TokenIdentifier {
			continue
		}
		callName := strings.ToLower(tokens[i].Text)
		if callName != "doproc" && callName != "execfunction" {
			continue
		}
		parenIdx := nextSignificantTokenIndex(tokens, i+1)
		if parenIdx < 0 || tokens[parenIdx].Text != "(" {
			continue
		}
		nameIdx := nextSignificantTokenIndex(tokens, parenIdx+1)
		if nameIdx < 0 || tokens[nameIdx].Type != lexer.TokenString {
			continue
		}
		targetName := strings.ToLower(unquoteSSLString(tokens[nameIdx].Text))
		target, ok := procIndex[targetName]
		if !ok || len(target.Parameters) == 0 {
			continue
		}

		// Look for the args array `, { ... }`.
		commaIdx := nextSignificantTokenIndex(tokens, nameIdx+1)
		if commaIdx < 0 || tokens[commaIdx].Text != "," {
			continue
		}
		braceIdx := nextSignificantTokenIndex(tokens, commaIdx+1)
		if braceIdx < 0 || tokens[braceIdx].Text != "{" {
			continue
		}

		argIdents := collectArgIdentifiers(tokens, braceIdx)
		for pos, argName := range argIdents {
			if pos >= len(target.Parameters) {
				break
			}
			if argName == "" {
				continue
			}
			argShape, has := shapes[strings.ToLower(argName)]
			if !has || len(argShape.Properties) == 0 {
				continue
			}
			paramKey := strings.ToLower(target.Parameters[pos])
			existing := shapes[paramKey]
			merged, mergedChanged := mergeShapes(existing, argShape)
			if mergedChanged {
				shapes[paramKey] = merged
				changed = true
			}
		}
	}
	return changed
}

// collectArgIdentifiers reads positional argument identifiers from `{a, b, c}`
// starting at the opening brace. Non-identifier arguments are recorded as an
// empty string so positional indexing stays correct.
func collectArgIdentifiers(tokens []lexer.Token, openIdx int) []string {
	var args []string
	depth := 0
	expecting := true // expecting next significant token to be the start of an arg
	for j := openIdx; j < len(tokens); j++ {
		tok := tokens[j]
		if tok.Type == lexer.TokenWhitespace || tok.Type == lexer.TokenComment {
			continue
		}
		switch tok.Text {
		case "{":
			depth++
			continue
		case "}":
			depth--
			if depth == 0 {
				return args
			}
			continue
		}
		if depth != 1 {
			continue
		}
		if tok.Text == "," {
			expecting = true
			continue
		}
		if !expecting {
			continue
		}
		expecting = false
		if tok.Type == lexer.TokenIdentifier {
			args = append(args, tok.Text)
		} else {
			args = append(args, "")
		}
	}
	return args
}

// mergeShapes merges src into dst, preserving order and avoiding duplicates.
// Returns the merged shape and whether dst actually changed.
func mergeShapes(dst, src UDObjectShape) (UDObjectShape, bool) {
	existing := map[string]bool{}
	for _, p := range dst.Properties {
		existing[strings.ToLower(p.Name)] = true
	}
	changed := false
	for _, p := range src.Properties {
		if existing[strings.ToLower(p.Name)] {
			continue
		}
		dst.Properties = append(dst.Properties, p)
		existing[strings.ToLower(p.Name)] = true
		changed = true
	}
	return dst, changed
}

// hasProperty reports whether the shape already contains a property of the
// given name (case-insensitive).
func hasProperty(shape UDObjectShape, name string) bool {
	lower := strings.ToLower(name)
	for _, p := range shape.Properties {
		if strings.ToLower(p.Name) == lower {
			return true
		}
	}
	return false
}

// atStatementStart reports whether tokens[i] is at the start of a statement
// — i.e. the previous significant token is a semicolon, a block keyword, or
// the start of file. This filters out identifiers that happen to be part of
// a larger expression (function call argument, etc.).
func atStatementStart(tokens []lexer.Token, i int) bool {
	for j := i - 1; j >= 0; j-- {
		t := tokens[j]
		if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
			continue
		}
		if t.Text == ";" {
			return true
		}
		if t.Type == lexer.TokenKeyword {
			return true
		}
		return false
	}
	return true
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
