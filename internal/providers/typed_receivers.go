package providers

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
)

// Typed-receiver inference (issue #123 piece D2). Mirrors the UDObject
// shape pass (udobject_shapes.go): a token scan over the file, fixpoint
// iteration, last-write-wins, file-global. Where shapes track ad-hoc
// property sets, this tracks *named types* — built-in classes and
// returns-category objects — so `oResp:StatusCode` resolves when oResp
// came from a producer chain:
//
//	oVar := ClassName{...}                → ClassName
//	oVar := <recv>:Method(...)            → Method's declared return type,
//	         where <recv> is a class literal (WebServices{}), an already-
//	         typed variable, or ambient Request/Response in endpoint files
//	oVar := BuiltinFn(...)                → the function's declared return
//	         type, when it names a class or returns object
//
// Chained producer calls on the right-hand side resolve hop by hop
// (`WebServices{}:CreateHttpClient():GetResponse()` → HttpResponse).
// Deliberately deferred: cross-procedure propagation and hover on
// mid-chain calls that never land in a variable.

// BuildTypedReceivers returns a map from variable name (lowercase) to the
// canonical PascalCase type name of the class or returns object it holds.
// endpointFile enables the ambient Request/Response receivers.
func BuildTypedReceivers(tokens []lexer.Token, endpointFile bool) map[string]string {
	typed := make(map[string]string)
	for iter := 0; iter < 8; iter++ {
		if !typedReceiverPass(tokens, typed, endpointFile) {
			break
		}
	}
	return typed
}

// typedReceiverPass runs one assignment scan; reports whether any binding
// changed (so the caller can iterate producer chains to a fixpoint).
func typedReceiverPass(tokens []lexer.Token, typed map[string]string, endpointFile bool) bool {
	changed := false
	for i := 0; i < len(tokens); i++ {
		if tokens[i].Type != lexer.TokenIdentifier {
			continue
		}
		// Only variable assignments bind a type: the LHS must open its
		// statement. Without this, `oCfg:Client := WebServices{};` (a
		// property assignment) would bind the unrelated variable `Client`.
		if !atStatementStart(tokens, i) {
			continue
		}
		opIdx := nextSignificantTokenIndex(tokens, i+1)
		if opIdx < 0 || tokens[opIdx].Type != lexer.TokenOperator || tokens[opIdx].Text != ":=" {
			continue
		}
		rhsIdx := nextSignificantTokenIndex(tokens, opIdx+1)
		if rhsIdx < 0 {
			continue
		}
		if typeName := resolveProducerExpr(tokens, rhsIdx, typed, endpointFile); typeName != "" {
			key := strings.ToLower(tokens[i].Text)
			if typed[key] != typeName {
				typed[key] = typeName
				changed = true
			}
		}
	}
	return changed
}

// resolveProducerExpr resolves the type produced by the expression starting
// at rhsIdx, or "" when it isn't a recognized producer.
func resolveProducerExpr(tokens []lexer.Token, rhsIdx int, typed map[string]string, endpointFile bool) string {
	if tokens[rhsIdx].Type != lexer.TokenIdentifier {
		return ""
	}
	head := tokens[rhsIdx].Text
	nextIdx := nextSignificantTokenIndex(tokens, rhsIdx+1)
	if nextIdx < 0 {
		return ""
	}

	// After whitespace the lexer fuses `:Method` into one keyword token,
	// so a chain can open either way (mirrors resolveCallChain).
	next := tokens[nextIdx]
	chainOpens := next.Text == ":" || isFusedMemberToken(next)

	switch {
	case next.Text == "{":
		// Class constructor literal, optionally followed by a producer
		// chain: WebServices{}:CreateHttpClient() assigns HttpClient, a
		// bare Email{...} assigns Email.
		if !constants.IsSSLClass(head) {
			return ""
		}
		afterIdx := indexAfterBalanced(tokens, nextIdx, "{", "}")
		if afterIdx < 0 {
			return ""
		}
		return resolveCallChain(tokens, afterIdx, CanonicalReceiverTypeName(head))
	case chainOpens:
		// Member call on a typed variable or an endpoint ambient.
		base := ""
		if t, ok := typed[strings.ToLower(head)]; ok {
			base = t
		} else if endpointFile && strings.EqualFold(head, "Request") {
			base = "SSLRequest"
		} else if endpointFile && strings.EqualFold(head, "Response") {
			base = "SSLResponse"
		}
		if base == "" {
			return ""
		}
		// A hop is always attempted (chainIdx points at ':'); a bare
		// `oX := oY` without a producer call never reaches this case.
		return resolveCallChain(tokens, nextIdx, base)
	case next.Text == "(":
		// Built-in function whose declared return names a class or
		// returns object (e.g. GetConnectionByName → SQLConnection),
		// optionally followed by a producer chain.
		summary, ok := constants.GeneratedFunctionSummaries[strings.ToLower(head)]
		if !ok {
			return ""
		}
		base := CanonicalReceiverTypeName(summary.Returns)
		if base == "" {
			return ""
		}
		afterIdx := indexAfterBalanced(tokens, nextIdx, "(", ")")
		if afterIdx < 0 {
			return ""
		}
		return resolveCallChain(tokens, afterIdx, base)
	}
	return ""
}

// resolveCallChain consumes `:Method(...)` hops starting at chainIdx and
// resolves each hop's return type against the current type's method table.
// Returns the final type, or "" as soon as a hop doesn't resolve to a class
// or returns object. Two token shapes open a hop: a bare `:` punctuation
// followed by the method identifier (after identifiers and `)`), or a fused
// `:Method` keyword token (how the lexer reads a colon after `}`).
func resolveCallChain(tokens []lexer.Token, chainIdx int, typeName string) string {
	for typeName != "" {
		if chainIdx < 0 || chainIdx >= len(tokens) {
			return typeName
		}
		var method string
		var parenIdx int
		tok := tokens[chainIdx]
		switch {
		case tok.Text == ":":
			methodIdx := nextSignificantTokenIndex(tokens, chainIdx+1)
			if methodIdx < 0 || tokens[methodIdx].Type != lexer.TokenIdentifier {
				return ""
			}
			method = tokens[methodIdx].Text
			parenIdx = nextSignificantTokenIndex(tokens, methodIdx+1)
		case isFusedMemberToken(tok):
			method = tok.Text[1:]
			parenIdx = nextSignificantTokenIndex(tokens, chainIdx+1)
		default:
			return typeName
		}
		if parenIdx < 0 || parenIdx >= len(tokens) || tokens[parenIdx].Text != "(" {
			return ""
		}
		typeName = memberReturnType(typeName, method)
		chainIdx = indexAfterBalanced(tokens, parenIdx, "(", ")")
		if chainIdx < 0 {
			return ""
		}
	}
	return ""
}

// isFusedMemberToken reports whether tok is a `:Name` keyword token — the
// lexer's reading of a colon that follows `}` or starts a statement.
func isFusedMemberToken(tok lexer.Token) bool {
	return tok.Type == lexer.TokenKeyword && len(tok.Text) > 1 && strings.HasPrefix(tok.Text, ":")
}

// indexAfterBalanced returns the index of the first significant token after
// the group opened at openIdx (len(tokens) when the group closes at EOF),
// or -1 when the group never closes.
func indexAfterBalanced(tokens []lexer.Token, openIdx int, open, close string) int {
	depth := 0
	for j := openIdx; j < len(tokens); j++ {
		switch tokens[j].Text {
		case open:
			depth++
		case close:
			depth--
			if depth == 0 {
				if idx := nextSignificantTokenIndex(tokens, j+1); idx >= 0 {
					return idx
				}
				return len(tokens)
			}
		}
	}
	return -1
}

// memberReturnType resolves a method's declared return type on a class or
// returns object, canonicalized; "" when the method or type is unknown or
// the return is scalar.
func memberReturnType(typeName, method string) string {
	for _, m := range receiverMethods(typeName) {
		if strings.EqualFold(m.Name, method) {
			return CanonicalReceiverTypeName(m.Returns)
		}
	}
	return ""
}

func receiverMethods(typeName string) []constants.ClassMethod {
	lower := strings.ToLower(typeName)
	if det, ok := constants.GeneratedClassDetails[lower]; ok {
		return det.Methods
	}
	if det, ok := constants.GeneratedReturnsObjectDetails[lower]; ok {
		return det.Methods
	}
	return nil
}

// CanonicalReceiverTypeName resolves name to the canonical PascalCase form
// of a built-in class or returns object; "" for anything else (scalar
// returns like "string"/"object", unknown names).
func CanonicalReceiverTypeName(name string) string {
	lower := strings.ToLower(name)
	if _, ok := constants.GeneratedReturnsObjectDetails[lower]; ok {
		for _, n := range constants.GeneratedReturnsObjectNames {
			if strings.ToLower(n) == lower {
				return n
			}
		}
	}
	if constants.IsSSLClass(name) {
		for _, n := range constants.SSLClassNames {
			if strings.ToLower(n) == lower {
				return n
			}
		}
	}
	return ""
}

// AmbientReceiverType maps the endpoint ambients to their returns-object
// types; "" for any other receiver name or when not in an endpoint file.
func AmbientReceiverType(receiver string, endpointFile bool) string {
	if !endpointFile {
		return ""
	}
	if strings.EqualFold(receiver, "Request") {
		return "SSLRequest"
	}
	if strings.EqualFold(receiver, "Response") {
		return "SSLResponse"
	}
	return ""
}

// GetReturnsMemberCompletions returns completion items for the members of a
// returns-category object (mirror of GetClassMemberCompletions).
func GetReturnsMemberCompletions(name string) []CompletionItem {
	det, ok := constants.GeneratedReturnsObjectDetails[strings.ToLower(name)]
	if !ok {
		return nil
	}
	canonical := CanonicalReceiverTypeName(name)

	items := make([]CompletionItem, 0, len(det.Methods)+len(det.Properties))
	for _, m := range det.Methods {
		doc := m.Description
		if m.Returns != "" && m.Returns != "none" {
			if doc != "" {
				doc = fmt.Sprintf("Returns `%s`. %s", m.Returns, doc)
			} else {
				doc = fmt.Sprintf("Returns `%s`.", m.Returns)
			}
		}
		items = append(items, CompletionItem{
			Label:            m.Name,
			Kind:             CompletionKindMethod,
			Detail:           fmt.Sprintf("%s method", canonical),
			Documentation:    doc,
			InsertText:       m.Name,
			InsertTextFormat: InsertTextFormatPlainText,
		})
	}
	for _, p := range det.Properties {
		doc := p.Description
		if p.Type != "" {
			if doc != "" {
				doc = fmt.Sprintf("Type `%s` (%s). %s", p.Type, orEmpty(p.Access, "read/write"), doc)
			} else {
				doc = fmt.Sprintf("Type `%s` (%s).", p.Type, orEmpty(p.Access, "read/write"))
			}
		}
		items = append(items, CompletionItem{
			Label:            p.Name,
			Kind:             CompletionKindProperty,
			Detail:           fmt.Sprintf("%s property", canonical),
			Documentation:    doc,
			InsertText:       p.Name,
			InsertTextFormat: InsertTextFormatPlainText,
		})
	}
	return items
}

// GetTypedMemberCompletions returns member completions for a typed receiver,
// whichever category its type belongs to.
func GetTypedMemberCompletions(typeName string) []CompletionItem {
	lower := strings.ToLower(typeName)
	if _, ok := constants.GeneratedReturnsObjectDetails[lower]; ok {
		return GetReturnsMemberCompletions(typeName)
	}
	if constants.IsSSLClass(typeName) {
		return GetClassMemberCompletions(typeName)
	}
	return nil
}

// RenderTypedMemberHover renders hover markdown for `receiver:member` where
// receiver holds a value of typeName (class or returns object). Returns ""
// when the type has no such member — the caller should answer null rather
// than fall through to an unrelated symbol, matching the UDObject-shape
// contract.
func RenderTypedMemberHover(typeName, receiver, member string) string {
	lower := strings.ToLower(typeName)
	var (
		properties []constants.ClassProperty
		methods    []constants.ClassMethod
	)
	if det, ok := constants.GeneratedClassDetails[lower]; ok {
		properties, methods = det.Properties, det.Methods
	} else if det, ok := constants.GeneratedReturnsObjectDetails[lower]; ok {
		properties, methods = det.Properties, det.Methods
	} else {
		return ""
	}
	canonical := CanonicalReceiverTypeName(typeName)

	for _, p := range properties {
		if !strings.EqualFold(p.Name, member) {
			continue
		}
		var b strings.Builder
		fmt.Fprintf(&b, "**%s**\n\n*%s property", p.Name, canonical)
		if p.Type != "" {
			fmt.Fprintf(&b, " (%s", p.Type)
			if p.Access != "" {
				fmt.Fprintf(&b, ", %s", p.Access)
			}
			b.WriteString(")")
		}
		fmt.Fprintf(&b, "* of `%s`", receiver)
		if p.Description != "" {
			fmt.Fprintf(&b, "\n\n%s", p.Description)
		}
		return b.String()
	}
	for _, m := range methods {
		if !strings.EqualFold(m.Name, member) {
			continue
		}
		var b strings.Builder
		fmt.Fprintf(&b, "**%s**\n\n*%s method* of `%s`", m.Name, canonical, receiver)
		if m.Returns != "" && m.Returns != "none" {
			fmt.Fprintf(&b, " → `%s`", m.Returns)
		}
		if m.Description != "" {
			fmt.Fprintf(&b, "\n\n%s", m.Description)
		}
		return b.String()
	}
	return ""
}
