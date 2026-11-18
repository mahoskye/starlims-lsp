// Class sites: where a file creates an object of a user-defined class,
// calls a method on one, or inherits one. They are the call graph's class
// edges (issue #369). Normative behavior: catalog/features/call_graph.md.
package providers

import (
	"strings"
	"unicode/utf8"

	"starlims-lsp/internal/lexer"
)

// ClassSiteKind classifies a class site.
type ClassSiteKind int

const (
	// ClassCreate is a CreateUdObject first-argument string naming a class.
	ClassCreate ClassSiteKind = iota
	// ClassInherit is the parent named by an :INHERIT statement.
	ClassInherit
	// ClassMethod is a method call: Me:Name(), Base:Name(), or oVar:Name()
	// on a variable whose class the procedure settles.
	ClassMethod
)

// ClassSite is one site found by ExtractClassSites. Positions are 0-based.
type ClassSite struct {
	Kind ClassSiteKind
	// Raw is the site as written: the class target (create, inherit),
	// or receiver:method (method).
	Raw string
	// Range covers Raw: the string content, quotes excluded, for a create.
	Range Range
	// Receiver and Method are set on method sites: the receiver as
	// written (Me, Base, or a variable) and the method's name.
	Receiver string
	Method   string
	// Class is the class target of a variable receiver, as written in
	// its CreateUdObject. Empty for Me and Base.
	Class string
}

// ExtractClassSites walks the token stream and returns the file's class
// sites in order of appearance.
//
// A create is the walk ExtractCallSites does for a dispatch: the function
// identifier, '(', then a quoted one-line string. CreateUdObject() and
// CreateUdObject(aPropertyDefs) make an SSLExpando, not a class instance,
// and are not sites.
//
// A method call is receiver, ':', name with nothing between them, then
// '('. A receiver that is itself a member (a:b:Go()) or an element
// (a[1]:Go()) is not a known object and gives no site. A variable receiver
// gives a site only when its class is settled in the procedure holding the
// call (or in the code outside every procedure): every assignment to it
// there is a whole-statement CreateUdObject("Class", …) of one class, and
// it is not a parameter.
func ExtractClassSites(tokens []lexer.Token) []ClassSite {
	scopes, classes := variableClasses(tokens)
	var sites []ClassSite
	for i, token := range tokens {
		switch token.Type {
		case lexer.TokenKeyword:
			if !strings.EqualFold(strings.TrimPrefix(token.Text, ":"), "INHERIT") {
				continue
			}
			if target, startCol, endCol := includePathAfter(tokens, i); target != "" {
				sites = append(sites, ClassSite{
					Kind: ClassInherit,
					Raw:  target,
					Range: Range{
						Start: Position{Line: token.Line - 1, Character: startCol},
						End:   Position{Line: token.Line - 1, Character: endCol},
					},
				})
			}
		case lexer.TokenIdentifier:
			if str, ok := createdClassAt(tokens, i); ok {
				content := str.Text[1 : len(str.Text)-1]
				sites = append(sites, ClassSite{
					Kind: ClassCreate,
					Raw:  content,
					Range: Range{
						Start: Position{Line: str.Line - 1, Character: str.Column},
						End:   Position{Line: str.Line - 1, Character: str.Column + utf8.RuneCountInString(content)},
					},
				})
				continue
			}
			if site, ok := methodCallAt(tokens, i); ok {
				switch {
				case strings.EqualFold(site.Receiver, "Me"), strings.EqualFold(site.Receiver, "Base"):
				default:
					class := classes[scopes[i]][strings.ToLower(site.Receiver)]
					if class == "" {
						continue
					}
					site.Class = class
				}
				sites = append(sites, site)
			}
		}
	}
	return sites
}

// createdClassAt reports whether the identifier at i begins
// CreateUdObject("Class"…, returning the string token.
func createdClassAt(tokens []lexer.Token, i int) (lexer.Token, bool) {
	if !strings.EqualFold(tokens[i].Text, "CreateUdObject") {
		return lexer.Token{}, false
	}
	parenIdx := nextSignificantTokenIndex(tokens, i+1)
	if parenIdx < 0 || tokens[parenIdx].Type != lexer.TokenPunctuation || tokens[parenIdx].Text != "(" {
		return lexer.Token{}, false
	}
	strIdx := nextSignificantTokenIndex(tokens, parenIdx+1)
	if strIdx < 0 || tokens[strIdx].Type != lexer.TokenString {
		return lexer.Token{}, false
	}
	str := tokens[strIdx]
	if len(str.Text) < 3 || (str.Text[0] != '"' && str.Text[0] != '\'') ||
		str.Text[len(str.Text)-1] != str.Text[0] || strings.Contains(str.Text, "\n") {
		return lexer.Token{}, false
	}
	return str, true
}

// methodCallAt reports whether the identifier at i is the receiver of a
// method call written receiver:Name( with the three tokens adjacent.
func methodCallAt(tokens []lexer.Token, i int) (ClassSite, bool) {
	if i+2 >= len(tokens) {
		return ClassSite{}, false
	}
	colon, name := tokens[i+1], tokens[i+2]
	if colon.Type != lexer.TokenPunctuation || colon.Text != ":" || name.Type != lexer.TokenIdentifier {
		return ClassSite{}, false
	}
	parenIdx := nextSignificantTokenIndex(tokens, i+3)
	if parenIdx < 0 || tokens[parenIdx].Type != lexer.TokenPunctuation || tokens[parenIdx].Text != "(" {
		return ClassSite{}, false
	}
	// The receiver must be a whole name: not the member of something else.
	if prev := prevSignificantTokenIndex(tokens, i-1); prev >= 0 &&
		tokens[prev].Type == lexer.TokenPunctuation && tokens[prev].Text == ":" {
		return ClassSite{}, false
	}
	receiver := tokens[i]
	return ClassSite{
		Kind:     ClassMethod,
		Raw:      receiver.Text + ":" + name.Text,
		Receiver: receiver.Text,
		Method:   name.Text,
		Range: Range{
			Start: Position{Line: receiver.Line - 1, Character: receiver.Column - 1},
			End:   Position{Line: name.Line - 1, Character: name.Column - 1 + utf8.RuneCountInString(name.Text)},
		},
	}, true
}

// variableClasses returns the scope of every token (0 for code outside
// every procedure, then one number per procedure) and, per scope, the
// class of each variable whose class is settled there, by lowercased name.
func variableClasses(tokens []lexer.Token) ([]int, map[int]map[string]string) {
	scopes := make([]int, len(tokens))
	classes := map[int]map[string]string{}
	unsettled := map[int]map[string]bool{}
	scope, procedures := 0, 0
	for i, token := range tokens {
		if token.Type == lexer.TokenKeyword {
			switch strings.ToUpper(strings.TrimPrefix(token.Text, ":")) {
			case "PROCEDURE":
				procedures++
				scope = procedures
			case "ENDPROC":
				scopes[i] = scope
				scope = 0
				continue
			case "PARAMETERS":
				// A parameter holds what the caller passed, whatever the
				// procedure assigns to it later.
				for j := i + 1; j < len(tokens) && !(tokens[j].Type == lexer.TokenPunctuation && tokens[j].Text == ";"); j++ {
					if tokens[j].Type == lexer.TokenIdentifier {
						unsettle(unsettled, classes, scope, strings.ToLower(tokens[j].Text))
					}
				}
			}
		}
		scopes[i] = scope
		if token.Type != lexer.TokenIdentifier {
			continue
		}
		opIdx := nextSignificantTokenIndex(tokens, i+1)
		if opIdx < 0 || tokens[opIdx].Type != lexer.TokenOperator || tokens[opIdx].Text != ":=" {
			continue
		}
		// oObject:Property := … assigns a member, not the variable.
		if prev := prevSignificantTokenIndex(tokens, i-1); prev >= 0 &&
			tokens[prev].Type == lexer.TokenPunctuation && tokens[prev].Text == ":" {
			continue
		}
		name := strings.ToLower(token.Text)
		class := wholeStatementCreate(tokens, opIdx)
		known, seen := classes[scope][name]
		switch {
		case class == "" || unsettled[scope][name] || (seen && !strings.EqualFold(known, class)):
			unsettle(unsettled, classes, scope, name)
		case !seen:
			if classes[scope] == nil {
				classes[scope] = map[string]string{}
			}
			classes[scope][name] = class
		}
	}
	return scopes, classes
}

// unsettle records that the variable's class is not settled in the scope.
func unsettle(unsettled map[int]map[string]bool, classes map[int]map[string]string, scope int, name string) {
	if unsettled[scope] == nil {
		unsettled[scope] = map[string]bool{}
	}
	unsettled[scope][name] = true
	delete(classes[scope], name)
}

// wholeStatementCreate returns the class target when the right side of the
// assignment whose := is at opIdx is CreateUdObject("Class", …) and nothing
// else, and "" otherwise.
func wholeStatementCreate(tokens []lexer.Token, opIdx int) string {
	rhsIdx := nextSignificantTokenIndex(tokens, opIdx+1)
	if rhsIdx < 0 || tokens[rhsIdx].Type != lexer.TokenIdentifier {
		return ""
	}
	str, ok := createdClassAt(tokens, rhsIdx)
	if !ok {
		return ""
	}
	depth := 0
	for j := nextSignificantTokenIndex(tokens, rhsIdx+1); j >= 0 && j < len(tokens); j++ {
		if tokens[j].Type != lexer.TokenPunctuation {
			continue
		}
		switch tokens[j].Text {
		case "(":
			depth++
		case ")":
			depth--
			if depth == 0 {
				end := nextSignificantTokenIndex(tokens, j+1)
				if end >= 0 && tokens[end].Type == lexer.TokenPunctuation && tokens[end].Text == ";" {
					return str.Text[1 : len(str.Text)-1]
				}
				return ""
			}
		}
	}
	return ""
}
