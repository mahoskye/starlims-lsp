// Class members: what `Me:` and `Base:` reach inside a class the workspace
// declares (issue #380). Normative behavior: catalog/features/completion.md,
// "Class members".
package providers

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/parser"
)

// ClassMember is one field or method a class file declares, or one
// procedure a script it includes declares.
type ClassMember struct {
	Name string
	// IsMethod is true for a :PROCEDURE, false for a field (a name a
	// :DECLARE outside every procedure declares).
	IsMethod bool
	// Parameters and Doc describe a method; Line is the 1-based line of
	// the declaration, 0 when it is not known.
	Parameters []string
	Doc        parser.ProcedureDoc
	Line       int
}

// ClassMemberOrigin says how a class comes by a member.
type ClassMemberOrigin int

const (
	// ClassMemberOwn is a member the class itself declares.
	ClassMemberOwn ClassMemberOrigin = iota
	// ClassMemberIncluded is a procedure of a script the class file
	// includes, reached as Me:Name(...).
	ClassMemberIncluded
	// ClassMemberInherited is a member of an ancestor class.
	ClassMemberInherited
)

// ClassMemberSource is one file's contribution to a class's members.
type ClassMemberSource struct {
	Origin ClassMemberOrigin
	// Name is the class's name for its own members, and the :INCLUDE or
	// :INHERIT target as written for the others.
	Name    string
	Members []ClassMember
}

// ClassMembersFromParse returns the members a class file declares: its
// fields — the names declared outside every procedure, each once — then its
// methods, both in declaration order.
func ClassMembersFromParse(procedures []parser.ProcedureInfo, variables []parser.VariableInfo) []ClassMember {
	var members []ClassMember
	seen := make(map[string]bool)
	for _, v := range variables {
		if v.Scope == parser.ScopeParameter || parser.FindProcedureAtLine(procedures, v.Line) != nil {
			continue
		}
		key := strings.ToLower(v.Name)
		if seen[key] {
			continue
		}
		seen[key] = true
		members = append(members, ClassMember{Name: v.Name, Line: v.Line})
	}
	for _, proc := range procedures {
		members = append(members, ClassMember{
			Name:       proc.Name,
			IsMethod:   true,
			Parameters: proc.Parameters,
			Doc:        proc.Doc,
			Line:       proc.StartLine,
		})
	}
	return members
}

// ClassFieldNames returns the names of the fields a class file declares,
// in declaration order: what the workspace index keeps of them.
func ClassFieldNames(procedures []parser.ProcedureInfo, variables []parser.VariableInfo) []string {
	var names []string
	for _, m := range ClassMembersFromParse(procedures, variables) {
		if !m.IsMethod {
			names = append(names, m.Name)
		}
	}
	return names
}

// GetUserClassMemberCompletions returns the completion items for `Me:`
// (base false) or `Base:` (base true) from the sources in order of
// precedence: one item per name, case-insensitively, from the first source
// that declares it. `Constructor` is offered after `Base:` only — it is how
// a derived constructor passes arguments up — and never after `Me:`.
// Returns nil when no member is left to offer.
func GetUserClassMemberCompletions(sources []ClassMemberSource, base bool) []CompletionItem {
	var items []CompletionItem
	seen := make(map[string]bool)
	for _, src := range sources {
		for _, m := range src.Members {
			key := strings.ToLower(m.Name)
			if seen[key] {
				continue
			}
			if m.IsMethod && key == "constructor" && (!base || src.Origin != ClassMemberInherited) {
				continue
			}
			seen[key] = true
			items = append(items, classMemberCompletion(src, m))
		}
	}
	return items
}

func classMemberCompletion(src ClassMemberSource, m ClassMember) CompletionItem {
	var where string
	switch src.Origin {
	case ClassMemberIncluded:
		where = fmt.Sprintf("Procedure of the included script `%s`.", src.Name)
	case ClassMemberInherited:
		where = fmt.Sprintf("Inherited from `%s`.", src.Name)
	default:
		where = fmt.Sprintf("Declared in this class (`%s`).", src.Name)
	}
	if m.Line > 0 {
		where += fmt.Sprintf(" Line %d.", m.Line)
	}

	if !m.IsMethod {
		detail := src.Name + " field"
		if src.Origin == ClassMemberInherited {
			detail += " (inherited)"
		}
		return CompletionItem{
			Label:            m.Name,
			Kind:             CompletionKindField,
			Detail:           detail,
			Documentation:    where,
			InsertText:       m.Name,
			InsertTextFormat: InsertTextFormatPlainText,
		}
	}

	detail := src.Name + " method"
	switch src.Origin {
	case ClassMemberIncluded:
		detail = src.Name + " procedure (included)"
	case ClassMemberInherited:
		detail += " (inherited)"
	}
	proc := parser.ProcedureInfo{Name: m.Name, Parameters: m.Parameters, Doc: m.Doc, StartLine: m.Line}
	return CompletionItem{
		Label:            m.Name,
		Kind:             CompletionKindMethod,
		Detail:           detail,
		Documentation:    renderClassMethodDoc(proc, where),
		InsertText:       classMethodSnippet(m),
		InsertTextFormat: InsertTextFormatSnippet,
	}
}

// classMethodSnippet is the text a method inserts after `Me:` or `Base:`:
// its name and its parameters as placeholders, the receiver being typed
// already.
func classMethodSnippet(m ClassMember) string {
	placeholders := make([]string, 0, len(m.Parameters))
	for i, param := range m.Parameters {
		placeholders = append(placeholders, fmt.Sprintf("${%d:%s}", i+1, param))
	}
	return fmt.Sprintf("%s(%s)", m.Name, strings.Join(placeholders, ", "))
}

// renderClassMethodDoc is the documentation panel of a method item: its
// docblock description, parameters and return text, then where it comes
// from.
func renderClassMethodDoc(proc parser.ProcedureInfo, where string) string {
	var b strings.Builder
	fmt.Fprintf(&b, "**Method:** %s", proc.Name)
	if proc.Doc.Description != "" {
		fmt.Fprintf(&b, "\n\n%s", proc.Doc.Description)
	}
	if len(proc.Parameters) > 0 {
		b.WriteString("\n\n**Parameters:**")
		for _, name := range proc.Parameters {
			if desc := lookupParamDoc(proc.Doc.ParameterDocs, name); desc != "" {
				fmt.Fprintf(&b, "\n- `%s` — %s", name, desc)
			} else {
				fmt.Fprintf(&b, "\n- `%s`", name)
			}
		}
	} else {
		b.WriteString("\n\n*No parameters*")
	}
	if proc.Doc.Returns != "" {
		fmt.Fprintf(&b, "\n\n**Returns:** %s", proc.Doc.Returns)
	}
	fmt.Fprintf(&b, "\n\n%s", where)
	return b.String()
}
