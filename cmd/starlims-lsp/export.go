package main

import (
	"encoding/json"
	"fmt"
	"os"
	"sort"

	"starlims-lsp/internal/constants"
)

// ExportedParameter represents a function parameter in the exported JSON.
type ExportedParameter struct {
	Name        string `json:"name"`
	Type        string `json:"type"`
	Required    bool   `json:"required"`
	Description string `json:"description,omitempty"`
}

// ExportedFunction represents a function signature in the exported JSON.
type ExportedFunction struct {
	Name        string              `json:"name"`
	Description string              `json:"description"`
	ReturnType  string              `json:"return_type"`
	Parameters  []ExportedParameter `json:"parameters"`
}

// ExportedConstructor describes one constructor form for a built-in class.
type ExportedConstructor struct {
	Signature   string              `json:"signature"`
	Description string              `json:"description,omitempty"`
	Parameters  []ExportedParameter `json:"parameters,omitempty"`
}

// ExportedClassMember describes a method or property on a built-in class.
type ExportedClassMember struct {
	Name        string `json:"name"`
	Returns     string `json:"returns,omitempty"`
	Type        string `json:"type,omitempty"`
	Access      string `json:"access,omitempty"`
	Description string `json:"description,omitempty"`
}

// ExportedClass is a built-in SSL class entry in the exported JSON.
type ExportedClass struct {
	Name         string                `json:"name"`
	Summary      string                `json:"summary,omitempty"`
	BaseClass    string                `json:"base_class,omitempty"`
	Constructors []ExportedConstructor `json:"constructors,omitempty"`
	Properties   []ExportedClassMember `json:"properties,omitempty"`
	Methods      []ExportedClassMember `json:"methods,omitempty"`
}

// ExportedOperatorBehavior is one row of an operator's type-behavior table.
type ExportedOperatorBehavior struct {
	Left     string `json:"left"`
	Right    string `json:"right,omitempty"`
	Result   string `json:"result"`
	Behavior string `json:"behavior"`
}

// ExportedOperator is one operator entry in the exported JSON.
type ExportedOperator struct {
	Slug         string                     `json:"slug"`
	Symbol       string                     `json:"symbol,omitempty"`
	Title        string                     `json:"title"`
	Summary      string                     `json:"summary,omitempty"`
	Syntax       string                     `json:"syntax,omitempty"`
	TypeBehavior []ExportedOperatorBehavior `json:"type_behavior,omitempty"`
}

// ExportedData is the top-level structure for the exported JSON.
type ExportedData struct {
	Version   string             `json:"version"`
	Functions []ExportedFunction `json:"functions"`
	Classes   []ExportedClass    `json:"classes"`
	Operators []ExportedOperator `json:"operators"`
	Keywords  []string           `json:"keywords"`
}

// runExportSignatures handles the --export-signatures CLI mode.
func runExportSignatures() {
	data := ExportedData{
		Version:   version,
		Functions: make([]ExportedFunction, 0, len(constants.SSLFunctionSignatures)),
		Classes:   make([]ExportedClass, 0, len(constants.SSLClassNames)),
		Operators: make([]ExportedOperator, 0, len(constants.GeneratedOperatorDetails)),
		Keywords:  constants.SSLKeywords,
	}

	// Export functions in canonical name order
	for _, name := range constants.SSLFunctionNames {
		sig, ok := constants.SSLFunctionSignatures[toLower(name)]
		if !ok {
			continue
		}

		params := make([]ExportedParameter, 0, len(sig.Parameters))
		for _, p := range sig.Parameters {
			params = append(params, ExportedParameter{
				Name:        p.Name,
				Type:        p.Type,
				Required:    p.Required,
				Description: p.Description,
			})
		}

		data.Functions = append(data.Functions, ExportedFunction{
			Name:        sig.Name,
			Description: sig.Description,
			ReturnType:  sig.ReturnType,
			Parameters:  params,
		})
	}

	// Export classes with constructors, methods, properties.
	for _, name := range constants.SSLClassNames {
		entry := ExportedClass{Name: name}
		if det, ok := constants.GeneratedClassDetails[toLower(name)]; ok {
			entry.Summary = det.Summary
			entry.BaseClass = det.BaseClass
			entry.Constructors = make([]ExportedConstructor, 0, len(det.Constructors))
			for _, c := range det.Constructors {
				params := make([]ExportedParameter, 0, len(c.Parameters))
				for _, p := range c.Parameters {
					params = append(params, ExportedParameter{
						Name:        p.Name,
						Type:        p.Type,
						Required:    p.Required,
						Description: p.Description,
					})
				}
				entry.Constructors = append(entry.Constructors, ExportedConstructor{
					Signature:   c.Signature,
					Description: c.Description,
					Parameters:  params,
				})
			}
			for _, p := range det.Properties {
				entry.Properties = append(entry.Properties, ExportedClassMember{
					Name:        p.Name,
					Type:        p.Type,
					Access:      p.Access,
					Description: p.Description,
				})
			}
			for _, m := range det.Methods {
				entry.Methods = append(entry.Methods, ExportedClassMember{
					Name:        m.Name,
					Returns:     m.Returns,
					Description: m.Description,
				})
			}
		}
		data.Classes = append(data.Classes, entry)
	}

	// Export operators with type-behavior tables. Sort by slug for determinism.
	slugs := make([]string, 0, len(constants.GeneratedOperatorDetails))
	for slug := range constants.GeneratedOperatorDetails {
		slugs = append(slugs, slug)
	}
	sort.Strings(slugs)
	for _, slug := range slugs {
		det := constants.GeneratedOperatorDetails[slug]
		entry := ExportedOperator{
			Slug:    slug,
			Symbol:  det.Symbol,
			Title:   det.Title,
			Summary: det.Summary,
			Syntax:  det.Syntax,
		}
		for _, row := range det.TypeBehavior {
			entry.TypeBehavior = append(entry.TypeBehavior, ExportedOperatorBehavior{
				Left:     row.Left,
				Right:    row.Right,
				Result:   row.Result,
				Behavior: row.Behavior,
			})
		}
		data.Operators = append(data.Operators, entry)
	}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	if err := encoder.Encode(data); err != nil {
		fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
		os.Exit(1)
	}
}

func toLower(s string) string {
	b := make([]byte, len(s))
	for i := range s {
		c := s[i]
		if c >= 'A' && c <= 'Z' {
			c += 'a' - 'A'
		}
		b[i] = c
	}
	return string(b)
}
