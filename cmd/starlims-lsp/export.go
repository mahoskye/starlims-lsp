package main

import (
	"encoding/json"
	"fmt"
	"os"

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

// ExportedData is the top-level structure for the exported JSON.
type ExportedData struct {
	Version   string             `json:"version"`
	Functions []ExportedFunction `json:"functions"`
	Classes   []string           `json:"classes"`
	Keywords  []string           `json:"keywords"`
}

// runExportSignatures handles the --export-signatures CLI mode.
func runExportSignatures() {
	data := ExportedData{
		Version:   version,
		Functions: make([]ExportedFunction, 0, len(constants.SSLFunctionSignatures)),
		Classes:   constants.SSLClassNames,
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
