package providers

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/constants"
)

// functionDoc is the rendered documentation for one built-in function,
// shared by hover, signature help, and completion (issue #252: all three
// render the reference-generated signature through this one path).
type functionDoc struct {
	// Label is the primary call form with optional parameters bracketed
	// and the return type, e.g. `Left(sSource, nCount) → string`.
	Label string
	// Detail is the completion detail line (same text as Label).
	Detail string
	// Description is the function's published summary.
	Description string
	// ReturnLine is the rendered `**Returns:**` line, or "".
	ReturnLine string
	// Documentation is Description plus ReturnLine, for signature help and
	// completion.
	Documentation string
	// Parameters documents every parameter, in published order.
	Parameters []ParameterInformation
	// Forms holds one rendered call form per FunctionSignature.Forms entry,
	// or the single flat form when the function has no alternatives.
	Forms []functionForm
}

// functionForm is one call form: its label and the parameters it takes.
type functionForm struct {
	Label      string
	Parameters []ParameterInformation
}

func buildFunctionDoc(sig constants.FunctionSignature) functionDoc {
	byName := make(map[string]constants.FunctionParameter, len(sig.Parameters))
	params := make([]ParameterInformation, 0, len(sig.Parameters))
	for _, p := range sig.Parameters {
		byName[p.Name] = p
		params = append(params, ParameterInformation{
			Label:         p.Name,
			Documentation: parameterDoc(p),
		})
	}

	var forms []functionForm
	if len(sig.Forms) == 0 {
		forms = []functionForm{renderForm(sig.Name, sig.Parameters, sig.Variadic)}
	} else {
		for _, f := range sig.Forms {
			fp := make([]constants.FunctionParameter, 0, len(f.Parameters))
			for _, name := range f.Parameters {
				fp = append(fp, byName[name])
			}
			forms = append(forms, renderForm(sig.Name, fp, false))
		}
	}

	// The label shows the fullest form; Forms carries every one.
	label := forms[0].Label
	most := len(forms[0].Parameters)
	for _, f := range forms[1:] {
		if len(f.Parameters) > most {
			label, most = f.Label, len(f.Parameters)
		}
	}
	if sig.ReturnType != "" {
		label += " → " + sig.ReturnType
	}

	ret := returnLine(sig)
	doc := sig.Description
	if ret != "" {
		if doc != "" {
			doc += "\n\n"
		}
		doc += ret
	}

	return functionDoc{
		Label:         label,
		Detail:        label,
		Description:   sig.Description,
		ReturnLine:    ret,
		Documentation: doc,
		Parameters:    params,
		Forms:         forms,
	}
}

// renderForm renders `Name(p1, [p2], ...)` for one call form. A parameter
// row missing from the signature (a zero value) is skipped rather than
// rendered as an empty slot.
func renderForm(name string, params []constants.FunctionParameter, variadic bool) functionForm {
	parts := make([]string, 0, len(params)+1)
	infos := make([]ParameterInformation, 0, len(params))
	for _, p := range params {
		if p.Name == "" {
			continue
		}
		part := p.Name
		if !p.Required && !p.Conditional {
			part = "[" + part + "]"
		}
		parts = append(parts, part)
		infos = append(infos, ParameterInformation{Label: p.Name, Documentation: parameterDoc(p)})
	}
	if variadic {
		parts = append(parts, "...")
	}
	return functionForm{
		Label:      fmt.Sprintf("%s(%s)", name, strings.Join(parts, ", ")),
		Parameters: infos,
	}
}

// parameterDoc renders `*(type, optional, default: x)* description`.
func parameterDoc(p constants.FunctionParameter) string {
	var attrs []string
	if p.Type != "" {
		attrs = append(attrs, p.Type)
	}
	switch {
	case p.Conditional:
		attrs = append(attrs, "required by its call form")
	case !p.Required:
		attrs = append(attrs, "optional")
		if p.Default != "" {
			attrs = append(attrs, "default: "+p.Default)
		}
	}
	desc := strings.TrimSpace(p.Description)
	if len(attrs) == 0 {
		return desc
	}
	head := "*(" + strings.Join(attrs, ", ") + ")*"
	if desc == "" {
		return head
	}
	return head + " " + desc
}

// returnLine renders `**Returns:** `type` — description`.
func returnLine(sig constants.FunctionSignature) string {
	if sig.ReturnType == "" && sig.ReturnDescription == "" {
		return ""
	}
	line := "**Returns:**"
	if sig.ReturnType != "" {
		line += " `" + sig.ReturnType + "`"
	}
	if sig.ReturnDescription != "" {
		if sig.ReturnType != "" {
			line += " —"
		}
		line += " " + sig.ReturnDescription
	}
	return line
}

func buildFunctionSnippet(functionName string, sig constants.FunctionSignature) string {
	var snippetParams []string
	paramIdx := 1
	for _, param := range sig.Parameters {
		if param.Required {
			snippetParams = append(snippetParams, fmt.Sprintf("${%d:%s}", paramIdx, param.Name))
			paramIdx++
		}
	}

	if len(snippetParams) == 0 {
		return fmt.Sprintf("%s($0)", functionName)
	}

	return fmt.Sprintf("%s(%s)", functionName, strings.Join(snippetParams, ", "))
}
