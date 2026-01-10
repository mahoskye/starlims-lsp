package providers

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/constants"
)

type functionDoc struct {
	Label         string
	Detail        string
	Documentation string
	Parameters    []ParameterInformation
}

func buildFunctionDoc(sig constants.FunctionSignature) functionDoc {
	detailParts := make([]string, 0, len(sig.Parameters))
	signatureParts := make([]string, 0, len(sig.Parameters))
	params := make([]ParameterInformation, 0, len(sig.Parameters))

	for _, param := range sig.Parameters {
		paramLabel := param.Name
		if param.Type != "" {
			paramLabel = fmt.Sprintf("%s: %s", param.Name, param.Type)
		}
		detailParts = append(detailParts, paramLabel)

		signatureLabel := paramLabel
		if !param.Required {
			signatureLabel = "[" + signatureLabel + "]"
		}
		signatureParts = append(signatureParts, signatureLabel)

		params = append(params, ParameterInformation{
			Label:         param.Name,
			Documentation: param.Description,
		})
	}

	detail := fmt.Sprintf("%s(%s)", sig.Name, strings.Join(detailParts, ", "))
	label := fmt.Sprintf("%s(%s)", sig.Name, strings.Join(signatureParts, ", "))
	if sig.ReturnType != "" {
		detail += " → " + sig.ReturnType
		label += " → " + sig.ReturnType
	}

	doc := sig.Description
	if sig.ReturnType != "" {
		doc += fmt.Sprintf("\n\n**Returns:** `%s`", sig.ReturnType)
	}

	return functionDoc{
		Label:         label,
		Detail:        detail,
		Documentation: doc,
		Parameters:    params,
	}
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
