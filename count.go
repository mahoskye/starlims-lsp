package main

import (
	"fmt"
	"starlims-lsp/internal/constants"
)

func main() {
	fmt.Printf("Functions: %d\n", len(constants.SSLFunctionNames))
	fmt.Printf("Classes: %d\n", len(constants.SSLClassNames))
}
