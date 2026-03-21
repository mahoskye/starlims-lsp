package main
import (
  "fmt"
  c "starlims-lsp/internal/constants"
)
func main(){ fmt.Printf("keywords=%d functions=%d classes=%d\n", len(c.SSLKeywords), len(c.SSLFunctionNames), len(c.SSLClassNames)) }
