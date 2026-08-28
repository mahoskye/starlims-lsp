// Command exprcoverage measures expression-AST coverage (issue #184) over
// a corpus of SSL files: what fraction of expression-bearing statements
// parse Complete (every significant token inside a tree). Run it after
// grammar work to catch regressions and discover unhandled shapes.
//
// Usage: go run ./dev/exprcoverage file1.ssl file2.ssl ...
// (or xargs -a filelist.txt go run ./dev/exprcoverage)
package main

import (
	"fmt"
	"os"
	"sort"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

func main() {
	var total, complete int
	type miss struct {
		file string
		line int
		text string
	}
	var misses []miss

	for _, path := range os.Args[1:] {
		data, err := os.ReadFile(path)
		if err != nil {
			fmt.Fprintf(os.Stderr, "skip %s: %v\n", path, err)
			continue
		}
		tokens := lexer.NewLexer(string(data)).Tokenize()
		for _, se := range parser.ExtractStatementExpressions(tokens) {
			total++
			if se.Complete {
				complete++
				continue
			}
			t := tokens[se.Start]
			text := ""
			for i := se.Start; i <= se.End && i < len(tokens); i++ {
				text += tokens[i].Text
				if len(text) > 100 {
					break
				}
			}
			misses = append(misses, miss{path, t.Line + 1, text})
		}
	}

	fmt.Printf("statements with expressions: %d\n", total)
	fmt.Printf("parsed complete:             %d (%.2f%%)\n", complete, 100*float64(complete)/float64(max(total, 1)))
	fmt.Printf("incomplete:                  %d\n", len(misses))

	sort.Slice(misses, func(i, j int) bool { return misses[i].file < misses[j].file })
	limit := 600
	if len(misses) < limit {
		limit = len(misses)
	}
	for _, m := range misses[:limit] {
		fmt.Printf("  %s:%d  %s\n", m.file, m.line, m.text)
	}
	if len(misses) > limit {
		fmt.Printf("  ... and %d more\n", len(misses)-limit)
	}
}
