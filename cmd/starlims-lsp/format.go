package main

import (
	"fmt"
	"io"
	"os"

	"starlims-lsp/internal/providers"
)

// formatCLIOptions holds the flags of the --format mode (issue #100).
type formatCLIOptions struct {
	stdin bool
	write bool
	check bool
	fmt   providers.FormattingOptions
	files []string
}

// runFormat handles the --format CLI mode: stdout (default), --write
// (in place), or --check (exit 1 when files need formatting).
func runFormat(args []string) {
	opts, err := parseFormatArgs(args)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		fmt.Fprintln(os.Stderr, "Run 'starlims-lsp --format --help' for usage")
		os.Exit(2)
	}

	if opts.stdin {
		content, err := io.ReadAll(os.Stdin)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error reading stdin: %v\n", err)
			os.Exit(1)
		}
		formatted := formatContent(string(content), opts.fmt)
		if opts.check {
			if formatted != string(content) {
				fmt.Fprintln(os.Stderr, "<stdin> is not formatted")
				os.Exit(1)
			}
			return
		}
		fmt.Print(formatted)
		return
	}

	hasErrors := false
	needsFormat := []string{}
	for i, filePath := range opts.files {
		content, err := os.ReadFile(filePath)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error reading %s: %v\n", filePath, err)
			hasErrors = true
			continue
		}
		formatted := formatContent(string(content), opts.fmt)

		switch {
		case opts.check:
			if formatted != string(content) {
				needsFormat = append(needsFormat, filePath)
			}
		case opts.write:
			if formatted != string(content) {
				if err := os.WriteFile(filePath, []byte(formatted), 0o644); err != nil {
					fmt.Fprintf(os.Stderr, "Error writing %s: %v\n", filePath, err)
					hasErrors = true
					continue
				}
				fmt.Fprintf(os.Stderr, "formatted %s\n", filePath)
			}
		default:
			if i > 0 {
				fmt.Println()
			}
			if len(opts.files) > 1 {
				fmt.Fprintf(os.Stderr, "==> %s <==\n", filePath)
			}
			fmt.Print(formatted)
		}
	}

	if opts.check && len(needsFormat) > 0 {
		for _, f := range needsFormat {
			fmt.Fprintf(os.Stderr, "%s is not formatted\n", f)
		}
		os.Exit(1)
	}
	if hasErrors {
		os.Exit(1)
	}
}

// parseFormatArgs parses --format's flags and file arguments.
func parseFormatArgs(args []string) (formatCLIOptions, error) {
	opts := formatCLIOptions{fmt: providers.DefaultFormattingOptions()}

	i := 0
	next := func(flag string) (string, error) {
		i++
		if i >= len(args) {
			return "", fmt.Errorf("%s requires a value", flag)
		}
		return args[i], nil
	}

	for ; i < len(args); i++ {
		arg := args[i]
		switch arg {
		case "--help", "-h":
			printFormatHelp()
			os.Exit(0)
		case "--stdin":
			opts.stdin = true
		case "--write", "-w":
			opts.write = true
		case "--check":
			opts.check = true
		case "--indent-style":
			v, err := next(arg)
			if err != nil {
				return opts, err
			}
			if v != "tab" && v != "space" {
				return opts, fmt.Errorf("--indent-style must be 'tab' or 'space'")
			}
			opts.fmt.IndentStyle = v
		case "--indent-size":
			v, err := next(arg)
			if err != nil {
				return opts, err
			}
			n := 0
			if _, err := fmt.Sscanf(v, "%d", &n); err != nil || n < 1 {
				return opts, fmt.Errorf("--indent-size must be a positive integer")
			}
			opts.fmt.IndentSize = n
		case "--max-line-length":
			v, err := next(arg)
			if err != nil {
				return opts, err
			}
			n := -1
			if _, err := fmt.Sscanf(v, "%d", &n); err != nil || n < 0 {
				return opts, fmt.Errorf("--max-line-length must be a non-negative integer (0 disables wrapping)")
			}
			opts.fmt.MaxLineLength = n
		case "--no-sql":
			opts.fmt.SQL.Enabled = false
		default:
			if len(arg) > 1 && arg[0] == '-' {
				return opts, fmt.Errorf("unknown flag %q", arg)
			}
			opts.files = append(opts.files, arg)
		}
	}

	if opts.write && opts.check {
		return opts, fmt.Errorf("--write and --check cannot be combined")
	}
	if opts.stdin && opts.write {
		return opts, fmt.Errorf("--stdin cannot be combined with --write")
	}
	if opts.stdin && len(opts.files) > 0 {
		return opts, fmt.Errorf("--stdin cannot be combined with file arguments")
	}
	if !opts.stdin && len(opts.files) == 0 {
		return opts, fmt.Errorf("no input files specified")
	}
	return opts, nil
}

func printFormatHelp() {
	fmt.Println("starlims-lsp --format - Format SSL source code")
	fmt.Println()
	fmt.Println("Usage:")
	fmt.Println("  starlims-lsp --format [flags] <file.ssl> [file2.ssl ...]")
	fmt.Println("  starlims-lsp --format [flags] --stdin")
	fmt.Println("  cat script.ssl | starlims-lsp --format --stdin")
	fmt.Println()
	fmt.Println("Modes:")
	fmt.Println("  (default)   Write formatted output to stdout")
	fmt.Println("  --write     Rewrite files in place (reports changed files on stderr)")
	fmt.Println("  --check     Write nothing; exit 1 listing files that need formatting")
	fmt.Println()
	fmt.Println("Flags:")
	fmt.Println("  --stdin                Read from stdin instead of files")
	fmt.Println("  --indent-style <v>     'tab' (default) or 'space'")
	fmt.Println("  --indent-size <n>      Spaces per level in space mode; tab width for line accounting (default 4)")
	fmt.Println("  --max-line-length <n>  Wrap limit in columns; 0 disables (default 90)")
	fmt.Println("  --no-sql               Disable embedded SQL formatting")
	fmt.Println("  --help                 Print this help message")
	fmt.Println()
	fmt.Println("Documents whose content is SQL-mode data-source content (plain SQL,")
	fmt.Println("or builder directives followed by SQL) pass through unchanged.")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  starlims-lsp --format --write *.ssl")
	fmt.Println("  starlims-lsp --format --check *.ssl        # CI gate")
	fmt.Println("  starlims-lsp --format --indent-style space --stdin < script.ssl")
}

func formatContent(content string, opts providers.FormattingOptions) string {
	// The CLI has no file-type context, so it gates on content alone: a
	// document that classifies as SQL-mode data-source content (plain SQL,
	// or builder directives followed by SQL) passes through unchanged —
	// the SSL formatter would corrupt it (issues #84/#104).
	if providers.IsSQLModeDataSource(content) {
		return content
	}

	edits := providers.FormatDocument(content, opts)
	if len(edits) > 0 {
		return edits[0].NewText
	}
	return content
}
