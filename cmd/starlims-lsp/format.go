package main

import (
	"errors"
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
	ds    bool // --ds: treat every input as a data source
	fmt   providers.FormattingOptions
	files []string
}

// runFormat handles the --format CLI mode: stdout (default), --write
// (in place), or --check (exit 1 when files need formatting).
func runFormat(args []string) {
	os.Exit(formatMain(args, os.Stdin, os.Stdout, os.Stderr))
}

// formatMain runs one --format invocation against the given streams and
// returns the process exit code (feature.cli_contract): 0 success, 1 an
// I/O failure or --check finding a file to change, 2 a usage error.
func formatMain(args []string, stdin io.Reader, stdout, stderr io.Writer) int {
	opts, err := parseFormatArgs(args)
	if errors.Is(err, errHelp) {
		printFormatHelp(stdout)
		return exitOK
	}
	if err != nil {
		fmt.Fprintf(stderr, "Error: %v\n", err)
		fmt.Fprintln(stderr, "Run 'starlims-lsp --format --help' for usage")
		return exitUsage
	}

	if opts.stdin {
		content, err := io.ReadAll(stdin)
		if err != nil {
			fmt.Fprintf(stderr, "Error reading stdin: %v\n", err)
			return exitFinding
		}
		formatted := formatContent(string(content), opts.fmt, opts.ds)
		if opts.check {
			if formatted != string(content) {
				fmt.Fprintln(stderr, "<stdin> is not formatted")
				return exitFinding
			}
			return exitOK
		}
		fmt.Fprint(stdout, formatted)
		return exitOK
	}

	hasErrors := false
	needsFormat := []string{}
	for i, filePath := range opts.files {
		content, err := os.ReadFile(filePath)
		if err != nil {
			fmt.Fprintf(stderr, "Error reading %s: %v\n", filePath, err)
			hasErrors = true
			continue
		}
		formatted := formatContent(string(content), opts.fmt, opts.ds || isDataSourcePath(filePath))

		switch {
		case opts.check:
			if formatted != string(content) {
				needsFormat = append(needsFormat, filePath)
			}
		case opts.write:
			if formatted != string(content) {
				if err := os.WriteFile(filePath, []byte(formatted), 0o644); err != nil {
					fmt.Fprintf(stderr, "Error writing %s: %v\n", filePath, err)
					hasErrors = true
					continue
				}
				fmt.Fprintf(stderr, "formatted %s\n", filePath)
			}
		default:
			if i > 0 {
				fmt.Fprintln(stdout)
			}
			if len(opts.files) > 1 {
				fmt.Fprintf(stderr, "==> %s <==\n", filePath)
			}
			fmt.Fprint(stdout, formatted)
		}
	}

	if opts.check && len(needsFormat) > 0 {
		for _, f := range needsFormat {
			fmt.Fprintf(stderr, "%s is not formatted\n", f)
		}
		return exitFinding
	}
	if hasErrors {
		return exitFinding
	}
	return exitOK
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
		if arg == "--" {
			// End of flags: everything after is a file path, even a
			// dash-prefixed one (feature.cli_contract).
			opts.files = append(opts.files, args[i+1:]...)
			break
		}
		switch arg {
		case "--help", "-h":
			return opts, errHelp
		case "--stdin":
			opts.stdin = true
		case "--write", "-w":
			opts.write = true
		case "--check":
			opts.check = true
		case "--ds":
			opts.ds = true
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
			if isFlagLike(arg) {
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

func printFormatHelp(w io.Writer) {
	fmt.Fprintln(w, "starlims-lsp --format - Format SSL source code")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Usage:")
	fmt.Fprintln(w, "  starlims-lsp --format [flags] <file.ssl> [file2.ssl ...]")
	fmt.Fprintln(w, "  starlims-lsp --format [flags] --stdin")
	fmt.Fprintln(w, "  cat script.ssl | starlims-lsp --format --stdin")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Modes:")
	fmt.Fprintln(w, "  (default)   Write formatted output to stdout")
	fmt.Fprintln(w, "  --write     Rewrite files in place (reports changed files on stderr)")
	fmt.Fprintln(w, "  --check     Write nothing; exit 1 listing files that need formatting")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Flags:")
	fmt.Fprintln(w, "  --stdin                Read from stdin instead of files")
	fmt.Fprintln(w, "  --indent-style <v>     'tab' (default) or 'space'")
	fmt.Fprintln(w, "  --indent-size <n>      Spaces per level in space mode; tab width for line accounting (default 4)")
	fmt.Fprintln(w, "  --max-line-length <n>  Wrap limit in columns; 0 disables (default 90)")
	fmt.Fprintln(w, "  --no-sql               Disable embedded SQL formatting")
	fmt.Fprintln(w, "  --ds                   Treat input as a data source (implied for .ds/.ds.txt files)")
	fmt.Fprintln(w, "  --help                 Print this help message")
	fmt.Fprintln(w, "  --                     End of flags: every later argument is a file path")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Any other argument starting with '-' is rejected as an unknown flag.")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Exit codes:")
	fmt.Fprintln(w, "  0 - Success (with --check: every input is already formatted)")
	fmt.Fprintln(w, "  1 - An input could not be read or written, or --check found one to change")
	fmt.Fprintln(w, "  2 - Usage error (unknown flag, bad value, conflicting modes, no inputs)")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Data sources (.ds/.ds.txt files, or any input under --ds) whose content")
	fmt.Fprintln(w, "is SQL-mode (plain SQL, or builder directives followed by SQL) pass")
	fmt.Fprintln(w, "through unchanged. Other inputs are always formatted as SSL.")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Examples:")
	fmt.Fprintln(w, "  starlims-lsp --format --write *.ssl")
	fmt.Fprintln(w, "  starlims-lsp --format --check *.ssl        # CI gate")
	fmt.Fprintln(w, "  starlims-lsp --format --indent-style space --stdin < script.ssl")
}

func formatContent(content string, opts providers.FormattingOptions, dataSource bool) string {
	// A data source (a .ds/.ds.txt path, or --ds) whose content classifies
	// as SQL-mode (plain SQL, or builder directives followed by SQL) passes
	// through unchanged — the SSL formatter would corrupt it (issues
	// #84/#104). The classifier defaults marker-free content to SQL (D15),
	// so it must only ever see known data sources: an ordinary script with
	// no colon keyword and no `:=` is still SSL (issue #255).
	if dataSource && providers.IsSQLModeDataSource(content) {
		return content
	}

	edits := providers.FormatDocument(content, opts)
	if len(edits) > 0 {
		return edits[0].NewText
	}
	return content
}
