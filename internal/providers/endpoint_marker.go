package providers

import "regexp"

// HasEndpointMarker reports whether content carries a leading-docblock
// `Endpoint:` marker in its first ~30 lines (the convention the SSL agent
// guides use), which makes `Request` and `Response` pre-injected ambients
// (DECISIONS.md D4). The editor combines it with the configured
// ssl.diagnostics.endpointPatterns; the CLI, which has no settings, uses
// the marker alone.
func HasEndpointMarker(content string) bool {
	// Scan only the leading docblock region — keep this cheap and
	// resistant to false positives from a stray "Endpoint:" appearing
	// deeper in the file (e.g. inside a string literal or comment block
	// that documents something unrelated).
	const maxLines = 30
	lines := 0
	for i := 0; i < len(content) && lines < maxLines; i++ {
		if content[i] == '\n' {
			lines++
		}
	}
	head := content
	if lines >= maxLines {
		// Truncate to the first `maxLines` lines.
		count := 0
		for i := 0; i < len(content); i++ {
			if content[i] == '\n' {
				count++
				if count >= maxLines {
					head = content[:i]
					break
				}
			}
		}
	}
	// Match `Endpoint:` on a docblock line (preceded by `*` or at line start).
	// Case-insensitive.
	return endpointMarkerRegexp.MatchString(head)
}

var endpointMarkerRegexp = regexp.MustCompile(`(?im)^[\s*]*Endpoint\s*:`)
