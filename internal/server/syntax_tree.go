package server

// The ssl/syntaxTree request: the statement-level tree of an open
// document's current parse. Normative behavior:
// catalog/features/syntax_tree.md.

import (
	"starlims-lsp/internal/providers"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// MethodSyntaxTree is the custom request for a document's syntax tree.
const MethodSyntaxTree = "ssl/syntaxTree"

// SyntaxTreeParams are the parameters of an ssl/syntaxTree request.
type SyntaxTreeParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
}

// SyntaxTreeResult is the tree of one document version.
type SyntaxTreeResult struct {
	URI string `json:"uri"`
	// Version is the document version the tree was built from, so a client
	// can drop a tree that a later edit has made stale.
	Version int              `json:"version"`
	Nodes   []SyntaxTreeNode `json:"nodes"`
}

// SyntaxTreeNode is one node of the tree (providers.SyntaxNode on the
// wire, positions in UTF-16).
type SyntaxTreeNode struct {
	Kind        string           `json:"kind"`
	Range       protocol.Range   `json:"range"`
	Keyword     string           `json:"keyword,omitempty"`
	Name        string           `json:"name,omitempty"`
	NameRange   *protocol.Range  `json:"nameRange,omitempty"`
	HeaderRange *protocol.Range  `json:"headerRange,omitempty"`
	CloserRange *protocol.Range  `json:"closerRange,omitempty"`
	Children    []SyntaxTreeNode `json:"children,omitempty"`
}

// handleSyntaxTree answers ssl/syntaxTree. A document the client has not
// opened has no tree: the result is null.
func (s *SSLServer) handleSyntaxTree(params *SyntaxTreeParams) (*SyntaxTreeResult, error) {
	uri := canonicalURI(params.TextDocument.URI)
	content, ok := s.documents.GetDocument(uri)
	if !ok {
		return nil, nil
	}

	version := s.documentVersion[uri]
	cache := s.documents.ParseDocument(uri, version)
	tree := providers.BuildSyntaxTree(content, cache.Tokens, cache.AST)
	return &SyntaxTreeResult{
		URI:     uri,
		Version: version,
		Nodes:   syntaxTreeNodes(tree, newUTF16Mapper(content)),
	}, nil
}

// syntaxTreeNodes converts provider nodes to the wire form; the result is
// never nil, so an empty document answers with an empty list.
func syntaxTreeNodes(nodes []*providers.SyntaxNode, m *utf16Mapper) []SyntaxTreeNode {
	out := make([]SyntaxTreeNode, len(nodes))
	optional := func(r *providers.Range) *protocol.Range {
		if r == nil {
			return nil
		}
		pr := m.rng(*r)
		return &pr
	}
	for i, n := range nodes {
		out[i] = SyntaxTreeNode{
			Kind:        n.Kind,
			Range:       m.rng(n.Range),
			Keyword:     n.Keyword,
			Name:        n.Name,
			NameRange:   optional(n.NameRange),
			HeaderRange: optional(n.Header),
			CloserRange: optional(n.Closer),
		}
		if len(n.Children) > 0 {
			out[i].Children = syntaxTreeNodes(n.Children, m)
		}
	}
	return out
}
