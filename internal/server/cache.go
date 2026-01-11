package server

import (
	"sync"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// DocumentCache stores parsed document information.
type DocumentCache struct {
	Version    int
	Tokens     []lexer.Token
	AST        *parser.Node
	Procedures []parser.ProcedureInfo
	Variables  []parser.VariableInfo
}

// DocumentManager manages documents and their caches.
type DocumentManager struct {
	documents map[string]string         // URI -> content
	cache     map[string]*DocumentCache // URI -> cache
	mu        sync.RWMutex
}

// NewDocumentManager creates a new DocumentManager.
func NewDocumentManager() *DocumentManager {
	return &DocumentManager{
		documents: make(map[string]string),
		cache:     make(map[string]*DocumentCache),
	}
}

// SetDocument stores or updates a document.
func (dm *DocumentManager) SetDocument(uri string, content string, version int) {
	dm.mu.Lock()
	defer dm.mu.Unlock()

	dm.documents[uri] = content

	// Invalidate cache if version changed
	if cached, ok := dm.cache[uri]; ok && cached.Version != version {
		delete(dm.cache, uri)
	}
}

// GetDocument returns the content of a document.
func (dm *DocumentManager) GetDocument(uri string) (string, bool) {
	dm.mu.RLock()
	defer dm.mu.RUnlock()

	content, ok := dm.documents[uri]
	return content, ok
}

// RemoveDocument removes a document and its cache.
func (dm *DocumentManager) RemoveDocument(uri string) {
	dm.mu.Lock()
	defer dm.mu.Unlock()

	delete(dm.documents, uri)
	delete(dm.cache, uri)
}

// GetCache returns the cached parse results for a document.
func (dm *DocumentManager) GetCache(uri string, version int) (*DocumentCache, bool) {
	dm.mu.RLock()
	cached, ok := dm.cache[uri]
	dm.mu.RUnlock()

	if ok && cached.Version == version {
		return cached, true
	}
	return nil, false
}

// SetCache stores parsed results for a document.
func (dm *DocumentManager) SetCache(uri string, cache *DocumentCache) {
	dm.mu.Lock()
	defer dm.mu.Unlock()

	dm.cache[uri] = cache
}

// ParseDocument parses a document and returns cached results.
func (dm *DocumentManager) ParseDocument(uri string, version int) *DocumentCache {
	// Check cache first
	if cached, ok := dm.GetCache(uri, version); ok {
		return cached
	}

	// Get document content
	content, ok := dm.GetDocument(uri)
	if !ok {
		return &DocumentCache{Version: version}
	}

	// Parse
	lex := lexer.NewLexer(content)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()

	cache := &DocumentCache{
		Version:    version,
		Tokens:     tokens,
		AST:        ast,
		Procedures: p.ExtractProcedures(ast),
		Variables:  p.ExtractVariables(ast),
	}

	dm.SetCache(uri, cache)
	return cache
}

// AllDocuments returns all document URIs.
func (dm *DocumentManager) AllDocuments() []string {
	dm.mu.RLock()
	defer dm.mu.RUnlock()

	uris := make([]string, 0, len(dm.documents))
	for uri := range dm.documents {
		uris = append(uris, uri)
	}
	return uris
}
