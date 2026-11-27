package server

import (
	"starlims-lsp/internal/providers"
)

// Class members for `Me:` and `Base:` completion (issue #380). Normative
// behavior: catalog/features/completion.md, "Class members".

// classFileView is what member completion reads of one class file: its
// members and the parent it inherits. An open document is read from its
// buffer, any other file from the index.
type classFileView struct {
	isClass bool
	members []providers.ClassMember
	inherit string // :INHERIT target as written; "" when the file has none
}

// classMemberSources returns, in order of precedence, the files whose
// members `Me:` reaches from the class this document declares: the class
// itself, the scripts it includes, then its ancestors. For `Base:` (base
// true) it returns the ancestors only.
func (s *SSLServer) classMemberSources(uri string, cache *DocumentCache, className string, base bool) []providers.ClassMemberSource {
	var sources []providers.ClassMemberSource
	if !base {
		sources = append(sources, providers.ClassMemberSource{
			Origin:  providers.ClassMemberOwn,
			Name:    className,
			Members: providers.ClassMembersFromParse(cache.Procedures, cache.Variables),
		})
		sources = append(sources, s.includedMemberSources(uri, providers.ExtractIncludeTargets(cache.Tokens))...)
	}
	parent := inheritTarget(providers.ExtractClassSites(cache.Tokens))
	return append(sources, s.inheritedMemberSources(uri, parent)...)
}

// inheritedMemberSources walks :INHERIT up from the parent named target,
// the nearer class first. A step is taken only when the name resolves to
// exactly one class file: an unresolved or ambiguous parent ends the walk,
// as does a class already seen (D10 — a member offered from the wrong
// class does not compile).
func (s *SSLServer) inheritedMemberSources(selfURI, target string) []providers.ClassMemberSource {
	if s.workspaceIndex == nil {
		return nil
	}
	var sources []providers.ClassMemberSource
	seen := map[string]bool{selfURI: true}
	for target != "" {
		resolved := s.workspaceIndex.ResolveClassTarget(target)
		if len(resolved) != 1 || seen[resolved[0].URI] {
			break
		}
		parentURI := resolved[0].URI
		seen[parentURI] = true
		view, ok := s.classFileViewOf(parentURI)
		if !ok || !view.isClass {
			break
		}
		sources = append(sources, providers.ClassMemberSource{
			Origin:  providers.ClassMemberInherited,
			Name:    target,
			Members: view.members,
		})
		target = view.inherit
	}
	return sources
}

// includedMemberSources returns the procedures of each script the class
// file itself includes, which a method reaches as Me:Name(...). Only an
// include that resolves to exactly one file contributes, and only its
// procedures without a visibility annotation in force; what the included
// script includes in turn is not followed. Each limit is a behavior that
// has not been run (D17).
func (s *SSLServer) includedMemberSources(selfURI string, targets []string) []providers.ClassMemberSource {
	if s.workspaceIndex == nil {
		return nil
	}
	var sources []providers.ClassMemberSource
	seen := map[string]bool{selfURI: true}
	for _, target := range targets {
		resolved := s.workspaceIndex.ResolveIncludeTarget(target)
		if len(resolved) != 1 || seen[resolved[0].URI] {
			continue
		}
		seen[resolved[0].URI] = true
		view, ok := s.scriptProcedureMembers(resolved[0].URI)
		if !ok || len(view) == 0 {
			continue
		}
		sources = append(sources, providers.ClassMemberSource{
			Origin:  providers.ClassMemberIncluded,
			Name:    target,
			Members: view,
		})
	}
	return sources
}

// classFileViewOf reads a class file's members and parent: from the live
// parse when the file is open, from the index otherwise.
func (s *SSLServer) classFileViewOf(uri string) (classFileView, bool) {
	if _, open := s.documents.OpenURIs()[uri]; open {
		cache := s.documents.ParseDocument(uri, s.documentVersion[uri])
		return classFileView{
			isClass: isClassFileFromTokens(cache.Tokens),
			members: providers.ClassMembersFromParse(cache.Procedures, cache.Variables),
			inherit: inheritTarget(providers.ExtractClassSites(cache.Tokens)),
		}, true
	}
	fs, ok := s.workspaceIndex.FileSymbolsFor(uri)
	if !ok {
		return classFileView{}, false
	}
	members := make([]providers.ClassMember, 0, len(fs.ClassFields)+len(fs.Procedures))
	for _, name := range fs.ClassFields {
		members = append(members, providers.ClassMember{Name: name})
	}
	for _, proc := range fs.Procedures {
		members = append(members, indexedProcedureMember(proc))
	}
	return classFileView{
		isClass: fs.IsClass,
		members: members,
		inherit: inheritTarget(fs.ClassSites),
	}, true
}

// scriptProcedureMembers returns the procedures of the script at uri that
// carry no visibility annotation in force, as method members.
func (s *SSLServer) scriptProcedureMembers(uri string) ([]providers.ClassMember, bool) {
	if _, open := s.documents.OpenURIs()[uri]; open {
		cache := s.documents.ParseDocument(uri, s.documentVersion[uri])
		var members []providers.ClassMember
		for _, proc := range cache.Procedures {
			if proc.IsPrivate {
				continue
			}
			members = append(members, providers.ClassMember{
				Name:       proc.Name,
				IsMethod:   true,
				Parameters: proc.Parameters,
				Doc:        proc.Doc,
				Line:       proc.StartLine,
			})
		}
		return members, true
	}
	fs, ok := s.workspaceIndex.FileSymbolsFor(uri)
	if !ok {
		return nil, false
	}
	var members []providers.ClassMember
	for _, proc := range fs.Procedures {
		if proc.IsPrivate {
			continue
		}
		members = append(members, indexedProcedureMember(proc))
	}
	return members, true
}

func indexedProcedureMember(proc IndexedProcedure) providers.ClassMember {
	return providers.ClassMember{
		Name:       proc.Name,
		IsMethod:   true,
		Parameters: proc.Parameters,
		Doc:        proc.Doc,
		Line:       proc.StartLine,
	}
}

// inheritTarget returns the parent a file's :INHERIT names, as written, or
// "" when it has none.
func inheritTarget(sites []providers.ClassSite) string {
	for _, site := range sites {
		if site.Kind == providers.ClassInherit {
			return site.Raw
		}
	}
	return ""
}
