package providers

import (
	"reflect"
	"testing"
)

func memberNames(members []ClassMember) (fields, methods []string) {
	for _, m := range members {
		if m.IsMethod {
			methods = append(methods, m.Name)
		} else {
			fields = append(fields, m.Name)
		}
	}
	return fields, methods
}

// [spec feature.completion/A4] — a class's fields are the names declared
// outside every procedure; a method's parameters and locals are not, and a
// name declared twice is one field.
func TestClassMembersFromParse(t *testing.T) {
	text := `:CLASS Counter;
:DECLARE nCount, sLabel;
:DECLARE NCOUNT;

:PROCEDURE Bump;
	:PARAMETERS nBy;
	:DECLARE nLocal;
	nLocal := nBy;
	:RETURN nCount;
:ENDPROC;

:PROCEDURE Current;
	:RETURN nCount;
:ENDPROC;`
	procedures, variables := parseText(text)
	members := ClassMembersFromParse(procedures, variables)

	fields, methods := memberNames(members)
	if !reflect.DeepEqual(fields, []string{"nCount", "sLabel"}) {
		t.Errorf("fields = %v, want [nCount sLabel]", fields)
	}
	if !reflect.DeepEqual(methods, []string{"Bump", "Current"}) {
		t.Errorf("methods = %v, want [Bump Current]", methods)
	}
	for _, m := range members {
		if m.Name == "Bump" && !reflect.DeepEqual(m.Parameters, []string{"nBy"}) {
			t.Errorf("Bump parameters = %v, want [nBy]", m.Parameters)
		}
	}
	if got := ClassFieldNames(procedures, variables); !reflect.DeepEqual(got, []string{"nCount", "sLabel"}) {
		t.Errorf("ClassFieldNames = %v, want [nCount sLabel]", got)
	}
}

// [spec feature.completion/A16] — one item per name from the first source
// that declares it; `Constructor` only after Base:, and only an inherited
// one.
func TestGetUserClassMemberCompletions_Precedence(t *testing.T) {
	method := func(name string, params ...string) ClassMember {
		return ClassMember{Name: name, IsMethod: true, Parameters: params}
	}
	own := ClassMemberSource{Origin: ClassMemberOwn, Name: "Child", Members: []ClassMember{
		{Name: "nCount"}, method("Constructor"), method("Bump", "nBy"),
	}}
	included := ClassMemberSource{Origin: ClassMemberIncluded, Name: "Lab.Helpers", Members: []ClassMember{
		method("Normalize", "sText"), method("BUMP"),
	}}
	parent := ClassMemberSource{Origin: ClassMemberInherited, Name: "Lab.Parent", Members: []ClassMember{
		{Name: "sName"}, method("Constructor", "sInitial"), method("Bump", "nStep"), method("normalize"),
	}}

	labels := func(items []CompletionItem) []string {
		var out []string
		for _, item := range items {
			out = append(out, item.Label+"|"+item.Detail)
		}
		return out
	}

	me := GetUserClassMemberCompletions([]ClassMemberSource{own, included, parent}, false)
	wantMe := []string{
		"nCount|Child field",
		"Bump|Child method",
		"Normalize|Lab.Helpers procedure (included)",
		"sName|Lab.Parent field (inherited)",
	}
	if got := labels(me); !reflect.DeepEqual(got, wantMe) {
		t.Errorf("Me: items\n got %v\nwant %v", got, wantMe)
	}

	base := GetUserClassMemberCompletions([]ClassMemberSource{parent}, true)
	wantBase := []string{
		"sName|Lab.Parent field (inherited)",
		"Constructor|Lab.Parent method (inherited)",
		"Bump|Lab.Parent method (inherited)",
		"normalize|Lab.Parent method (inherited)",
	}
	if got := labels(base); !reflect.DeepEqual(got, wantBase) {
		t.Errorf("Base: items\n got %v\nwant %v", got, wantBase)
	}
	for _, item := range base {
		if item.Label == "Constructor" && item.InsertText != "Constructor(${1:sInitial})" {
			t.Errorf("Constructor inserts %q", item.InsertText)
		}
	}

	if items := GetUserClassMemberCompletions(nil, false); items != nil {
		t.Errorf("no source must give nil, got %v", items)
	}
	onlyConstructor := ClassMemberSource{Origin: ClassMemberOwn, Name: "Empty", Members: []ClassMember{method("Constructor")}}
	if items := GetUserClassMemberCompletions([]ClassMemberSource{onlyConstructor}, false); items != nil {
		t.Errorf("a class with only a constructor offers nothing after Me:, got %v", items)
	}
}
