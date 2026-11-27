package providers

import (
	"strings"
	"testing"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// [spec diag.iif_guard_ineffective] Each line is one IIf call. The first
// block must be reported, the second must not.
func TestIIfGuardIneffective(t *testing.T) {
	flagged := []string{
		`IIf(Empty(aRows), 0, aRows[1, 1])`,
		`IIf(!Empty(aRows), aRows[1, 1], -1)`,
		`IIf(.NOT. Empty(aRows), aRows[1], -1)`,
		`IIf(Len(aRows) > 0, aRows[1], NIL)`,
		`IIf(Len(aRows) >= 1, aRows[1], NIL)`,
		`IIf(Len(aRows) != 0, aRows[1], NIL)`,
		`IIf(Len(aRows) == 2, aRows[2], NIL)`,
		`IIf(Len(aRows) < 1, "none", aRows[1])`,
		`IIf(Len(aRows) <= 0, "none", aRows[1])`,
		`IIf(Len(aRows) == 0, "none", aRows[1])`,
		`IIf(Len(aRows) = 0, "none", aRows[1])`,
		`IIf(Len(aRows) != 1, "none", aRows[1])`,
		`IIf(0 < ALen(aRows), aRows[1], "")`,
		`IIf(1 > Len(aRows), "", aRows[1])`,
		`iif(len(AROWS) > 0, arows[1], NIL)`,
		`IIf((Len(aRows) > 0), (aRows)[1], NIL)`,
		`IIf(Len(aRows) > 0 .AND. bAny, aRows[1], NIL)`,
		`IIf(Empty(aRows) .OR. bAny, "", aRows[1])`,
		`IIf(Empty(aRows), "x", Upper(aRows[1, 2]) + "/" + aRows[1, 1])`,
		`IIf(Len(aParts[3]) == 1, aParts[3][1], "many")`,
		`IIf(Empty(aParts[3]), "", aParts[3, 1])`,
		`IIf(Empty(Me:aRows), 0, Me:aRows[1])`,
		`IIf(Len(aRows) > 0, IIf(bAny, aRows[1], aRows[2]), NIL)`,
		`IIf(aRows != NIL, aRows[1], "")`,
		`IIf(oOwner == NIL, "Unassigned", oOwner:displayName)`,
		`IIf(oOwner = NIL, "Unassigned", oOwner:displayName)`,
		`IIf(NIL != oOwner, oOwner:GetName(), "")`,
		`IIf(oOwner <> NIL, oOwner:GetName(), "")`,
		`IIf(vThing # NIL, vThing:value, "")`,
		`IIf(Empty(oOwner), "none", oOwner:value)`,
		`IIf(Empty(Me:oClient), "none", Me:oClient:name)`,
	}
	silent := []string{
		`IIf(Len(aRows) > 1, "many", aRows[1])`,
		`IIf(Empty(aRows[1]), "", aRows[1])`,
		`IIf(Empty(aRows), aRows[1], 0)`,
		`IIf(Len(aRows) >= 0, aRows[1], NIL)`,
		`IIf(Len(aRows) < 0, "", aRows[1])`,
		`IIf(Len(aRows) > nMin, aRows[1], NIL)`,
		`IIf(Len(aRows) > 0 .OR. bAny, aRows[1], NIL)`,
		`IIf(Empty(aRows) .AND. bAny, "", aRows[1])`,
		`IIf(Len(aRows) > 0, Len(aRows) > 0 .AND. aRows[1] == 1, .F.)`,
		`IIf(Empty(aRows), .F., Empty(aRows) .OR. aRows[1] == 1)`,
		`IIf(Empty(aRows), 0, Len(aRows))`,
		`IIf(Empty(aRows), 0, aOther[1])`,
		`IIf(Empty(BuildRows()), 0, BuildRows()[1])`,
		`IIf(Empty(aRows), 0, {|x| aRows[1]})`,
		`IIf(Empty(sName), "", sName:Trim())`,
		`IIf(Empty(vThing), "", vThing:value)`,
		`IIf(Len(aRows) > 0, aRows:Count, 0)`,
		`IIf(oOwner == NIL, oOwner:displayName, "")`,
		`IIf(NIL == NIL, 1, aRows[1])`,
		`oOwner:IIf(Empty(aRows), 0, aRows[1])`,
		`IIf(Empty(aRows), aRows[1])`,
		`IIf(Empty(aRows),, aRows[1], 0)`,
	}

	build := func(calls []string) string {
		var b strings.Builder
		b.WriteString(":PROCEDURE Probe;\n:PARAMETERS aRows, aOther, aParts, oOwner, sName, vThing, bAny, nMin;\n:DECLARE vOut;\n")
		for _, call := range calls {
			b.WriteString("vOut := " + call + ";\n")
		}
		b.WriteString(":RETURN vOut;\n:ENDPROC;\n")
		return b.String()
	}

	got := codesAt(t, CodeIIfGuardIneffective, build(flagged))
	seen := map[int]bool{}
	for _, line := range got {
		seen[line] = true
	}
	for i, call := range flagged {
		if !seen[i+3] {
			t.Errorf("not reported: %s", call)
		}
	}
	if len(got) != len(flagged) {
		t.Errorf("want one finding per call (%d), got %d: %v", len(flagged), len(got), got)
	}

	for _, line := range codesAt(t, CodeIIfGuardIneffective, build(silent)) {
		t.Errorf("reported, and must not be: %s", silent[line-3])
	}
}

// The finding is a warning on the read, and its message names the read,
// the subject and the state the test was for.
func TestIIfGuardIneffectiveRangeAndMessage(t *testing.T) {
	cases := []struct {
		script   string
		start    int
		end      int
		contains []string
	}{
		{
			"vOut := IIf(Empty(aRows), 0, aRows[1, 1]);",
			29, 40,
			[]string{"aRows[1, 1] is read even when aRows is empty", ":IF"},
		},
		{
			"vOut := IIf(oOwner == NIL, \"\", oOwner:GetName());",
			31, 45,
			[]string{"oOwner:GetName is read even when oOwner is NIL"},
		},
		{
			"vOut := IIf(Empty(aRows), \"\", Upper(aRows[ 1 ,\n\t2 ]));",
			36, 4,
			[]string{"aRows[ 1 , 2 ] is read even when aRows is empty"},
		},
		{
			"vOut := IIf(Empty(aParts[3]), \"\", aParts[3, 1]);",
			34, 46,
			[]string{"aParts[3, 1] is read even when aParts[3] is empty"},
		},
		{
			"vOut := IIf(Len(aParts[3]) == 1, aParts[3][1], \"\");",
			33, 45,
			[]string{"aParts[3][1] is read even when aParts[3] is empty"},
		},
	}
	for _, c := range cases {
		var found []Diagnostic
		for _, d := range GetDiagnostics(c.script, DefaultDiagnosticOptions()) {
			if d.Code == CodeIIfGuardIneffective {
				found = append(found, d)
			}
		}
		if len(found) != 1 {
			t.Errorf("%q: want one finding, got %d", c.script, len(found))
			continue
		}
		d := found[0]
		if d.Severity != SeverityWarning {
			t.Errorf("%q: severity = %v, want warning", c.script, d.Severity)
		}
		if d.Range.Start.Line != 0 || d.Range.Start.Character != c.start || d.Range.End.Character != c.end {
			t.Errorf("%q: range = %+v, want it to start at 0:%d and end at character %d", c.script, d.Range, c.start, c.end)
		}
		for _, want := range c.contains {
			if !strings.Contains(d.Message, want) {
				t.Errorf("%q: message %q does not contain %q", c.script, d.Message, want)
			}
		}
	}
}

// Two value arguments can each hold an unprotected read; a read is
// reported once however many IIf calls enclose it.
func TestIIfGuardIneffectiveCounts(t *testing.T) {
	both := "vOut := IIf(Empty(aRows) .OR. oOwner != NIL, oOwner:name, aRows[1]);"
	if got := len(codesAt(t, CodeIIfGuardIneffective, both)); got != 1 {
		// True proves nothing (a disjunction); false proves aRows has an
		// element, so only the third argument is protected.
		t.Errorf("disjunction: want 1 finding, got %d", got)
	}

	nested := "vOut := IIf(Len(aRows) > 0, IIf(Len(aRows) > 1, aRows[2], aRows[1]), NIL);"
	if got := len(codesAt(t, CodeIIfGuardIneffective, nested)); got != 1 {
		t.Errorf("nested: the same read reported by both calls, got %d findings", got)
	}
}

// conditionProofs: what each outcome proves, for the shapes the rule reads.
func TestConditionProofs(t *testing.T) {
	cases := []struct {
		cond      string
		whenTrue  string
		whenFalse string
	}{
		{`Empty(aRows)`, "", "arows=sized"},
		{`Empty(oOwner)`, "", "oowner=sized+notnil"},
		{`!Empty(aRows)`, "arows=sized", ""},
		{`Len(aRows) > 3`, "arows=sized", ""},
		{`Len(aRows) <= 3`, "", "arows=sized"},
		{`Len(aRows) == 0`, "", "arows=sized"},
		{`Len(aRows) != 1`, "", "arows=sized"},
		{`2 <= Len(aRows)`, "arows=sized", ""},
		{`oOwner != NIL`, "oowner=notnil", ""},
		{`NIL == oOwner`, "", "oowner=notnil"},
		{`Len(aRows) > 0 .AND. oOwner != NIL`, "arows=sized oowner=notnil", ""},
		{`Empty(aRows) .OR. oOwner == NIL`, "", "arows=sized oowner=notnil"},
		{`Empty(aRows) .AND. Empty(aRows)`, "", "arows=sized"},
		{`.NOT. (Empty(aRows) .OR. bAny)`, "arows=sized", ""},
		{`bAny`, "", ""},
		{`Len(aRows) > nMin`, "", ""},
		{`Empty(GetRows())`, "", ""},
	}
	for _, c := range cases {
		tokens := lexer.NewLexer("vOut := IIf(" + c.cond + ", 1, 2);").Tokenize()
		calls := parser.CollectCalls(tokens)
		var cond *parser.Expr
		for _, call := range calls {
			if strings.EqualFold(call.Name, "IIf") && len(call.Args) == 3 {
				cond = call.Args[0]
			}
		}
		if cond == nil {
			t.Errorf("%q: no IIf call parsed", c.cond)
			continue
		}
		whenTrue, whenFalse := conditionProofs(cond)
		if got := describeProofs(whenTrue); got != c.whenTrue {
			t.Errorf("%q when true: got %q, want %q", c.cond, got, c.whenTrue)
		}
		if got := describeProofs(whenFalse); got != c.whenFalse {
			t.Errorf("%q when false: got %q, want %q", c.cond, got, c.whenFalse)
		}
	}
}

func describeProofs(proofs iifProofs) string {
	var keys []string
	for key := range proofs {
		keys = append(keys, key)
	}
	// Small and test-only: an insertion sort keeps the output stable.
	for i := 1; i < len(keys); i++ {
		for j := i; j > 0 && keys[j] < keys[j-1]; j-- {
			keys[j], keys[j-1] = keys[j-1], keys[j]
		}
	}
	var parts []string
	for _, key := range keys {
		var kinds []string
		if proofs[key]&proofSized != 0 {
			kinds = append(kinds, "sized")
		}
		if proofs[key]&proofNotNil != 0 {
			kinds = append(kinds, "notnil")
		}
		parts = append(parts, key+"="+strings.Join(kinds, "+"))
	}
	return strings.Join(parts, " ")
}
