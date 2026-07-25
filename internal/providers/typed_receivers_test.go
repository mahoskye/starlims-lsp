package providers

import (
	"strings"
	"testing"
)

// Issue #123 piece D2: typed-receiver inference over producer chains.

func TestBuildTypedReceivers_ConstructorLiteral(t *testing.T) {
	src := `:PROCEDURE Demo;
:DECLARE oMail;
oMail := Email{};
:ENDPROC;`
	typed := BuildTypedReceivers(tokenize(t, src), false)
	if got := typed["omail"]; got != "Email" {
		t.Errorf("oMail type = %q, want Email", got)
	}
}

func TestBuildTypedReceivers_ProducerChainFromClassLiteral(t *testing.T) {
	src := `:PROCEDURE Demo;
:DECLARE oClient, oResp;
oClient := WebServices{}:CreateHttpClient();
oResp := WebServices{}:CreateHttpClient():GetResponse();
:ENDPROC;`
	typed := BuildTypedReceivers(tokenize(t, src), false)
	if got := typed["oclient"]; got != "HttpClient" {
		t.Errorf("oClient type = %q, want HttpClient", got)
	}
	if got := typed["oresp"]; got != "HttpResponse" {
		t.Errorf("oResp type = %q, want HttpResponse (chained producer)", got)
	}
}

func TestBuildTypedReceivers_HopThroughTypedVariable(t *testing.T) {
	src := `:PROCEDURE Demo;
:DECLARE oClient, oReq, oResp, oErr;
oClient := WebServices{}:CreateHttpClient();
oReq := oClient:CreateHttpRequest("GET", sUrl);
oResp := oClient:GetResponse();
oErr := oClient:GetLastServerException();
:ENDPROC;`
	typed := BuildTypedReceivers(tokenize(t, src), false)
	for varName, want := range map[string]string{
		"oreq":  "HttpRequest",
		"oresp": "HttpResponse",
		"oerr":  "HttpException",
	} {
		if got := typed[varName]; got != want {
			t.Errorf("%s type = %q, want %q", varName, got, want)
		}
	}
}

func TestBuildTypedReceivers_SoapChain(t *testing.T) {
	src := `:PROCEDURE Demo;
:DECLARE oSoap, oResult;
oSoap := WebServices{}:CreateSoapClient();
oResult := oSoap:CallWebService(sMethod, oParams);
:ENDPROC;`
	typed := BuildTypedReceivers(tokenize(t, src), false)
	if got := typed["osoap"]; got != "SoapClient" {
		t.Errorf("oSoap type = %q, want SoapClient", got)
	}
	if got := typed["oresult"]; got != "SoapResponse" {
		t.Errorf("oResult type = %q, want SoapResponse", got)
	}
}

func TestBuildTypedReceivers_BuiltinFunctionReturn(t *testing.T) {
	src := `:PROCEDURE Demo;
:DECLARE oConn;
oConn := GetConnectionByName("LIMSProd");
:ENDPROC;`
	typed := BuildTypedReceivers(tokenize(t, src), false)
	if got := typed["oconn"]; got != "SQLConnection" {
		t.Errorf("oConn type = %q, want SQLConnection", got)
	}
}

func TestBuildTypedReceivers_ReassignmentLastWriteWins(t *testing.T) {
	src := `:PROCEDURE Demo;
:DECLARE oThing;
oThing := WebServices{}:CreateHttpClient();
oThing := WebServices{}:CreateSoapClient();
:ENDPROC;`
	typed := BuildTypedReceivers(tokenize(t, src), false)
	if got := typed["othing"]; got != "SoapClient" {
		t.Errorf("oThing type = %q, want SoapClient (last write wins)", got)
	}
}

func TestBuildTypedReceivers_ScalarReturnIsNotTyped(t *testing.T) {
	src := `:PROCEDURE Demo;
:DECLARE oClient, sText;
oClient := WebServices{}:CreateHttpClient();
sText := oClient:GetText();
:ENDPROC;`
	typed := BuildTypedReceivers(tokenize(t, src), false)
	if got, ok := typed["stext"]; ok {
		t.Errorf("sText should not be typed (GetText returns string), got %q", got)
	}
}

func TestBuildTypedReceivers_AmbientRequiresEndpointMode(t *testing.T) {
	src := `:PROCEDURE Demo;
:DECLARE sPath;
sPath := Request:SaveInputStream(sDir);
:ENDPROC;`
	// SaveInputStream returns string — never a typed var — but the ambient
	// gate itself is what AmbientReceiverType pins:
	if got := AmbientReceiverType("Request", true); got != "SSLRequest" {
		t.Errorf("AmbientReceiverType(Request, endpoint) = %q, want SSLRequest", got)
	}
	if got := AmbientReceiverType("Response", true); got != "SSLResponse" {
		t.Errorf("AmbientReceiverType(Response, endpoint) = %q, want SSLResponse", got)
	}
	if got := AmbientReceiverType("Request", false); got != "" {
		t.Errorf("AmbientReceiverType(Request, non-endpoint) = %q, want empty", got)
	}
	if got := AmbientReceiverType("oOther", true); got != "" {
		t.Errorf("AmbientReceiverType(oOther, endpoint) = %q, want empty", got)
	}
	// And the token pass must not bind anything for a scalar ambient call.
	typed := BuildTypedReceivers(tokenize(t, src), true)
	if got, ok := typed["spath"]; ok {
		t.Errorf("sPath should not be typed, got %q", got)
	}
}

func TestGetReturnsMemberCompletions(t *testing.T) {
	items := GetReturnsMemberCompletions("SSLResponse")
	if len(items) == 0 {
		t.Fatal("expected completions for SSLResponse members")
	}
	found := false
	for _, it := range items {
		if it.Label == "Redirect" {
			found = true
			if it.InsertText != "Redirect" {
				t.Errorf("InsertText = %q, want bare method name", it.InsertText)
			}
			if !strings.Contains(it.Detail, "SSLResponse") {
				t.Errorf("Detail = %q, want it to name SSLResponse", it.Detail)
			}
		}
		if strings.Contains(it.Label, "(") {
			t.Errorf("completion label %q carries a paren signature", it.Label)
		}
	}
	if !found {
		t.Error("expected Redirect method completion for SSLResponse")
	}
	if items := GetReturnsMemberCompletions("NotAThing"); items != nil {
		t.Error("expected nil for unknown returns object")
	}
}

func TestGetTypedMemberCompletions_DispatchesByCategory(t *testing.T) {
	if items := GetTypedMemberCompletions("HttpClient"); len(items) == 0 {
		t.Error("expected completions for returns object HttpClient")
	}
	if items := GetTypedMemberCompletions("Email"); len(items) == 0 {
		t.Error("expected completions for class Email")
	}
	if items := GetTypedMemberCompletions("string"); items != nil {
		t.Error("expected nil for a scalar type name")
	}
}

func TestRenderTypedMemberHover(t *testing.T) {
	// Property on a returns object.
	md := RenderTypedMemberHover("HttpResponse", "oResp", "ContentType")
	if md == "" {
		t.Fatal("expected hover for HttpResponse property ContentType")
	}
	if !strings.Contains(md, "ContentType") || !strings.Contains(md, "HttpResponse") || !strings.Contains(md, "oResp") {
		t.Errorf("hover missing expected parts:\n%s", md)
	}

	// Method on a returns object, case-insensitive member match.
	md = RenderTypedMemberHover("HttpClient", "oClient", "getresponse")
	if md == "" {
		t.Fatal("expected hover for HttpClient method GetResponse")
	}
	if !strings.Contains(md, "HttpResponse") {
		t.Errorf("method hover should mention its return type:\n%s", md)
	}

	// Unknown member of a typed receiver renders nothing — the caller
	// answers null rather than falling through to an unrelated symbol.
	if md := RenderTypedMemberHover("HttpResponse", "oResp", "NoSuchMember"); md != "" {
		t.Errorf("expected empty hover for unknown member, got:\n%s", md)
	}
}

func TestCanonicalReceiverTypeName(t *testing.T) {
	for input, want := range map[string]string{
		"httpclient":    "HttpClient",
		"HttpClient":    "HttpClient",
		"email":         "Email",
		"sqlconnection": "SQLConnection",
		"string":        "",
		"object":        "",
		"Nope":          "",
	} {
		if got := CanonicalReceiverTypeName(input); got != want {
			t.Errorf("CanonicalReceiverTypeName(%q) = %q, want %q", input, got, want)
		}
	}
}

// Pre-v0.14.0 review finding M2: a property assignment's property token is
// not a variable assignment — `oCfg:Client := WebServices{};` must not
// bind the unrelated variable name `Client`.
func TestBuildTypedReceivers_PropertyAssignmentNotBound(t *testing.T) {
	src := `:PROCEDURE Demo;
:DECLARE oCfg;
oCfg:Client := WebServices{};
:ENDPROC;`
	typed := BuildTypedReceivers(tokenize(t, src), false)
	if got, ok := typed["client"]; ok {
		t.Errorf("property assignment bound variable Client to %q", got)
	}
}

// Pre-v0.14.0 review finding L3: the FIRST chain hop split onto its own
// line lexes as a fused ':Method' keyword and must still resolve.
func TestBuildTypedReceivers_FirstHopOnContinuationLine(t *testing.T) {
	src := ":PROCEDURE Demo;\n:DECLARE oClient, oResp;\noClient := WebServices{}:CreateHttpClient();\noResp := oClient\n    :GetResponse();\n:ENDPROC;"
	typed := BuildTypedReceivers(tokenize(t, src), false)
	if got := typed["oresp"]; got != "HttpResponse" {
		t.Errorf("oResp type = %q, want HttpResponse (fused first hop)", got)
	}
}
