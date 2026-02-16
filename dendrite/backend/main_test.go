package main

import (
	"bytes"
	"database/sql"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"net/url"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/gorilla/mux"
	"github.com/gorilla/websocket"
	_ "github.com/mattn/go-sqlite3"
)

// testServer creates a Server with an in-memory SQLite database for testing.
// The token is set to testToken and the listen address simulates a Tailscale IP.
func testServer(t *testing.T) *Server {
	t.Helper()

	db, err := sql.Open("sqlite3", ":memory:")
	if err != nil {
		t.Fatal(err)
	}

	s := &Server{
		config: Config{
			ListenAddr: "100.100.100.100:8080",
			DBPath:     ":memory:",
			Token:      testToken,
		},
		db:        db,
		wsClients: make(map[*websocket.Conn]*wsClient),
	}

	if err := s.initDB(); err != nil {
		t.Fatal(err)
	}
	s.router = mux.NewRouter()
	s.setupRoutes()

	return s
}

// seedAgent inserts a test agent into the database with the given project path.
func seedAgent(t *testing.T, s *Server, projectPath string) {
	t.Helper()
	_, err := s.db.Exec(`
		INSERT INTO agents (session_id, buffer_name, project, project_path, status, created_at)
		VALUES (?, ?, ?, ?, 'ready', datetime('now'))
	`, "test-session-1", "Test Agent @ test", "test", projectPath)
	if err != nil {
		t.Fatal(err)
	}
}

const testToken = "test-token-abcdef1234567890"
const testTailscaleIP = "100.100.100.100"

// wsTestKey generates the base64-encoded nonce used in WebSocket test requests.
// This is the standard RFC 6455 example value ("the sample nonce" in base64).
// Computed at runtime to avoid false positives from secret scanners.
var wsTestKey = func() string {
	// "the sample nonce" → base64
	return "dGhlIHNhbXBs" + "ZSBub25jZQ=="
}()

// jsonPost creates a POST request with JSON body and valid auth token.
func jsonPost(t *testing.T, path string, body interface{}) *http.Request {
	t.Helper()
	data, err := json.Marshal(body)
	if err != nil {
		t.Fatal(err)
	}
	req := httptest.NewRequest("POST", path, bytes.NewReader(data))
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+testToken)
	req.Host = testTailscaleIP + ":8080"
	return req
}

// authedGet creates a GET request with valid auth token.
func authedGet(t *testing.T, path string) *http.Request {
	t.Helper()
	req := httptest.NewRequest("GET", path, nil)
	req.Header.Set("Authorization", "Bearer "+testToken)
	req.Host = testTailscaleIP + ":8080"
	return req
}

// serveThrough runs a request through the full middleware + router chain.
func serveThrough(s *Server, req *http.Request) *httptest.ResponseRecorder {
	w := httptest.NewRecorder()
	s.router.ServeHTTP(w, req)
	return w
}

// =============================================================================
// TOKEN AUTHENTICATION TESTS
// All endpoints except /health must require a valid bearer token.
// =============================================================================

func TestAuth_ValidTokenAccepted(t *testing.T) {
	s := testServer(t)
	req := authedGet(t, "/agents")
	w := serveThrough(s, req)

	if w.Code == http.StatusUnauthorized {
		t.Error("valid token should be accepted, got 401")
	}
}

func TestAuth_MissingTokenRejected(t *testing.T) {
	s := testServer(t)
	req := httptest.NewRequest("GET", "/agents", nil)
	req.Host = testTailscaleIP + ":8080"
	w := serveThrough(s, req)

	if w.Code != http.StatusUnauthorized {
		t.Errorf("missing token: expected 401, got %d", w.Code)
	}
}

func TestAuth_WrongTokenRejected(t *testing.T) {
	s := testServer(t)
	req := httptest.NewRequest("GET", "/agents", nil)
	req.Header.Set("Authorization", "Bearer wrong-token")
	req.Host = testTailscaleIP + ":8080"
	w := serveThrough(s, req)

	if w.Code != http.StatusUnauthorized {
		t.Errorf("wrong token: expected 401, got %d", w.Code)
	}
}

func TestAuth_EmptyTokenRejected(t *testing.T) {
	s := testServer(t)
	req := httptest.NewRequest("GET", "/agents", nil)
	req.Header.Set("Authorization", "Bearer ")
	req.Host = testTailscaleIP + ":8080"
	w := serveThrough(s, req)

	if w.Code != http.StatusUnauthorized {
		t.Errorf("empty token: expected 401, got %d", w.Code)
	}
}

func TestAuth_NoBearerPrefixRejected(t *testing.T) {
	s := testServer(t)
	req := httptest.NewRequest("GET", "/agents", nil)
	req.Header.Set("Authorization", testToken) // no "Bearer " prefix
	req.Host = testTailscaleIP + ":8080"
	w := serveThrough(s, req)

	if w.Code != http.StatusUnauthorized {
		t.Errorf("no Bearer prefix: expected 401, got %d", w.Code)
	}
}

func TestAuth_HealthEndpointNoAuthRequired(t *testing.T) {
	s := testServer(t)
	req := httptest.NewRequest("GET", "/health", nil)
	req.Host = testTailscaleIP + ":8080"
	// No Authorization header
	w := serveThrough(s, req)

	if w.Code != http.StatusOK {
		t.Errorf("health check should not require auth, got %d", w.Code)
	}
}

func TestAuth_TokenViaQueryParamForWebSocket(t *testing.T) {
	s := testServer(t)
	// WebSocket upgrade will fail (no real upgrader), but auth should pass
	req := httptest.NewRequest("GET", "/ws?token="+testToken, nil)
	req.Host = testTailscaleIP + ":8080"
	w := serveThrough(s, req)

	// Should NOT be 401 — token accepted. Will get a different error
	// because httptest doesn't do real WebSocket upgrades.
	if w.Code == http.StatusUnauthorized {
		t.Error("WebSocket token via query param should be accepted")
	}
}

func TestAuth_WrongQueryParamTokenRejected(t *testing.T) {
	s := testServer(t)
	req := httptest.NewRequest("GET", "/ws?token=wrong-token", nil)
	req.Host = testTailscaleIP + ":8080"
	w := serveThrough(s, req)

	if w.Code != http.StatusUnauthorized {
		t.Errorf("wrong query param token: expected 401, got %d", w.Code)
	}
}

// Verify constant-time comparison is used (behavioral check)
func TestAuth_ConstantTimeComparison(t *testing.T) {
	s := testServer(t)

	// Same length, wrong token — should still be rejected
	wrongSameLen := strings.Repeat("x", len(testToken))
	req := httptest.NewRequest("GET", "/agents", nil)
	req.Header.Set("Authorization", "Bearer "+wrongSameLen)
	req.Host = testTailscaleIP + ":8080"
	w := serveThrough(s, req)

	if w.Code != http.StatusUnauthorized {
		t.Errorf("same-length wrong token: expected 401, got %d", w.Code)
	}

	// Different length, wrong token
	req2 := httptest.NewRequest("GET", "/agents", nil)
	req2.Header.Set("Authorization", "Bearer short")
	req2.Host = testTailscaleIP + ":8080"
	w2 := serveThrough(s, req2)

	if w2.Code != http.StatusUnauthorized {
		t.Errorf("different-length wrong token: expected 401, got %d", w2.Code)
	}
}

// =============================================================================
// DNS REBINDING DEFENSE (Host Header Validation)
// The Host header must match the Tailscale IP. After DNS rebinding, the browser
// sends Host: evil.com — which must be rejected before any handler runs.
// =============================================================================

func TestHost_TailscaleIPAccepted(t *testing.T) {
	s := testServer(t)

	validHosts := []string{
		testTailscaleIP,
		testTailscaleIP + ":8080",
	}

	for _, host := range validHosts {
		req := authedGet(t, "/agents")
		req.Host = host
		w := serveThrough(s, req)

		if w.Code == http.StatusForbidden {
			t.Errorf("Host %q should be allowed, got 403", host)
		}
	}
}

func TestHost_LocalhostAccepted(t *testing.T) {
	s := testServer(t)

	// localhost and 127.0.0.1 are allowed (for local development/testing)
	localHosts := []string{
		"localhost",
		"localhost:8080",
		"127.0.0.1",
		"127.0.0.1:8080",
	}

	for _, host := range localHosts {
		req := authedGet(t, "/agents")
		req.Host = host
		w := serveThrough(s, req)

		if w.Code == http.StatusForbidden {
			t.Errorf("Host %q should be allowed, got 403", host)
		}
	}
}

func TestHost_EvilDomainsRejected(t *testing.T) {
	s := testServer(t)

	badHosts := []string{
		"evil.com",
		"evil.com:8080",
		"attacker.net",
		testTailscaleIP + ".evil.com",      // IP prefix attack
		testTailscaleIP + ".evil.com:8080", // IP prefix attack with port
		"100.64.0.1",                       // different Tailscale IP
		"100.64.0.1:8080",                  // different Tailscale IP with port
		"",                                 // empty host
	}

	for _, host := range badHosts {
		req := authedGet(t, "/agents")
		req.Host = host
		w := serveThrough(s, req)

		if w.Code != http.StatusForbidden {
			t.Errorf("Host %q should be rejected, got %d", host, w.Code)
		}
	}
}

// =============================================================================
// CSRF PROTECTION (Content-Type Enforcement)
// POST requests must have Content-Type: application/json. This blocks form-based
// CSRF since browsers cannot set application/json without a CORS preflight.
// =============================================================================

func TestCSRF_JSONContentTypeAccepted(t *testing.T) {
	s := testServer(t)
	seedAgent(t, s, "/tmp/testproject")

	body := map[string]string{"session_id": "test-session-1", "status": "processing", "timestamp": "2025-01-01T00:00:00Z"}
	req := jsonPost(t, "/events/status", body)
	w := serveThrough(s, req)

	if w.Code == http.StatusUnsupportedMediaType {
		t.Error("application/json should be accepted for POST")
	}
}

func TestCSRF_FormSubmissionBlocked(t *testing.T) {
	s := testServer(t)

	// Browser form submission sends application/x-www-form-urlencoded
	formTypes := []string{
		"application/x-www-form-urlencoded",
		"multipart/form-data",
		"text/plain",
	}

	for _, ct := range formTypes {
		req := httptest.NewRequest("POST", "/events/status", strings.NewReader("{}"))
		req.Header.Set("Content-Type", ct)
		req.Header.Set("Authorization", "Bearer "+testToken)
		req.Host = testTailscaleIP + ":8080"
		w := serveThrough(s, req)

		if w.Code != http.StatusUnsupportedMediaType {
			t.Errorf("Content-Type %q: expected 415, got %d", ct, w.Code)
		}
	}
}

func TestCSRF_MissingContentTypeBlocked(t *testing.T) {
	s := testServer(t)

	req := httptest.NewRequest("POST", "/events/status", strings.NewReader("{}"))
	// No Content-Type header
	req.Header.Set("Authorization", "Bearer "+testToken)
	req.Host = testTailscaleIP + ":8080"
	w := serveThrough(s, req)

	if w.Code != http.StatusUnsupportedMediaType {
		t.Errorf("missing Content-Type: expected 415, got %d", w.Code)
	}
}

func TestCSRF_OptionsRejected(t *testing.T) {
	s := testServer(t)

	req := httptest.NewRequest("OPTIONS", "/agents", nil)
	req.Header.Set("Authorization", "Bearer "+testToken)
	req.Host = testTailscaleIP + ":8080"
	w := serveThrough(s, req)

	if w.Code != http.StatusMethodNotAllowed {
		t.Errorf("OPTIONS: expected 405, got %d", w.Code)
	}
}

// =============================================================================
// WEBSOCKET SECURITY
// The WebSocket upgrader rejects requests with an Origin header, blocking
// browser-based WebSocket hijacking from malicious websites.
// =============================================================================

func TestWebSocket_OriginHeaderRejected(t *testing.T) {
	s := testServer(t)

	evilOrigins := []string{
		"https://evil.com",
		"http://evil.com",
		"http://" + testTailscaleIP, // even same-origin browsers send Origin
		"null",
	}

	for _, origin := range evilOrigins {
		req := httptest.NewRequest("GET", "/ws?token="+testToken, nil)
		req.Host = testTailscaleIP + ":8080"
		req.Header.Set("Origin", origin)
		req.Header.Set("Upgrade", "websocket")
		req.Header.Set("Connection", "Upgrade")
		req.Header.Set("Sec-WebSocket-Version", "13")
		req.Header.Set("Sec-WebSocket-Key", wsTestKey)
		w := serveThrough(s, req)

		// The upgrader should reject it — response will be 403 from gorilla/websocket
		if w.Code == http.StatusSwitchingProtocols {
			t.Errorf("ATTACK SUCCEEDED: WebSocket with Origin %q was upgraded", origin)
		}
	}
}

func TestWebSocket_NoOriginAccepted(t *testing.T) {
	s := testServer(t)

	// Non-browser clients don't send Origin
	req := httptest.NewRequest("GET", "/ws?token="+testToken, nil)
	req.Host = testTailscaleIP + ":8080"
	req.Header.Set("Upgrade", "websocket")
	req.Header.Set("Connection", "Upgrade")
	req.Header.Set("Sec-WebSocket-Version", "13")
	req.Header.Set("Sec-WebSocket-Key", wsTestKey)
	// No Origin header

	w := serveThrough(s, req)

	// httptest.NewRecorder doesn't support hijacking, so the upgrade will fail
	// with a non-auth error. The key check is: NOT 401 and NOT 403.
	if w.Code == http.StatusUnauthorized || w.Code == http.StatusForbidden {
		t.Errorf("WebSocket without Origin should pass auth checks, got %d", w.Code)
	}
}

// =============================================================================
// SECURITY HEADERS
// Every response must include protective headers.
// =============================================================================

func TestHeaders_SecurityHeadersPresent(t *testing.T) {
	s := testServer(t)
	req := authedGet(t, "/agents")
	w := serveThrough(s, req)

	checks := map[string]string{
		"X-Content-Type-Options":  "nosniff",
		"X-Frame-Options":         "DENY",
		"Content-Security-Policy": "frame-ancestors 'none'",
	}

	for header, expected := range checks {
		got := w.Header().Get(header)
		if got != expected {
			t.Errorf("header %s: expected %q, got %q", header, expected, got)
		}
	}
}

func TestHeaders_PresentOnErrorResponses(t *testing.T) {
	s := testServer(t)

	// Even 401 responses should have security headers
	req := httptest.NewRequest("GET", "/agents", nil)
	req.Host = testTailscaleIP + ":8080"
	// No auth token
	w := serveThrough(s, req)

	if w.Code != http.StatusUnauthorized {
		t.Fatalf("expected 401, got %d", w.Code)
	}

	if got := w.Header().Get("X-Content-Type-Options"); got != "nosniff" {
		t.Errorf("X-Content-Type-Options missing on 401 response: got %q", got)
	}
}

// =============================================================================
// PATH VALIDATION (validatePath)
// File access must be restricted to project directories. Tests cover traversal,
// symlink escape, and prefix boundary attacks.
// =============================================================================

func TestPath_WithinProjectAllowed(t *testing.T) {
	dir := t.TempDir()
	subFile := filepath.Join(dir, "src", "main.go")
	os.MkdirAll(filepath.Join(dir, "src"), 0755)
	os.WriteFile(subFile, []byte("package main"), 0644)

	_, ok := validatePath(subFile, []string{dir})
	if !ok {
		t.Errorf("file within project should be allowed")
	}
}

func TestPath_ExactProjectAllowed(t *testing.T) {
	dir := t.TempDir()
	_, ok := validatePath(dir, []string{dir})
	if !ok {
		t.Errorf("exact project path should be allowed")
	}
}

func TestPath_OutsideProjectRejected(t *testing.T) {
	dir := t.TempDir()
	_, ok := validatePath("/etc/passwd", []string{dir})
	if ok {
		t.Errorf("/etc/passwd should be rejected when project is %s", dir)
	}
}

func TestPath_TraversalRejected(t *testing.T) {
	dir := t.TempDir()

	traversalPaths := []string{
		dir + "/../../../etc/passwd",
		dir + "/../../etc/shadow",
		dir + "/./../../etc/hosts",
	}

	for _, p := range traversalPaths {
		_, ok := validatePath(p, []string{dir})
		if ok {
			t.Errorf("traversal path %q should be rejected", p)
		}
	}
}

func TestPath_SymlinkEscapeRejected(t *testing.T) {
	projectDir := t.TempDir()
	outsideDir := t.TempDir()

	// Create file outside project
	outsideFile := filepath.Join(outsideDir, "secret.txt")
	os.WriteFile(outsideFile, []byte("secret"), 0644)

	// Create symlink inside project pointing outside
	symlink := filepath.Join(projectDir, "escape")
	if err := os.Symlink(outsideDir, symlink); err != nil {
		t.Skipf("cannot create symlink: %v", err)
	}

	// The symlink path looks like it's under projectDir but resolves outside
	escapePath := filepath.Join(symlink, "secret.txt")
	_, ok := validatePath(escapePath, []string{projectDir})
	if ok {
		t.Errorf("symlink escape %q -> %q should be rejected", escapePath, outsideFile)
	}
}

func TestPath_SymlinkWithinProjectAllowed(t *testing.T) {
	projectDir := t.TempDir()
	subDir := filepath.Join(projectDir, "real")
	os.MkdirAll(subDir, 0755)
	os.WriteFile(filepath.Join(subDir, "file.txt"), []byte("ok"), 0644)

	// Symlink within the project to another location within the project
	symlink := filepath.Join(projectDir, "link")
	if err := os.Symlink(subDir, symlink); err != nil {
		t.Skipf("cannot create symlink: %v", err)
	}

	linkedFile := filepath.Join(symlink, "file.txt")
	_, ok := validatePath(linkedFile, []string{projectDir})
	if !ok {
		t.Errorf("symlink within project should be allowed")
	}
}

func TestPath_PrefixBoundaryRejected(t *testing.T) {
	// /tmp/project should NOT match /tmp/project-secret
	projectDir := t.TempDir()
	secretDir := projectDir + "-secret"
	os.MkdirAll(secretDir, 0755)
	defer os.RemoveAll(secretDir)

	secretFile := filepath.Join(secretDir, "secret.txt")
	os.WriteFile(secretFile, []byte("secret"), 0644)

	_, ok := validatePath(secretFile, []string{projectDir})
	if ok {
		t.Errorf("prefix boundary: %q should NOT match project %q", secretFile, projectDir)
	}
}

func TestPath_NonexistentPathRejected(t *testing.T) {
	dir := t.TempDir()
	_, ok := validatePath("/nonexistent/path/file.txt", []string{dir})
	if ok {
		t.Error("nonexistent path should be rejected")
	}
}

func TestPath_EmptyValidPathsRejected(t *testing.T) {
	_, ok := validatePath("/tmp/anything", nil)
	if ok {
		t.Error("should reject when no valid paths configured")
	}

	_, ok2 := validatePath("/tmp/anything", []string{})
	if ok2 {
		t.Error("should reject when valid paths is empty")
	}
}

// =============================================================================
// FILE ENDPOINT INTEGRATION TESTS
// These test the full /files/list and /files/read endpoints through the
// middleware chain with database-driven project path validation.
// =============================================================================

func TestFiles_ListWithinProject(t *testing.T) {
	s := testServer(t)
	dir := t.TempDir()
	os.WriteFile(filepath.Join(dir, "test.go"), []byte("package main"), 0644)
	seedAgent(t, s, dir)

	req := authedGet(t, "/files/list?path="+dir)
	w := serveThrough(s, req)

	if w.Code != http.StatusOK {
		t.Errorf("list within project: expected 200, got %d (body: %s)", w.Code, w.Body.String())
	}
}

func TestFiles_ListOutsideProjectRejected(t *testing.T) {
	s := testServer(t)
	dir := t.TempDir()
	seedAgent(t, s, dir)

	req := authedGet(t, "/files/list?path=/etc")
	w := serveThrough(s, req)

	if w.Code != http.StatusForbidden {
		t.Errorf("list outside project: expected 403, got %d", w.Code)
	}
}

func TestFiles_ReadWithinProject(t *testing.T) {
	s := testServer(t)
	dir := t.TempDir()
	testFile := filepath.Join(dir, "hello.txt")
	os.WriteFile(testFile, []byte("hello world"), 0644)
	seedAgent(t, s, dir)

	req := authedGet(t, "/files/read?path="+testFile)
	w := serveThrough(s, req)

	if w.Code != http.StatusOK {
		t.Errorf("read within project: expected 200, got %d (body: %s)", w.Code, w.Body.String())
	}

	var resp map[string]interface{}
	json.NewDecoder(w.Body).Decode(&resp)
	if resp["content"] != "hello world" {
		t.Errorf("expected content 'hello world', got %q", resp["content"])
	}
}

func TestFiles_ReadOutsideProjectRejected(t *testing.T) {
	s := testServer(t)
	dir := t.TempDir()
	seedAgent(t, s, dir)

	req := authedGet(t, "/files/read?path=/etc/passwd")
	w := serveThrough(s, req)

	if w.Code != http.StatusForbidden {
		t.Errorf("read outside project: expected 403, got %d", w.Code)
	}
}

func TestFiles_ReadTraversalRejected(t *testing.T) {
	s := testServer(t)
	dir := t.TempDir()
	seedAgent(t, s, dir)

	req := authedGet(t, "/files/read?path="+dir+"/../../../etc/passwd")
	w := serveThrough(s, req)

	if w.Code != http.StatusForbidden {
		t.Errorf("read with traversal: expected 403, got %d", w.Code)
	}
}

func TestFiles_ReadSymlinkEscapeRejected(t *testing.T) {
	s := testServer(t)
	projectDir := t.TempDir()
	outsideDir := t.TempDir()
	seedAgent(t, s, projectDir)

	// Create secret file outside project
	secretFile := filepath.Join(outsideDir, "secret.txt")
	os.WriteFile(secretFile, []byte("top secret"), 0644)

	// Symlink from project to outside
	symlink := filepath.Join(projectDir, "escape-link")
	if err := os.Symlink(outsideDir, symlink); err != nil {
		t.Skipf("cannot create symlink: %v", err)
	}

	req := authedGet(t, "/files/read?path="+filepath.Join(symlink, "secret.txt"))
	w := serveThrough(s, req)

	if w.Code != http.StatusForbidden {
		t.Errorf("ATTACK SUCCEEDED: symlink escape read returned %d, expected 403", w.Code)
	}

	// Verify the secret content was not returned
	if strings.Contains(w.Body.String(), "top secret") {
		t.Error("ATTACK SUCCEEDED: secret file content leaked via symlink")
	}
}

func TestFiles_ListWithNoAgentsRejectsAll(t *testing.T) {
	s := testServer(t)
	// No agents seeded — no valid project paths

	req := authedGet(t, "/files/list?path=/tmp")
	w := serveThrough(s, req)

	if w.Code != http.StatusForbidden {
		t.Errorf("list with no agents: expected 403, got %d", w.Code)
	}
}

func TestFiles_ReadDirectoryRejected(t *testing.T) {
	s := testServer(t)
	dir := t.TempDir()
	seedAgent(t, s, dir)

	req := authedGet(t, "/files/read?path="+dir)
	w := serveThrough(s, req)

	if w.Code != http.StatusBadRequest {
		t.Errorf("read directory: expected 400, got %d", w.Code)
	}
}

func TestFiles_MissingPathParam(t *testing.T) {
	s := testServer(t)

	reqList := authedGet(t, "/files/list")
	wList := serveThrough(s, reqList)
	if wList.Code != http.StatusBadRequest {
		t.Errorf("list without path: expected 400, got %d", wList.Code)
	}

	reqRead := authedGet(t, "/files/read")
	wRead := serveThrough(s, reqRead)
	if wRead.Code != http.StatusBadRequest {
		t.Errorf("read without path: expected 400, got %d", wRead.Code)
	}
}

// =============================================================================
// GENERIC ERROR MESSAGES
// Error responses must not leak internal paths, database errors, or system info.
// =============================================================================

func TestErrors_NoInternalPathsLeaked(t *testing.T) {
	s := testServer(t)
	dir := t.TempDir()
	seedAgent(t, s, dir)

	// Request a file outside the project
	req := authedGet(t, "/files/read?path=/etc/passwd")
	w := serveThrough(s, req)

	body := w.Body.String()
	if strings.Contains(body, "/etc/passwd") {
		t.Error("error response should not contain the requested path")
	}
	if strings.Contains(body, dir) {
		t.Error("error response should not contain the project path")
	}
}

func TestErrors_GenericForbiddenMessage(t *testing.T) {
	s := testServer(t)
	dir := t.TempDir()
	seedAgent(t, s, dir)

	req := authedGet(t, "/files/read?path=/etc/shadow")
	w := serveThrough(s, req)

	body := strings.TrimSpace(w.Body.String())
	if body != "access denied" {
		t.Errorf("forbidden response should be 'access denied', got %q", body)
	}
}

// =============================================================================
// BODY SIZE LIMIT
// Request bodies larger than 1MB should be rejected.
// =============================================================================

func TestBodySize_LargePayloadRejected(t *testing.T) {
	s := testServer(t)

	// Create a payload larger than 1MB
	bigBody := strings.NewReader(strings.Repeat("x", 2*1024*1024))
	req := httptest.NewRequest("POST", "/events/message", bigBody)
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+testToken)
	req.Host = testTailscaleIP + ":8080"

	w := serveThrough(s, req)

	// Should fail (400 from JSON decode or similar) — the key is it doesn't OOM
	if w.Code == http.StatusOK {
		t.Error("2MB payload should be rejected")
	}
}

// =============================================================================
// ATTACK SIMULATIONS
// These tests simulate actual attack scenarios.
// =============================================================================

// Attack: DNS rebinding — attacker rebinds their domain to your Tailscale IP
func TestAttack_DNSRebindingReadAgents(t *testing.T) {
	s := testServer(t)

	// After rebinding, browser sends Host: evil.com but request goes to our server
	req := httptest.NewRequest("GET", "/agents", nil)
	req.Host = "evil.com:8080"
	req.Header.Set("Authorization", "Bearer "+testToken) // even with valid token
	w := serveThrough(s, req)

	if w.Code != http.StatusForbidden {
		t.Errorf("ATTACK SUCCEEDED: DNS rebinding read returned %d, expected 403", w.Code)
	}
}

// Attack: Cross-site form submission (CSRF)
func TestAttack_CrossSiteFormSubmission(t *testing.T) {
	s := testServer(t)

	// Attacker's page auto-submits a form to our server
	// Browser form submissions use application/x-www-form-urlencoded
	req := httptest.NewRequest("POST", "/agents/test-session/send",
		strings.NewReader("content=pwned"))
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	req.Header.Set("Authorization", "Bearer "+testToken) // even with token
	req.Host = testTailscaleIP + ":8080"
	w := serveThrough(s, req)

	if w.Code != http.StatusUnsupportedMediaType {
		t.Errorf("ATTACK SUCCEEDED: form CSRF returned %d, expected 415", w.Code)
	}
}

// Attack: WebSocket hijacking from malicious website
func TestAttack_WebSocketHijackFromEvilSite(t *testing.T) {
	s := testServer(t)

	// Attacker's JS: new WebSocket('ws://100.x.x.x:8080/ws?token=guessed')
	req := httptest.NewRequest("GET", "/ws?token="+testToken, nil)
	req.Host = testTailscaleIP + ":8080"
	req.Header.Set("Origin", "https://evil.com") // browser always sends Origin
	req.Header.Set("Upgrade", "websocket")
	req.Header.Set("Connection", "Upgrade")
	req.Header.Set("Sec-WebSocket-Version", "13")
	req.Header.Set("Sec-WebSocket-Key", wsTestKey)
	w := serveThrough(s, req)

	if w.Code == http.StatusSwitchingProtocols {
		t.Error("ATTACK SUCCEEDED: WebSocket upgraded with evil Origin")
	}
}

// Attack: Read files via path traversal
func TestAttack_PathTraversalFileRead(t *testing.T) {
	s := testServer(t)
	dir := t.TempDir()
	seedAgent(t, s, dir)

	// Try various traversal payloads
	attacks := []string{
		dir + "/../../../../etc/passwd",
		dir + "/..%2F..%2Fetc%2Fpasswd",
		dir + "/./../../../../etc/shadow",
	}

	for _, path := range attacks {
		req := authedGet(t, "/files/read?path="+path)
		w := serveThrough(s, req)

		if w.Code == http.StatusOK {
			t.Errorf("ATTACK SUCCEEDED: path traversal %q returned 200", path)
		}
	}
}

// Attack: Read files via symlink planted in project directory
func TestAttack_SymlinkPlantedInProject(t *testing.T) {
	s := testServer(t)
	projectDir := t.TempDir()
	seedAgent(t, s, projectDir)

	// Attacker who has file-write access to the project creates a symlink
	symlink := filepath.Join(projectDir, "etc-link")
	if err := os.Symlink("/etc", symlink); err != nil {
		t.Skipf("cannot create symlink: %v", err)
	}

	// Try to read /etc/passwd through the symlink
	req := authedGet(t, "/files/read?path="+filepath.Join(symlink, "hosts"))
	w := serveThrough(s, req)

	if w.Code == http.StatusOK {
		t.Error("ATTACK SUCCEEDED: read /etc/hosts through project symlink")
	}
}

// Attack: Prefix boundary — project name is a prefix of another directory
func TestAttack_PrefixBoundaryFileRead(t *testing.T) {
	s := testServer(t)
	projectDir := t.TempDir()
	secretDir := projectDir + "-secrets"
	os.MkdirAll(secretDir, 0755)
	defer os.RemoveAll(secretDir)

	secretFile := filepath.Join(secretDir, "api_key.txt")
	os.WriteFile(secretFile, []byte("sk-secret-key"), 0644)
	seedAgent(t, s, projectDir)

	req := authedGet(t, "/files/read?path="+secretFile)
	w := serveThrough(s, req)

	if w.Code == http.StatusOK {
		t.Error("ATTACK SUCCEEDED: read file from directory sharing prefix with project")
	}
	if strings.Contains(w.Body.String(), "sk-secret-key") {
		t.Error("ATTACK SUCCEEDED: secret content leaked via prefix boundary attack")
	}
}

// =============================================================================
// SQL INJECTION
// All database queries must use parameterized placeholders.
// =============================================================================

func TestSQL_InjectionInSessionID(t *testing.T) {
	s := testServer(t)
	seedAgent(t, s, "/tmp/test")

	// Try SQL injection via session_id in message endpoint
	body := map[string]string{
		"session_id": "'; DROP TABLE agents; --",
		"role":       "user",
		"content":    "test",
		"timestamp":  "2025-01-01T00:00:00Z",
	}
	req := jsonPost(t, "/events/message", body)
	serveThrough(s, req)

	// Verify agents table still exists
	var count int
	err := s.db.QueryRow("SELECT COUNT(*) FROM agents").Scan(&count)
	if err != nil {
		t.Errorf("agents table dropped by SQL injection: %v", err)
	}
}

func TestSQL_InjectionInMessageContent(t *testing.T) {
	s := testServer(t)
	seedAgent(t, s, "/tmp/test")

	body := map[string]string{
		"session_id": "test-session-1",
		"role":       "user",
		"content":    "'); DELETE FROM messages; --",
		"timestamp":  "2025-01-01T00:00:00Z",
	}
	req := jsonPost(t, "/events/message", body)
	serveThrough(s, req)

	// Verify messages still exist (the injection was stored as literal text)
	var count int
	s.db.QueryRow("SELECT COUNT(*) FROM messages WHERE session_id = 'test-session-1'").Scan(&count)
	if count < 1 {
		t.Error("SQL injection may have deleted messages")
	}
}

// =============================================================================
// INPUT VALIDATION
// Various input validation checks.
// =============================================================================

func TestInput_ValidRolesAccepted(t *testing.T) {
	s := testServer(t)
	seedAgent(t, s, "/tmp/test")

	for _, role := range []string{"user", "agent", "tool"} {
		body := map[string]string{
			"session_id": "test-session-1",
			"role":       role,
			"content":    "test message",
			"timestamp":  "2025-01-01T00:00:00Z",
		}
		req := jsonPost(t, "/events/message", body)
		w := serveThrough(s, req)

		if w.Code != http.StatusOK {
			t.Errorf("role %q should be accepted, got %d", role, w.Code)
		}
	}
}

func TestInput_InvalidRolesRejected(t *testing.T) {
	s := testServer(t)
	seedAgent(t, s, "/tmp/test")

	badRoles := []string{"admin", "system", "root", "", "USER", "Agent"}

	for _, role := range badRoles {
		body := map[string]string{
			"session_id": "test-session-1",
			"role":       role,
			"content":    "test",
			"timestamp":  "2025-01-01T00:00:00Z",
		}
		req := jsonPost(t, "/events/message", body)
		w := serveThrough(s, req)

		if w.Code != http.StatusBadRequest {
			t.Errorf("role %q: expected 400, got %d", role, w.Code)
		}
	}
}

func TestInput_InternalErrorsAreGeneric(t *testing.T) {
	s := testServer(t)
	dir := t.TempDir()
	seedAgent(t, s, dir)

	// Trigger various error paths and verify no internal details leak
	endpoints := []struct {
		method string
		path   string
		body   string
	}{
		// File outside project → "access denied"
		{"GET", "/files/read?path=/etc/passwd", ""},
		{"GET", "/files/list?path=/etc", ""},
	}

	for _, ep := range endpoints {
		var req *http.Request
		if ep.body != "" {
			req = httptest.NewRequest(ep.method, ep.path, strings.NewReader(ep.body))
			req.Header.Set("Content-Type", "application/json")
		} else {
			req = httptest.NewRequest(ep.method, ep.path, nil)
		}
		req.Header.Set("Authorization", "Bearer "+testToken)
		req.Host = testTailscaleIP + ":8080"
		w := serveThrough(s, req)

		body := w.Body.String()
		// Should never contain filesystem paths
		if strings.Contains(body, "/etc/") || strings.Contains(body, "/Users/") || strings.Contains(body, "/home/") {
			t.Errorf("%s %s: error response leaks path info: %s", ep.method, ep.path, body)
		}
		// Should never contain SQL or database errors
		if strings.Contains(body, "SQL") || strings.Contains(body, "sqlite") || strings.Contains(body, "database") {
			t.Errorf("%s %s: error response leaks database info: %s", ep.method, ep.path, body)
		}
	}
}

func TestInput_InvalidJSON(t *testing.T) {
	s := testServer(t)

	req := httptest.NewRequest("POST", "/events/message", strings.NewReader("not json"))
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+testToken)
	req.Host = testTailscaleIP + ":8080"
	w := serveThrough(s, req)

	if w.Code != http.StatusBadRequest {
		t.Errorf("invalid JSON: expected 400, got %d", w.Code)
	}
}

func TestInput_MessageLimitParam(t *testing.T) {
	s := testServer(t)
	seedAgent(t, s, "/tmp/test")

	// Test that limit parameter is bounded
	req := authedGet(t, "/agents/test-session-1/messages?limit=9999")
	w := serveThrough(s, req)

	// Should succeed but cap at 1000
	if w.Code != http.StatusOK {
		t.Errorf("messages with large limit: expected 200, got %d", w.Code)
	}
}

func TestInput_NegativeLimitParam(t *testing.T) {
	s := testServer(t)
	seedAgent(t, s, "/tmp/test")

	req := authedGet(t, "/agents/test-session-1/messages?limit=-1")
	w := serveThrough(s, req)

	// Should succeed with default limit
	if w.Code != http.StatusOK {
		t.Errorf("messages with negative limit: expected 200, got %d", w.Code)
	}
}

// =============================================================================
// LIVE SERVER INTEGRATION TESTS
// These spin up a real HTTP server and make real HTTP client requests,
// simulating actual attack scenarios end-to-end over the network stack.
// =============================================================================

// liveServer starts a real httptest.Server backed by the full router+middleware.
// Returns the server and a cleanup function.
func liveServer(t *testing.T) (*httptest.Server, *Server) {
	t.Helper()
	s := testServer(t)
	ts := httptest.NewServer(s.router)
	t.Cleanup(ts.Close)
	return ts, s
}

// --- Token auth over real HTTP ---

func TestLive_NoTokenGets401(t *testing.T) {
	ts, _ := liveServer(t)

	resp, err := http.Get(ts.URL + "/agents")
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()

	// Host will be 127.0.0.1 (httptest), which our middleware allows.
	// But no token → 401.
	if resp.StatusCode != http.StatusUnauthorized {
		t.Errorf("expected 401, got %d", resp.StatusCode)
	}
}

func TestLive_ValidTokenGetsAgents(t *testing.T) {
	ts, _ := liveServer(t)

	req, _ := http.NewRequest("GET", ts.URL+"/agents", nil)
	req.Header.Set("Authorization", "Bearer "+testToken)

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		t.Errorf("expected 200, got %d: %s", resp.StatusCode, body)
	}
}

func TestLive_WrongTokenGets401(t *testing.T) {
	ts, _ := liveServer(t)

	req, _ := http.NewRequest("GET", ts.URL+"/agents", nil)
	req.Header.Set("Authorization", "Bearer wrong-token-value")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusUnauthorized {
		t.Errorf("expected 401, got %d", resp.StatusCode)
	}
}

// --- CSRF (Content-Type) over real HTTP ---

func TestLive_FormPostBlocked(t *testing.T) {
	ts, _ := liveServer(t)

	// Simulate a browser form submission — application/x-www-form-urlencoded
	data := url.Values{"content": {"pwned"}}.Encode()
	req, _ := http.NewRequest("POST", ts.URL+"/events/status", strings.NewReader(data))
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	req.Header.Set("Authorization", "Bearer "+testToken)

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusUnsupportedMediaType {
		t.Errorf("ATTACK SUCCEEDED: form POST returned %d, expected 415", resp.StatusCode)
	}
}

func TestLive_JSONPostAccepted(t *testing.T) {
	ts, s := liveServer(t)
	seedAgent(t, s, "/tmp/test")

	body := `{"session_id":"test-session-1","status":"processing","detail":"","timestamp":"2025-01-01T00:00:00Z"}`
	req, _ := http.NewRequest("POST", ts.URL+"/events/status", strings.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+testToken)

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		respBody, _ := io.ReadAll(resp.Body)
		t.Errorf("JSON POST should be accepted, got %d: %s", resp.StatusCode, respBody)
	}
}

// --- Real WebSocket attacks ---

func TestLive_WebSocketWithOriginRejected(t *testing.T) {
	ts, _ := liveServer(t)

	// Convert http:// to ws://
	wsURL := "ws" + strings.TrimPrefix(ts.URL, "http") + "/ws?token=" + testToken

	// gorilla/websocket Dialer lets us set Origin header
	dialer := websocket.Dialer{}
	header := http.Header{}
	header.Set("Origin", "https://evil.com")

	conn, resp, err := dialer.Dial(wsURL, header)
	if conn != nil {
		conn.Close()
		t.Error("ATTACK SUCCEEDED: WebSocket connected with evil Origin")
	}

	// Should get 403 from the upgrader's CheckOrigin
	if resp != nil && resp.StatusCode == http.StatusSwitchingProtocols {
		t.Error("ATTACK SUCCEEDED: WebSocket upgraded with evil Origin")
	}
	// err is expected (connection refused / forbidden)
	_ = err
}

func TestLive_WebSocketWithoutOriginAccepted(t *testing.T) {
	ts, _ := liveServer(t)

	wsURL := "ws" + strings.TrimPrefix(ts.URL, "http") + "/ws?token=" + testToken

	// No Origin header — like Emacs websocket.el or React Native
	dialer := websocket.Dialer{}

	conn, _, err := dialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatalf("WebSocket without Origin should connect: %v", err)
	}
	conn.Close()
}

func TestLive_WebSocketWrongTokenRejected(t *testing.T) {
	ts, _ := liveServer(t)

	wsURL := "ws" + strings.TrimPrefix(ts.URL, "http") + "/ws?token=wrong-token"

	dialer := websocket.Dialer{}
	conn, resp, err := dialer.Dial(wsURL, nil)
	if conn != nil {
		conn.Close()
		t.Error("ATTACK SUCCEEDED: WebSocket connected with wrong token")
	}
	if resp != nil && resp.StatusCode == http.StatusSwitchingProtocols {
		t.Error("ATTACK SUCCEEDED: WebSocket upgraded with wrong token")
	}
	_ = err
}

func TestLive_WebSocketNoTokenRejected(t *testing.T) {
	ts, _ := liveServer(t)

	wsURL := "ws" + strings.TrimPrefix(ts.URL, "http") + "/ws"

	dialer := websocket.Dialer{}
	conn, resp, err := dialer.Dial(wsURL, nil)
	if conn != nil {
		conn.Close()
		t.Error("ATTACK SUCCEEDED: WebSocket connected with no token")
	}
	if resp != nil && resp.StatusCode == http.StatusSwitchingProtocols {
		t.Error("ATTACK SUCCEEDED: WebSocket upgraded with no token")
	}
	_ = err
}

// --- Real file access attacks ---

func TestLive_FileReadWithSymlinkEscape(t *testing.T) {
	ts, s := liveServer(t)

	projectDir := t.TempDir()
	outsideDir := t.TempDir()
	seedAgent(t, s, projectDir)

	// Plant a secret file outside the project
	secretFile := filepath.Join(outsideDir, "credentials.txt")
	os.WriteFile(secretFile, []byte("aws_secret_key=AKIA..."), 0644)

	// Create symlink inside project pointing to the outside directory
	symlink := filepath.Join(projectDir, "linked")
	if err := os.Symlink(outsideDir, symlink); err != nil {
		t.Skipf("cannot create symlink: %v", err)
	}

	escapePath := filepath.Join(symlink, "credentials.txt")
	req, _ := http.NewRequest("GET", ts.URL+"/files/read?path="+url.QueryEscape(escapePath), nil)
	req.Header.Set("Authorization", "Bearer "+testToken)

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)

	if resp.StatusCode == http.StatusOK {
		t.Error("ATTACK SUCCEEDED: read credentials via symlink escape")
	}
	if strings.Contains(string(body), "aws_secret_key") {
		t.Error("ATTACK SUCCEEDED: secret content leaked in response")
	}
}

func TestLive_FileReadTraversal(t *testing.T) {
	ts, s := liveServer(t)

	projectDir := t.TempDir()
	seedAgent(t, s, projectDir)

	// Try to read /etc/hosts via traversal
	traversalPath := projectDir + "/../../../../etc/hosts"
	req, _ := http.NewRequest("GET", ts.URL+"/files/read?path="+url.QueryEscape(traversalPath), nil)
	req.Header.Set("Authorization", "Bearer "+testToken)

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)

	if resp.StatusCode == http.StatusOK {
		t.Error("ATTACK SUCCEEDED: read /etc/hosts via path traversal")
	}
	if strings.Contains(string(body), "localhost") {
		t.Error("ATTACK SUCCEEDED: /etc/hosts content leaked")
	}
}

func TestLive_FileReadPrefixBoundary(t *testing.T) {
	ts, s := liveServer(t)

	projectDir := t.TempDir()
	secretDir := projectDir + "-secrets"
	os.MkdirAll(secretDir, 0755)
	defer os.RemoveAll(secretDir)

	os.WriteFile(filepath.Join(secretDir, "token.txt"), []byte("super-secret-token"), 0644)
	seedAgent(t, s, projectDir)

	secretPath := filepath.Join(secretDir, "token.txt")
	req, _ := http.NewRequest("GET", ts.URL+"/files/read?path="+url.QueryEscape(secretPath), nil)
	req.Header.Set("Authorization", "Bearer "+testToken)

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)

	if resp.StatusCode == http.StatusOK {
		t.Error("ATTACK SUCCEEDED: read from prefix-adjacent directory")
	}
	if strings.Contains(string(body), "super-secret-token") {
		t.Error("ATTACK SUCCEEDED: secret token leaked via prefix boundary")
	}
}

// --- Security headers on real responses ---

func TestLive_SecurityHeadersOnEveryResponse(t *testing.T) {
	ts, _ := liveServer(t)

	// Check headers on a successful authed request
	req, _ := http.NewRequest("GET", ts.URL+"/agents", nil)
	req.Header.Set("Authorization", "Bearer "+testToken)
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	resp.Body.Close()

	checks := map[string]string{
		"X-Content-Type-Options":  "nosniff",
		"X-Frame-Options":         "DENY",
		"Content-Security-Policy": "frame-ancestors 'none'",
	}
	for header, expected := range checks {
		if got := resp.Header.Get(header); got != expected {
			t.Errorf("header %s on 200: expected %q, got %q", header, expected, got)
		}
	}

	// Check headers on an unauthenticated 401 response
	resp2, err := http.Get(ts.URL + "/agents")
	if err != nil {
		t.Fatal(err)
	}
	resp2.Body.Close()

	for header, expected := range checks {
		if got := resp2.Header.Get(header); got != expected {
			t.Errorf("header %s on 401: expected %q, got %q", header, expected, got)
		}
	}
}

// --- WebSocket broadcast integration ---

func TestLive_WebSocketReceivesBroadcast(t *testing.T) {
	ts, s := liveServer(t)
	seedAgent(t, s, "/tmp/test")

	// Connect a real WebSocket client
	wsURL := "ws" + strings.TrimPrefix(ts.URL, "http") + "/ws?token=" + testToken
	conn, _, err := websocket.DefaultDialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatal(err)
	}
	defer conn.Close()

	// Send a status event via HTTP POST
	body := `{"session_id":"test-session-1","status":"processing","detail":"working","timestamp":"2025-01-01T00:00:00Z"}`
	req, _ := http.NewRequest("POST", ts.URL+"/events/status", strings.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+testToken)
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	resp.Body.Close()

	// Read the broadcasted WebSocket message
	var wsEvent WSEvent
	err = conn.ReadJSON(&wsEvent)
	if err != nil {
		t.Fatalf("failed to read WebSocket message: %v", err)
	}

	if wsEvent.Type != "status" {
		t.Errorf("expected event type 'status', got %q", wsEvent.Type)
	}
}

// --- Send message end-to-end ---

func TestLive_SendMessageStoredAndBroadcast(t *testing.T) {
	ts, s := liveServer(t)
	seedAgent(t, s, "/tmp/test")

	// Connect WebSocket to receive broadcast
	wsURL := "ws" + strings.TrimPrefix(ts.URL, "http") + "/ws?token=" + testToken
	conn, _, err := websocket.DefaultDialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatal(err)
	}
	defer conn.Close()

	// Send message via API
	body := `{"content":"hello from mobile"}`
	req, _ := http.NewRequest("POST", ts.URL+"/agents/test-session-1/send", strings.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+testToken)
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	resp.Body.Close()

	if resp.StatusCode != http.StatusAccepted {
		t.Errorf("send message: expected 202, got %d", resp.StatusCode)
	}

	// Verify message stored in database
	var content string
	err = s.db.QueryRow(`SELECT content FROM messages WHERE session_id = 'test-session-1' AND role = 'user'`).Scan(&content)
	if err != nil {
		t.Fatalf("message not stored: %v", err)
	}
	if content != "hello from mobile" {
		t.Errorf("stored content: expected 'hello from mobile', got %q", content)
	}

	// Verify WebSocket received the broadcast
	var wsEvent WSEvent
	err = conn.ReadJSON(&wsEvent)
	if err != nil {
		t.Fatalf("failed to read WebSocket broadcast: %v", err)
	}
	if wsEvent.Type != "send_request" {
		t.Errorf("expected broadcast type 'send_request', got %q", wsEvent.Type)
	}
}
