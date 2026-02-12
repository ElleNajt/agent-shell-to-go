package main

import (
	"context"
	"database/sql"
	"encoding/base64"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net"
	"net/http"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/gorilla/mux"
	"github.com/gorilla/websocket"
	_ "github.com/mattn/go-sqlite3"
)

// Config holds server configuration
type Config struct {
	ListenAddr string // Tailscale IP:port to listen on
	DBPath     string // Path to SQLite database
}

// Server holds all server state
type Server struct {
	config    Config
	db        *sql.DB
	router    *mux.Router
	wsClients map[*websocket.Conn]bool
	wsMutex   sync.RWMutex
}

// Agent represents an agent session
type Agent struct {
	SessionID       string     `json:"session_id"`
	BufferName      string     `json:"buffer_name"`
	Project         string     `json:"project"`
	ProjectPath     string     `json:"project_path"`
	ParentSessionID *string    `json:"parent_session_id"`
	Status          string     `json:"status"`
	LastMessage     string     `json:"last_message"`
	LastMessageRole string     `json:"last_message_role"`
	LastActivity    time.Time  `json:"last_activity"`
	CreatedAt       time.Time  `json:"created_at"`
	ClosedAt        *time.Time `json:"closed_at"`
}

// Message represents a chat message
type Message struct {
	ID        int64     `json:"id"`
	SessionID string    `json:"session_id"`
	Role      string    `json:"role"` // "user", "agent", "tool"
	Content   string    `json:"content"`
	Timestamp time.Time `json:"timestamp"`
}

// Event types for incoming events from Emacs
type AgentSpawnEvent struct {
	SessionID       string  `json:"session_id"`
	BufferName      string  `json:"buffer_name"`
	Project         string  `json:"project"`
	ProjectPath     string  `json:"project_path"`
	ParentSessionID *string `json:"parent_session_id"`
	Timestamp       string  `json:"timestamp"`
}

type AgentCloseEvent struct {
	SessionID string `json:"session_id"`
	Timestamp string `json:"timestamp"`
}

type MessageEvent struct {
	SessionID string `json:"session_id"`
	Role      string `json:"role"`
	Content   string `json:"content"`
	Timestamp string `json:"timestamp"`
}

type StatusEvent struct {
	SessionID string `json:"session_id"`
	Status    string `json:"status"`
	Detail    string `json:"detail"`
	Timestamp string `json:"timestamp"`
}

// WebSocket event wrapper for broadcasting to clients
type WSEvent struct {
	Type    string      `json:"type"`
	Payload interface{} `json:"payload"`
}

func NewServer(config Config) (*Server, error) {
	db, err := sql.Open("sqlite3", config.DBPath)
	if err != nil {
		return nil, fmt.Errorf("failed to open database: %w", err)
	}

	s := &Server{
		config:    config,
		db:        db,
		router:    mux.NewRouter(),
		wsClients: make(map[*websocket.Conn]bool),
	}

	if err := s.initDB(); err != nil {
		return nil, fmt.Errorf("failed to initialize database: %w", err)
	}

	s.setupRoutes()
	return s, nil
}

func (s *Server) initDB() error {
	schema := `
	CREATE TABLE IF NOT EXISTS agents (
		session_id TEXT PRIMARY KEY,
		buffer_name TEXT NOT NULL,
		project TEXT NOT NULL,
		project_path TEXT DEFAULT '',
		parent_session_id TEXT,
		status TEXT DEFAULT 'ready',
		last_message TEXT DEFAULT '',
		last_message_role TEXT DEFAULT '',
		last_activity DATETIME DEFAULT CURRENT_TIMESTAMP,
		created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
		closed_at DATETIME
	);

	CREATE TABLE IF NOT EXISTS messages (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		session_id TEXT NOT NULL,
		role TEXT NOT NULL,
		content TEXT NOT NULL,
		timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
		FOREIGN KEY (session_id) REFERENCES agents(session_id)
	);

	CREATE INDEX IF NOT EXISTS idx_messages_session ON messages(session_id);
	CREATE INDEX IF NOT EXISTS idx_messages_timestamp ON messages(timestamp);
	CREATE INDEX IF NOT EXISTS idx_agents_project ON agents(project);
	CREATE INDEX IF NOT EXISTS idx_agents_parent ON agents(parent_session_id);
	`
	_, err := s.db.Exec(schema)
	if err != nil {
		return err
	}

	// Migration: add project_path column if it doesn't exist
	_, _ = s.db.Exec(`ALTER TABLE agents ADD COLUMN project_path TEXT DEFAULT ''`)

	return nil
}

func (s *Server) setupRoutes() {
	// CORS middleware
	s.router.Use(s.corsMiddleware)
	// Tailscale is the auth layer - no token auth needed

	// Events from Emacs (POST)
	s.router.HandleFunc("/events/agent-spawn", s.handleAgentSpawn).Methods("POST")
	s.router.HandleFunc("/events/agent-close", s.handleAgentClose).Methods("POST")
	s.router.HandleFunc("/events/message", s.handleMessage).Methods("POST")
	s.router.HandleFunc("/events/status", s.handleStatus).Methods("POST")

	// API for mobile app (GET/POST) - include OPTIONS for CORS preflight
	s.router.HandleFunc("/agents", s.handleGetAgents).Methods("GET", "OPTIONS")
	s.router.HandleFunc("/agents/{session_id}/messages", s.handleGetMessages).Methods("GET", "OPTIONS")
	s.router.HandleFunc("/agents/{session_id}/send", s.handleSendMessage).Methods("POST", "OPTIONS")
	s.router.HandleFunc("/agents/{session_id}/stop", s.handleStopAgent).Methods("POST", "OPTIONS")
	s.router.HandleFunc("/agents/{session_id}/close", s.handleCloseAgent).Methods("POST", "OPTIONS")
	s.router.HandleFunc("/agents/{session_id}/restart", s.handleRestartAgent).Methods("POST", "OPTIONS")

	// Actions for spawning new agents
	s.router.HandleFunc("/actions/new-agent", s.handleNewAgent).Methods("POST", "OPTIONS")
	s.router.HandleFunc("/actions/new-dispatcher", s.handleNewDispatcher).Methods("POST", "OPTIONS")
	s.router.HandleFunc("/actions/projects", s.handleListProjects).Methods("GET", "OPTIONS")
	s.router.HandleFunc("/actions/big-red-button", s.handleBigRedButton).Methods("POST", "OPTIONS")
	s.router.HandleFunc("/actions/prune-sessions", s.handlePruneSessions).Methods("POST", "OPTIONS")

	// Events from Emacs for session management
	s.router.HandleFunc("/events/alive-sessions", s.handleAliveSessions).Methods("POST", "OPTIONS")

	// File explorer
	s.router.HandleFunc("/files/list", s.handleListFiles).Methods("GET", "OPTIONS")
	s.router.HandleFunc("/files/read", s.handleReadFile).Methods("GET", "OPTIONS")

	// WebSocket for real-time updates
	s.router.HandleFunc("/ws", s.handleWebSocket)

	// Health check (no auth required - useful for monitoring)
	s.router.HandleFunc("/health", func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("ok"))
	}).Methods("GET")

	// Debug endpoints
	s.router.HandleFunc("/debug/ws-status", s.handleDebugWSStatus).Methods("GET")
	s.router.HandleFunc("/debug/sessions", s.handleDebugSessions).Methods("GET")
	s.router.HandleFunc("/debug/send-message", s.handleDebugSendMessage).Methods("POST", "GET")
	s.router.HandleFunc("/debug/messages/{session_id}", s.handleDebugMessages).Methods("GET")
}

func (s *Server) corsMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Allow requests from any origin (for local development)
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Authorization, Content-Type")

		// Handle preflight requests
		if r.Method == "OPTIONS" {
			w.WriteHeader(http.StatusOK)
			return
		}

		next.ServeHTTP(w, r)
	})
}

// Event handlers (from Emacs)

func (s *Server) handleAgentSpawn(w http.ResponseWriter, r *http.Request) {
	var event AgentSpawnEvent
	if err := json.NewDecoder(r.Body).Decode(&event); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	_, err := s.db.Exec(`
		INSERT INTO agents (session_id, buffer_name, project, project_path, parent_session_id, status, created_at)
		VALUES (?, ?, ?, ?, ?, 'ready', ?)
		ON CONFLICT(session_id) DO UPDATE SET
			buffer_name = excluded.buffer_name,
			project = excluded.project,
			project_path = excluded.project_path,
			parent_session_id = excluded.parent_session_id,
			status = 'ready',
			closed_at = NULL
	`, event.SessionID, event.BufferName, event.Project, event.ProjectPath, event.ParentSessionID, event.Timestamp)

	if err != nil {
		log.Printf("error inserting agent: %v", err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// Broadcast to WebSocket clients
	s.broadcast(WSEvent{Type: "agent_spawn", Payload: event})

	w.WriteHeader(http.StatusOK)
}

func (s *Server) handleAgentClose(w http.ResponseWriter, r *http.Request) {
	var event AgentCloseEvent
	if err := json.NewDecoder(r.Body).Decode(&event); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	_, err := s.db.Exec(`
		UPDATE agents SET closed_at = ?, status = 'closed'
		WHERE session_id = ?
	`, event.Timestamp, event.SessionID)

	if err != nil {
		log.Printf("error closing agent: %v", err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	s.broadcast(WSEvent{Type: "agent_close", Payload: event})
	w.WriteHeader(http.StatusOK)
}

func (s *Server) handleMessage(w http.ResponseWriter, r *http.Request) {
	var event MessageEvent
	if err := json.NewDecoder(r.Body).Decode(&event); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	// Insert message
	_, err := s.db.Exec(`
		INSERT INTO messages (session_id, role, content, timestamp)
		VALUES (?, ?, ?, ?)
	`, event.SessionID, event.Role, event.Content, event.Timestamp)

	if err != nil {
		log.Printf("error inserting message: %v", err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// Update agent's last message
	_, err = s.db.Exec(`
		UPDATE agents SET 
			last_message = ?,
			last_message_role = ?,
			last_activity = ?
		WHERE session_id = ?
	`, truncate(event.Content, 200), event.Role, event.Timestamp, event.SessionID)

	if err != nil {
		log.Printf("error updating agent last message: %v", err)
	}

	s.broadcast(WSEvent{Type: "message", Payload: event})
	w.WriteHeader(http.StatusOK)
}

func (s *Server) handleStatus(w http.ResponseWriter, r *http.Request) {
	var event StatusEvent
	if err := json.NewDecoder(r.Body).Decode(&event); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	_, err := s.db.Exec(`
		UPDATE agents SET status = ?, last_activity = ?
		WHERE session_id = ?
	`, event.Status, event.Timestamp, event.SessionID)

	if err != nil {
		log.Printf("error updating agent status: %v", err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	s.broadcast(WSEvent{Type: "status", Payload: event})
	w.WriteHeader(http.StatusOK)
}

// API handlers (for mobile app)

func (s *Server) handleGetAgents(w http.ResponseWriter, r *http.Request) {
	// By default, only show active agents (not closed)
	showClosed := r.URL.Query().Get("include_closed") == "true"

	query := `
		SELECT session_id, buffer_name, project, project_path, parent_session_id, 
		       status, last_message, last_message_role, last_activity, created_at, closed_at
		FROM agents
	`
	if !showClosed {
		query += " WHERE closed_at IS NULL"
	}
	query += " ORDER BY last_activity DESC"

	rows, err := s.db.Query(query)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var agents []Agent
	for rows.Next() {
		var a Agent
		var projectPath sql.NullString
		var parentID, lastMsg, lastMsgRole sql.NullString
		var closedAt sql.NullTime

		err := rows.Scan(&a.SessionID, &a.BufferName, &a.Project, &projectPath, &parentID,
			&a.Status, &lastMsg, &lastMsgRole, &a.LastActivity, &a.CreatedAt, &closedAt)
		if err != nil {
			log.Printf("error scanning agent: %v", err)
			continue
		}

		if projectPath.Valid {
			a.ProjectPath = projectPath.String
		}
		if parentID.Valid {
			a.ParentSessionID = &parentID.String
		}
		if lastMsg.Valid {
			a.LastMessage = lastMsg.String
		}
		if lastMsgRole.Valid {
			a.LastMessageRole = lastMsgRole.String
		}
		if closedAt.Valid {
			a.ClosedAt = &closedAt.Time
		}

		agents = append(agents, a)
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{"agents": agents})
}

func (s *Server) handleGetMessages(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	sessionID := vars["session_id"]

	limit := 50
	if l := r.URL.Query().Get("limit"); l != "" {
		fmt.Sscanf(l, "%d", &limit)
	}

	// "before" parameter for pagination - get messages older than this timestamp
	before := r.URL.Query().Get("before")

	var rows *sql.Rows
	var err error
	if before != "" {
		rows, err = s.db.Query(`
			SELECT id, session_id, role, content, timestamp
			FROM messages
			WHERE session_id = ? AND timestamp < ?
			ORDER BY timestamp DESC
			LIMIT ?
		`, sessionID, before, limit)
	} else {
		rows, err = s.db.Query(`
			SELECT id, session_id, role, content, timestamp
			FROM messages
			WHERE session_id = ?
			ORDER BY timestamp DESC
			LIMIT ?
		`, sessionID, limit)
	}
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var messages []Message
	for rows.Next() {
		var m Message
		err := rows.Scan(&m.ID, &m.SessionID, &m.Role, &m.Content, &m.Timestamp)
		if err != nil {
			log.Printf("error scanning message: %v", err)
			continue
		}
		messages = append(messages, m)
	}

	// Reverse to get chronological order
	for i, j := 0, len(messages)-1; i < j; i, j = i+1, j-1 {
		messages[i], messages[j] = messages[j], messages[i]
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{"messages": messages})
}

func (s *Server) handleSendMessage(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	sessionID := vars["session_id"]

	var req struct {
		Content string `json:"content"`
	}
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	log.Printf("send request for session %s: %s", sessionID, req.Content)

	// Store the user message in the database immediately
	// (Don't rely on Emacs echoing it back - that can fail)
	timestamp := time.Now().UTC().Format(time.RFC3339)
	_, err := s.db.Exec(`
		INSERT INTO messages (session_id, role, content, timestamp)
		VALUES (?, 'user', ?, ?)
	`, sessionID, req.Content, timestamp)
	if err != nil {
		log.Printf("error storing user message: %v", err)
		http.Error(w, "failed to store message", http.StatusInternalServerError)
		return
	}

	// Update agent's last message
	_, _ = s.db.Exec(`
		UPDATE agents SET 
			last_message = ?,
			last_message_role = 'user',
			last_activity = ?
		WHERE session_id = ?
	`, truncate(req.Content, 200), timestamp, sessionID)

	// Broadcast send_request for Emacs to inject the message
	s.broadcast(WSEvent{
		Type: "send_request",
		Payload: map[string]string{
			"session_id": sessionID,
			"content":    req.Content,
		},
	})

	w.WriteHeader(http.StatusAccepted)
	json.NewEncoder(w).Encode(map[string]string{"status": "sent"})
}

func (s *Server) handleStopAgent(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	sessionID := vars["session_id"]

	s.wsMutex.RLock()
	clientCount := len(s.wsClients)
	s.wsMutex.RUnlock()

	log.Printf("[STOP] Request for session %s (ws clients: %d)", sessionID, clientCount)

	// Broadcast as a "stop_request" event - Emacs will interrupt the agent
	s.broadcast(WSEvent{
		Type: "stop_request",
		Payload: map[string]string{
			"session_id": sessionID,
		},
	})

	w.WriteHeader(http.StatusAccepted)
	json.NewEncoder(w).Encode(map[string]string{"status": "stopping"})
}

func (s *Server) handleCloseAgent(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	sessionID := vars["session_id"]

	log.Printf("close request for session %s", sessionID)

	// Broadcast as a "close_request" event - Emacs will kill the buffer
	s.broadcast(WSEvent{
		Type: "close_request",
		Payload: map[string]string{
			"session_id": sessionID,
		},
	})

	w.WriteHeader(http.StatusAccepted)
	json.NewEncoder(w).Encode(map[string]string{"status": "closing"})
}

func (s *Server) handleRestartAgent(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	sessionID := vars["session_id"]

	var req struct {
		ResumeMessageCount int `json:"resume_message_count"`
	}
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		// Default to 10 messages if not specified
		req.ResumeMessageCount = 10
	}

	// Get agent info before we close it
	var bufferName, project, projectPath string
	var parentSessionID *string
	err := s.db.QueryRow(`
		SELECT buffer_name, project, project_path, parent_session_id
		FROM agents WHERE session_id = ?
	`, sessionID).Scan(&bufferName, &project, &projectPath, &parentSessionID)

	if err != nil {
		log.Printf("restart request for unknown session %s: %v", sessionID, err)
		http.Error(w, "session not found", http.StatusNotFound)
		return
	}

	// Get last N messages for resume context
	rows, err := s.db.Query(`
		SELECT role, content FROM messages
		WHERE session_id = ?
		ORDER BY timestamp DESC
		LIMIT ?
	`, sessionID, req.ResumeMessageCount)

	var resumeMessages []map[string]string
	if err == nil {
		defer rows.Close()
		for rows.Next() {
			var role, content string
			if err := rows.Scan(&role, &content); err == nil {
				resumeMessages = append(resumeMessages, map[string]string{
					"role":    role,
					"content": content,
				})
			}
		}
		// Reverse to chronological order
		for i, j := 0, len(resumeMessages)-1; i < j; i, j = i+1, j-1 {
			resumeMessages[i], resumeMessages[j] = resumeMessages[j], resumeMessages[i]
		}
	}

	log.Printf("restart request for session %s (buffer: %s, %d resume messages)", sessionID, bufferName, len(resumeMessages))

	// Broadcast as a "restart_request" event - Emacs will kill buffer and respawn
	// TODO: Use proper acp/agent-shell resume functionality when available
	s.broadcast(WSEvent{
		Type: "restart_request",
		Payload: map[string]interface{}{
			"session_id":      sessionID,
			"buffer_name":     bufferName,
			"project":         project,
			"project_path":    projectPath,
			"resume_messages": resumeMessages,
		},
	})

	w.WriteHeader(http.StatusAccepted)
	json.NewEncoder(w).Encode(map[string]string{"status": "restarting"})
}

func (s *Server) handleNewAgent(w http.ResponseWriter, r *http.Request) {
	var req struct {
		Name string `json:"name"`
		Path string `json:"path"`
		Task string `json:"task"`
	}
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	log.Printf("new agent request: name=%s path=%s", req.Name, req.Path)

	s.broadcast(WSEvent{
		Type: "new_agent_request",
		Payload: map[string]string{
			"name": req.Name,
			"path": req.Path,
			"task": req.Task,
		},
	})

	w.WriteHeader(http.StatusAccepted)
	json.NewEncoder(w).Encode(map[string]string{"status": "spawning"})
}

func (s *Server) handleNewDispatcher(w http.ResponseWriter, r *http.Request) {
	var req struct {
		Path string `json:"path"`
	}
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	log.Printf("new dispatcher request: path=%s", req.Path)

	s.broadcast(WSEvent{
		Type: "new_dispatcher_request",
		Payload: map[string]string{
			"path": req.Path,
		},
	})

	w.WriteHeader(http.StatusAccepted)
	json.NewEncoder(w).Encode(map[string]string{"status": "spawning"})
}

func (s *Server) handleListFiles(w http.ResponseWriter, r *http.Request) {
	path := r.URL.Query().Get("path")
	if path == "" {
		http.Error(w, "path parameter required", http.StatusBadRequest)
		return
	}

	// Security: only allow listing within known project directories
	// Get the list of valid project paths from the database
	rows, err := s.db.Query(`SELECT DISTINCT project_path FROM agents WHERE closed_at IS NULL AND project_path != ''`)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var validPaths []string
	for rows.Next() {
		var projectPath string
		if err := rows.Scan(&projectPath); err == nil {
			validPaths = append(validPaths, projectPath)
		}
	}

	// Check if the requested path is within a valid project
	isValid := false
	for _, vp := range validPaths {
		if strings.HasPrefix(path, vp) {
			isValid = true
			break
		}
	}
	if !isValid {
		http.Error(w, "path not within a known project", http.StatusForbidden)
		return
	}

	// Read directory
	entries, err := os.ReadDir(path)
	if err != nil {
		http.Error(w, err.Error(), http.StatusNotFound)
		return
	}

	type FileEntry struct {
		Name  string `json:"name"`
		IsDir bool   `json:"is_dir"`
		Size  int64  `json:"size"`
	}

	// Check if hidden files should be shown
	showHidden := r.URL.Query().Get("show_hidden") == "true"

	var files []FileEntry
	for _, entry := range entries {
		// Skip hidden files unless show_hidden=true, but always show .claude
		if strings.HasPrefix(entry.Name(), ".") && entry.Name() != ".claude" && !showHidden {
			continue
		}

		info, err := entry.Info()
		if err != nil {
			continue
		}

		files = append(files, FileEntry{
			Name:  entry.Name(),
			IsDir: entry.IsDir(),
			Size:  info.Size(),
		})
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{"files": files, "path": path})
}

func (s *Server) handleReadFile(w http.ResponseWriter, r *http.Request) {
	path := r.URL.Query().Get("path")
	if path == "" {
		http.Error(w, "path parameter required", http.StatusBadRequest)
		return
	}

	// Security: only allow reading within known project directories
	rows, err := s.db.Query(`SELECT DISTINCT project_path FROM agents WHERE closed_at IS NULL AND project_path != ''`)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var validPaths []string
	for rows.Next() {
		var projectPath string
		if err := rows.Scan(&projectPath); err == nil {
			validPaths = append(validPaths, projectPath)
		}
	}

	isValid := false
	for _, vp := range validPaths {
		if strings.HasPrefix(path, vp) {
			isValid = true
			break
		}
	}
	if !isValid {
		http.Error(w, "path not within a known project", http.StatusForbidden)
		return
	}

	// Check file info
	info, err := os.Stat(path)
	if err != nil {
		http.Error(w, err.Error(), http.StatusNotFound)
		return
	}

	if info.IsDir() {
		http.Error(w, "cannot read directory", http.StatusBadRequest)
		return
	}

	// Determine content type
	ext := strings.ToLower(strings.TrimPrefix(filepath.Ext(path), "."))
	isImage := ext == "png" || ext == "jpg" || ext == "jpeg" || ext == "gif" || ext == "webp" || ext == "svg"
	isText := ext == "txt" || ext == "md" || ext == "org" || ext == "el" || ext == "py" || ext == "js" ||
		ext == "ts" || ext == "tsx" || ext == "jsx" || ext == "go" || ext == "rs" || ext == "json" ||
		ext == "yaml" || ext == "yml" || ext == "toml" || ext == "sh" || ext == "bash" || ext == "zsh" ||
		ext == "css" || ext == "html" || ext == "xml" || ext == "sql" || ext == "c" || ext == "cpp" ||
		ext == "h" || ext == "hpp" || ext == "java" || ext == "rb" || ext == "lua" || ext == ""

	// Limit file size
	maxSize := int64(1024 * 1024) // 1MB for text
	if isImage {
		maxSize = 10 * 1024 * 1024 // 10MB for images
	}

	if info.Size() > maxSize {
		http.Error(w, "file too large", http.StatusRequestEntityTooLarge)
		return
	}

	data, err := os.ReadFile(path)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	if isImage {
		// Return as base64 for images
		contentType := "image/" + ext
		if ext == "svg" {
			contentType = "image/svg+xml"
		}
		encoded := base64.StdEncoding.EncodeToString(data)
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]interface{}{
			"type":    "image",
			"content": encoded,
			"mime":    contentType,
			"path":    path,
		})
	} else if isText {
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]interface{}{
			"type":    "text",
			"content": string(data),
			"path":    path,
		})
	} else {
		http.Error(w, "unsupported file type", http.StatusUnsupportedMediaType)
	}
}

func (s *Server) handleBigRedButton(w http.ResponseWriter, r *http.Request) {
	log.Printf("BIG RED BUTTON pressed - interrupting all agents")

	s.broadcast(WSEvent{
		Type:    "big_red_button",
		Payload: map[string]string{},
	})

	w.WriteHeader(http.StatusAccepted)
	json.NewEncoder(w).Encode(map[string]string{"status": "interrupting_all"})
}

func (s *Server) handlePruneSessions(w http.ResponseWriter, r *http.Request) {
	log.Printf("Prune sessions requested - asking Emacs for alive sessions")

	// Broadcast request to Emacs to report alive sessions
	s.broadcast(WSEvent{
		Type:    "check_sessions_request",
		Payload: map[string]string{},
	})

	w.WriteHeader(http.StatusAccepted)
	json.NewEncoder(w).Encode(map[string]string{"status": "checking"})
}

// SessionInfo represents a session reported during sync
type SessionInfo struct {
	SessionID   string `json:"session_id"`
	BufferName  string `json:"buffer_name"`
	Project     string `json:"project"`
	ProjectPath string `json:"project_path"`
	Timestamp   string `json:"timestamp"`
}

func (s *Server) handleAliveSessions(w http.ResponseWriter, r *http.Request) {
	var req struct {
		Sessions []SessionInfo `json:"sessions"`
	}
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	log.Printf("Sync: received %d sessions from Emacs", len(req.Sessions))

	// Build a set of alive session IDs for fast lookup
	aliveSet := make(map[string]bool)
	for _, s := range req.Sessions {
		aliveSet[s.SessionID] = true
	}

	// Get all unclosed sessions from the database
	rows, err := s.db.Query(`SELECT session_id FROM agents WHERE closed_at IS NULL`)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var deadSessions []string
	for rows.Next() {
		var sessionID string
		if err := rows.Scan(&sessionID); err != nil {
			continue
		}
		if !aliveSet[sessionID] {
			deadSessions = append(deadSessions, sessionID)
		}
	}

	// Mark dead sessions as closed
	now := time.Now().UTC().Format(time.RFC3339)
	for _, sessionID := range deadSessions {
		_, err := s.db.Exec(`
			UPDATE agents SET closed_at = ?, status = 'closed'
			WHERE session_id = ?
		`, now, sessionID)
		if err != nil {
			log.Printf("Error closing dead session %s: %v", sessionID, err)
		} else {
			log.Printf("Pruned dead session: %s", sessionID)
			// Broadcast close event so UI updates
			s.broadcast(WSEvent{
				Type: "agent_close",
				Payload: AgentCloseEvent{
					SessionID: sessionID,
					Timestamp: now,
				},
			})
		}
	}

	// Upsert all sessions - this ensures any that are missing get added
	var addedSessions []string
	for _, session := range req.Sessions {
		// Check if session exists before upserting
		var exists int
		s.db.QueryRow(`SELECT COUNT(*) FROM agents WHERE session_id = ?`, session.SessionID).Scan(&exists)
		isNew := exists == 0

		_, err := s.db.Exec(`
			INSERT INTO agents (session_id, buffer_name, project, project_path, status, created_at)
			VALUES (?, ?, ?, ?, 'ready', ?)
			ON CONFLICT(session_id) DO UPDATE SET
				closed_at = NULL,
				status = CASE WHEN status = 'closed' THEN 'ready' ELSE status END
		`, session.SessionID, session.BufferName, session.Project, session.ProjectPath, session.Timestamp)

		if err != nil {
			log.Printf("Error upserting session %s: %v", session.SessionID, err)
		} else if isNew {
			log.Printf("Added new session: %s (%s)", session.SessionID, session.BufferName)
			addedSessions = append(addedSessions, session.SessionID)
			// Broadcast spawn event so UI updates
			s.broadcast(WSEvent{
				Type: "agent_spawn",
				Payload: AgentSpawnEvent{
					SessionID:   session.SessionID,
					BufferName:  session.BufferName,
					Project:     session.Project,
					ProjectPath: session.ProjectPath,
					Timestamp:   session.Timestamp,
				},
			})
		}
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"pruned_count": len(deadSessions),
		"pruned":       deadSessions,
		"added_count":  len(addedSessions),
		"added":        addedSessions,
	})
}

func (s *Server) handleListProjects(w http.ResponseWriter, r *http.Request) {
	// This will be populated by Emacs via a request/response pattern
	// For now, broadcast a request and return a placeholder
	// TODO: implement request/response pattern for this

	s.broadcast(WSEvent{
		Type:    "list_projects_request",
		Payload: map[string]string{},
	})

	// Return the unique projects we know about from agents
	rows, err := s.db.Query(`SELECT DISTINCT project FROM agents WHERE closed_at IS NULL ORDER BY project`)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var projects []string
	for rows.Next() {
		var project string
		if err := rows.Scan(&project); err == nil {
			projects = append(projects, project)
		}
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{"projects": projects})
}

// Debug handlers

func (s *Server) handleDebugWSStatus(w http.ResponseWriter, r *http.Request) {
	s.wsMutex.RLock()
	count := len(s.wsClients)
	// Count authorized vs unauthorized clients
	authorized := 0
	for _, auth := range s.wsClients {
		if auth {
			authorized++
		}
	}
	s.wsMutex.RUnlock()

	// Get active session count
	var activeCount int
	s.db.QueryRow(`SELECT COUNT(*) FROM agents WHERE closed_at IS NULL`).Scan(&activeCount)

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"websocket_clients":     count,
		"authorized_clients":    authorized,
		"active_agent_sessions": activeCount,
		"server_time":           time.Now().Format(time.RFC3339),
	})
}

func (s *Server) handleDebugSessions(w http.ResponseWriter, r *http.Request) {
	rows, err := s.db.Query(`
		SELECT session_id, buffer_name, project, status 
		FROM agents 
		WHERE closed_at IS NULL 
		ORDER BY last_activity DESC
	`)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var sessions []map[string]string
	for rows.Next() {
		var sessionID, bufferName, project, status string
		if err := rows.Scan(&sessionID, &bufferName, &project, &status); err == nil {
			sessions = append(sessions, map[string]string{
				"session_id":  sessionID,
				"buffer_name": bufferName,
				"project":     project,
				"status":      status,
			})
		}
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{"sessions": sessions})
}

func (s *Server) handleDebugSendMessage(w http.ResponseWriter, r *http.Request) {
	sessionID := r.URL.Query().Get("session_id")
	content := r.URL.Query().Get("content")

	if sessionID == "" || content == "" {
		http.Error(w, "session_id and content query params required", http.StatusBadRequest)
		return
	}

	log.Printf("[DEBUG] Simulating send for session %s: %s", sessionID, content)

	// Store the user message
	timestamp := time.Now().UTC().Format(time.RFC3339)
	_, err := s.db.Exec(`
		INSERT INTO messages (session_id, role, content, timestamp)
		VALUES (?, 'user', ?, ?)
	`, sessionID, content, timestamp)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// Broadcast send_request for Emacs
	s.broadcast(WSEvent{
		Type: "send_request",
		Payload: map[string]string{
			"session_id": sessionID,
			"content":    content,
		},
	})

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"status":     "sent",
		"session_id": sessionID,
		"content":    content,
		"timestamp":  timestamp,
	})
}

func (s *Server) handleDebugMessages(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	sessionID := vars["session_id"]

	rows, err := s.db.Query(`
		SELECT id, role, content, timestamp
		FROM messages
		WHERE session_id = ?
		ORDER BY timestamp ASC
	`, sessionID)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var messages []map[string]interface{}
	for rows.Next() {
		var id int64
		var role, content, timestamp string
		if err := rows.Scan(&id, &role, &content, &timestamp); err == nil {
			messages = append(messages, map[string]interface{}{
				"id":        id,
				"role":      role,
				"content":   content,
				"timestamp": timestamp,
			})
		}
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"session_id": sessionID,
		"count":      len(messages),
		"messages":   messages,
	})
}

// WebSocket handling

var upgrader = websocket.Upgrader{
	CheckOrigin: func(r *http.Request) bool {
		return true // Allow any origin (Tailscale is the auth layer)
	},
}

func (s *Server) handleWebSocket(w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Printf("[WS] upgrade error: %v", err)
		return
	}

	remoteAddr := r.RemoteAddr
	connectedAt := time.Now()

	s.wsMutex.Lock()
	s.wsClients[conn] = true
	clientCount := len(s.wsClients)
	s.wsMutex.Unlock()

	log.Printf("[WS] Client connected: %s (total clients: %d)", remoteAddr, clientCount)

	// We don't expect to receive messages from Emacs - it only listens.
	// But we need a read goroutine to detect connection close.
	// The "bad MASK" error at 5 seconds suggests something is sending malformed data.
	// Let's try just keeping the connection open without reading.
	go func() {
		defer func() {
			s.wsMutex.Lock()
			delete(s.wsClients, conn)
			remainingClients := len(s.wsClients)
			s.wsMutex.Unlock()
			conn.Close()
			duration := time.Since(connectedAt).Round(time.Second)
			log.Printf("[WS] Client disconnected: %s (was connected %v, remaining clients: %d)", remoteAddr, duration, remainingClients)
		}()

		// Set a very long read deadline and just wait
		// This keeps the connection goroutine alive without actively reading
		conn.SetReadDeadline(time.Now().Add(24 * time.Hour))

		// Still need to read to detect close, but ignore content
		for {
			_, _, err := conn.ReadMessage()
			if err != nil {
				log.Printf("[WS] Connection ended for %s: %v (connected for %v)", remoteAddr, err, time.Since(connectedAt).Round(time.Millisecond))
				return
			}
		}
	}()
}

func (s *Server) broadcast(event WSEvent) {
	s.broadcastWithRetry(event, 0)
}

func (s *Server) broadcastWithRetry(event WSEvent, attempt int) {
	data, err := json.Marshal(event)
	if err != nil {
		log.Printf("[BROADCAST] Error marshaling event: %v", err)
		return
	}

	s.wsMutex.RLock()
	clientCount := 0
	sentCount := 0
	var sendErrors []string
	for conn, authorized := range s.wsClients {
		if !authorized {
			continue
		}
		clientCount++
		err := conn.WriteMessage(websocket.TextMessage, data)
		if err != nil {
			sendErrors = append(sendErrors, err.Error())
		} else {
			sentCount++
		}
	}
	s.wsMutex.RUnlock()

	// Log broadcast result for important event types
	switch event.Type {
	case "send_request", "stop_request", "close_request", "restart_request":
		if sentCount > 0 {
			log.Printf("[BROADCAST] %s: sent to %d/%d clients", event.Type, sentCount, clientCount)
		} else if clientCount > 0 {
			log.Printf("[BROADCAST] %s: FAILED to send to any of %d clients: %v", event.Type, clientCount, sendErrors)
		} else {
			log.Printf("[BROADCAST] %s: no clients connected", event.Type)
		}
	}

	// For send_request and stop_request, retry if no clients received it
	if (event.Type == "send_request" || event.Type == "stop_request") && sentCount == 0 && attempt < 3 {
		log.Printf("[BROADCAST] %s: retrying in 1s (attempt %d/3)", event.Type, attempt+1)
		time.AfterFunc(time.Second, func() {
			s.broadcastWithRetry(event, attempt+1)
		})
	}
}

// Utilities

func truncate(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen] + "..."
}

func getTailscaleIP() (string, error) {
	cmd := exec.Command("tailscale", "ip", "-4")
	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get Tailscale IP: %v", err)
	}
	return strings.TrimSpace(string(output)), nil
}

func main() {
	listenAddr := flag.String("listen", "", "Address to listen on (default: auto-detect Tailscale IP)")
	port := flag.String("port", "8080", "Port to listen on")
	dbPath := flag.String("db", "agents.db", "Path to SQLite database")
	flag.Parse()

	var addr string
	if *listenAddr != "" {
		addr = *listenAddr
	} else {
		// Auto-detect Tailscale IP
		ip, err := getTailscaleIP()
		if err != nil {
			log.Fatalf("Could not detect Tailscale IP: %v\nMake sure 'tailscale' is in your PATH, or use --listen to specify address manually", err)
		}
		addr = ip + ":" + *port
		log.Printf("Auto-detected Tailscale IP: %s", ip)
	}

	config := Config{
		ListenAddr: addr,
		DBPath:     *dbPath,
	}

	server, err := NewServer(config)
	if err != nil {
		log.Fatalf("failed to create server: %v", err)
	}

	// Create listener explicitly on the specified address
	listener, err := net.Listen("tcp", config.ListenAddr)
	if err != nil {
		log.Fatalf("failed to listen on %s: %v", config.ListenAddr, err)
	}

	httpServer := &http.Server{
		Handler: server.router,
		// Note: ReadTimeout and WriteTimeout are NOT set because they break WebSocket
		// connections. WebSocket needs long-lived connections without read deadlines.
	}

	// Graceful shutdown
	go func() {
		sigChan := make(chan os.Signal, 1)
		signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
		<-sigChan

		log.Println("shutting down...")
		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		httpServer.Shutdown(ctx)
	}()

	log.Printf("listening on %s (Tailscale-only)", config.ListenAddr)
	if err := httpServer.Serve(listener); err != http.ErrServerClosed {
		log.Fatalf("server error: %v", err)
	}
}
