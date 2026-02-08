package main

import (
	"context"
	"crypto/subtle"
	"database/sql"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net"
	"net/http"
	"os"
	"os/signal"
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
	AuthToken  string // Bearer token for authentication
}

// Server holds all server state
type Server struct {
	config    Config
	db        *sql.DB
	router    *mux.Router
	wsClients map[*websocket.Conn]bool
	wsMutex   sync.RWMutex
	upgrader  websocket.Upgrader
}

// Agent represents an agent session
type Agent struct {
	SessionID       string    `json:"session_id"`
	BufferName      string    `json:"buffer_name"`
	Project         string    `json:"project"`
	ParentSessionID *string   `json:"parent_session_id"`
	Status          string    `json:"status"`
	LastMessage     string    `json:"last_message"`
	LastMessageRole string    `json:"last_message_role"`
	LastActivity    time.Time `json:"last_activity"`
	CreatedAt       time.Time `json:"created_at"`
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
		upgrader: websocket.Upgrader{
			CheckOrigin: func(r *http.Request) bool {
				// Allow connections from tailnet only (already enforced by listener)
				return true
			},
		},
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
	return err
}

func (s *Server) setupRoutes() {
	// CORS middleware (must be before auth)
	s.router.Use(s.corsMiddleware)
	// Auth middleware for all routes
	s.router.Use(s.authMiddleware)

	// Events from Emacs (POST)
	s.router.HandleFunc("/events/agent-spawn", s.handleAgentSpawn).Methods("POST")
	s.router.HandleFunc("/events/agent-close", s.handleAgentClose).Methods("POST")
	s.router.HandleFunc("/events/message", s.handleMessage).Methods("POST")
	s.router.HandleFunc("/events/status", s.handleStatus).Methods("POST")

	// API for mobile app (GET/POST) - include OPTIONS for CORS preflight
	s.router.HandleFunc("/agents", s.handleGetAgents).Methods("GET", "OPTIONS")
	s.router.HandleFunc("/agents/{session_id}/messages", s.handleGetMessages).Methods("GET", "OPTIONS")
	s.router.HandleFunc("/agents/{session_id}/send", s.handleSendMessage).Methods("POST", "OPTIONS")

	// WebSocket for real-time updates
	s.router.HandleFunc("/ws", s.handleWebSocket)

	// Health check (no auth required - useful for monitoring)
	s.router.HandleFunc("/health", func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("ok"))
	}).Methods("GET")
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

func (s *Server) authMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Skip auth for health check
		if r.URL.Path == "/health" {
			next.ServeHTTP(w, r)
			return
		}

		// Skip auth if token is "NOAUTH" (for testing)
		if s.config.AuthToken == "NOAUTH" {
			next.ServeHTTP(w, r)
			return
		}

		// Check Bearer token
		auth := r.Header.Get("Authorization")
		if auth == "" {
			http.Error(w, "missing authorization header", http.StatusUnauthorized)
			return
		}

		if !strings.HasPrefix(auth, "Bearer ") {
			http.Error(w, "invalid authorization format", http.StatusUnauthorized)
			return
		}

		token := strings.TrimPrefix(auth, "Bearer ")
		if subtle.ConstantTimeCompare([]byte(token), []byte(s.config.AuthToken)) != 1 {
			http.Error(w, "invalid token", http.StatusUnauthorized)
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
		INSERT INTO agents (session_id, buffer_name, project, parent_session_id, status, created_at)
		VALUES (?, ?, ?, ?, 'ready', ?)
		ON CONFLICT(session_id) DO UPDATE SET
			buffer_name = excluded.buffer_name,
			project = excluded.project,
			parent_session_id = excluded.parent_session_id,
			status = 'ready',
			closed_at = NULL
	`, event.SessionID, event.BufferName, event.Project, event.ParentSessionID, event.Timestamp)

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
		SELECT session_id, buffer_name, project, parent_session_id, 
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
		var parentID, lastMsg, lastMsgRole sql.NullString
		var closedAt sql.NullTime

		err := rows.Scan(&a.SessionID, &a.BufferName, &a.Project, &parentID,
			&a.Status, &lastMsg, &lastMsgRole, &a.LastActivity, &a.CreatedAt, &closedAt)
		if err != nil {
			log.Printf("error scanning agent: %v", err)
			continue
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

	rows, err := s.db.Query(`
		SELECT id, session_id, role, content, timestamp
		FROM messages
		WHERE session_id = ?
		ORDER BY timestamp DESC
		LIMIT ?
	`, sessionID, limit)
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

	// TODO: Forward to Emacs via some mechanism
	// For now, just broadcast that a message was requested
	// The Emacs side will need to poll or we need a reverse connection

	log.Printf("send request for session %s: %s", sessionID, req.Content)

	// Broadcast as a "send_request" event - Emacs can listen for these
	s.broadcast(WSEvent{
		Type: "send_request",
		Payload: map[string]string{
			"session_id": sessionID,
			"content":    req.Content,
		},
	})

	w.WriteHeader(http.StatusAccepted)
	json.NewEncoder(w).Encode(map[string]string{"status": "queued"})
}

// WebSocket handling

// WSClient represents a connected WebSocket client
type WSClient struct {
	conn       *websocket.Conn
	authorized bool
}

func (s *Server) handleWebSocket(w http.ResponseWriter, r *http.Request) {
	// Check auth via query param (WS doesn't support headers from browsers/apps)
	token := r.URL.Query().Get("token")
	authorized := token != "" && subtle.ConstantTimeCompare([]byte(token), []byte(s.config.AuthToken)) == 1

	conn, err := s.upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Printf("websocket upgrade error: %v", err)
		return
	}

	client := &WSClient{conn: conn, authorized: authorized}

	s.wsMutex.Lock()
	s.wsClients[conn] = authorized
	s.wsMutex.Unlock()

	if authorized {
		log.Printf("websocket client connected (authorized): %s", conn.RemoteAddr())
	} else {
		log.Printf("websocket client connected (pending auth): %s", conn.RemoteAddr())
	}

	// Keep connection alive, handle messages
	go func() {
		defer func() {
			s.wsMutex.Lock()
			delete(s.wsClients, conn)
			s.wsMutex.Unlock()
			conn.Close()
			log.Printf("websocket client disconnected: %s", conn.RemoteAddr())
		}()

		for {
			_, msgBytes, err := conn.ReadMessage()
			if err != nil {
				return
			}

			// Handle incoming messages (for auth or commands)
			var msg map[string]interface{}
			if err := json.Unmarshal(msgBytes, &msg); err != nil {
				continue
			}

			// Handle auth message (alternative to query param)
			if msgType, ok := msg["type"].(string); ok && msgType == "auth" {
				if tokenStr, ok := msg["token"].(string); ok {
					if subtle.ConstantTimeCompare([]byte(tokenStr), []byte(s.config.AuthToken)) == 1 {
						s.wsMutex.Lock()
						s.wsClients[conn] = true
						client.authorized = true
						s.wsMutex.Unlock()
						log.Printf("websocket client authorized: %s", conn.RemoteAddr())

						// Send auth success
						conn.WriteJSON(map[string]string{"type": "auth_success"})
					} else {
						conn.WriteJSON(map[string]string{"type": "auth_failed"})
					}
				}
			}
		}
	}()
}

func (s *Server) broadcast(event WSEvent) {
	data, err := json.Marshal(event)
	if err != nil {
		log.Printf("error marshaling event: %v", err)
		return
	}

	s.wsMutex.RLock()
	defer s.wsMutex.RUnlock()

	for conn, authorized := range s.wsClients {
		if !authorized {
			continue // Only send to authorized clients
		}
		err := conn.WriteMessage(websocket.TextMessage, data)
		if err != nil {
			log.Printf("error sending to websocket: %v", err)
		}
	}
}

// Utilities

func truncate(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen] + "..."
}

func main() {
	listenAddr := flag.String("listen", "", "Address to listen on (e.g., 100.x.x.x:8080)")
	dbPath := flag.String("db", "agents.db", "Path to SQLite database")
	tokenFile := flag.String("token-file", "", "Path to file containing auth token")
	allowLocalhost := flag.Bool("allow-localhost", false, "Allow binding to localhost (for testing)")
	flag.Parse()

	// Require explicit listen address (should be Tailscale IP)
	if *listenAddr == "" {
		log.Fatal("--listen is required (use your Tailscale IP, e.g., 100.x.x.x:8080)")
	}

	// Validate it looks like a Tailscale IP or localhost
	host, _, err := net.SplitHostPort(*listenAddr)
	if err != nil {
		log.Fatalf("invalid listen address: %v", err)
	}
	isLocalhost := host == "127.0.0.1" || host == "localhost"
	isTailscale := strings.HasPrefix(host, "100.")
	
	if !isTailscale && !isLocalhost {
		log.Printf("WARNING: listen address %s doesn't look like a Tailscale IP (100.x.x.x)", host)
		log.Printf("This server should only be exposed on your Tailscale network!")
	}
	if isLocalhost && !*allowLocalhost {
		log.Fatal("localhost binding requires --allow-localhost flag (for testing only)")
	}

	// Load auth token
	var authToken string
	if *tokenFile != "" {
		data, err := os.ReadFile(*tokenFile)
		if err != nil {
			log.Fatalf("failed to read token file: %v", err)
		}
		authToken = strings.TrimSpace(string(data))
	} else {
		authToken = os.Getenv("AGENT_SHELL_API_TOKEN")
	}

	if authToken == "" {
		log.Fatal("auth token required: set AGENT_SHELL_API_TOKEN or use --token-file")
	}

	config := Config{
		ListenAddr: *listenAddr,
		DBPath:     *dbPath,
		AuthToken:  authToken,
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
		Handler:      server.router,
		ReadTimeout:  10 * time.Second,
		WriteTimeout: 10 * time.Second,
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

	log.Printf("listening on %s (Tailscale-only, token auth required)", config.ListenAddr)
	if err := httpServer.Serve(listener); err != http.ErrServerClosed {
		log.Fatalf("server error: %v", err)
	}
}
