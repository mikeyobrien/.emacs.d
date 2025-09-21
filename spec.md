# ACP Emacs Package Specification

## Project Overview

### Name
`acp-mode` - Agent Client Protocol integration for Emacs

### Purpose
Provide a unified chat interface within Emacs for interacting with ACP (Agent Client Protocol) compatible agents like Claude Code, Gemini CLI, and other agentic AI tools with real-time streaming capabilities.

### Inspiration
- **CodeCompanion.nvim**: Neovim's ACP client implementation
- **gptel**: Emacs chat interface patterns
- **eca-emacs**: Chat buffer architecture

## Problem Statement

Emacs users currently lack a standardized way to interact with the growing ecosystem of ACP-compatible AI coding agents. Each tool requires custom integrations, leading to fragmented experiences and limiting the ability to leverage the best agents for different tasks.

## Solution

A unified Emacs package that implements the Agent Client Protocol (ACP) using `jsonrpc.el`, providing a clean chat interface that works with any ACP-compatible agent while following Emacs conventions and leveraging existing ecosystem patterns.

## Technical Specifications

### Architecture

#### Core Stack
- **Protocol Layer**: `jsonrpc.el` for JSON-RPC 2.0 communication
- **Process Management**: Emacs `make-process` with filters for streaming
- **UI Framework**: Dedicated chat buffers with overlays and text properties
- **Project Integration**: `project.el` for project root detection
- **File Operations**: `ediff` integration for permission handling

#### Package Structure
```
lisp/
├── acp.el              # Main entry point and public API
├── acp-connection.el   # JSON-RPC connection management
├── acp-chat.el         # Chat buffer implementation
├── acp-session.el      # Session management
├── acp-tools.el        # Tool call handling
├── acp-files.el        # File operations and permissions
└── acp-utils.el        # Utility functions
```

### Core Components

#### 1. Connection Management (`acp-connection.el`)

**Primary Class:**
```elisp
(defclass acp-connection (jsonrpc-process-connection)
  ((capabilities :initform nil :accessor acp-capabilities)
   (session-buffers :initform (make-hash-table :test 'equal))
   (agent-command :initarg :agent-command :reader acp-agent-command)
   (project-root :initarg :project-root :reader acp-project-root))
  "ACP server connection with streaming support.")
```

**Key Methods:**
- `acp-start-agent (agent-command project-root)` - Initialize ACP agent connection
- `acp-handle-notification (connection method params)` - Dispatch streaming notifications
- `acp-handle-request (connection method params)` - Handle agent requests to client

#### 2. Session Management (`acp-session.el`)

**Session Strategy:**
- **Primary**: Defer to agent when `loadSession` capability available
- **Fallback**: Client-side session tracking for agents without persistence

**Core Functions:**
- `acp-create-session (connection &optional existing-id)` - Create or restore session
- `acp-session-management-strategy (capabilities)` - Determine persistence approach
- `acp-save-session-state (session-id state)` - Client-side persistence fallback

#### 3. Chat Buffer (`acp-chat.el`)

**Buffer Management:**
- Dedicated buffers with naming: `*acp-chat-{session-id}*`
- Per-buffer agent selection with default fallback
- Buffer-local variables for session state

**Key Features:**
- Real-time streaming via `session/update` notifications
- Chronological message display
- Interactive tool call elements
- Status display in header-line
- Context management system

**Core Functions:**
- `acp-chat-create-buffer (session agent)` - Create chat buffer
- `acp-chat-send-message (message)` - Send user prompt
- `acp-chat-handle-update (update)` - Process streaming updates

#### 4. Tool Call Handling (`acp-tools.el`)

**Permission System:**
- Integration with `ediff` for file operation previews
- Granular permission options (allow_once, allow_always, reject_once, reject_always)
- Interactive approval workflow

**Core Functions:**
- `acp-handle-tool-call (call-id name args)` - Display tool call request
- `acp-request-permission (tool-call options)` - User permission workflow
- `acp-show-file-diff (current-content proposed-content path)` - ediff integration

#### 5. File Operations (`acp-files.el`)

**Buffer Synchronization:**
- Detect conflicts between ACP file changes and open buffers
- Automatic synchronization for unmodified buffers
- User choice for handling conflicts in modified buffers

**Core Functions:**
- `acp-handle-file-read (path)` - Handle fs/read_text_file requests
- `acp-handle-file-write (path content)` - Handle fs/write_text_file requests
- `acp-sync-buffer-with-file (path content)` - Buffer synchronization

### User Interface Specifications

#### Main Entry Points

**Primary Command:**
```elisp
(defun acp-chat (&optional arg)
  "Start ACP chat session.
With prefix ARG, prompt for agent selection.
Otherwise use `acp-default-agent`."
  (interactive "P"))
```

**Configuration Variables:**
```elisp
(defcustom acp-default-agent "claude-code"
  "Default ACP agent command."
  :type 'string
  :group 'acp)

(defcustom acp-agent-commands
  '(("claude-code" . ("claude-code"))
    ("gemini" . ("gemini" "chat" "--acp")))
  "Alist of agent names to command lists."
  :type '(alist :key-type string :value-type (repeat string))
  :group 'acp)
```

#### Chat Buffer Layout

```
*acp-chat-sess_123*
═══════════════════════════════════════════════════════════════
 Agent: claude-code | Session: sess_123 | Project: my-project
═══════════════════════════════════════════════════════════════

[User] How can I optimize this function?

[Agent] I'll analyze the function and suggest optimizations.

🔧 Tool: read_file
   • File: src/utils.js
   ✓ Approved

Looking at your code, here are three key optimizations:

1. **Reduce complexity**: The nested loops can be simplified...
2. **Memoization**: Cache results for repeated inputs...
3. **Early returns**: Add guard clauses to exit early...

[Diff] src/utils.js
   • 15 additions, 8 deletions
   ✓ Apply changes

═══════════════════════════════════════════════════════════════
> Type your message here...
```

#### Interactive Elements

**Tool Call Display:**
```
🔧 Tool: edit_file                           [⚠ Pending]
   • File: src/components/Header.tsx
   • Action: Add TypeScript interfaces
   
   [Allow Once] [Allow Always] [Reject] [View Diff]
```

**Permission Dialog (via ediff):**
- Side-by-side comparison of current vs proposed content
- Standard ediff navigation and merge capabilities
- Custom keybindings for approval actions

### Protocol Implementation

#### JSON-RPC Methods

**Client → Agent:**
- `initialize` - Capability negotiation
- `session/new` - Create new session
- `session/load` - Restore existing session (if supported)
- `session/prompt` - Send user message
- `session/cancel` - Cancel ongoing operations

**Agent → Client:**
- `session/update` - Streaming content updates
- `session/request_permission` - Request tool call approval
- `fs/read_text_file` - Read file contents
- `fs/write_text_file` - Write file contents

#### Streaming Implementation

**Update Handler:**
```elisp
(cl-defmethod acp-handle-notification 
  ((conn acp-connection) (method (eql session/update))
   &key sessionId update &allow-other-keys)
  "Handle streaming session updates."
  (let ((buffer (acp-get-session-buffer conn sessionId)))
    (when buffer
      (with-current-buffer buffer
        (acp-process-update update)))))
```

**Update Types:**
- `content` - Text content streaming
- `tool_call` - Tool execution requests
- `tool_call_update` - Tool execution progress
- `diff` - File modification previews

### Configuration System

#### Basic Configuration
```elisp
;; Minimal setup
(require 'acp)
(setq acp-default-agent "claude-code")
```

#### Advanced Configuration
```elisp
;; Custom agent configuration
(setq acp-agent-commands
      '(("claude-code" . ("claude-code"))
        ("gemini" . ("gemini" "chat" "--acp"))
        ("custom-agent" . ("my-agent" "--stdio"))))

;; Chat buffer preferences  
(setq acp-chat-display-action '((display-buffer-pop-up-window)
                               (window-height . 0.4)))

;; Permission handling
(setq acp-auto-approve-read-operations t)
(setq acp-require-confirmation-for-writes t)

;; Project integration
(setq acp-respect-project-root t)
```

### Security Model

#### Permission Framework
- **File Operations**: All file read/write operations require explicit user approval
- **Granular Control**: Per-operation permission levels (once/always/never)
- **Visual Confirmation**: ediff integration for reviewing file changes
- **Safe Defaults**: Conservative permission defaults, user can relax as needed

#### Agent Command Validation
- Users must explicitly define agent commands in configuration
- No automatic discovery or execution of unknown agents
- Clear separation between agent communication and system access

### Error Handling

#### Connection Management
- Automatic reconnection attempts with exponential backoff
- Clear error messages for connection failures
- Graceful degradation when agents become unavailable

#### Session Recovery
- Automatic session recovery for agents supporting `loadSession`
- Client-side conversation replay for unsupported agents
- User notification of session state loss

#### Tool Call Failures
- Clear error reporting for failed tool operations
- Rollback mechanisms for partial file operations
- User choice in error recovery strategies

## Development Phases

### Phase 1: Core Infrastructure (MVP)
**Duration**: 2-3 weeks
**Features**:
- Basic ACP connection using jsonrpc.el
- Simple chat buffer with text display
- Agent command configuration
- Project root detection via project.el
- Basic session management

**Deliverables**:
- Working prototype with one agent
- Core package structure
- Basic documentation

### Phase 2: Streaming & Interaction
**Duration**: 2-3 weeks
**Features**:
- Real-time streaming via session/update
- Tool call display and basic permission handling
- Status updates in mode-line/header-line
- Error handling and reconnection

**Deliverables**:
- Full streaming implementation
- Interactive tool call approval
- Robust error handling

### Phase 3: Advanced Features
**Duration**: 3-4 weeks
**Features**:
- ediff integration for file permissions
- Buffer synchronization for file operations
- Session persistence (client-side fallback)
- Context management system

**Deliverables**:
- Production-ready file operation handling
- Complete permission system
- Session management

### Phase 4: Polish & Documentation
**Duration**: 1-2 weeks
**Features**:
- UI refinements and optimizations
- Comprehensive documentation
- Package distribution preparation
- Community feedback integration

**Deliverables**:
- Release-ready package
- User and developer documentation
- MELPA submission

## Success Criteria

### Functional Requirements
- [ ] Successfully communicate with multiple ACP agents (Claude Code, Gemini)
- [ ] Real-time streaming chat interface
- [ ] Secure file operation permission system
- [ ] Session persistence (agent or client-managed)
- [ ] Buffer synchronization without data loss
- [ ] Project-aware context management

### User Experience Requirements
- [ ] Intuitive Emacs-native interface
- [ ] Smooth performance with large responses
- [ ] Clear visual feedback for all operations
- [ ] Minimal configuration required for basic usage
- [ ] Integration with existing Emacs workflows

### Technical Requirements
- [ ] Robust error handling and recovery
- [ ] Memory efficient streaming implementation
- [ ] Clean package architecture and documentation
- [ ] Compatibility with major Emacs versions (28+)
- [ ] Comprehensive test coverage

## Risk Assessment

### Technical Risks
- **jsonrpc.el Limitations**: Risk of discovering protocol limitations
  - *Mitigation*: Early prototyping and fallback plans
- **Agent Compatibility**: Different ACP implementations may have subtle differences
  - *Mitigation*: Test with multiple agents, implement adaptability
- **Performance**: Large streaming responses may impact Emacs performance
  - *Mitigation*: Implement chunked processing and memory management

### User Adoption Risks
- **Configuration Complexity**: Users may find agent setup difficult
  - *Mitigation*: Provide clear documentation and sensible defaults
- **Learning Curve**: ACP concepts may be unfamiliar
  - *Mitigation*: Progressive disclosure and excellent onboarding

### Ecosystem Risks
- **ACP Evolution**: Protocol changes may require package updates
  - *Mitigation*: Version detection and backward compatibility
- **Agent Availability**: Some agents may become unavailable
  - *Mitigation*: Support multiple agents, clear error messages

## Future Enhancements

### Near-term (6 months)
- **Multiple Chat Sessions**: Support for concurrent conversations
- **Context Templates**: Predefined context patterns for common tasks  
- **Agent Marketplace**: Easy discovery and installation of new agents
- **Advanced Permissions**: Rule-based automatic approval systems

### Long-term (12+ months)
- **Multi-modal Support**: Handle images, audio, and other content types
- **Collaborative Features**: Share sessions between team members
- **Custom Agent Support**: Framework for building custom ACP agents
- **Integration Ecosystem**: Hooks for other Emacs packages

## Conclusion

This specification provides a comprehensive roadmap for building a production-ready ACP client for Emacs. By leveraging existing Emacs infrastructure (`jsonrpc.el`, `project.el`) and following established patterns from successful implementations, we can deliver a robust, secure, and user-friendly interface to the growing ecosystem of ACP-compatible agents.

The phased approach ensures early value delivery while building toward a complete solution that positions Emacs users at the forefront of the agentic AI revolution in software development.