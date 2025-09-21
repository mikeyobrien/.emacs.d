# CodeCompanion Architecture Analysis for Emacs Implementation

## Overview
This document analyzes the CodeCompanion Neovim package to understand architectural patterns that can be adapted for Emacs. CodeCompanion is a sophisticated AI agent system with support for both HTTP-based LLMs and the ACP (Agent Communication Protocol) for local Claude CLI agents.

## 1. Architecture and Code Organization

### Core Structure
```
codecompanion/
├── init.lua                    # Main entry point and public API
├── config.lua                 # Comprehensive configuration system
├── adapters/                  # Adapter pattern for different AI providers
│   ├── init.lua              # Factory methods for adapter resolution
│   ├── http/                 # HTTP-based adapters (OpenAI, Anthropic, etc.)
│   └── acp/                  # ACP (Agent Communication Protocol) adapters
├── strategies/               # Different interaction patterns
│   ├── chat/                # Chat-based interaction
│   ├── inline/              # Inline code editing
│   └── cmd/                 # Command execution
├── acp/                     # ACP protocol implementation
├── utils/                   # Shared utilities
└── providers/               # UI providers (telescope, fzf, etc.)
```

### Key Design Patterns

1. **Strategy Pattern**: Different interaction modes (chat, inline, cmd) as strategies
2. **Adapter Pattern**: Unified interface for different AI providers
3. **Factory Pattern**: Centralized adapter resolution and creation
4. **Observer Pattern**: Event system with subscribers and watchers
5. **Command Pattern**: Tools and slash commands as discrete operations

## 2. Chat Buffer Implementation and Streaming UI

### Chat Buffer Core (`strategies/chat/init.lua`)
```lua
---@class CodeCompanion.Chat
local Chat = {
  adapter = nil,           -- HTTP or ACP adapter
  bufnr = 0,              -- Neovim buffer number
  messages = {},          -- Message history
  context_items = {},     -- Additional context (files, tools, etc.)
  tool_registry = nil,    -- Available tools
  ui = nil,              -- UI management
  current_request = nil,  -- Active request tracking
  -- ... more fields
}
```

**Key Features:**
- **Persistent State**: Chat maintains full conversation history and context
- **Tool Integration**: Built-in tool registry with automatic schema generation
- **Context Management**: Smart context tracking for files, buffers, and variables
- **Streaming**: Real-time response rendering with chunks

### UI Management (`strategies/chat/ui/`)
```lua
-- UI Builder Pattern
local Builder = {
  chat = nil,
  formatters = {},  -- Message formatters for different types
  icons = {},       -- Status and type icons
}

-- Key methods:
function Builder:add_message(data, opts)
  -- Formats and renders messages to buffer
end

function Builder:apply_extmarks(bufnr, start_line, end_line, opts)
  -- Applies syntax highlighting and visual markers
end
```

**Streaming Implementation:**
1. **Chunked Updates**: Messages arrive as chunks and are immediately rendered
2. **Extmarks**: Uses Neovim's extmark API for rich text formatting
3. **Folding**: Automatic folding of tool output and reasoning sections
4. **Real-time Tokens**: Live token count updates during streaming

### Tree-sitter Integration
- **Markdown Parser**: Chat buffer uses markdown with custom queries
- **YAML Parser**: Settings parsing with validation
- **Custom Queries**: Specific tree-sitter queries for chat parsing

## 3. File Operations and Buffer Integration

### Dual Mode Operation
CodeCompanion intelligently handles both files and open buffers:

```lua
local function edit_file_or_buffer(action, chat_bufnr, output_handler, opts)
  local bufnr = buffers.get_bufnr_from_filepath(action.filepath)
  if bufnr then
    return edit_buffer(bufnr, chat_bufnr, action, output_handler, opts)
  else
    return edit_file(action, chat_bufnr, output_handler, opts)
  end
end
```

### File Tools (`strategies/chat/tools/catalog/`)

#### Read File Tool
```lua
-- Supports line-range reading with 0-based indexing
{
  filepath = "path/to/file",
  start_line_number_base_zero = 0,
  end_line_number_base_zero = -1  -- -1 = end of file
}
```

#### Edit File Tool (`insert_edit_into_file.lua`)
- **Patch-based Editing**: Uses sophisticated patch algorithm
- **Context Matching**: Fuzzy matching with confidence scoring
- **Buffer Integration**: Seamless editing of open buffers vs files
- **Diff Visualization**: Real-time diff previews before applying changes

### Patch Algorithm
```lua
-- Example patch format with markers
local PROMPT = [[
*** Begin Patch ***
function old_function() {
    // old implementation
}
*** End Patch ***

*** Begin Patch ***
function new_function() {
    // new implementation  
}
*** End Patch ***
]]
```

## 4. Permission Handling for Agent Operations

### ACP Permission System (`strategies/chat/acp/request_permission.lua`)

**Permission Types:**
- `allow_always` - Permanent permission
- `allow_once` - Single operation permission  
- `reject_once` - Deny this operation
- `reject_always` - Permanent denial

```lua
-- Permission request structure
{
  tool_call = {
    kind = "edit_file",
    title = "Edit config.lua", 
    content = { type = "diff", oldText = "...", newText = "..." }
  },
  options = {
    { kind = "allow_once", optionId = "allow_1" },
    { kind = "reject_once", optionId = "reject_1" }
  }
}
```

### Interactive Permission Flow
1. **Diff Preview**: Shows visual diff in floating window
2. **Keymap Setup**: Dynamic keymaps for permission responses
3. **Timeout Handling**: Configurable timeout with default response
4. **Auto-cleanup**: Automatic resource cleanup on decision

### Permission Keymaps
```lua
-- From config.strategies.chat.keymaps
_acp_allow_always = { modes = { n = "g1" } },
_acp_allow_once = { modes = { n = "g2" } },
_acp_reject_once = { modes = { n = "g3" } },
_acp_reject_always = { modes = { n = "g4" } }
```

## 5. Agent Management and Configuration System

### Adapter Factory Pattern
```lua
-- adapters/init.lua - Unified interface for all agent types
function M.resolve(adapter, opts)
  if adapter_type(adapter) == "acp" then
    return require("codecompanion.adapters.acp").resolve(adapter, opts)
  end
  return require("codecompanion.adapters.http").resolve(adapter, opts)
end
```

### Configuration System (`config.lua`)
- **Layered Defaults**: Deep configuration merging
- **Environment Variables**: Automatic env var resolution
- **Schema Validation**: Runtime validation of adapter settings
- **Hot Reloading**: Dynamic adapter switching

### Agent Types

#### HTTP Adapters
```lua
adapters = {
  http = {
    anthropic = "anthropic",
    openai = "openai", 
    copilot = "copilot",
    -- ... more providers
  }
}
```

#### ACP Adapters  
```lua
adapters = {
  acp = {
    gemini_cli = "gemini_cli"  -- Local Claude CLI via ACP
  }
}
```

### Tool Management
```lua
tools = {
  groups = {
    ["full_stack_dev"] = {
      tools = { "cmd_runner", "create_file", "read_file", "insert_edit_into_file" }
    }
  }
}
```

## 6. ACP Protocol and JSON-RPC Communication

### ACP Connection Management (`acp/init.lua`)
```lua
---@class CodeCompanion.ACP.Connection
local Connection = {
  adapter = nil,
  session_id = nil,
  pending_responses = {},  -- Request ID tracking
  _state = {
    handle = nil,          -- Process handle
    next_id = 1,          -- JSON-RPC ID counter
    stdout_buffer = ""     -- Buffered output
  }
}
```

### Connection Lifecycle
1. **Process Spawning**: Starts ACP agent process
2. **Initialization**: JSON-RPC initialize request
3. **Authentication**: Optional auth method negotiation  
4. **Session Management**: Create or load session
5. **Message Handling**: Bidirectional JSON-RPC communication

### JSON-RPC Implementation
```lua
-- Synchronous request pattern
function Connection:_send_request(method, params)
  local id = self._state.next_id
  local request = {
    jsonrpc = "2.0",
    id = id, 
    method = method,
    params = params or {}
  }
  self:_write_to_process(encode(request) .. "\n")
  return self:_wait_for_response(id)
end
```

### ACP Methods (`acp/methods.lua`)
```lua
local METHODS = {
  INITIALIZE = "agent/initialize",
  AUTHENTICATE = "agent/authenticate", 
  SESSION_NEW = "session/new",
  SESSION_LOAD = "session/load",
  SESSION_UPDATE = "session/update",
  SESSION_REQUEST_PERMISSION = "session/request_permission",
  FS_READ_TEXT_FILE = "fs/read_text_file",
  FS_WRITE_TEXT_FILE = "fs/write_text_file"
}
```

### Prompt Builder (`acp/prompt_builder.lua`)
**Fluent Interface Pattern:**
```lua
connection:prompt(messages)
  :on_message_chunk(function(content) ... end)
  :on_thought_chunk(function(content) ... end)
  :on_tool_call(function(call) ... end)
  :on_permission_request(function(req) ... end)
  :on_complete(function(reason) ... end)
  :send()
```

## Key Implementation Insights for Emacs

### 1. Buffer Management
- Use Emacs overlays equivalent to Neovim's extmarks
- Implement buffer-local variables for chat state
- Use text properties for message metadata

### 2. Process Management  
- `make-process` for ACP agent spawning
- JSON-RPC library or custom implementation
- Process filters for streaming data handling

### 3. UI Framework
- Modal interfaces using completing-read or custom minibuffer
- Popup/child frames for diff previews
- Keybinding contexts for permission handling

### 4. Integration Patterns
- Use Emacs hooks system equivalent to CodeCompanion's subscribers
- File watchers using `file-notify-add-watch`
- LSP integration through existing LSP clients

### 5. Configuration Architecture
- Custom.el integration for user configuration
- Use `defcustom` for configuration schema
- Environment variable resolution patterns

## Conclusion

CodeCompanion demonstrates several sophisticated patterns that translate well to Emacs:
- **Modular Architecture**: Clear separation of concerns with adapters, strategies, and tools
- **Event-Driven Design**: Extensive use of callbacks and event handlers
- **Flexible UI**: Provider pattern for different completion frameworks
- **Protocol Abstraction**: Clean separation between HTTP and ACP protocols
- **Security Model**: Comprehensive permission system for agent operations

The codebase provides an excellent blueprint for implementing a similar system in Emacs while leveraging Emacs-specific capabilities like the built-in completion system, text properties, and process management.