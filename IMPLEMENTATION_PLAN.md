## Stage 1: Audit Current Meow/Eat Integration
**Goal**: Confirm how Meow state changes interact with Eat input modes and list the current shortcomings.
**Success Criteria**: Document that Eat stays in semi-char mode and Meow hooks never execute; identify fixes to apply.
**Tests**: Manual repro in an `eat` buffer by toggling Meow modes and observing key handling.
**Status**: Complete

## Stage 2: Wire Meow hooks to Eat modes
**Goal**: Ensure Meow entering/exiting insert mode switches Eat between char and emacs modes, and the integration actually loads.
**Success Criteria**: `eat-mode` buffers run a setup hook that registers Meow state hooks, disables conflicting mouse settings, and exposes leader keys.
**Tests**: Evaluate `lisp/init-misc.el`, open an `eat` buffer, confirm `i` enters insert mode and `C-c` bindings work as expected.
**Status**: Complete

## Stage 3: Validate UX and tidy integration
**Goal**: Sanity check keybindings/cursor behavior and keep the config simple/explicit.
**Success Criteria**: Manual test passes; no stray references to internal Meow functions; integration lives under the correct `use-package` section with brief comments.
**Tests**: Manual toggling (`i`, `C-[`, `C-c C-c`), exit terminal to ensure hooks clean up.
**Status**: Complete

## Stage 4: Add tab management keybinds
**Goal**: Enable built-in `tab-bar` and add ergonomic keybindings (global and Meow leader) for creating, navigating, closing, and renaming tabs.
**Success Criteria**: `SPC t` prefix provides tab actions; `C-c t` works in terminal; switching by numbers 1–9 functions; tabs visible only when multiple exist.
**Tests**: Open multiple tabs, use `SPC t [`/`]` and `C-c t [`/`]` to navigate; `SPC t n/d/o/r` for new/close/close-others/rename; `SPC t 2` selects tab 2.
**Status**: In Progress

## Stage 5: Fix init.el load failure
**Goal**: Ensure `init.el` loads cleanly without top-level errors across launch modes.
**Success Criteria**: No top-level void-function errors; auto-save/backup dirs created correctly; batch load `-Q` succeeds when packages are present.
**Tests**: `emacs --batch -Q -l init.el` with `user-emacs-directory` set; smoke-load `lisp/init-meow.el` with meow available.
**Status**: Complete
