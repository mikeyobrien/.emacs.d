# Emacs Configuration Testing Guide

This document describes the testing strategy and infrastructure for this Emacs configuration.

## Overview

The test suite is divided into two main categories:

1. **Unit Tests** - Test individual modules in isolation
2. **Integration Tests** - Test how modules work together

## Test Structure

```
test/
├── test-helper.el              # Core test utilities
├── integration-helper.el       # Integration test utilities
├── run-tests.el               # Test runner
├── unit/                      # Unit tests
│   ├── init-config-test.el
│   ├── ai-helpers-test.el
│   └── ...
└── integration/               # Integration tests
    ├── full-config-loading-test.el
    ├── module-loading-test.el
    ├── keybinding-integration-test.el
    ├── package-integration-test.el
    └── ai-integration-test.el
```

## Running Tests

### Run All Tests

```bash
make test
```

### Run Unit Tests Only

```bash
make test-unit
```

### Run Integration Tests Only

```bash
make test-integration
```

### Run Specific Test File

```bash
emacs -Q --batch -l test/test-helper.el \
  -l test/integration/module-loading-test.el \
  -f ert-run-tests-batch-and-exit
```

### Run Specific Test

```bash
emacs -Q --batch -l test/test-helper.el \
  -l test/integration/module-loading-test.el \
  --eval "(ert-run-tests-batch-and-exit 'integration-init-config-loads-first)"
```

## Integration Test Categories

### 1. Full Configuration Loading Tests

**File:** `integration/full-config-loading-test.el`

Tests that verify the entire configuration loads without errors:

- `integration-full-init-loads-without-errors` - Full init.el loading
- `integration-early-init-gc-optimization` - GC optimization setup
- `integration-package-enable-disabled` - Package.el disabled
- `integration-android-detection` - Android environment detection

### 2. Module Loading Tests

**File:** `integration/module-loading-test.el`

Tests that verify modules load in correct order with proper dependencies:

- `integration-init-config-loads-first` - Config module independence
- `integration-config-variables-have-correct-types` - Type checking
- `integration-init-utils-loads` - Utils module functions
- `integration-machine-config-detection` - Machine type detection
- `integration-ai-backends-graceful-degradation` - Missing secrets handling
- `integration-modules-load-order` - Load order verification

### 3. Keybinding Integration Tests

**File:** `integration/keybinding-integration-test.el`

Tests that verify keybindings are properly configured:

- `integration-meow-setup-defines-keybindings` - Meow keybinding setup
- `integration-meow-terminal-integration` - Terminal mode integration
- `integration-meow-leader-map-has-org-support` - Org-mode support
- `integration-hydra-definitions-exist` - Hydra menus defined

### 4. Package Integration Tests

**File:** `integration/package-integration-test.el`

Tests that verify package configuration:

- `integration-straight-use-package-available` - Package manager setup
- `integration-completion-framework-components` - Completion system
- `integration-embark-keybindings` - Embark actions
- `integration-orderless-configuration` - Orderless completion
- `integration-tab-always-indent-complete` - TAB completion

### 5. AI Integration Tests

**File:** `integration/ai-integration-test.el`

Tests that verify AI backend and helper integration:

- `integration-ai-backend-switching` - Backend switching
- `integration-ai-backend-toggle` - Toggle between backends
- `integration-ai-helpers-region-or-buffer` - Helper utilities
- `integration-ai-helpers-all-functions-interactive` - Interactive commands
- `integration-ai-model-id-consistency` - Model ID consistency

## Test Utilities

### integration-helper.el Functions

#### Mock Functions

- `with-mocked-packages` - Mock package loading
- `with-mocked-keybindings` - Mock keybinding functions
- `integration-test-mock-completion-system` - Mock Vertico/Corfu/Consult
- `integration-test-mock-meow` - Mock Meow modal editing
- `integration-test-mock-hydra` - Mock Hydra menus
- `integration-test-mock-gptel` - Mock GPTel AI integration

#### Test Environment

- `integration-test-setup-environment` - Set up complete mock environment
- `with-integration-test-environment` - Run tests in isolated environment

#### Assertions

- `should-be-loadable` - Assert module loads without errors
- `should-define-function` - Assert function is defined
- `should-define-variable` - Assert variable is defined
- `should-be-interactive` - Assert command is interactive

## Writing Integration Tests

### Basic Pattern

```elisp
(require 'ert)
(require 'integration-helper)

(ert-deftest my-integration-test ()
  "Test description."
  (with-integration-test-environment
    ;; Your test code here
    (should-be-loadable 'my-module)
    (should-define-function 'my-function)))
```

### Testing Module Loading

```elisp
(ert-deftest integration-module-loads-correctly ()
  "Test that module loads with dependencies."
  ;; Mock dependencies
  (integration-test-mock-gptel)

  ;; Load module
  (should (require 'my-module nil t))

  ;; Verify functions/variables
  (should (fboundp 'my-function))
  (should (boundp 'my-variable)))
```

### Testing Keybindings

```elisp
(ert-deftest integration-keybindings-configured ()
  "Test that keybindings are set up."
  (integration-test-mock-meow)

  (require 'init-meow)

  ;; Verify keybinding functions exist
  (should (fboundp 'meow-setup))

  ;; Verify mode states configured
  (should (assq 'eat-mode meow-mode-state-list)))
```

### Testing AI Integration

```elisp
(ert-deftest integration-ai-feature-works ()
  "Test AI feature integration."
  ;; Mock GPTel
  (integration-test-mock-gptel)
  (setq openwebui-api-key "test-key")

  ;; Load backend
  (require 'backends)

  ;; Test switching
  (gptel-switch-to-bedrock)
  (should (eq gptel-backend gptel-bedrock)))
```

## Continuous Integration

Integration tests are designed to run in CI/CD environments:

```yaml
# .github/workflows/test.yml
- name: Run Integration Tests
  run: make test-integration
```

See `test/CI-CD.md` for detailed CI/CD setup.

## Best Practices

### 1. Isolation

Each integration test should be independent:

```elisp
(ert-deftest my-test ()
  "Test something."
  (with-integration-test-environment
    ;; This creates a clean environment
    (require 'my-module)
    (should ...)))
```

### 2. Mocking

Mock external dependencies to avoid:
- Network calls
- File system writes
- Package installations

```elisp
;; Mock instead of real API call
(defun gptel-request (_prompt &rest _args)
  "Mock GPTel request."
  nil)
```

### 3. Clear Naming

Use descriptive test names:

- ✅ `integration-ai-backend-switching-works-correctly`
- ❌ `test-ai-1`

### 4. Test One Thing

Each test should verify one specific behavior:

```elisp
;; Good - tests one specific thing
(ert-deftest integration-backend-switches-to-bedrock ()
  "Test switching to Bedrock backend."
  ...)

;; Bad - tests multiple unrelated things
(ert-deftest integration-test-everything ()
  "Test all AI features."
  ...)
```

### 5. Helpful Messages

Use descriptive assertion messages:

```elisp
(should (fboundp 'my-function))  ; OK
(should-define-function 'my-function)  ; Better - clearer message
```

## Troubleshooting

### Tests Fail Locally But Pass in CI

- Check for machine-specific configuration (init-work.el, secrets.el)
- Verify all mocks are in place
- Ensure tests don't depend on local state

### Tests Are Slow

- Ensure packages aren't being installed during tests
- Use mocks instead of loading actual packages
- Check that `use-package` is properly mocked

### Tests Are Flaky

- Tests shouldn't depend on each other
- Use `with-integration-test-environment` for isolation
- Reset state between tests

## Adding New Integration Tests

1. Identify what needs integration testing
2. Create test file in `test/integration/`
3. Use appropriate mocks from `integration-helper.el`
4. Write descriptive tests
5. Run tests: `make test-integration`
6. Document any new testing patterns here

## Coverage Goals

- **Unit Tests:** Individual functions and modules
- **Integration Tests:**
  - Module loading order
  - Inter-module dependencies
  - Keybinding setup
  - Package configuration
  - Feature integration (AI, completion, etc.)

## Future Improvements

- [ ] Add performance benchmarking tests
- [ ] Add tests for custom themes
- [ ] Add tests for org-mode configuration
- [ ] Add tests for project management features
- [ ] Increase test coverage to 80%+
- [ ] Add mutation testing
- [ ] Set up test coverage reporting

## References

- [ERT Manual](https://www.gnu.org/software/emacs/manual/html_node/ert/)
- [Emacs Testing Best Practices](https://www.emacswiki.org/emacs/UnitTesting)
- [Buttercup](https://github.com/jorgenschaefer/emacs-buttercup) - Alternative test framework
