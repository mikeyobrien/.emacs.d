.PHONY: test test-unit test-integration clean

EMACS ?= emacs
TEST_DIR = test

test: ## Run all tests
	$(EMACS) -Q --batch -l $(TEST_DIR)/run-tests.el

test-unit: ## Run unit tests only
	$(EMACS) -Q --batch -l $(TEST_DIR)/test-helper.el \
		-l $(TEST_DIR)/unit/*.el \
		-f ert-run-tests-batch-and-exit

test-integration: ## Run integration tests only
	$(EMACS) -Q --batch -l $(TEST_DIR)/test-helper.el \
		-l $(TEST_DIR)/integration/*.el \
		-f ert-run-tests-batch-and-exit

clean: ## Clean test artifacts
	find $(TEST_DIR) -name "*.elc" -delete

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
