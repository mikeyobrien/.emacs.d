#!/bin/bash
# verify-tests.sh - Verify test structure without running tests

set -e

echo "🔍 Verifying Emacs configuration test structure..."
echo ""

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check test directories
echo -e "${BLUE}📁 Checking test directories...${NC}"
if [ -d "test/unit" ]; then
    echo -e "${GREEN}✓${NC} Unit test directory exists"
else
    echo -e "${YELLOW}⚠${NC} Unit test directory missing"
fi

if [ -d "test/integration" ]; then
    echo -e "${GREEN}✓${NC} Integration test directory exists"
else
    echo -e "${YELLOW}⚠${NC} Integration test directory missing"
fi

echo ""

# Count test files
echo -e "${BLUE}📊 Test file statistics:${NC}"
unit_count=$(find test/unit -name "*-test.el" 2>/dev/null | wc -l)
integration_count=$(find test/integration -name "*-test.el" 2>/dev/null | wc -l)
echo -e "  Unit test files: ${GREEN}${unit_count}${NC}"
echo -e "  Integration test files: ${GREEN}${integration_count}${NC}"

echo ""

# Count individual tests
echo -e "${BLUE}🧪 Test case statistics:${NC}"
if [ $unit_count -gt 0 ]; then
    unit_tests=$(grep -r "ert-deftest" test/unit/*.el 2>/dev/null | wc -l)
    echo -e "  Unit tests: ${GREEN}${unit_tests}${NC}"
fi

if [ $integration_count -gt 0 ]; then
    integration_tests=$(grep -r "ert-deftest" test/integration/*.el 2>/dev/null | wc -l)
    echo -e "  Integration tests: ${GREEN}${integration_tests}${NC}"
fi

echo ""

# Verify test helpers
echo -e "${BLUE}🔧 Checking test utilities...${NC}"
if [ -f "test/test-helper.el" ]; then
    echo -e "${GREEN}✓${NC} test-helper.el exists"
else
    echo -e "${YELLOW}⚠${NC} test-helper.el missing"
fi

if [ -f "test/integration-helper.el" ]; then
    echo -e "${GREEN}✓${NC} integration-helper.el exists"
else
    echo -e "${YELLOW}⚠${NC} integration-helper.el missing"
fi

if [ -f "test/run-tests.el" ]; then
    echo -e "${GREEN}✓${NC} run-tests.el exists"
else
    echo -e "${YELLOW}⚠${NC} run-tests.el missing"
fi

echo ""

# List integration test files
echo -e "${BLUE}📝 Integration test files:${NC}"
for file in test/integration/*-test.el; do
    if [ -f "$file" ]; then
        test_count=$(grep -c "ert-deftest" "$file")
        echo -e "  ${GREEN}✓${NC} $(basename $file) (${test_count} tests)"
    fi
done

echo ""

# Verify Makefile targets
echo -e "${BLUE}🎯 Checking Makefile targets...${NC}"
if [ -f "Makefile" ]; then
    if grep -q "^test:" Makefile; then
        echo -e "${GREEN}✓${NC} 'make test' target exists"
    fi
    if grep -q "^test-unit:" Makefile; then
        echo -e "${GREEN}✓${NC} 'make test-unit' target exists"
    fi
    if grep -q "^test-integration:" Makefile; then
        echo -e "${GREEN}✓${NC} 'make test-integration' target exists"
    fi
else
    echo -e "${YELLOW}⚠${NC} Makefile not found"
fi

echo ""
echo -e "${GREEN}✅ Test structure verification complete!${NC}"
echo ""
echo "To run tests (requires Emacs installed):"
echo "  make test              # Run all tests"
echo "  make test-unit         # Run unit tests only"
echo "  make test-integration  # Run integration tests only"
