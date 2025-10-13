# CI/CD Integration for Emacs Configuration Tests

## Overview
This document describes how to integrate ERT tests into CI/CD pipelines for automated testing of Emacs configuration.

## GitHub Actions

### Basic Workflow
Create `.github/workflows/test.yml`:

```yaml
name: Test Emacs Configuration

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        emacs-version: ['28.2', '29.1', '29.2']
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Install Emacs
      uses: purcell/setup-emacs@master
      with:
        version: ${{ matrix.emacs-version }}
    
    - name: Run tests
      run: make test
```

## GitLab CI

### Basic Pipeline
Create `.gitlab-ci.yml`:

```yaml
test:
  image: silex/emacs:28.2
  script:
    - make test
  only:
    - main
    - merge_requests
```

## Local Pre-commit Hook

### Setup
Create `.git/hooks/pre-commit`:

```bash
#!/bin/bash
echo "Running Emacs configuration tests..."
make test
if [ $? -ne 0 ]; then
    echo "Tests failed. Commit aborted."
    exit 1
fi
```

Make executable:
```bash
chmod +x .git/hooks/pre-commit
```

## Docker Testing

### Dockerfile
```dockerfile
FROM silex/emacs:29.1

WORKDIR /config
COPY . .

RUN make test
```

### Build and Test
```bash
docker build -t emacs-config-test .
docker run --rm emacs-config-test
```

## Test Coverage

### Running Specific Test Suites
```bash
# Unit tests only
make test-unit

# Integration tests only
make test-integration

# All tests
make test
```

## Troubleshooting

### Common Issues
1. **Missing dependencies**: Ensure straight.el can bootstrap in CI environment
2. **Network access**: Some packages may require internet during first run
3. **Display issues**: Use `emacs -Q --batch` for headless testing

### Debug Mode
Run tests with verbose output:
```bash
emacs -Q --batch -l test/run-tests.el 2>&1 | tee test-output.log
```
