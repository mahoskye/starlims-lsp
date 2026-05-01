BINARY_NAME=starlims-lsp
VERSION=$(shell git describe --tags --always --dirty 2>/dev/null || echo "dev")
BUILD_TIME=$(shell date -u +"%Y-%m-%dT%H:%M:%SZ")
LDFLAGS=-ldflags "-X main.version=${VERSION} -X main.buildTime=${BUILD_TIME}"

.PHONY: all build clean test install build-all build-linux build-darwin build-windows

all: build

build:
	go build ${LDFLAGS} -o bin/${BINARY_NAME} ./cmd/starlims-lsp

build-all: build-linux build-darwin build-windows

# CGO_ENABLED=0 forces a pure-Go static binary on every target. Without
# it, the host-architecture build (typically linux-amd64 on a Linux runner)
# links against the build host's glibc and fails on systems with older
# glibc — cross-compiled targets are unaffected because cross-compilation
# auto-disables CGO.
build-linux:
	CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build ${LDFLAGS} -o bin/${BINARY_NAME}-linux-amd64 ./cmd/starlims-lsp
	CGO_ENABLED=0 GOOS=linux GOARCH=arm64 go build ${LDFLAGS} -o bin/${BINARY_NAME}-linux-arm64 ./cmd/starlims-lsp

build-darwin:
	CGO_ENABLED=0 GOOS=darwin GOARCH=amd64 go build ${LDFLAGS} -o bin/${BINARY_NAME}-darwin-amd64 ./cmd/starlims-lsp
	CGO_ENABLED=0 GOOS=darwin GOARCH=arm64 go build ${LDFLAGS} -o bin/${BINARY_NAME}-darwin-arm64 ./cmd/starlims-lsp

build-windows:
	CGO_ENABLED=0 GOOS=windows GOARCH=amd64 go build ${LDFLAGS} -o bin/${BINARY_NAME}-windows-amd64.exe ./cmd/starlims-lsp

test:
	go test -v -race ./...

test-coverage:
	go test -v -race -coverprofile=coverage.out ./...
	go tool cover -html=coverage.out -o coverage.html

clean:
	rm -rf bin/
	rm -f coverage.out coverage.html

install:
	go install ${LDFLAGS} ./cmd/starlims-lsp

lint:
	golangci-lint run

fmt:
	go fmt ./...
	goimports -w .

tidy:
	go mod tidy

run:
	go run ./cmd/starlims-lsp --stdio
