build:
	stack build

test: build build-test
	stack test

format:
	find src/ app/ test/ -name "*.hs" -exec fourmolu -i -o '-XTypeApplications' -o '-XImportQualifiedPost' {} +

ghcid:
	ghcid -c "stack repl"

clean:
	stack clean

.PHONY: build test format ghcid clean
