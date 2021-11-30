build:
	stack build

test:
	stack test

format:
	find src/ app/ test/ -name "*.hs" -exec fourmolu -i -o '-XTypeApplications' -o '-XImportQualifiedPost' {} +

format-check:
	find src/ app/ test/ -name "*.hs" -exec fourmolu -m check -o '-XTypeApplications' -o '-XImportQualifiedPost' {} +

ghcid:
	ghcid -c "stack repl"

clean:
	stack clean

.PHONY: build test format format-check ghcid clean
