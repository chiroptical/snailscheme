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

ghcid-test:
	ghcid -c "stack ghci snailscheme:snailscheme-test"

clean:
	stack clean

hlint:
	hlint .

.PHONY: build test format format-check ghcid ghcid-test clean hlint
