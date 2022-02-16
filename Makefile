OPTIONS := -Wall

build:
	stack build --ghc-options='${OPTIONS}'

test:
	stack test --ghc-options='${OPTIONS}'

format:
	find src/ app/ test/ -name "*.hs" -exec fourmolu -i -o '-XTypeApplications' -o '-XImportQualifiedPost' {} +

format-check:
	find src/ app/ test/ -name "*.hs" -exec fourmolu -m check -o '-XTypeApplications' -o '-XImportQualifiedPost' {} +

ghcid:
	ghcid -c "stack repl --ghc-options='${OPTIONS}'"

ghcid-test:
	ghcid -c "stack ghci snailscheme:snailscheme-test --ghc-options='${OPTIONS}'"

clean:
	stack clean

hlint:
	hlint .

.PHONY: build test format format-check ghcid ghcid-test clean hlint
