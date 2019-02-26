.PHONY: clean setup build run-watch

clean:
	stack clean

setup:
	stack setup
	stack build ghcid

build:
	stack build

run-watch:
	stack exec ghcid -- -c "stack ghci --main-is herxheim2018:exe:herxheim2018-exe" -T="main"

test:
	stack test --fast

test-watch:
	env DATABASE_URL='postgres://localhost/herxheim2018_test' stack exec ghcid -- -c "stack ghci test/Spec.hs" -T="main"
