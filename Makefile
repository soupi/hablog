
.PHONY: setup
setup:
	stack setup


.PHONY: build
build:
	stack build

.PHONY: ghci
ghci:
	stack ghci

.PHONY: dev
dev:
	stack build --fast --file-watch

.PHONY: run
run:
	stack build && stack exec -- hablog http --port 8000

.PHONY: clean
clean:
	stack clean && rm -r .stack-work

