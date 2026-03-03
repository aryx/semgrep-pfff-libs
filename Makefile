###############################################################################
# semgrep-libs Makefile
###############################################################################

.PHONY: default
default:
	dune build

.PHONY: clean
clean:
	dune clean

.PHONY: test
test:
	dune runtest

.PHONY: install
install:
	dune install

.PHONY: setup
setup:
	opam install --confirm-level=unsafe-yes --deps-only .
