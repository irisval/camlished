#
# This Makefile is not called from Opam but only used for 
# convenience during development
#

DUNE 	= dune

.PHONY: all install test clean uninstall format

all: 
	$(DUNE) build src/main.exe
	$(DUNE) build src/stateTest.exe

docs: all
	$(DUNE) build @doc
	
exec:
	$(DUNE) exec src/main.exe

install: all
	$(DUNE) install

uninstall:
	$(DUNE) uninstall

test:
	$(DUNE) exec src/stateTest.exe

clean:
	$(DUNE) clean

format:
	$(DUNE) build --auto-promote @fmt
	opam lint
	git ls-files '**/*.[ch]' | xargs -n1 indent -nut -i8

# SRC=src/
# MODULES=src/gameState src/gameData src/input src/main src/mapGenerator src/names src/perlin src/renderer
# MLS=$(MODULES:=.ml)
# MLIS=$(MODULES:=.mli)

# docs: docs-public docs-private

# docs-public: all
# 	mkdir -p doc.public
# 	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
# 		-html -stars -d doc.public $(MLIS)

# docs-private: all
# 	mkdir -p doc.private
# 	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
# 		-html -stars -d doc.private \
# 		-inv-merge-ml-mli -m A $(MLIS) $(MLS)