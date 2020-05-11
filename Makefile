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

zip:
	zip camlished.zip data/* images/* saves/* src/* dune-project INSTALL.txt Makefile readme.md

format:
	$(DUNE) build --auto-promote @fmt
	opam lint
	git ls-files '**/*.[ch]' | xargs -n1 indent -nut -i8