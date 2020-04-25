all: 
	$(DUNE) build src/main.exe

exec:
	$(DUNE) exec src/main.exe