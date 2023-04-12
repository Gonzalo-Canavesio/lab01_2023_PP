GHCICMD = stack repl
TESTDIR = Tests

.PHONY: test
test: 
	echo ":main " | $(GHCICMD) $(TESTDIR)/All.hs

.PHONY: esch
esch: 
	echo ":main Escher" | $(GHCICMD) Main.hs

.PHONY: gri
gri: 
	echo ":main Grilla" | $(GHCICMD) Main.hs
