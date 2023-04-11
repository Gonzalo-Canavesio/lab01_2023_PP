GHCICMD = stack repl
TESTOPTIONS =--ghc-options -i./tests/
TESTDIR = tests

.PHONY: test
test: 
	echo ":main" | $(GHCICMD) $(TESTOPTIONS) $(TESTDIR)/all.hs

