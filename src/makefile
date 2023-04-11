GHCICMD = ghci
STACKGHCICMD = stack repl
TESTDIR = Tests

.PHONY: test
test: 
	echo ":main " | $(GHCICMD) $(TESTDIR)/All.hs

.PHONY: stest
stest: 
	echo ":main " | $(STACKGHCICMD) $(TESTDIR)/All.hs

