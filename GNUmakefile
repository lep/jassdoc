VERSION := $(shell git rev-parse --short HEAD)

HSC := cabal exec -- ghc
HSFLAGS := --make

.PHONY: all clean release

all: jass.db

db.sql: mkdocs mksrc *.j
	./mkdocs $(filter %.j,$?) > $@
	./mksrc $(filter %.j,$?) >> $@

mkdocs: Jass/Parser.hs Jass/Types.hs Jass/Ast.hs mkdocs.hs
	$(HSC) $(HSFLAGS) mkdocs

jass.db: db.sql
	sqlite3 $@ < $<

jass-$(VERSION).zip: jass.db
	zip -q $@ $<

release: jass-$(VERSION).zip

clean:
	rm -f *.o *.hi
	rm -f jass-*.zip
	rm -f db.sql

