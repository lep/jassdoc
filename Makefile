VERSION := $(shell git rev-parse --short HEAD)

HSC := cabal exec -- ghc
HSFLAGS := --make -O2

.PHONY: all clean release

all: jass.db

Jass/Tokenizer.hs: Jass/jass.x
	alex $< -o $@

db.sql: mkdocs *.j 
	./mkdocs $(filter %.j,$?) > $@

mkdocs: Jass/Tokenizer.hs Jass/Parser.hs Jass/Types.hs Jass/Ast.hs mkdocs.hs
	$(HSC) $(HSFLAGS) mkdocs

jass.db: db.sql
	sqlite3 $@ < $<

jass-$(VERSION).zip: jass.db
	zip -q $@ $<

release: jass-$(VERSION).zip

clean:
	rm -f *.o *.hi
	rm -f jass-*.zip
	rm -f mkdocs
	rm -f db.sql

