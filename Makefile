VERSION := $(shell git rev-parse --short HEAD)

HSC := cabal exec -- ghc
HSFLAGS := --make

.PHONY: mkdocs all clean release

all: jass.db

Jass/Tokenizer.hs: Jass/jass.x
	alex $< -o $@

db.sql: *.j |mkdocs
	./mkdocs $? > $@

mkdocs: Jass/Tokenizer.hs
	$(HSC) $(HSFLAGS) mkdocs

jass.db: db.sql
	sqlite3 $@ < $<

jass.zip: jass.db
	zip -q $@ $<

release: jass-$(VERSION).zip

clean:
	rm -f *.o *.hi
	rm -f jass-*.zip
