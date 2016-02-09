HSC := cabal exec -- ghc
HSFLAGS := --make

.PHONY: mkdocs all

all: mkdocs

Jass/Tokenizer.hs: Jass/jass.x
	alex $< -o $@

db.sql: *.j |mkdocs
	./mkdocs $? > $@

mkdocs: Jass/Tokenizer.hs
	$(HSC) $(HSFLAGS) mkdocs

jass.db: db.sql
	sqlite3 $@ < $<
