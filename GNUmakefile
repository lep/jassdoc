MKDOCS ?= cabal run mkdocs --

.PHONY: all clean release check

SRC := common.j Blizzard.j common.ai builtin-types.j

all: jass.db

db.sql: mksrc $(SRC)
	$(MKDOCS) $(filter %.j %.ai,$?) --output "$@"
	perl mksrc $(filter %.j %.ai,$?) >> $@
	sh mkmetadata >> $@

jass.db: db.sql
	sqlite3 $@ < $<

check: jass.db
	sqlite3 $< < check-wrong-params.sql

clean:
	rm -f db.sql

