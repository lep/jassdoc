MKDOCS ?= cabal run mkdocs --

.PHONY: all clean release check
.PHONY: check-missing check-params check-git-revision

SRC := common.j Blizzard.j common.ai builtin-types.j

all: jass.db

db.sql: mksrc $(SRC)
	$(MKDOCS) $(filter %.j %.ai,$?) --output "$@"
	perl mksrc $(filter %.j %.ai,$?) >> $@
	sh mkmetadata >> $@

jass.db: db.sql
	sqlite3 $@ < $<

check-missing: jass.db
	perl lint $(SRC)

check-params: jass.db
	sqlite3 $< < check-wrong-params.sql

check-git-revision: jass.db
	sh check-revision $<

check: check-missing check-params check-git-revision

clean:
	rm -f db.sql jass.db

