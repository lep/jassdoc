VERSION := $(shell git rev-parse --short HEAD)

.PHONY: all clean release

SRC := trackable.j quest.j random.j leaderboard.j terrain.j fog-of-war.j
SRC += game-event-api.j computer-ai.j destructable.j unit.j doodad.j timer.j
SRC += boolexpr.j image.j force.j sound.j map-setup.j ui.j dialog.j
SRC += timer-dialog.j Blizzard.j camera.j game.j misc.j gamecache.j
SRC += builtin-types.j math.j hashtable.j group.j visuals.j
SRC += player-based-event-api.j effects.j unit-based-event-api.j ubersplat.j
SRC += common.j blight.j multiboard.j player.j region-location.j trigger.j
SRC += item.j widget.j campaign.j ability.j string.j
SRC += common.ai

all: jass.db

db.sql: mksrc $(SRC)
	cabal run --verbose=0 mkdocs -- $(filter %.j %.ai,$?) > $@
	./mksrc $(filter %.j %.ai,$?) >> $@
	./mkmetadata >> $@

jass.db: db.sql
	sqlite3 $@ < $<

jass-$(VERSION).zip: jass.db
	zip -q $@ $<

release: jass-$(VERSION).zip

clean:
	rm -f *.o *.hi
	rm -f jass-*.zip
	rm -f db.sql

