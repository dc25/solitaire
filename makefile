HASTE_SOURCES = Card.hs  Game.hs  Setup.hs  Shuffle.hs  Solitare.hs

TYPESCRIPT_SOURCES = cards.ts
JAVASCRIPT_FROM_TYPESCRIPT = $(patsubst %.ts,%.js, $(TYPESCRIPT_SOURCES))

default:all

Solitare.js: $(HASTE_SOURCES)
	hastec Solitare.hs

%.js: %.ts
	tsc $< 

all: Solitare.js $(JAVASCRIPT_FROM_TYPESCRIPT)

