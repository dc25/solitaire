HASTE_SOURCES = Card.hs  Game.hs  Setup.hs  Shuffle.hs  Solitaire.hs

TYPESCRIPT_SOURCES = cards.ts
JAVASCRIPT_FROM_TYPESCRIPT = $(patsubst %.ts,%.js, $(TYPESCRIPT_SOURCES))

default:all

Solitaire.js: $(HASTE_SOURCES)
	hastec Solitaire.hs

%.js: %.ts
	tsc $< 

all: Solitaire.js $(JAVASCRIPT_FROM_TYPESCRIPT)

