SOURCES = \
Draw.ml \
DancingLinks.ml \
Pentomino.ml \
Main.ml \
Event51.ml \
Tiles.ml \
UI.ml \

all: $(SOURCES)
	corebuild -quiet -lib graphics Draw.native

check: $(SOURCES)
	@chmod u+x ../check_width
	@../check_width Main.ml; \
        ../check_width DancingLinks.ml; \
	../check_width Pentomino.ml; \
	../check_width Draw.ml; \
	../check_width Event51.ml; \
	../check_width UI.ml; \
	../check_width Tiles.ml; \


clean:
	rm -rf _build Draw.native
