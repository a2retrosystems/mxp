all: slinky.rom

slinky.rom: rom.s equates.inc bootrom.inc execute.inc makecat.inc
	cl65 -o $@ -t apple2 -C rom.cfg $<
