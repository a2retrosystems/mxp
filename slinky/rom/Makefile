all: slinky.rom

megs ?= 1

slinky.rom: rom.s equates.inc bootrom.inc execute.inc makecat.inc
	cl65 -o $@ -t apple2 -C rom.cfg -Wl -Dmegs=$(megs) $<
