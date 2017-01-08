all: slinky.diags

slinky.diags: diags.s
	cl65 -o $@ -t apple2 -C apple2-asm.cfg --start-addr 0x2000 $<

slinky.dsk: slinky.diags
	cp prodos.dsk $@
	java -jar $(AC) -p $@ $< bin < $<
