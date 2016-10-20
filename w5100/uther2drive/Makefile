all: vedrive.bin

dsk: vedrive.dsk vedrive.po

$(IP65)/ip65/ip65.lib:
	make -C $(IP65)/ip65

$(IP65)/drivers/a2uther2.lib:
	make -C $(IP65)/drivers

%.o: %.s
	ca65 -t apple2 -I $(IP65) $<

%.bin: %.o $(IP65)/ip65/ip65.lib $(IP65)/drivers/a2uther2.lib
	ld65 -C $*.cfg -o $*.bin -m $*.map $^ apple2.lib

vedrive.dsk vedrive.po: vedrive.bin vedrive.setup.txt
	cp prodos$(suffix $@) $@
	java -jar $(AC) -p    $@ vedrive.system sys < $(shell cl65 --print-target-path)/apple2/util/loader.system
	java -jar $(AC) -cc65 $@ vedrive        bin < vedrive.bin
	java -jar $(AC) -p    $@ vedrive.text   txt < vedrive.setup.txt

clean:
	-rm -f *.o *.bin *.map vedrive.dsk vedrive.po
