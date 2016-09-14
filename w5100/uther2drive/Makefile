all: uther2drive.bin

dsk: uther2drive.dsk uther2drive.po

$(IP65)/ip65/ip65.lib:
	make -C $(IP65)/ip65

$(IP65)/drivers/a2uther2.lib:
	make -C $(IP65)/drivers

%.o: %.s
	ca65 -t apple2 -I $(IP65) $<

%.bin: %.o $(IP65)/ip65/ip65.lib $(IP65)/drivers/a2uther2.lib
	ld65 -C $*.cfg -o $*.bin -m $*.map $^ apple2.lib

uther2drive.dsk uther2drive.po: uther2drive.bin uther2drive.setup.txt
	cp prodos$(suffix $@) $@
	java -jar $(AC) -p    $@ u2d.system     sys < $(shell cl65 --print-target-path)/apple2/util/loader.system
	java -jar $(AC) -cc65 $@ u2d            bin < uther2drive.bin
	java -jar $(AC) -p    $@ u2d.setup.text txt < uther2drive.setup.txt

clean:
	-rm -f *.o *.bin *.map uther2drive.dsk uther2drive.po
