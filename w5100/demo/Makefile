all:    w5100_demo.dsk

%.bin: %.s %_main.c
	cl65 -o $@ -t apple2enh -m $(basename $@).map $^

stream.bin: stream.c stream_main.c
	cl65 -o $@ -t apple2enh -m $(basename $@).map $^

w5100_demo.dsk: udp.bin tcp.bin stream.bin
	cp prodos.dsk $@
	java -jar $(AC) -cc65 $@ udp    bin < udp.bin
	java -jar $(AC) -cc65 $@ tcp    bin < tcp.bin
	java -jar $(AC) -cc65 $@ stream bin < stream.bin

clean:
	-rm -f *.o *.map *.bin w5100_demo.dsk
