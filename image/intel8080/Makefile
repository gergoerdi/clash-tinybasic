all: tinybasic-2.0.bin

%.bin: %.hex
	objcopy --gap-fill 0 --input-target=ihex --output-target=binary $< $@

