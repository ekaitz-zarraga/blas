#ARCH=rv32i
ARCH+=rv32ima
ABI+=ilp32

all: build/RUN

run:
	spike -m1 --isa=$(shell echo $(ARCH) | tr a-z A-Z) build/RUN
debug:
	rlwrap spike -d -m1 --isa=$(shell echo $(ARCH) | tr a-z A-Z) build/RUN

build/RUN: code.s
	mkdir -p build
	riscv32-unknown-elf-gcc -g -Wall -march=$(ARCH) -mabi=$(ABI) -nostartfiles \
		-ffreestanding -nostdlib -T help/link.ld -o build/RUN code.s

clean:
	rm build -r

.PHONY: clean all
