TESTS := nested_coherency_basic

build/%.riscv: %/main.c
	riscv64-unknown-elf-gcc $*/*.c -o $@

$(TESTS): %: build/%.riscv