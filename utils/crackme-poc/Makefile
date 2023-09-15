CC=gcc
CFLAGS=-Wall -O0 -g -std=c11 -Wno-frame-address
CFLAGS_CRACKME=$(CFLAGS) -Wno-builtin-declaration-mismatch -fno-stack-protector
LDFLAGS=-O0 -g
LDFLAGS_CRACKME=$(LDFLAGS) -nostdlib -nostartfiles -nodefaultlibs -nolibc -static \
		-z noexecstack -Wl,-z,noseparate-code
AS=as

.PHONY: all clean
all: crackme
crackme: patcher/patcher crackme-for-patch
	patcher/patcher -p crackme-for-patch crackme
crackme-for-patch: start.o patched_engine.o crackme.o table.inc
	$(CC) $(LDFLAGS_CRACKME) start.o crackme.o patched_engine.o -o crackme-for-patch
patched_engine.o: decrypt_engine.c table.inc
	$(CC) $(CFLAGS_CRACKME) -c decrypt_engine.c -o patched_engine.o
table.inc: patcher/patcher crackme-unpatched
	./patcher/patcher -c crackme-unpatched table.inc
crackme-unpatched: crackme.o start.o decrypt_engine.o
	$(CC) $(LDFLAGS_CRACKME) start.o crackme.o decrypt_engine.o -o crackme-unpatched
crackme.o: crackme.c
	$(CC) $(CFLAGS_CRACKME) -c crackme.c -o crackme.o
decrypt_engine.o: decrypt_engine.c
	echo "#define FUNCS_TABLE_SIZE 7" > table.inc
	$(CC) $(CFLAGS_CRACKME) -c decrypt_engine.c -o decrypt_engine.o
start.o: start.s
	$(AS) -c start.s -o start.o
patcher/patcher: patcher/patcher.o patcher/funcs_table.o
	$(CC) $(LDFLAGS) patcher/patcher.o patcher/funcs_table.o -o patcher/patcher
patcher/patcher.o: patcher/patcher.c
	$(CC) $(CFLAGS) -c patcher/patcher.c -o patcher/patcher.o
patcher/funcs_table.o: patcher/funcs_table.c
	$(CC) $(CFLAGS) -c patcher/funcs_table.c -o patcher/funcs_table.o
clean:
	rm -f start.o crackme.o decrypt_engine.o patched_engine.o \
		table.inc patcher/patcher.o patcher/funcs_table.o patcher/patcher \
		crackme-unpatched crackme-for-patch crackme
