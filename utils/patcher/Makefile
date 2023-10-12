CC=gcc
CFLAGS=-Wall -std=c11 -O0
LDFLAGS=-O0

.PHONY: all clean

all: patcher
patcher: patcher.o funcs_table.o aes.o
	$(CC) $(LDFLAGS) patcher.o funcs_table.o aes.o -o patcher
patcher.o: patcher.c
	$(CC) $(CFLAGS) -c patcher.c -o patcher.o
funcs_table.o: funcs_table.c
	$(CC) $(CFLAGS) -c funcs_table.c -o funcs_table.o
aes.o: aes.c
	$(CC) $(CFLAGS) -c aes.c -o aes.o
clean:
	rm -f funcs_table.o patcher.o aes.o patcher
