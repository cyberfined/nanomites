include ../config.mk

CFLAGS += -fPIC

.PHONY: all clean

all: libc.a
libc.a: memcpy.o memmove.o strlen.o strcmp.o strchr.o
	ar rcs libc.a memcpy.o memmove.o strcmp.o strlen.o strchr.o
strlen.o: strlen.c
	$(CC) $(CFLAGS) -c strlen.c -o strlen.o
strcmp.o: strcmp.c
	$(CC) $(CFLAGS) -c strcmp.c -o strcmp.o
strchr.o: strchr.c
	$(CC) $(CFLAGS) -c strchr.c -o strchr.o
memcpy.o: memcpy.s
	$(AS) -c memcpy.s -o memcpy.o
memmove.o: memmove.s
	$(AS) -c memmove.s -o memmove.o
clean:
	rm -f memcpy.o memmove.o strlen.o strcmp.o strchr.o libc.a
