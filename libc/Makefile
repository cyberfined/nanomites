include ../config.mk

CFLAGS += -fPIC

.PHONY: all clean

all: libc.a
libc.a: memset.o memcpy.o memmove.o strlen.o strcmp.o strchr.o fgets.o printf.o \
	strerror.o fputs.o putchar.o fwrite.o
	ar rcs libc.a memset.o memcpy.o memmove.o strcmp.o strlen.o strchr.o fgets.o \
		printf.o strerror.o fputs.o putchar.o fwrite.o
strlen.o: strlen.c
	$(CC) $(CFLAGS) -c strlen.c -o strlen.o
strcmp.o: strcmp.c
	$(CC) $(CFLAGS) -c strcmp.c -o strcmp.o
strchr.o: strchr.c
	$(CC) $(CFLAGS) -c strchr.c -o strchr.o
fgets.o: fgets.c
	$(CC) $(CFLAGS) -c fgets.c -o fgets.o
printf.o: printf.c
	$(CC) $(CFLAGS) -c printf.c -o printf.o
strerror.o: strerror.c
	$(CC) $(CFLAGS) -c strerror.c -o strerror.o
fputs.o: fputs.c
	$(CC) $(CFLAGS) -c fputs.c -o fputs.o
putchar.o: putchar.c
	$(CC) $(CFLAGS) -c putchar.c -o putchar.o
fwrite.o: fwrite.c
	$(CC) $(CFLAGS) -c fwrite.c -o fwrite.o
memset.o: memset.s
	$(AS) -c memset.s -o memset.o
memcpy.o: memcpy.s
	$(AS) -c memcpy.s -o memcpy.o
memmove.o: memmove.s
	$(AS) -c memmove.s -o memmove.o
clean:
	rm -f memset.o memcpy.o memmove.o strlen.o strcmp.o strchr.o fgets.o printf.o \
		libc.a
