CC=gcc
CFLAGS=-std=c11 -Wall -O3 -D_DEFAULT_SOURCE
LDFLAGS=
SRC=main.c file_map.c htable.c offset_table.c elf_parser.c offset_parser.c
OBJ=$(patsubst %.c, %.o, $(SRC))
TARGET=num_setter

.PHONY: all clean
all: $(TARGET)
$(TARGET): $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) -o $(TARGET)
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
clean:
	rm -f $(OBJ) $(TARGET)
