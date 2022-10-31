CC=clang
CFLAGS=-std=c11 -Wall -O2 -D_DEFAULT_SOURCE
LDFLAGS=-static

NUM_SETTER_DIR=utils/num_setter
NUM_SETTER=$(NUM_SETTER_DIR)/num_setter
GEN_TABLE=utils/gen_table/gen_table.py

SRC=main.c table.c debug.c
OBJ=$(patsubst %.c, %.o, $(SRC))
TARGET=main
PATCHED_TARGET=crackme

.PHONY: all clean
all: $(PATCHED_TARGET)
$(PATCHED_TARGET): $(TARGET)
	$(MAKE) -C $(NUM_SETTER_DIR)
	$(NUM_SETTER) $(TARGET) table.h $(PATCHED_TARGET)
$(TARGET): $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) -o $(TARGET)
%.o: %.c | table.h
	$(CC) $(CFLAGS) -c $< -o $@
table.h: nanocalls.py
	$(GEN_TABLE) nanocalls.py
clean:
	$(MAKE) -C $(NUM_SETTER_DIR) clean
	rm -rf $(OBJ) $(TARGET) $(PATCHED_TARGET) table.h table.inc
