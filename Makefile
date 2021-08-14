CC=musl-gcc
CFLAGS=-std=c11 -Wall -O3 -D_DEFAULT_SOURCE
LDFLAGS=-static

NUM_SETTER_DIR=utils/num_setter
NUM_SETTER_OUT_DIR=setter_out
NUM_SETTER=$(NUM_SETTER_DIR)/num_setter
GEN_TABLE=utils/gen_table/gen_table.py

NANOSRC=main.c
SRC=$(NANOSRC) table.c
SECOND_SRC=$(addprefix $(NUM_SETTER_OUT_DIR)/, $(NANOSRC))
SECOND_OBJ=$(patsubst %.c, %.o, $(SECOND_SRC))
FINAL_OBJ=$(SECOND_OBJ) $(patsubst %.c, %.o, $(filter-out $(NANOSRC), $(SRC)))
OBJ=$(patsubst %.c, %.o, $(SRC))
TARGET=main

.PHONY: all clean
all: $(TARGET) $(SECOND_SRC) $(FINAL_OBJ)
	$(CC) $(LDFLAGS) $(FINAL_OBJ) -o $(TARGET)
$(TARGET): $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) -o $(TARGET)
$(NUM_SETTER_OUT_DIR)/%.o: $(NUM_SETTER_OUT_DIR)/%.c
	$(CC) $(CFLAGS) -DSECOND_PASS -c $< -o $@
%.o: %.c | table.h
	$(CC) $(CFLAGS) -c $< -o $@
table.h: nanocalls.py
	$(GEN_TABLE) nanocalls.py
$(SECOND_SRC): $(NANOSRC) | $(NUM_SETTER_OUT_DIR)
	$(MAKE) -C $(NUM_SETTER_DIR)
	$(NUM_SETTER) $(TARGET) table.h $(NUM_SETTER_OUT_DIR) $(NANOSRC)
	cp table.h $(NUM_SETTER_OUT_DIR)
$(NUM_SETTER_OUT_DIR):
	mkdir $(NUM_SETTER_OUT_DIR)
clean:
	$(MAKE) -C $(NUM_SETTER_DIR) clean
	rm -rf $(OBJ) $(TARGET) $(NUM_SETTER_OUT_DIR) table.h table.inc
