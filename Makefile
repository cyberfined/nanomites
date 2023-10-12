include config.mk

.PHONY: all clean
all: crackme
crackme: crackme-unstripped utils/patcher/patcher
	./utils/patcher/patcher -s crackme-unstripped crackme
crackme-unstripped: crackme-encrypted engine/engine utils/patcher/patcher
	./utils/patcher/patcher -p crackme-encrypted crackme-unstripped engine/engine
engine/engine: engine/engine.o
	$(CC) $(LDFLAGS) -T engine/linker.ld -pie -Xlinker --no-dynamic-linker \
		engine/engine.o -o engine/engine
engine/engine.o: crackme-encrypted engine/engine.c
	$(CC) $(CFLAGS) -fno-function-sections -O3 -mno-sse -fno-omit-frame-pointer -Wno-overflow -fPIC -c \
		engine/engine.c -o engine/engine.o
crackme-encrypted: crackme-with-nanomites utils/patcher/patcher \
	utils/gen-add-round-key/build
	cd utils/gen-add-round-key && ../patcher/patcher -e ../../crackme-with-nanomites \
		../../engine/table.inc ../../engine/AddRoundKey.c ../../crackme-encrypted
crackme-with-nanomites: crackme-without-nanomites utils/num_setter/num_setter
	utils/num_setter/num_setter crackme-without-nanomites table.h \
		crackme-with-nanomites
crackme-without-nanomites: linker.ld start.o crackme.o libc/libc.a
	$(CC) $(LDFLAGS) -T linker.ld start.o crackme.o libc/libc.a -o \
		crackme-without-nanomites
linker.ld: start.o crackme.o libc/libc.a
	readelf --wide -S start.o crackme.o libc/libc.a | \
		awk -f create_linker_script.awk > linker.ld
crackme.o: crackme.c table.h
	$(CC) $(CFLAGS) -c crackme.c -o crackme.o
table.h: nanocalls.py
	utils/gen_table/gen_table.py nanocalls.py
start.o: start.s
	$(AS) -c start.s -o start.o
libc/libc.a: FORCE
	$(MAKE) $(MFLAGS) -C libc
utils/num_setter/num_setter: FORCE
	$(MAKE) $(MFLAGS) -C utils/num_setter
utils/patcher/patcher: FORCE
	$(MAKE) $(MFLAGS) -C utils/patcher
utils/gen-add-round-key/build: FORCE
	@test "$(shell cd utils/gen-add-round-key && cabal build)" = "Up to date" \
		|| { touch utils/gen-add-round-key/build; } \
		&& { test -f utils/gen-add-round-key/build || \
		touch utils/gen-add-round-key/build; }
FORCE:

clean:
	$(MAKE) -C libc clean
	$(MAKE) -C utils/num_setter clean
	$(MAKE) -C utils/patcher clean
	rm -f start.o crackme.o linker.ld table.h table.inc \
		utils/gen-add-round-key/build crackme-without-nanomites \
		engine/table.inc engine/AddRoundKey.c engine/engine.o engine/engine \
		crackme-with-nanomites crackme-encrypted crackme-unstripped crackme
