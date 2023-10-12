CC=gcc
CFLAGS=-Wall -std=c11 -fno-stack-protector -Wno-builtin-declaration-mismatch -fno-stack-protector -ffunction-sections -fno-omit-frame-pointer -O0
LDFLAGS=-nostdlib -nostartfiles -nodefaultlibs -static -z noexecstack -Wl,-z,noseparate-code
AS=as
