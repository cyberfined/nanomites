# nanomites

Linux amd64 crackme with AES CBC encrypted functions and nanomites.

## Build dependencies

1. gcc
2. gnu as
3. cabal
4. awk
5. python3
6. readelf
7. make

## Build

`make`

## Tested with

1. gcc 13.2.1
2. linux 6.5.6

## AES implementation

I used [kokke's](https://github.com/kokke/tiny-AES-c) tiny-AES-c.

## Libc implementation

I used [musl-libc](https://www.musl-libc.org/).
