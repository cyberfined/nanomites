ENTRY(engine)
SECTIONS {
    .text : {
        * (.text)
        * (.data)
        * (.rodata)
    }
}
