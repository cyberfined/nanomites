BEGIN {
    print "ENTRY(_start)\nSECTIONS {\n    . = 0x400000;\n" \
          "    .text : {\n        * (.data)\n        * (.rodata)"
}

{
    if($2 ~ /^\.text\..*/)
        matched=$2;
    else if($3 ~ /^\.text\..*/)
        matched=$3;
    else
        next;

    print "\n        . = ALIGN(16);\n        * ("matched")\n        . = ALIGN(16);"
}

END {
    print "    }\n}"
}
