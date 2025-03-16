#!/bin/sh

64tass --cbm-prg --long-branch --list clock.txt --output clock.prg clock.asm

64tass --cbm-prg --list init.txt --output init.prg init.asm

petcat -v -f -40 -w40 -o start.prg -- start.bas

cat << EOF | c1541
format clock,00 d80 clock.d80 8
write start.prg start.prg
write init.prg init.prg
write clock.prg clock.prg
EOF
