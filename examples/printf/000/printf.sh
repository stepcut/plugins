#!/bin/sh
printf "%d\n"          42
printf "%u\n"          42
printf "0%o\n"         42
printf "0x%x\n"        42
printf "0x%X\n"        42

printf "%e\n"          42.1234
printf "%E\n"          42.1234
printf "%g\n"          42.1234
printf "%G\n"          42.1234
printf "%f\n"          42.1234

printf "%c:%c:%c\n"    'a' 'b' 'c'
printf "%s\n"          "printf"

printf "%+d\n"          42
printf "%+0d\n"         42
printf "%0+d\n"         42
printf "%10d\n"         42
printf "%-010d\n"       42
printf "%-010.2d\n"     42

printf "%+f\n"          42.1234
printf "%+0f\n"         42.1234
printf "%0+f\n"         42.1234
printf "%10f\n"         42.1234
printf "%-010f\n"       42.1234
printf "%-010.2f\n"     42.1234

printf "%10s\n"  	"printf"
printf "%-10s\n"  	"printf"
printf "%10.2s\n"  	"printf"
printf "%2.10s\n"  	"printf"
printf "%-2.10s\n"  	"printf"
printf "%-10.2s\n"  	"printf"
