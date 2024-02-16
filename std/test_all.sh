#!/bin/sh

echo "# useful for running all tests of the standard library" >All.bruijn
echo >>All.bruijn

FILES="$(find * -type f -name "*.bruijn" ! -name "All.bruijn")"

for f in $FILES; do
	echo ":import std/${f%*.bruijn} ." >>All.bruijn
done

# for ci, just run `bruijn All.bruijn`
echo >>All.bruijn
echo "main [[0]]" >>All.bruijn

stack run -- All.bruijn | grep "ERROR" && exit 1 || exit 0
