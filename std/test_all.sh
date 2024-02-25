#!/bin/sh

if [ -z "$1" ]; then
	echo "Usage: $0 <reducer>"
	exit 1
fi

echo "# useful for running all tests of the standard library" >All.bruijn
echo >>All.bruijn

FILES="$(find * -type f -name "*.bruijn" ! -name "All.bruijn")"

for f in $FILES; do
	echo ":import std/${f%*.bruijn} ." >>All.bruijn
done

# for ci, just run `bruijn All.bruijn`
echo >>All.bruijn
echo "main [[0]]" >>All.bruijn

if bruijn -v All.bruijn -r "$1" | tee /dev/fd/2 | grep -q "ERROR"; then
	exit 1
fi

hyperfine --warmup 5 --runs 20 "bruijn -r $1 All.bruijn"
