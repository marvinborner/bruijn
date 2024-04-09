#!/bin/sh

if [ -z "$1" ]; then
	echo "Usage: $0 <reducer>"
	exit 1
fi

# =================
# Euler/Fun/Rosetta
# =================

echo "# useful for run the tests of all samples at once" >All.bruijn
echo >>All.bruijn

FILES="$(find euler fun rosetta -type f -name "*.bruijn")"

for f in $FILES; do
	echo ":import ${f%*.bruijn} ." >>All.bruijn
done

# for ci, just run `bruijn All.bruijn`
echo >>All.bruijn
echo "main [[0]]" >>All.bruijn

if cat /dev/null | bruijn -v All.bruijn -r "$1" | tee /dev/fd/2 | grep -q "ERROR"; then
	exit 1
fi

# ===
# AOC
# ===

FILES="$(find aoc -type f -name "*.bruijn")"

for f in $FILES; do
	dir="$(dirname "$f")"
	cat "$dir/input" | bruijn -r "$1" "$f" | tail -n1 >temp.out
	cmp temp.out "$dir/output.check" || (
		echo "AOC check $f failed"
		exit 1
	)
done

rm -f temp.out
