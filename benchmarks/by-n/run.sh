#!/bin/env sh

rm -rf temp.bruijn *.json
for file in $(find . -type f -name "*.bruijn" | sort); do
	echo "testing $file"
	spec=$(head -n 1 "$file")
	range=${spec#??}
	nseq=$(seq -s, $range) # requires splitting, no quotes!
	reducers=$(find ../../src/Reducer/ -type f -name "*.hs" -exec basename {} .hs \; | paste -sd, -)
	hyperfine --export-json "$file.json" --warmup 3 -r 3 -L REDUCER "$reducers" -L N $nseq "BENCH_N={N} envsubst <$file >temp.bruijn; bruijn -y -r {REDUCER} temp.bruijn </dev/null &>/dev/null;"
done
