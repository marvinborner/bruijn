#!/bin/env bash

set -e

rm -rf samples/ && mkdir -p samples/

files=$(find ../samples/ -type f -name "*.bruijn" -printf '%h\0%d\0%p\n' | sort -t '\0' -n | awk -F '\0' '{print $3}')
links=""

prefix=""
for file in $files; do
	name=$(cut -c12- <<<"$file")
	_prefix=$(cut -d/ -f1 <<<"$name")
	if ! [ "$prefix" = "$_prefix" ]; then
		prefix=$_prefix
		links="$links\n</ul><h2>$prefix</h2><ul>"
	fi
	filename=$(sed s@/@_@g <<<"$name")
	links="$links\n<li><a href="$filename.html">$name</a></li>"

	if [ "$prefix" = "euler" ]; then
		info="<a href='https://projecteuler.net/problem=$(basename "$name" .bruijn)'>Problem description</a>"
	elif [ "$prefix" = "aoc" ]; then
		year=$(cut -c5-8 <<<"$name")
		day=$(cut -c10-11 <<<"$name" | sed 's/^0*//')
		info="<a href='https://adventofcode.com/$year/day/$day'>Problem description</a>"
	elif [ "$prefix" = "rosetta" ]; then
		info="<a href='https://rosettacode.org/wiki/$(basename "$name" .bruijn)'>Problem description</a>"
	else
		info=""
	fi

	awk 'NR==FNR { gsub("<", "\\&lt;", $0); gsub(">", "\\&gt;", $0); a[n++]=$0; next } /CONTENT/ { for (i=0;i<n;++i) print a[i]; next } 1' "$file" content.template >"samples/$filename.html"
	sed -i -e "s@NAME@$name@g" -e "s@INFO@$info@g" "samples/$filename.html"
done

sed -e "s@LINKS@$links@g" samples.template >samples/index.html

cp res/* code.js content.css index.css code.css samples/
