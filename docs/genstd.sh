#!/bin/env bash

set -e

rm -rf std/ && mkdir -p std/

files=$(find ../std/ -type f -printf '%h\0%d\0%p\n' | sort -t '\0' -n | awk -F '\0' '{print $3}')
links=""

for file in $files; do
	name=$(echo "$file" | cut -c8-)
	filename=$(sed s@/@_@g <<<"$name")
	links="$links\n<li><span class='com'>:import</span> <a href="$filename.html">$name</a></li>"
	awk 'NR==FNR { gsub("<", "\\&lt;", $0); gsub(">", "\\&gt;", $0); a[n++]=$0; next } /CONTENT/ { for (i=0;i<n;++i) print a[i]; next } 1' "$file" content.template >"std/$filename.html"
	sed -i -e "s@NAME@$name@g" "std/$filename.html"
done

sed -e "s@LINKS@$links@g" std.template >std/index.html

cp res/* code.js content.css index.css code.css std/
