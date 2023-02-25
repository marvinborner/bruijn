#!/bin/env bash

set -e

rm -rf std/ && mkdir -p std/

files=$(find ../std/ -type f | sort)
links=""

for file in $files; do
	name=$(echo $file | cut -c8-)
	filename=$(sed s@/@_@g <<<"$name")
	links="$links\n<li><span class='com'>:import</span> <a href="$filename.html">$name</a></li>"
	awk 'NR==FNR { gsub("<", "\\&lt;", $0); gsub(">", "\\&gt;", $0); a[n++]=$0; next } /CONTENT/ { for (i=0;i<n;++i) print a[i]; next } 1' "$file" content.template >std/$filename.html
	sed -i -e "s@NAME@$name@g" std/$filename.html
done

sed -e "s@LINKS@$links@g" index.template >std/index.html

cp content.js content.css index.css std/
