#!/bin/env bash

set -e

rm -rf std/ && mkdir -p std/

files=$(find ../std/ -type f -name "*.bruijn" -printf '%h\0%d\0%p\n' | sort -t '\0' -n | awk -F '\0' '{print $3}')
links=""

tot_ntests=0
tot_ndefs=0
prefix="seeelefant"
for file in $files; do
	name=$(cut -c8- <<<"$file")
	_prefix=$(cut -c8- <<<"$file" | sed -rn 's@^(.*)/.*$@\1@p')
	if ! [ "$prefix" = "$_prefix" ]; then
		prefix=$_prefix
		links="$links\n</ol><h2>std/$prefix</h2><ol>"
	fi
	filename=$(sed s@/@_@g <<<"$name")
	ndefs=$(grep -cP "^[^:# \t]" "$file" || true)
	ntests=$(grep -cP "^:test" "$file" || true)
	links="$links\n<li><span class='com'>:import</span> <a href=$filename.html>$(basename "$name" .bruijn)</a> <span class='stats'>($ndefs definitions, $ntests tests)</span></li>"
	awk 'NR==FNR { gsub("<", "\\&lt;", $0); gsub(">", "\\&gt;", $0); a[n++]=$0; next } /CONTENT/ { for (i=0;i<n;++i) print a[i]; next } 1' "$file" content.template >"std/$filename.html"
	sed -i -e "s@NAME@$name@g" -e "s@INFO@@g" "std/$filename.html"

	tot_ndefs=$((tot_ndefs + ndefs))
	tot_ntests=$((tot_ntests + ntests))
done

sed -e "s@STATS@Total: $tot_ndefs definitions, $tot_ntests tests@g" -e "s@LINKS@$links@g" std.template >std/index.html

cp res/* code.js content.css index.css code.css std/
