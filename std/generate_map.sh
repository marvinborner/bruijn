#!/bin/sh

FILES="$(find * -type f -name "*.bruijn" ! -name "All.bruijn" ! -path "*Generic*")"

list_defs() {
	grep -Po "^[^:# \t][^ \t]*" "$1" | sed -e "s#\\(.*\\)#{\"name\": \"\\1\", \"source\": \"$1\"}#g"
	inputs="$(awk '/^:input/ {print $2}' "$1")"
	for i in $inputs; do
		list_defs "${i#std/}.bruijn"
	done
}

{
	for f in $FILES; do
		echo "{\"$f\":"
		list_defs "$f" | sed 's/\\/\\\\/g' | jq -s .
		echo "}"
	done
} | jq -s add
