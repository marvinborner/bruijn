#!/bin/sh

FILES="$(find * -type f -name "*.bruijn" ! -name "All.bruijn" ! -path "*Generic*")"

list_defs() {
	grep -Po "^[^:# \t][^ \t]*" "$1"
	inputs="$(awk '/^:input/ {print $2}' "$1")"
	for i in $inputs; do
		list_defs "${i#std/}.bruijn"
	done
}

{
	for f in $FILES; do
		echo "{\"$f\":"
		list_defs "$f" | jq -R . | jq -s .
		echo "}"
	done
} | jq -s add
