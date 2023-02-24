#!/bin/env bash
# MIT License, Copyright (c) 2023 Marvin Borner
# TODO: jq is quite slow tbh hmm

# TODO: Custom path and absolute library path
path="std/"

pretty() {
	jq -c 'select(.type == "match") | .data' <<<"$1" |
		while IFS=$"\n" read -r c; do
			read file line name type < <(echo $(jq -r '.path.text, .line_number, (.lines.text | split(" ")[0]), (.lines.text | split("⧗")[1] | gsub("^\\s+|\\s+$";""))' <<<"$c" 2>/dev/null))
			desc=$(sed -n "$((line > 10 ? line - 10 : 1)),${line}p" "$file" | awk -v RS= 'END{print}' | awk '/^# /')
			alia=$(sed -n "$line,$ p" "$file" | sed -n "s/^\\(.*\\) $name$/\\1/p" | xargs)
			[ -z "$type" ] && type="Any"

			echo -e "\e[101m\e[30mFOUND\e[0m \e[95m$name\e[0m ⧗ \e[33m$type\e[0m"
			[ -z "$alia" ] || echo -e "\e[106m\e[30malso\e[0m \e[95m$alia\e[0m"
			echo -e "\e[104m\e[30min\e[0m $file:$line"
			[ -z "$desc" ] || echo -e "\e[3;2m$desc\e[0m"
			echo
		done
}

search_type() {
	json=$(rg --json "^.* .* ⧗ $1$" "$path")
	pretty "$json"
}

search_name() {
	json=$(rg --json "^$1 .*$" "$path")
	pretty "$json"
}

# TODO: Add search by comment

case $1 in
-t)
	shift
	search_type "${@//->/→}"
	;;
-f)
	shift
	search_name "$@"
	;;
-c)
	echo "sorry, not supported right now"
	;;
*)
	echo "unknown command $1, please use -t/f/c"
	;;
esac
