#!/usr/bin/env bash
set -euo pipefail

day=$1
if [[ -z $day ]]; then
    echo >&2 "Usage: $0 <day>"
    exit 1
fi

inputFile="inputs/day${day}"
if [[ ! -e $inputFile ]]; then
    echo "Downloading input file"
    sessionToken=$(cat ~/.config/aoc/session_token)
    wget --no-cookies --header "Cookie: session=${sessionToken}" -O "$inputFile" https://adventofcode.com/2025/day/${day}/input
fi

testFile="inputs/day${day}_test1"
if [[ ! -e $testFile ]]; then
   testInput=$(xmlstarlet fo -H <(curl https://adventofcode.com/2025/day/$day) 2>/dev/null \
     | xmlstarlet sel -t -v '//pre[1]/code[1]' | head -c -1 | tr '\n' '@' | sed 's/@/\\n/g')
   echo "$testInput" > $testFile
fi

binFile="Day${day}.hs"
if [[ ! -e $binFile ]]; then
    cat Day.hs.tmpl | sed "s/%DAY%/$day/" > "$binFile"
fi

