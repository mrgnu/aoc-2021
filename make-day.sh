#!/usr/bin/env sh

if [ -z $1 ]
then
    echo "usage: ${0} <day>"
    exit 1
fi

day=$1

# project name
pn="aoc_2021"

# template files
tsf="src/${pn}/day_FNORD.clj"
ttf="test/${pn}/day_FNORD_test.clj"

# target files
sf="src/${pn}/day_${day}.clj"
tf="test/${pn}/day_${day}_test.clj"

if [ -f "$sf" ]
then
    echo "src file exists: ${sf}"
    exit 1
fi
if [ -f "$tf" ]
then
    echo "test file exists: ${tf}"
    exit 1
fi

echo "generating source files for day ${day} ..."

echo "- src file ${sf} ..."
cat $tsf | sed "s/FNORD/${day}/g" > $sf

echo "- test file ${tf} ..."
cat $ttf | sed "s/FNORD/${day}/g" > $tf
