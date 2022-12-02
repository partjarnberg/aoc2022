#!/usr/bin/env bash
echo "Building docker image"
dockerImage=$(docker build -q .)

echo "Running part1 and part2"
executionTimePart1=$(TIMEFORMAT="%R"; { time docker run --rm -e part=part1 "${dockerImage}"; } 2>&1)
executionTimePart2=$(TIMEFORMAT="%R"; { time docker run --rm -e part=part2 "${dockerImage}"; } 2>&1)
docker image rm "${dockerImage}" 1> /dev/null

echo "Result part1: ${executionTimePart1}"
echo "Result part2: ${executionTimePart2}"