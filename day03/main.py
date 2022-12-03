import string
from functools import reduce
from operator import add
from os import environ

priorities = dict.fromkeys(string.ascii_lowercase + string.ascii_uppercase)
priorities.update((k, i) for i, k in enumerate(priorities, 1))


def unique_items(compartment):
    return "".join(set(compartment))


def solvePart1(input):
    foundInBoth = list()
    for rucksack in input:
        first_compartment = slice(0, len(rucksack) // 2)
        second_compartment = slice(len(rucksack) // 2, len(rucksack))
        for item in unique_items(rucksack[first_compartment]):
            if item in rucksack[second_compartment]:
                foundInBoth.append(item)
    return reduce(add, map(priorities.get, foundInBoth))


def solvePart2(input):
    foundInBoth = list()
    for group in range(0, len(input), 3):
        first = input[group]
        second = input[group + 1]
        third = input[group + 2]
        for item in unique_items(first):
            if item in second and item in third:
                foundInBoth.append(item)
    return reduce(add, map(priorities.get, foundInBoth))


with open('input.txt') as file:
    input = []
    for line in file.readlines():
        input.append(line.rstrip())

part = environ.get('part')

print(solvePart2(input)) if part == 'part2' else print(solvePart1(input))