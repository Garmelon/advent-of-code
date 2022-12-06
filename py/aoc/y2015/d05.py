import re


def part1_nice(word):
    vowels = re.search(r"([aeiou].*){3}", word)
    double = re.search(r"(.)\1", word)
    forbidden = re.search(r"ab|cd|pq|xy", word)
    return vowels and double and not forbidden


def part2_nice(word):
    pair = re.search(r"(..).*\1", word)
    repeats = re.search(r"(.).\1", word)
    return pair and repeats


def solve(inputstr):
    words = inputstr.splitlines()
    print(f"Part 1: {len(list(filter(part1_nice, words)))}")
    print(f"Part 2: {len(list(filter(part2_nice, words)))}")
