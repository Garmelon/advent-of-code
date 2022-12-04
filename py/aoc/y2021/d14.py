from pathlib import Path
from collections import Counter


def step(pairs, rules):
    result = Counter()
    for pair, amount in pairs.items():
        if pair in rules:
            a, b = rules[pair]
            result[a] += amount
            result[b] += amount
        else:
            result[pair] += amount
    return result


def count_chars(template, pairs):
    chars = Counter()
    for pair, amount in pairs.items():
        chars[pair[0]] += amount
    chars[template[-1]] += 1
    return chars


def solve(inputstr):
    template, rest = inputstr.split("\n\n", maxsplit=1)
    rules = {}
    for line in rest.splitlines():
        pair, char = line.split(" -> ")
        rules[pair] = (pair[0] + char, char + pair[1])

    pairs = Counter(template[i : i + 2] for i in range(len(template) - 1))

    for i in range(10):
        pairs = step(pairs, rules)

    chars = count_chars(template, pairs).most_common()
    print(f"Part 1: {chars[0][1] - chars[-1][1]}")

    for i in range(30):  # 40 steps total
        pairs = step(pairs, rules)

    chars = count_chars(template, pairs).most_common()
    print(f"Part 2: {chars[0][1] - chars[-1][1]}")
