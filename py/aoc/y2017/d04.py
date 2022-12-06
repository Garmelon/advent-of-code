import collections

# PART 1


def load_phrases(inputstr):
    phrases = []
    for line in inputstr.splitlines():
        phrase = line.split(" ")
        phrases.append(phrase)
    return phrases


def is_valid(phrase):
    return len(phrase) == len(set(phrase))


def count(what, when):
    return len(list(filter(when, what)))


# PART 2


def is_valid_2(phrase):
    phrase = ["".join(sorted(word)) for word in phrase]
    return len(phrase) == len(set(phrase))


def solve(inputstr):
    phrases = load_phrases(inputstr)
    correct = count(phrases, is_valid)
    print(f"Part 1: {correct}")
    correct_2 = count(phrases, is_valid_2)
    print(f"Part 2: {correct_2}")
