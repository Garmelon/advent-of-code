# PART 1


def has_n_chars(word, n):
    count = {}
    for char in word:
        count[char] = count.get(char, 0) + 1
    return n in count.values()


def count_words(words):
    twice = 0
    thrice = 0
    for word in words:
        if has_n_chars(word, 2):
            twice += 1
        if has_n_chars(word, 3):
            thrice += 1
    return twice, thrice


def checksum(words):
    twice, thrice = count_words(words)
    return twice * thrice


# PART 2


def differ_by(a, b):
    count = 0
    for x, y in zip(a, b):
        if x != y:
            count += 1
    return count


def find_ids(words):
    for i, a in enumerate(words):
        for b in words[i:]:
            if differ_by(a, b) == 1:
                return a, b


def common_chars(a, b):
    result = []
    for x, y in zip(a, b):
        if x == y:
            result.append(x)
    return "".join(result)


def solve(inputstr):
    words = inputstr.splitlines()
    print(f"Part 1: {checksum(words)}")
    a, b = find_ids(words)
    print(f"Part 2: {common_chars(a, b)}")
