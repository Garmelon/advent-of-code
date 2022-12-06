class Stream:
    def __init__(self, string):
        self.content = list(string)
        self.content.reverse()

    def peek(self):
        return self.content[-1]

    def pop(self):
        return self.content.pop()


def load_groups(inputstr):
    groups = []
    for line in inputstr.splitlines():
        stream = Stream(line)
        group, garbage = parse_group(stream)
        groups.append((group, garbage))
    return groups


def parse_group(stream):
    assert stream.pop() == "{"

    if stream.peek() == "}":
        assert stream.pop() == "}"
        return [], 0

    groups = []
    garbage = 0

    while True:
        # Determine which sub-parser to use
        if stream.peek() == "{":
            group, more_garbage = parse_group(stream)
            groups.append(group)
            garbage += more_garbage
        elif stream.peek() == "<":
            garbage += parse_garbage(stream)
        else:
            raise Exception("Incorrectly formatted input")

        # Determine whether to stop parsing
        if stream.peek() == "}":
            break
        elif stream.peek() == ",":
            assert stream.pop() == ","
        else:
            raise Exception("Incorrectly formatted input")

    assert stream.pop() == "}"
    return groups, garbage


def parse_garbage(stream):
    assert stream.pop() == "<"

    escaped = False
    garbage = 0

    while True:
        if escaped:
            stream.pop()
            escaped = False
        elif stream.peek() == "!":
            assert stream.pop() == "!"
            escaped = True
        elif stream.peek() == ">":
            break
        else:
            stream.pop()
            garbage += 1

    assert stream.pop() == ">"

    return garbage


def group_score(group, level=1):
    return sum(group_score(subgroup, level + 1) for subgroup in group) + level


def solve(inputstr):
    groups = load_groups(inputstr)
    for group, garbage in groups:
        score = group_score(group)
        print(f"Part 1: {score}")
        print(f"Part 2: {garbage}")
