def solve(inputstr):
    steps = [1 if c == "(" else -1 for c in inputstr.strip()]

    print(f"Part 1: {sum(steps)}")

    at = 0
    for i, step in enumerate(steps):
        at += step
        if at < 0:
            break
    print(f"Part 2: {i + 1}")
