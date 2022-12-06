# PART 1


def naive_react(pol):
    for i in range(len(pol) - 1):
        if pol[i].lower() == pol[i + 1].lower() and pol[i] != pol[i + 1]:
            print("Naive react says: More reacting is possible.")


def react(pol):
    while True:
        i = 0
        while i < len(pol) - 1:
            pol_cur, pol_next = pol[i], pol[i + 1]
            if pol_cur.lower() == pol_next.lower() and pol_cur != pol_next:
                del pol[i]
                del pol[i]
                i = max(0, i - 1)
            else:
                i += 1
        else:
            return


# def react(pol):
# 	start = 0
# 	while True:
# 		i = start
# 		while i < len(pol) - 1:
# 			if pol[i].lower() == pol[i+1].lower() and pol[i] != pol[i+1]:
# 				del pol[i]
# 				del pol[i]
# 				start = max(0, start - 1)
# 				break
# 			elif i == start + 1:
# 				start = i
# 			i += 1
# 		else:
# 			return


def result(pol):
    l = pol.copy()
    # print("".join(l))
    react(l)
    # print("->", "".join(l))
    naive_react(l)
    return len(l)


# PART 2


def removable_chars(pol):
    return set(c.lower() for c in pol)


def remove(pol, char):
    return [c for c in pol if c.lower() != char.lower()]


def find_obstructing(pol):
    results = []
    chars = removable_chars(pol)
    for c in sorted(chars):
        l = remove(pol, c)
        n = result(l)
        # print("Removed", c, "-> length", n)
        results.append(n)
    return min(results)


def solve(inputstr):
    pols = [list(line) for line in inputstr.splitlines()]
    for pol in pols:
        length = result(pol)
        print(f"Part 1: {length}")
        best_result = find_obstructing(pol)
        print(f"Part 2: {best_result}")
