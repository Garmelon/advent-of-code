import hashlib

# PART 1


def leading_zeroes(amount, start):
    n = 1
    what = "0" * amount
    while True:
        text = (start + str(n)).encode("utf-8")
        h = hashlib.md5(text).hexdigest()
        if h[:amount] == what:
            return n

        # if n % 100000 == 0:
        #     print(f"{n:9} {text} {h}")

        n += 1


# PART 2


def solve(inputstr):
    hashstart = inputstr.strip()
    n = leading_zeroes(5, hashstart)
    print(f"Part 1: {n}")
    n_2 = leading_zeroes(6, hashstart)
    print(f"Part 2: {n_2}")
