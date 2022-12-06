import re

# PART 1

ROOM_RE = r"(.*)-(\d+)\[(.{5})\]"


def load_rooms(inputstr):
    rooms = []
    for line in inputstr.splitlines():
        match = re.fullmatch(ROOM_RE, line)
        name, sector, checksum = match.groups()
        name = name.split("-")
        sector = int(sector)
        rooms.append((name, sector, checksum))
    return rooms


def checksum(name):
    name = "".join(name)
    freqs = {}
    for char in name:
        freqs[char] = freqs.get(char, 0) + 1

    top = freqs.items()
    top = sorted(top, key=lambda x: x[0])
    top = sorted(top, key=lambda x: x[1], reverse=True)
    top = list(top)[:5]
    top = map(lambda x: x[0], top)
    return "".join(top)


def sum_correct_sectors(rooms):
    total = 0
    for name, sector, chk in rooms:
        if checksum(name) == chk:
            total += sector
    return total


# PART 2


def shift(s):
    new_s = []
    for c in s:
        if ord("a") <= ord(c) <= ord("z"):
            new_c = chr(
                ord("a") + ((ord(c) - ord("a") + 1) % (ord("z") - ord("a") + 1))
            )
            new_s.append(new_c)
        elif ord("A") <= ord(c) <= ord("Z"):
            new_c = chr(
                ord("A") + ((ord(c) - ord("A") + 1) % (ord("Z") - ord("A") + 1))
            )
            new_s.append(new_c)
        else:
            new_s.append("?")
    return "".join(new_s)


def decrypt_name(name, sector):
    for _ in range(sector % 26):
        name = map(shift, name)
    return " ".join(name)


def decrypt_names(rooms, filename):
    with open(filename, "w") as f:
        for name, sector, chk in rooms:
            if checksum(name) == chk:
                name = decrypt_name(name, sector)
                line = f"{name}-{sector}[{chk}]\n"
                f.write(line)


def solve(inputstr):
    rooms = load_rooms(inputstr)
    total = sum_correct_sectors(rooms)
    print(f"Part 1: {total}")
    # filename_2 = inputstr + ".decrypted"
    # decrypt_names(rooms, filename_2)
    # print(f"Part 2: see {filename_2} (search for 'northpole object storage')")
