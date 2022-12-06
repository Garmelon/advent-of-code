import re

PACKET_RE = r"(\d+)x(\d+)x(\d+)"


def load_packets(inputstr):
    packets = []
    for line in inputstr.splitlines():
        match = re.fullmatch(PACKET_RE, line)
        a, b, c = match.groups()
        a, b, c = int(a), int(b), int(c)
        packets.append((a, b, c))
    return packets


# PART 1


def necessary_area(packet):
    a, b, c = sorted(packet)
    return 3 * a * b + 2 * a * c + 2 * b * c


def total_wrapping_paper(packets):
    return sum(map(necessary_area, packets))


# PART 2


def ribbon_length(packet):
    a, b, c = sorted(packet)
    return 2 * a + 2 * b + a * b * c


def total_ribbon_length(packets):
    return sum(map(ribbon_length, packets))


def solve(inputstr):
    packets = load_packets(inputstr)
    total = total_wrapping_paper(packets)
    print(f"Part 1: {total}")
    total_2 = total_ribbon_length(packets)
    print(f"Part 2: {total_2}")
