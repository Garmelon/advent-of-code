import re
import sys

PACKET_RE = r"(\d+)x(\d+)x(\d+)\n"

def load_packets(filename):
	packets = []
	with open(filename, "r") as f:
		for line in f:
			match = re.fullmatch(PACKET_RE, line)
			a, b, c = match.groups()
			a, b, c = int(a), int(b), int(c)
			packets.append((a, b, c))
	return packets

# PART 1

def necessary_area(packet):
	a, b, c = sorted(packet)
	return 3*a*b + 2*a*c + 2*b*c

def total_wrapping_paper(packets):
	return sum(map(necessary_area, packets))

# PART 2

def ribbon_length(packet):
	a, b, c = sorted(packet)
	return 2*a + 2*b + a*b*c

def total_ribbon_length(packets):
	return sum(map(ribbon_length, packets))

def main(filename):
	packets = load_packets(filename)
	print(f"Solutions for {filename}")
	total = total_wrapping_paper(packets)
	print(f"Part 1: {total}")
	total_2 = total_ribbon_length(packets)
	print(f"Part 2: {total_2}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
