import sys

def load_freqs(filename):
	freqs = []
	with open(filename, "r") as f:
		for line in f:
			n = int(line[1:-1])
			if line[0] == "-":
				n = -n
			freqs.append(n)
	return freqs

# PART 2

def find_repeat(freqs):
	total = 0
	found = {total}

	while True:
		for n in freqs:
			total += n
			if total in found:
				return total
			else:
				found.add(total)

def main(filename):
	freqs = load_freqs(filename)
	print(f"Solutions for {filename}")
	print(f"Part 1: {sum(freqs)}")
	print(f"Part 2: {find_repeat(freqs)}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
