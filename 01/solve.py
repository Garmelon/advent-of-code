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
	print("  Part 1")
	print(f"Total: {sum(freqs)}")
	print()
	print("  Part 2")
	print(f"First repeat: {find_repeat(freqs)}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
