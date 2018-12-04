import sys

# PART 1

def load_jumps(filename):
	jumps = []
	with open(filename, "r") as f:
		for line in f:
			jumps.append(int(line[:-1]))
	return jumps

def perform_jumps(jumps):
	jumps = jumps.copy()
	pos = 0
	steps = 0

	while 0 <= pos < len(jumps):
		next_pos = pos + jumps[pos]
		jumps[pos] += 1
		pos = next_pos
		steps += 1

	return steps

# PART 2

def perform_jumps_2(jumps):
	jumps = jumps.copy()
	pos = 0
	steps = 0

	while 0 <= pos < len(jumps):
		next_pos = pos + jumps[pos]
		if jumps[pos] >= 3:
			jumps[pos] -= 1
		else:
			jumps[pos] += 1
		pos = next_pos
		steps += 1

	return steps

def main(filename):
	jumps = load_jumps(filename)
	print("Previously calculated")
	print("Part 1: 343467")
	print("Part 2: 24774780")
	print(f"Solutions for {filename}")
	steps = perform_jumps(jumps)
	print(f"Part 1: {steps}")
	steps_2 = perform_jumps_2(jumps)
	print(f"Part 2: {steps_2}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
