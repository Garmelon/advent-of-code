import sys

def load_line(filename):
	with open(filename, "r") as f:
		return list(map(int, f.read()[:-1]))

# PART 1

def sum_matching(digits):
	offset = digits[1:] + digits
	total = 0
	for x, y in zip(digits, offset):
		if x == y:
			total += x
	return total

# PART 2

def sum_matching_2(digits):
	offset = digits[len(digits)//2:] + digits
	total = 0
	for x, y in zip(digits, offset):
		if x == y:
			total += x
	return total

def main(filename):
	digits = load_line(filename)
	print(f"Solutions for {filename}")
	total = sum_matching(digits)
	print(f"Part 1: {total}")
	total_2 = sum_matching_2(digits)
	print(f"Part 2: {total_2}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
