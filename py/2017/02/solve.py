import sys

def load_rows(filename):
	rows = []
	with open(filename, "r") as f:
		for line in f:
			digits = line[:-1].split("\t")
			row = list(map(int, digits))
			rows.append(row)
	return rows

# PART 1

def checksum(rows):
	total = 0
	for row in rows:
		total += max(row) - min(row)
	return total

# PART 2

def divide_evenly(row):
	for a in row:
		for b in row:
			if a != b and a % b == 0:
				return a // b

def checksum_even(rows):
	total = 0
	for row in rows:
		total += divide_evenly(row)
	return total

def main(filename):
	rows = load_rows(filename)
	print(f"Solutions for {filename}")
	check = checksum(rows)
	print(f"Part 1: {check}")
	check_even = checksum_even(rows)
	print(f"Part 2: {check_even}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
