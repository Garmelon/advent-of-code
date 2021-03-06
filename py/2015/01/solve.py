import sys

def load_steps(filename):
	with open(filename, "r") as f:
		return f.read()[:-1]

# PART 1

def count_floors(steps):
	return steps.count("(") - steps.count(")")

# PART 2

def find_basement_char(steps):
	pos = 0
	step_nr = 0
	for step in steps:
		step_nr += 1
		if step == "(":
			pos += 1
		if step == ")":
			pos -= 1
		if pos < 0:
			return step_nr

def main(filename):
	steps = load_steps(filename)
	print(f"Solutions for {filename}")
	floor = count_floors(steps)
	print(f"Part 1: {floor}")
	step_nr = find_basement_char(steps)
	print(f"Part 2: {step_nr}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
