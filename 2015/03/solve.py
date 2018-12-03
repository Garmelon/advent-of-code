import sys

def load_steps(filename):
	with open(filename, "r") as f:
		return f.read()[:-1]

# PART 1

def find_doubles(steps):
	x, y = 0, 0
	houses = {(0, 0)}
	for step in steps:
		if   step == "^": y += 1
		elif step == "v": y -= 1
		elif step == "<": x -= 1
		elif step == ">": x += 1
		houses.add((x, y))
	return houses

# PART 2

def split_string(s):
	s1 = ""
	s2 = ""
	while True:
		if not s: break
		s1 += s[0]
		s = s[1:]
		if not s: break
		s2 += s[0]
		s = s[1:]
	return s1, s2

def main(filename):
	steps = load_steps(filename)
	print(f"Solutions for {filename}")
	doubles = len(find_doubles(steps))
	print(f"Part 1: {doubles}")
	santa, robot = split_string(steps)
	santa_doubles = find_doubles(santa)
	robot_doubles = find_doubles(robot)
	doubles_2 = len(santa_doubles | robot_doubles)
	print(f"Part 2: {doubles_2}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
