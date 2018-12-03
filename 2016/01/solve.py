import sys

def load_steps(filename):
	steps = []
	with open(filename, "r") as f:
		for step in f.read()[:-1].split(","):
			step = step.strip()
			step = (step[0], int(step[1:]))
			steps.append(step)
	return steps

# PART 1

def turn(direction, where):
	dir_x, dir_y = direction

	if where == "R":
		return (dir_y, -dir_x)
	elif where == "L":
		return (-dir_y, dir_x)
	else:
		raise Exception("HEY! Don't do that!")

def manhattan(position):
	x, y = position
	return abs(x) + abs(y)

def find_distance(steps):
	pos_x, pos_y = 0, 0
	direction = (1, 0)

	for step in steps:
		where, amount = step
		direction = turn(direction, where)
		dir_x, dir_y = direction
		pos_x += dir_x * amount
		pos_y += dir_y * amount

	return manhattan((pos_x, pos_y))

# PART 2

def first_visited_twice(steps):
	pos_x, pos_y = 0, 0
	direction = (1, 0)
	visited = {(pos_x, pos_y)}

	for step in steps:
		where, amount = step
		direction = turn(direction, where)
		dir_x, dir_y = direction

		for i in range(amount):
			pos_x += dir_x
			pos_y += dir_y
			position = (pos_x, pos_y)

			if position in visited:
				return position
			else:
				visited.add(position)

def main(filename):
	steps = load_steps(filename)
	print(f"Solutions for {filename}")
	distance = find_distance(steps)
	print(f"Part 1: {distance}")
	pos = first_visited_twice(steps)
	distance_2 = manhattan(pos)
	print(f"Part 2: {distance_2}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
