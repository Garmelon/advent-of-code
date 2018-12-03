import sys

def load_moves(filename):
	moves = []
	with open(filename, "r") as f:
		for line in f:
			moves.append(line[:-1])
	return moves

# PART 1

LAYOUT_1 = {
	(0, 0): "1",
	(1, 0): "2",
	(2, 0): "3",
	(0, 1): "4",
	(1, 1): "5",
	(2, 1): "6",
	(0, 2): "7",
	(1, 2): "8",
	(2, 2): "9",
}

def pos_to_num(layout, pos):
	return layout.get(pos)

def do_step(layout, pos, step):
	x, y = pos
	if   step == "U": y -= 1
	elif step == "D": y += 1
	elif step == "L": x -= 1
	elif step == "R": x += 1
	newpos = (x, y)

	if newpos in layout:
		return newpos
	else:
		return pos

def do_move(layout, pos, steps):
	for step in steps:
		pos = do_step(layout, pos, step)
	return pos

def enter(layout, moves):
	pos = (1, 1)
	numbers = []
	for move in moves:
		pos = do_move(layout, pos, move)
		numbers.append(pos_to_num(layout, pos))

	numbers = "".join(numbers)
	return numbers

# PART 2

LAYOUT_2 = {
	(2, 0): "1",
	(1, 1): "2",
	(2, 1): "3",
	(3, 1): "4",
	(0, 2): "5",
	(1, 2): "6",
	(2, 2): "7",
	(3, 2): "8",
	(4, 2): "9",
	(1, 3): "A",
	(2, 3): "B",
	(3, 3): "C",
	(2, 4): "D",
}

def main(filename):
	moves = load_moves(filename)
	print(f"Solutions for {filename}")
	numbers = enter(LAYOUT_1, moves)
	print(f"Part 1: {numbers}")
	numbers_2 = enter(LAYOUT_2, moves)
	print(f"Part 2: {numbers_2}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
