import re
import sys

CLAIM_RE = r"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)\n"

def load_claims(filename):
	claims = {}
	with open(filename, "r") as f:
		for line in f:
			match = re.fullmatch(CLAIM_RE, line)
			elf, x, y, w, h = match.groups()
			elf, x, y, w, h = int(elf), int(x), int(y), int(w), int(h)
			claims[elf] = (x, y, w, h)
	return claims

# PART 1

def count_claims(claims):
	squares = {}
	for x, y, w, h in claims.values():
		for dw in range(w):
			for dh in range(h):
				coords = (x + dw, y + dh)
				squares[coords] = squares.get(coords, 0) + 1
	return squares

def find_doubles(squares):
	return len(list(filter(lambda x: x > 1, squares.values())))

# PART 2

def is_intact(squares, x, y, w, h):
	for dw in range(w):
		for dh in range(h):
			coords = (x + dw, y + dh)
			if squares.get(coords, 0) > 1:
				return False
	return True

def find_intact_claim(claims, squares):
	for elf, (x, y, w, h) in claims.items():
		if is_intact(squares, x, y, w, h):
			return elf

def main(filename):
	claims = load_claims(filename)
	print(f"Solutions for {filename}")
	squares = count_claims(claims)
	doubles = find_doubles(squares)
	print(f"Part 1: {doubles}")
	intact = find_intact_claim(claims, squares)
	print(f"Part 2: {intact}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
