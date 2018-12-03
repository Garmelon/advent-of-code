import re
import sys

TRIANGLE_RE = r"\s*(\d+)\s+(\d+)\s+(\d+)\n"

def load_triangles(filename):
	triangles = []
	with open(filename, "r") as f:
		for line in f:
			match = re.fullmatch(TRIANGLE_RE, line)
			a, b, c = match.groups()
			a, b, c = int(a), int(b), int(c)
			triangles.append((a, b, c))
	return triangles

# PART 1

def is_valid(triangle):
	a, b, c = sorted(triangle)
	return a + b > c

def count_valid(triangles):
	return len(list(filter(is_valid, triangles)))

# PART 2

def transform_triangles(triangles):
	new_triangles = []
	while triangles:
		t1, t2, t3 = triangles[:3]
		triangles = triangles[3:]
		new_triangles.extend(zip(t1, t2, t3))
	return new_triangles

def main(filename):
	triangles = load_triangles(filename)
	print(f"Solutions for {filename}")
	valid = count_valid(triangles)
	print(f"Part 1: {valid}")
	valid_2 = count_valid(transform_triangles(triangles))
	print(f"Part 2: {valid_2}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
