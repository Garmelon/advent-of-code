import sys

# PART 1

def load_polymer(filename):
	pols = []
	with open(filename, "r") as f:
		for line in f:
			pols.append(list(line[:-1]))
	return pols

def naive_react(pol):
	for i in range(len(pol) - 1):
		if pol[i].lower() == pol[i+1].lower() and pol[i] != pol[i+1]:
			print("Naive react says: More reacting is possible.")

def react(pol):
	start = 0
	while True:
		i = start
		while i < len(pol) - 1:
			if pol[i].lower() == pol[i+1].lower() and pol[i] != pol[i+1]:
				del pol[i]
				del pol[i]
				start = max(0, start - 1)
				break
			elif i == start + 1:
				start = i
			i += 1
		else:
			return

def result(pol):
	l = pol.copy()
	#print("".join(l))
	react(l)
	#print("->", "".join(l))
	naive_react(l)
	return len(l)

# PART 2

def removable_chars(pol):
	return set(c.lower() for c in pol)

def remove(pol, char):
	return [c for c in pol if c.lower() != char.lower()]

def find_obstructing(pol):
	results = []
	chars = removable_chars(pol)
	for c in sorted(chars):
		l = remove(pol, c)
		n = result(l)
		print("Removed", c, "-> length", n)
		results.append(n)
	return min(results)

def main(filename):
	print(f"Solutions for {filename}")
	pols = load_polymer(filename)
	for pol in pols:
		length = result(pol)
		print(f"Part 1: {length}")
		best_result = find_obstructing(pol)
		print(f"Part 2: {best_result}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
