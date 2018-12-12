import sys

# PART 1

def load_stream(filename):
	with open(filename) as f:
		# Too lazy to do this properly
		return list(reversed(list(map(int, f.read()[:-1].split(" ")))))

def parse_node(stream):
	amt_nodes = stream.pop()
	amt_meta = stream.pop()

	nodes = []
	for _ in range(amt_nodes):
		nodes.append(parse_node(stream))

	meta = []
	for _ in range(amt_meta):
		meta.append(stream.pop())

	return (nodes, meta)

def sum_of_meta(node):
	nodes, meta = node
	return sum(meta) + sum(map(sum_of_meta, nodes))

# PART 2

def value_of(node):
	nodes, meta = node
	if nodes:
		total = 0
		for i in meta:
			i -= 1
			if i >= 0 and i < len(nodes):
				total += value_of(nodes[i])
		return total
	else:
		return sum(meta)

def main(filename):
	print(f"Solutions for {filename}")
	stream = load_stream(filename)
	node = parse_node(stream)
	meta = sum_of_meta(node)
	print(f"Part 1: {meta}")
	value = value_of(node)
	print(f"Part 2: {value}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
