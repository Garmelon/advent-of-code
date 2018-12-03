import hashlib

# PART 1

def leading_zeroes(amount, start):
	n = 1
	what = "0"*amount
	while True:
		text = (start + str(n)).encode("utf-8")
		h = hashlib.md5(text).hexdigest()
		if h[:amount] == what:
			return n

		if n % 100000 == 0:
			print(f"{n:9} {text} {h}")

		n += 1

# PART 2

def main(hashstart):
	print("Previously calculated:")
	print("Part 1: 282749")
	print("Part 2: 9962624")
	print()
	print(f"Solutions")
	n = leading_zeroes(5, hashstart)
	print(f"Part 1: {n}")
	n_2 = leading_zeroes(6, hashstart)
	print(f"Part 2: {n_2}")

if __name__ == "__main__":
	main("yzbqklnj")
