import hashlib

# PART 1

def chars(start, amount):
	n = 0
	chars = []
	while amount > 0:
		text = (start + str(n)).encode("utf-8")
		h = hashlib.md5(text).hexdigest()
		if h[:5] == "00000":
			print(h, "->", char)
			char = h[5]
			chars.append(char)
			amount -= 1
		n += 1
	return "".join(chars)

# PART 2

def chars_2(start, amount):
	n = 0
	chars = [None]*amount
	while amount > 0:
		text = (start + str(n)).encode("utf-8")
		h = hashlib.md5(text).hexdigest()
		if h[:5] == "00000":
			char = h[6]
			pos = int(h[5], base=16)
			if pos < len(chars):
				if chars[pos] is None:
					chars[pos] = char
					pw = "".join("_" if x is None else x for x in chars)
					print(h, "->", char, "in position", pos, "->", pw)
					if None not in chars:
						return "".join(chars)
				else:
					print(h, "->", chars[pos], "already in position", pos)
			else:
				print(h, "->", "invalid position", pos)
		n += 1

def main(hashstart):
	print("Previously calculated")
	print("Part 1: 4543c154")
	print("Part 2: 1050cbbd")
	print(f"Solutions")
	pw = chars(hashstart, 8)
	print(f"Part 1: {pw}")
	pw_2 = chars_2(hashstart, 8)
	print(f"Part 2: {pw_2}")

if __name__ == "__main__":
	main("ojvtpuvg")
