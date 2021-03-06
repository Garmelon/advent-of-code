import sys

def load_words(filename):
	words = []
	with open(filename, "r") as f:
		for line in f:
			words.append(line[:-1])
	return words

# PART 1

def is_vowel(char):
	return char in "aeiou"

def has_double_letter(word):
	# This would look nicer im haskell:
	return count(zip(word, word[1:]), lambda x: x[0] == x[1]) > 0

def is_nice(word):
	if "ab" in word or "cd" in word or "pq" in word or "xy" in word:
		return False
	if len(list(filter(is_vowel, word))) < 3:
		return False
	return has_double_letter(word)

def count(what, function):
	# This would also look nicer in haskell
	return len(list(filter(function, what)))

# PART 2

def has_pair(word):
	for i in range(len(word)):
		if word[i:i+2] in word[i+2:]:
			return True
	return False

def has_repeat_letter(word):
	return count(zip(word, word[2:]), lambda x: x[0] == x[1]) > 0

def is_nice_2(word):
	return has_pair(word) and has_repeat_letter(word)

def main(filename):
	words = load_words(filename)
	print(f"Solutions for {filename}")
	amount = count(words, is_nice)
	print(f"Part 1: {amount}")
	amount_2 = count(words, is_nice_2)
	print(f"Part 2: {amount_2}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
