import collections
import sys

# PART 1

def load_phrases(filename):
	phrases = []
	with open(filename, "r") as f:
		for line in f:
			phrase = line[:-1].split(" ")
			phrases.append(phrase)
	return phrases

def is_valid(phrase):
	return len(phrase) == len(set(phrase))

def count(what, when):
	return len(list(filter(when, what)))

# PART 2

def is_valid_2(phrase):
	phrase = ["".join(sorted(word)) for word in phrase]
	return len(phrase) == len(set(phrase))

def main(filename):
	print(f"Solutions for {filename}")
	phrases = load_phrases(filename)
	correct = count(phrases, is_valid)
	print(f"Part 1: {correct}")
	correct_2 = count(phrases, is_valid_2)
	print(f"Part 2: {correct_2}")

if __name__ == "__main__":
	for filename in sys.argv[1:]:
		main(filename)
