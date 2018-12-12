import sys
import collections

# PART 1

class Circle:
	def __init__(self):
		self.marbles = collections.deque([0])
		#self.marbles = [0]
		self.next = 1

	def move_focus(self, steps):
		self.marbles.rotate(steps)
		#self.marbles = self.marbles[steps:] + self.marbles[:steps]

	def insert(self):
		self.marbles.append(self.next)
		#self.marbles = [self.next] + self.marbles
		self.next += 1

	def remove(self):
		return self.marbles.pop()
		#n = self.marbles[0]
		#self.marbles = self.marbles[1:]
		#return n

	def insert_marble(self):
		# returns the two marbles removed in a tuple, or None
		if self.next % 23 == 0:
			return self.insert_multiple()
		else:
			self.insert_normal()
			return None

	def insert_normal(self):
		self.move_focus(2)
		self.insert()

	def insert_multiple(self):
		cur = self.next
		self.next += 1

		self.move_focus(-7)
		left = self.remove()

		return (cur, left)

class Game:
	def __init__(self, elves, until):
		self.circle = Circle()
		self.until = until
		self.elves = [0 for _ in range(elves)]

	def play(self):
		while True:
			for elf in range(len(self.elves)):
				if self.circle.next % 10000 == 0:
					print(f"{self.circle.next:5} of {self.until:5} - {100 * self.circle.next / self.until:.04}%  {len(self.circle.marbles):8}")

				if self.circle.next > self.until:
					return

				result = self.circle.insert_marble()
				if result is not None:
					fst, snd = result
					self.elves[elf] += fst + snd

	def highscore(self):
		return max(self.elves)

# PART 2

def main(elves, until):
	game = Game(elves, until)
	game.play()
	score = game.highscore()
	print(score)

if __name__ == "__main__":
	#main( 9,   25)
	#main(10, 1618)
	#main(13, 7999)
	#main(17, 1104)
	#main(21, 6111)
	#main(30, 5807)
	main(429, 70901)
	main(429, 70901*100)
