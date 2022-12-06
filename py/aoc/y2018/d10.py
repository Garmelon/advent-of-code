import re


POINT_RE = r"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>"


class Point:
    def __init__(self, pos, vel):
        self.pos = pos
        self.vel = vel

    @classmethod
    def from_line(cls, line):
        match = re.fullmatch(POINT_RE, line)
        x, y, dx, dy = map(int, match.groups())
        return cls((x, y), (dx, dy))

    def step(self):
        x, y = self.pos
        dx, dy = self.vel
        self.pos = (x + dx, y + dy)


class Field:
    def __init__(self, points=None):
        self.points = points or []
        self.steps = 0

    @classmethod
    def from_str(cls, string):
        points = []
        for line in string.splitlines():
            points.append(Point.from_line(line))
        return cls(points=points)

    def step(self):
        for point in self.points:
            point.step()
        self.steps += 1

    def step_until_in_limits(self):
        while True:
            coords = set(point.pos for point in self.points)
            xs = set(x for (x, _) in coords)
            ys = set(y for (_, y) in coords)

            if max(xs) - min(xs) < 500:
                return

            self.step()

            print(min(xs), max(xs), min(ys), max(ys))

    def render(self):
        coords = set(point.pos for point in self.points)
        xs = set(x for (x, _) in coords)
        ys = set(y for (_, y) in coords)

        for y in range(min(ys), max(ys) + 1):
            for x in range(min(xs), max(xs) + 1):
                if (x, y) in coords:
                    print("#", end="")
                else:
                    print(".", end="")
            print()

        print(self.steps)


def solve(inputstr):
    print("Part 1:")
    field = Field.from_str(inputstr)
    field.step_until_in_limits()
    while input() == "":
        field.render()
        field.step()

    print("Part 2: NYI")
