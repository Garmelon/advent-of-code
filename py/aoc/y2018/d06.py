# PART 1


def load_coords(inputstr):
    coords = []
    for line in inputstr.splitlines():
        x, y = line.split(", ")
        x, y = int(x), int(y)
        coords.append((x, y))
    return coords


class Voronoi:
    def __init__(self, min_x, min_y, max_x, max_y):
        self.min_x = min_x
        self.min_y = min_y
        self.max_x = max_x
        self.max_y = max_y

        self.cells = {}

    def add(self, pos_x, pos_y):
        for x in range(self.min_x, self.max_x + 1):
            for y in range(self.min_y, self.max_y + 1):
                self.add_to_cell(pos_x, pos_y, x, y)
        # print(f"Added ({pos_x}, {pos_y})")

    def add_to_cell(self, pos_x, pos_y, cell_x, cell_y):
        pos = (pos_x, pos_y)
        cell = (cell_x, cell_y)
        dist = abs(pos_x - cell_x) + abs(pos_y - cell_y)

        if cell in self.cells:
            cdist, cpos = self.cells[cell]
            if dist < cdist:
                self.cells[cell] = (dist, pos)
            elif dist == cdist and cpos is not None:
                self.cells[cell] = (dist, None)
            # else: pass
        else:
            self.cells[cell] = (dist, pos)

    @classmethod
    def from_coords(cls, coords):
        xs = [x for x, _ in coords]
        ys = [y for _, y in coords]

        voro = cls(min(xs), min(ys), max(xs), max(ys))
        for x, y in coords:
            voro.add(x, y)

        return voro

    def find_ignore(self):
        ignore = set()
        for x in range(self.min_x, self.max_x + 1):
            _, pos1 = self.cells.get((x, self.min_y))
            _, pos2 = self.cells.get((x, self.max_y))
            ignore.add(pos1)
            ignore.add(pos2)
        for y in range(self.min_y, self.max_y + 1):
            _, pos1 = self.cells.get((self.min_x, y))
            _, pos2 = self.cells.get((self.max_x, y))
            ignore.add(pos1)
            ignore.add(pos2)
        return ignore

    def find_largest(self):
        ignore = self.find_ignore()

        count = {}
        for x in range(self.min_x + 1, self.max_x):
            for y in range(self.min_y + 1, self.max_y):
                _, pos = self.cells.get((x, y))
                if pos not in ignore:
                    count[pos] = count.get(pos, 0) + 1

        return max(count.values())


# PART 2


class Nearest:
    def __init__(self, coords):
        self.coords = coords

        xs = [x for x, _ in self.coords]
        ys = [y for _, y in self.coords]

        self.min_x = min(xs)
        self.min_y = min(ys)
        self.max_x = max(xs)
        self.max_y = max(ys)

    def find(self):
        cells = set()

        for x in range(self.min_x, self.max_x + 1):
            for y in range(self.min_y, self.max_y + 1):
                if self.evaluate(x, y):
                    cells.add((x, y))

        return len(cells)

    def evaluate(self, pos_x, pos_y):
        pos = (pos_x, pos_y)
        dist = 0
        for x, y in self.coords:
            dist += abs(pos_x - x) + abs(pos_y - y)
        return dist < 10000


def solve(inputstr):
    coords = load_coords(inputstr)
    voro = Voronoi.from_coords(coords)
    largest = voro.find_largest()
    print(f"Part 1: {largest}")
    nearest = Nearest(coords)
    largest_2 = nearest.find()
    print(f"Part 2: {largest_2}")
