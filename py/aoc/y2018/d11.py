# PART 1


def nth_digit(num, pos):
    # nth_digit(1234, 0) = 123[4] = 4
    # nth_digit(1234, 2) = 1[2]34 = 2
    # nth_digit(1234, 4) = [0]1234 = 0

    pos += 1
    s = ("{:0" + str(pos) + "}").format(num)  # UGLY!
    n = s[-pos]
    return int(n)


def power_level(number, x, y):
    rack_id = x + 10
    level = rack_id * y
    level += number
    level *= rack_id
    level = nth_digit(level, 2)  # 100-er digit
    level -= 5
    return level


def init_grid(number):
    grid = {}
    for x in range(1, 300 + 1):
        for y in range(1, 300 + 1):
            grid[(x, y)] = power_level(number, x, y)
    return grid


def power_of_square(grid, x, y):
    power = 0
    for dx in range(3):
        for dy in range(3):
            power += grid[(x + dx, y + dy)]
    return power


def find_max_power(grid):
    max_power = 0
    max_coords = None

    for x in range(1, 300 - 2 + 1):
        for y in range(1, 300 - 2 + 1):
            power = power_of_square(grid, x, y)
            if power > max_power:
                max_power = power
                max_coords = (x, y)

    return max_coords


# PART 2


def square_power(grid, x, y, size):
    pass


def max_square(grid):
    max_power = 0
    max_coords = None
    max_size = None

    for size in range(1, 300 + 1):
        for x in range(1, 300 + 1 - (size - 1)):
            for y in range(1, 300 + 1 - (size - 1)):
                pass


def solve(inputstr):
    number = int(inputstr.strip())
    grid = init_grid(number)
    x, y = find_max_power(grid)
    print(f"Part 1: {x},{y}")
    print(f"Part 2: NYI")
