def at(fs, path):
    for segment in path:
        fs = fs[segment]
    return fs


def calc_size(fs):
    if isinstance(fs, dict):
        return sum(calc_size(child) for child in fs.values())
    return fs


def dict_sizes(fs):
    result = []
    if isinstance(fs, dict):
        result.append(calc_size(fs))
        for child in fs.values():
            result.extend(dict_sizes(child))
    return result


def solve(inputstr):
    fs = {}
    path = []
    for line in inputstr.splitlines():
        if line == "$ cd /":
            path = []
        elif line == "$ cd ..":
            path.pop()
        elif line[:5] == "$ cd ":
            path.append(line[5:])
        elif line == "$ ls":
            pass
        elif line[:4] == "dir ":
            at(fs, path)[line[4:]] = {}
        else:
            size, name = line.split()
            at(fs, path)[name] = int(size)

    sizes = dict_sizes(fs)
    print("Part 1:", sum(size for size in sizes if size <= 100000))
    min_size = calc_size(fs) - 40000000
    print(f"Part 2:", min(size for size in sizes if size >= min_size))
