import sys

def main(path):
    with open(path) as f:
        values = list(sorted(int(i) for i in f))
    values.append(max(values) + 3)

    diffs = [y - x for x, y in zip([0] + values, values)]
    print(f"Part 1: {diffs.count(1) * diffs.count(3)}")

    combinations = [1] + [0] * max(values)
    for adapter in values:
        combinations[adapter] = sum(combinations[max(0, adapter-3):adapter])
    print(f"Part 2: {combinations[-1]}")

if __name__ == "__main__":
    main(sys.argv[1])
