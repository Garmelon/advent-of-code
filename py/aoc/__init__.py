import sys
import argparse
from pathlib import Path

DAYS = {}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("files", type=Path, nargs="+")
    args = parser.parse_args()

    first_day = True
    for file in args.files:
        if not first_day:
            print()
        first_day = False

        day = DAYS.get(file.stem)
        if day is None:
            print(f"### Could not determine day: {file}", file=sys.stderr)
            continue

        print(f"### Solving day {file.stem}")
        with open(file) as f:
            inputstr = f.read()
        day(inputstr)
