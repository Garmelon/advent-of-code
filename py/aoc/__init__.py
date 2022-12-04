import sys
import argparse
from pathlib import Path

from .y2022 import d01, d02, d03, d04

DAYS = {
    "2022_01": y2022.d01.solve,
    "2022_02": y2022.d02.solve,
    "2022_03": y2022.d03.solve,
    "2022_04": y2022.d04.solve,
}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("files", type=Path, nargs="+")
    args = parser.parse_args()

    for file in args.files:
        day = DAYS.get(file.stem)
        if day is None:
            print(f"### Could not determine day: {file}", file=sys.stderr)
            continue

        print(f"### Solving day {file.stem}")
        with open(file) as f:
            inputstr = f.read()
        day(inputstr)
        print()
