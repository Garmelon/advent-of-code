import sys
import argparse
from pathlib import Path

from .y2020 import d10
from .y2021 import d14
from .y2022 import d01, d02, d03, d04, d05, d06

DAYS = {
    "2020_10": y2020.d10.solve,
    "2021_14": y2021.d14.solve,
    "2022_01": y2022.d01.solve,
    "2022_02": y2022.d02.solve,
    "2022_03": y2022.d03.solve,
    "2022_04": y2022.d04.solve,
    "2022_05": y2022.d05.solve,
    "2022_06": y2022.d06.solve,
}


def eprint(*args, **kwargs):
    print(*args, **kwargs, file=sys.stderr)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("files", type=Path, nargs="+")
    args = parser.parse_args()

    for file in args.files:
        day = DAYS.get(file.stem[:7])
        if day is None:
            eprint(f"### Can't solve {file}")
            continue

        eprint(f"### Solving {file}")
        with open(file) as f:
            inputstr = f.read()
        day(inputstr)
        eprint()
