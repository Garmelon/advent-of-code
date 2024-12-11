import argparse
import sys
from pathlib import Path

from . import y2015, y2016, y2017, y2018, y2020, y2021, y2022

DAYS = {
    **y2015.DAYS,
    **y2016.DAYS,
    **y2017.DAYS,
    **y2018.DAYS,
    **y2020.DAYS,
    **y2021.DAYS,
    **y2022.DAYS,
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
