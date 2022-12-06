import sys
import argparse
from pathlib import Path

from .y2018 import d01, d02, d03, d04, d05, d06, d07, d08, d09, d10, d11
from .y2020 import d10
from .y2021 import d14
from .y2022 import d01, d02, d03, d04, d05, d06

DAYS = {
    "2018_01": y2018.d01.solve,
    "2018_02": y2018.d02.solve,
    "2018_03": y2018.d03.solve,
    "2018_04": y2018.d04.solve,
    "2018_05": y2018.d05.solve,
    "2018_06": y2018.d06.solve,
    "2018_07": y2018.d07.solve,
    "2018_08": y2018.d08.solve,
    "2018_09": y2018.d09.solve,
    "2018_10": y2018.d10.solve,
    "2018_11": y2018.d11.solve,
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
