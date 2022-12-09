import sys
import argparse
from pathlib import Path

from .y2015 import d01, d02, d03, d04, d05
from .y2016 import d01, d02, d03, d04, d05
from .y2017 import d01, d02, d03, d04, d05, d06, d07, d08, d09
from .y2018 import d01, d02, d03, d04, d05, d06, d07, d08, d09, d10, d11
from .y2020 import d10
from .y2021 import d14
from .y2022 import d01, d02, d03, d04, d05, d06, d07

DAYS = {
    # 2015
    "2015_01": y2015.d01.solve,
    "2015_02": y2015.d02.solve,
    "2015_03": y2015.d03.solve,
    "2015_04": y2015.d04.solve,
    "2015_05": y2015.d05.solve,
    # 2016
    "2016_01": y2016.d01.solve,
    "2016_02": y2016.d02.solve,
    "2016_03": y2016.d03.solve,
    "2016_04": y2016.d04.solve,
    "2016_05": y2016.d05.solve,
    # 2017
    "2017_01": y2017.d01.solve,
    "2017_02": y2017.d02.solve,
    "2017_03": y2017.d03.solve,
    "2017_04": y2017.d04.solve,
    "2017_05": y2017.d05.solve,
    "2017_06": y2017.d06.solve,
    "2017_07": y2017.d07.solve,
    "2017_08": y2017.d08.solve,
    "2017_09": y2017.d09.solve,
    # 2018
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
    # 2019
    # 2020
    "2020_10": y2020.d10.solve,
    # 2021
    "2021_14": y2021.d14.solve,
    # 2022
    "2022_01": y2022.d01.solve,
    "2022_02": y2022.d02.solve,
    "2022_03": y2022.d03.solve,
    "2022_04": y2022.d04.solve,
    "2022_05": y2022.d05.solve,
    "2022_06": y2022.d06.solve,
    "2022_07": y2022.d07.solve,
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
