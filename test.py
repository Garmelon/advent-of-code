#!/usr/bin/env python3

import argparse
import subprocess
from pathlib import Path

RESET = "\033[0m"
GRAY = "\033[1;90m"
GREEN = "\033[1;32m"
MAGENTA = "\033[1;35m"
RED = "\033[1;31m"


def find_solution(path, solutions):
    if path.suffix == ".input":
        solutions[path] = path.parent / (path.stem + ".solution")


def find_solutions(paths):
    solutions = {}
    for path in paths:
        if path.is_dir():
            for subpath in path.glob("**/*.input"):
                find_solution(subpath, solutions)
        else:
            find_solution(path, solutions)
    return solutions


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("file", type=Path, nargs="+")
    parser.add_argument("--command", "-c", required=True, action="append")
    parser.add_argument("--diff", "-d", default="diff")
    args = parser.parse_args()

    for inputpath, solutionpath in sorted(find_solutions(args.file).items()):
        try:
            with open(solutionpath) as f:
                solutionstr = f.read()
        except FileNotFoundError:
            print(f"{GRAY}No solution{RESET} for {inputpath}")
            continue

        resultstr = subprocess.run(
            args.command + [inputpath], check=True, capture_output=True, text=True
        ).stdout

        if resultstr.strip() == solutionstr.strip():
            print(f"{GREEN}Passed{RESET} {inputpath}")
        elif resultstr.strip() == "":
            print(f"{MAGENTA}No answer{RESET} for {inputpath}")
        else:
            print(f"{RED}Failed{RESET} {inputpath}")
            subprocess.run([args.diff, solutionpath, "-"], input=resultstr, text=True)


if __name__ == "__main__":
    main()
