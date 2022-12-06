import re

# PART 1

STEP_RE = r"Step (\S+) must be finished before step (\S+) can begin."


def load_steps(inputstr):
    steps = {}
    for line in inputstr.splitlines():
        match = re.fullmatch(STEP_RE, line)
        step, before = match.groups()
        steps[step] = steps.get(step, set()) | {before}

    return steps


def reverse_steps(steps):
    reverse = {}
    for step, befores in steps.items():
        # Make sure that step exists in reverse
        reverse[step] = reverse.get(step, set())

        for before in befores:
            reverse[before] = reverse.get(before, set()) | {step}

    return reverse


def duration_of(step):
    return ord(step) - ord("A") + 61


class Tree:
    def __init__(self, steps, workers=1):
        self.workers = {i: None for i in range(workers)}
        self.steps = reverse_steps(steps)  # Warning: Steps are reversed in Trees.
        self.result = []
        self.duration = 0

    def working(self):
        return {
            worker: work for worker, work in self.workers.items() if work is not None
        }

    def find_free(self):
        return {step for step, afters in self.steps.items() if len(afters) == 0}

    def find_working(self):
        return {step for (step, _) in self.working().values()}

    def find_available(self):
        return self.find_free() - self.find_working()

    def remove_step(self, step):
        try:
            del self.steps[step]
        except KeyError:
            pass

        for s in self.steps.values():
            try:
                s.remove(step)
            except KeyError:
                pass

    def update_workers(self):
        min_duration = min(duration for (_, duration) in self.working().values())
        self.duration += min_duration

        finished_steps = set()

        # Subtract min_duration from all workers
        for w, s in self.workers.items():
            if s is not None:
                step, duration = s
                duration -= min_duration

                if duration <= 0:
                    finished_steps.add(step)
                    self.remove_step(step)
                    self.workers[w] = None
                else:
                    self.workers[w] = (step, duration)

        self.result += list(finished_steps)

    def distribute_jobs(self):
        available = list(reversed(sorted(self.find_available())))

        for w, s in self.workers.items():
            if not available:
                break

            if s is None:
                step = available.pop()
                duration = duration_of(step)
                self.workers[w] = (step, duration)

    def run(self):
        while self.steps:
            self.distribute_jobs()
            self.update_workers()


def solve(inputstr):
    steps = load_steps(inputstr)
    tree = Tree(steps, workers=1)
    tree.run()
    sequence = "".join(tree.result)
    print(f"Part 1: {sequence}")
    tree = Tree(steps, workers=5)
    tree.run()
    duration = tree.duration
    print(f"Part 2: {duration}")
