import re

# PART 1

ACTION_RE = r"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.*)"
GUARD_RE = r"Guard #(\d+) begins shift"


def load_actions(inputstr):
    actions = []
    for line in inputstr.splitlines():
        match = re.fullmatch(ACTION_RE, line)
        y, m, d, hour, minute, action = match.groups()
        y, m, d, hour, minute = int(y), int(m), int(d), int(hour), int(minute)
        actions.append((y, m, d, hour, minute, action))
    return actions


def calculate_guards(actions):
    guards = {}

    guard = None
    asleep_since = None

    for action in sorted(actions):
        _, _, _, _, m, text = action

        match = re.fullmatch(GUARD_RE, text)
        if match:
            guard = int(match.group(1))

        elif text == "falls asleep":
            asleep_since = m

        elif text == "wakes up":
            l = guards.get(guard, [])
            guards[guard] = l

            l.append((asleep_since, m))

    return guards


def sleeps_longest(guards):
    sleepiest_guard = None
    sleepy_time = -1

    for guard, sleep_times in guards.items():
        total = 0
        for start, end in sleep_times:
            total += end - start

        if total > sleepy_time:
            sleepiest_guard = guard
            sleepy_time = total

    return sleepiest_guard


def sleepiest_minute(times):
    counter = {}
    for start, end in times:
        for m in range(start, end):
            counter[m] = counter.get(m, 0) + 1

    max_minute = None
    amount = -1
    for m, n in sorted(counter.items()):
        if n > amount:
            max_minute = m
            amount = n

    return max_minute


# PART 2


def sleep_times(times):
    minutes = {}
    for start, end in times:
        for m in range(start, end):
            minutes[m] = minutes.get(m, 0) + 1
    return minutes


def sleepy_minutes(guards):
    minutes = {}
    for guard, times in guards.items():
        for m, n in sleep_times(times).items():
            md = minutes.get(m, {})
            minutes[m] = md
            md[guard] = n

    sleepy_minute = m
    sleepy_guard = None
    sleep_time = -1

    for m, md in minutes.items():
        for guard, n in md.items():
            if n > sleep_time:
                sleepy_minute = m
                sleepy_guard = guard
                sleep_time = n

    return sleepy_guard, sleepy_minute


def solve(inputstr):
    actions = load_actions(inputstr)
    guards = calculate_guards(actions)
    guard = sleeps_longest(guards)
    minute = sleepiest_minute(guards[guard])
    print(f"Part 1: {guard * minute}")
    guard_2, minute_2 = sleepy_minutes(guards)
    print(f"Part 2: {guard_2 * minute_2}")
