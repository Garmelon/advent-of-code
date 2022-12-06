import collections
import re

# PART 1


class Instruction:
    INSTRUCTION_RE = r"(\S+) (inc|dec) (-?\d+) if (\S+) (<|<=|>|>=|==|!=) (-?\d+)"

    def __init__(self, register, mode, amount, c_register, c_operator, c_amount):
        self.register = register
        self.mode = mode
        self.amount = amount
        self.c_register = c_register
        self.c_operator = c_operator
        self.c_amount = c_amount

    @classmethod
    def from_line(cls, line):
        match = re.fullmatch(cls.INSTRUCTION_RE, line)

        register = match.group(1)
        mode = match.group(2)
        amount = int(match.group(3))
        c_register = match.group(4)
        c_operator = match.group(5)
        c_amount = int(match.group(6))

        return cls(register, mode, amount, c_register, c_operator, c_amount)

    def compare_to(self, n):
        if self.c_operator == "<":
            return n < self.c_amount
        elif self.c_operator == "<=":
            return n <= self.c_amount
        elif self.c_operator == ">":
            return n > self.c_amount
        elif self.c_operator == ">=":
            return n >= self.c_amount
        elif self.c_operator == "==":
            return n == self.c_amount
        elif self.c_operator == "!=":
            return n != self.c_amount

    def total(self):
        if self.mode == "inc":
            return self.amount
        elif self.mode == "dec":
            return -self.amount


class Machine:
    def __init__(self, instructions):
        self.instructions = instructions
        self.reset()

    def reset(self):
        self.registers = collections.defaultdict(lambda: 0)
        self.max_all_time = 0

    @classmethod
    def from_str(cls, s):
        instructions = []

        for line in s.splitlines():
            instructions.append(Instruction.from_line(line))

        return cls(instructions)

    def execute_instruction(self, i):
        c_amount = self.registers.get(i.c_register, 0)
        if i.compare_to(c_amount):
            self.registers[i.register] += i.total()
            self.max_all_time = max(self.max_all_time, self.registers[i.register])

    def execute(self):
        for i in self.instructions:
            self.execute_instruction(i)

    def max_register(self):
        return max(self.registers.values())


# PART 2


def solve(inputstr):
    machine = Machine.from_str(inputstr)
    machine.reset()
    machine.execute()
    max_register = machine.max_register()
    print(f"Part 1: {max_register}")
    print(f"Part 2: {machine.max_all_time}")
