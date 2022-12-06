import re

# PART 1


def any_key(d):
    return list(d)[0]


class Tower:
    PROG_RE = r"(\S+) \((\d+)\)( -> (.*))?"

    def __init__(self, weight_of, children_of, parent_of):
        self.wo = weight_of
        self.co = children_of
        self.po = parent_of

    @classmethod
    def from_str(cls, s):
        weight_of = {}  # name -> weight
        children_of = {}  # name -> children
        parent_of = {}  # name -> parent

        for line in s.splitlines():
            match = re.fullmatch(cls.PROG_RE, line)

            name = match.group(1)
            weight = int(match.group(2))
            if match.group(4):
                children = match.group(4).split(", ")
            else:
                children = []

            weight_of[name] = weight
            children_of[name] = children
            for child in children:
                parent_of[child] = name

        return cls(weight_of, children_of, parent_of)

    def find_root(self):
        program = any_key(self.po)
        while program in self.po:
            program = self.po[program]
        return program

    # PART 2

    # This part is implemented really shitty. It makes a lot of assumptions and
    # will probably break the second the input changes or you're just unlucky.
    # For my particular input, it worked though.
    #
    # The basic idea of the algorithm is:
    #
    # 1. Find the plate where one branch has a different weight from all the others
    # 2. Find out which branch weight is wrong and whic weights are correct
    # 3. Fix the branch's root program's weight

    def weight(self, name):
        return self.wo[name] + sum(self.weight(c) for c in self.co[name])

    def balanced(self, name):
        cs = self.co[name]
        ws = [self.weight(c) for c in cs]
        return min(ws) == max(ws)

    def unbalanced_child(self, name):
        for c in self.co[name]:
            if not self.balanced(c):
                return c

    def find_imbalance(self, name):
        c = self.unbalanced_child(name)
        if c is None:
            weights = [(c, self.weight(c)) for c in self.co[name]]
            return weights
        else:
            return self.find_imbalance(c)

    def fix_imbalance(self, weights):
        # Which weight do we need to correct?
        ws = [weight for (c, weight) in weights]
        if ws.count(max(ws)) < ws.count(min(ws)):
            weight = max(ws)
            other = min(ws)
        else:
            weight = min(ws)
            other = max(ws)

        # Wich program has that weight?
        prog = None
        for (p, w) in weights:
            if w == weight:
                prog = p
                break

        # w_prog_soll - w_prog = w_soll - w_branch
        # w_prog_soll = w_soll - w_branch + w_prog
        # w_prog_soll = w_soll - (w_branch - w_prog)
        w_prog = self.wo[prog]
        w_soll = other
        w_branch = self.weight(prog)
        return w_soll - (w_branch - w_prog)


def solve(inputstr):
    tower = Tower.from_str(inputstr)
    root = tower.find_root()
    print(f"Part 1: {root}")
    weights = tower.find_imbalance(root)
    fixed = tower.fix_imbalance(weights)
    print(f"Part 2: {fixed}")
