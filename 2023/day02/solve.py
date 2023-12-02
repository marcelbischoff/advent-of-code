from collections import defaultdict
from math import prod
from typing import Callable, Dict, Iterable, Tuple


def consume(it: Iterable[Tuple[int, str]]) -> int:
    st = defaultdict(int)
    for v, k in it: st[k] = max(st[k], v)
    return st


def solve(file: str, fn: Callable[[Dict[str, int]], int]) -> int:
    with open(file) as f:
        lines = f.readlines()
    return sum(fn(consume(
            (int((s := y.split(" "))[0]), s[1])
            for x in line.rstrip().split(": ")[1].split("; ")
            for y in x.split(", ")
        ), idx)
        for idx, line in enumerate(lines))


print(solve("input.txt", lambda x, i: (i + 1)
        * (x["red"] <= 12 and x["green"] <= 13 and x["blue"] <= 14)))
print(solve("input.txt", lambda x, _: prod(x.values())))
