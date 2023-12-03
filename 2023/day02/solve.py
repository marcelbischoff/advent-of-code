from collections import defaultdict
from math import prod
from typing import Callable, Dict, Iterable, Tuple


def find_max_by_color(it: Iterable[Tuple[int, str]]) -> Dict[str, int]:
    state: Dict[str, int] = defaultdict(int)
    for v, k in it:
        state[k] = max(state[k], v)
    return state


def solve(file: str, fn: Callable[[Dict[str, int], int], int]) -> int:
    with open(file) as f:
        return sum(
            fn(
                find_max_by_color(
                    (int((s := y.split(" "))[0]), s[1])
                    for x in line.rstrip().split(": ")[1].split("; ")
                    for y in x.split(", ")
                ),
                idx,
            )
            for idx, line in enumerate(f.readlines())
        )


print(
    solve(
        "input.txt",
        lambda x, i: (i + 1) * (x["red"] < 13 and x["green"] < 14 and x["blue"] < 15),
    )
)
print(solve("input.txt", lambda x, _: prod(x.values())))
