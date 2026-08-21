"""bo-audit core: machinery instrumentation for any ask/tell optimizer.

Central object: an AuditedObjective that wraps the true objective. Because
every optimizer must call the objective, revisit counting works uniformly
across libraries without touching their internals: each call decodes the
configuration to its categorical combination (plus rounded continuous
coordinates) and checks it against everything already evaluated.

Space schema (library-agnostic):
    space = [("name", "cat", [choices...]) | ("name", "float", lo, hi)
             | ("name", "int", lo, hi)]
"""
from collections import Counter


class AuditedObjective:
    """Wraps fn(config_dict) -> float; counts decoded-combination revisits.

    cont_decimals: continuous coords are rounded to this many decimals for
    the revisit key (default 6 -- far finer than any optimizer's meaningful
    resolution, so continuous near-misses are NOT counted as revisits; only
    exact categorical-combination repeats with numerically identical
    continuous parts, or pure-categorical repeats, register).
    """

    def __init__(self, fn, space, cont_decimals=6):
        self.fn = fn
        self.space = space
        self.cont_decimals = cont_decimals
        self.calls = []          # (key, value) in call order
        self.seen = Counter()

    def key_of(self, config):
        parts = []
        for spec in self.space:
            name, kind = spec[0], spec[1]
            v = config[name]
            if kind == "cat":
                parts.append(f"{name}={v}")
            elif kind == "int":
                parts.append(f"{name}={int(round(float(v)))}")
            else:
                # + 0.0 normalizes -0.0 to 0.0 so numerically equal rounded
                # values can never produce distinct keys (review H0 finding)
                parts.append(f"{name}={round(float(v), self.cont_decimals) + 0.0}")
        return "|".join(parts)

    def __call__(self, config):
        key = self.key_of(config)
        value = self.fn(config)
        self.calls.append((key, value))
        self.seen[key] += 1
        return value

    # ---- audit outputs ------------------------------------------------------
    @property
    def n_evals(self):
        return len(self.calls)

    @property
    def n_revisits(self):
        """Evaluations whose key had already been evaluated before them."""
        seen = set()
        n = 0
        for key, _ in self.calls:
            if key in seen:
                n += 1
            seen.add(key)
        return n

    @property
    def best_curve(self):
        best, out = float("inf"), []
        for _, v in self.calls:
            best = min(best, v)
            out.append(best)
        return out

    def summary(self):
        return {"evals": self.n_evals, "revisits": self.n_revisits,
                "revisit_frac": self.n_revisits / max(1, self.n_evals),
                "best": min(v for _, v in self.calls) if self.calls else None,
                "unique": len(self.seen)}


def categorical_only(space):
    return all(s[1] == "cat" for s in space)
