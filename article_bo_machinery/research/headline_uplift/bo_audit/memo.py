"""Memoized audit wrapper for H2's budget-equalized control.

Serves cached values for already-seen combination keys at zero
objective-budget cost; charges only unique evaluations. With a
deterministic objective the optimizer's trajectory is bit-identical to
running uncached — memoization changes what the budget is charged for,
not what the sampler sees (H2 DESIGN.md).
"""
from bo_audit.core import AuditedObjective


class AuditStop(Exception):
    """Raised on the first proposal after the unique-eval budget is spent."""


class MemoizedAuditedObjective(AuditedObjective):
    def __init__(self, fn, space, unique_budget, cont_decimals=6):
        super().__init__(fn, space, cont_decimals)
        self.unique_budget = unique_budget
        self.cache = {}

    def __call__(self, config):
        key = self.key_of(config)
        if key in self.cache:
            value = self.cache[key]          # free: not charged, still audited
        else:
            if len(self.cache) >= self.unique_budget:
                raise AuditStop
            value = self.fn(config)
            self.cache[key] = value
        self.calls.append((key, value))
        self.seen[key] += 1
        return value

    def summary(self):
        out = super().summary()
        out.update({"unique_evals_charged": len(self.cache),
                    "proposals": self.n_evals})
        return out
