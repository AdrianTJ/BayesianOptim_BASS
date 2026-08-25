"""Behavioral tests for the bo-audit instrument.

Each test defends an observable contract the paper's audit relies on:
exact revisit counting on scripted ground truth, rounding/canonicalization
key semantics, the -0.0 float-key fix found in review, and the memoized
wrapper's charge-only-unique budget with trajectory preservation.
"""
import unittest

from bo_audit import AuditStop, AuditedObjective, MemoizedAuditedObjective


def const_fn(value=0.0):
    return lambda config: value


CAT_SPACE = [("k", "cat", ["a", "b", "c"])]
FLOAT_SPACE = [("x", "float", 0.0, 1.0)]
MIXED_SPACE = [("k", "cat", ["a", "b"]), ("x", "float", 0.0, 1.0),
               ("n", "int", 0, 10)]


class TestRevisitCounting(unittest.TestCase):
    def test_scripted_ground_truth(self):
        """Calls on keys A,B,A,C,A must count exactly 2 revisits."""
        audited = AuditedObjective(const_fn(), CAT_SPACE)
        for k in ["a", "b", "a", "c", "a"]:
            audited({"k": k})
        self.assertEqual(audited.n_evals, 5)
        self.assertEqual(audited.n_revisits, 2)
        self.assertEqual(audited.summary()["unique"], 3)

    def test_zero_revisits_on_distinct_configs(self):
        audited = AuditedObjective(const_fn(), CAT_SPACE)
        for k in ["a", "b", "c"]:
            audited({"k": k})
        self.assertEqual(audited.n_revisits, 0)

    def test_continuous_near_miss_not_a_revisit(self):
        """Rounded keys: 1e-9 offsets collapse to one key, 0.01 does not."""
        audited = AuditedObjective(const_fn(), FLOAT_SPACE)
        audited({"x": 0.5000000001})
        audited({"x": 0.5 - 1e-9})
        self.assertEqual(audited.n_revisits, 1)  # second call is a repeat
        audited({"x": 0.51})
        self.assertEqual(audited.n_revisits, 1)  # near, not exact: no hit

    def test_negative_zero_shares_key_with_zero(self):
        """Regression: -0.0 must not mint a distinct key (review H0 fix)."""
        audited = AuditedObjective(const_fn(), FLOAT_SPACE)
        audited({"x": 0.0})
        audited({"x": -0.0})
        self.assertEqual(audited.n_revisits, 1)

    def test_int_rounding_collapses_float_draws(self):
        audited = AuditedObjective(const_fn(), [("n", "int", 0, 10)])
        audited({"n": 3})
        audited({"n": 3.0})
        audited({"n": 2.6})  # rounds to 3
        audited({"n": 4})
        self.assertEqual(audited.n_revisits, 2)
        self.assertEqual(audited.summary()["unique"], 2)

    def test_canonicalize_makes_inactive_coords_invisible(self):
        """Configs differing only in an inactive coordinate share a key."""
        def canon(cfg):
            cfg = dict(cfg)
            if cfg["mode"] == "off":
                cfg.pop("aux", None)
            return cfg

        space = [("mode", "cat", ["off", "on"]), ("aux", "cat", ["u", "v"])]
        audited = AuditedObjective(const_fn(), space, canonicalize=canon)
        audited({"mode": "off", "aux": "u"})
        audited({"mode": "off", "aux": "v"})
        self.assertEqual(audited.n_revisits, 1)
        self.assertIn("aux=∅", audited.key_of({"mode": "off"}))

    def test_best_curve_is_running_min(self):
        values = [1.0, 0.5, 0.7, 0.2]
        audited = AuditedObjective(const_fn(), CAT_SPACE)
        for i, k in enumerate(["a", "b", "a", "c"]):
            audited.fn = lambda cfg, v=values[i]: v
            audited({"k": k})
        self.assertEqual(audited.best_curve, [1.0, 0.5, 0.5, 0.2])
        self.assertEqual(audited.summary()["best"], 0.2)


class TestMemoizedWrapper(unittest.TestCase):
    def test_cache_hits_are_free_until_unique_budget_spent(self):
        seen = []

        def fn(cfg):
            seen.append(cfg["k"])
            return 0.0

        audited = MemoizedAuditedObjective(fn, CAT_SPACE, unique_budget=3)
        for k in ["a", "b", "a", "c", "a", "b"]:
            audited({"k": k})
        self.assertEqual(len(seen), 3)          # only unique evals hit fn
        self.assertEqual(audited.summary()["unique_evals_charged"], 3)
        self.assertEqual(audited.summary()["proposals"], 6)

    def test_audit_stop_after_unique_budget(self):
        audited = MemoizedAuditedObjective(const_fn(), CAT_SPACE,
                                           unique_budget=2)
        audited({"k": "a"})
        audited({"k": "b"})
        with self.assertRaises(AuditStop):
            audited({"k": "c"})

    def test_trajectory_values_match_uncached(self):
        """Same proposal sequence, deterministic fn: identical values."""
        seq = [{"k": k} for k in ["a", "b", "a", "c", "b", "a"]]
        plain = AuditedObjective(const_fn(), CAT_SPACE)
        memo = MemoizedAuditedObjective(const_fn(), CAT_SPACE,
                                        unique_budget=3)
        for cfg in seq:
            plain(cfg)
        for cfg in seq:
            memo(cfg)
        self.assertEqual([v for _, v in plain.calls],
                         [v for _, v in memo.calls])
        self.assertEqual(memo.n_revisits, 3)


if __name__ == "__main__":
    unittest.main()
