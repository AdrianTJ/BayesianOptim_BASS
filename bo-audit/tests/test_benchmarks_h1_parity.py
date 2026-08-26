"""Divergence guard for the vendored H1 benchmark fallback.

bo_audit.benchmarks.bench_by_name prefers the original `machinery`
module (article_bo_machinery/research/article_loop/experiments) for
cat_ackley_*, func2C and func3C, and falls back to the vendored copy
in bo_audit.benchmarks_h1 when `machinery` is not importable (e.g. a
plain `pip install` of the released package, with the research tree
not on the path). Vendoring means two copies of the same code exist,
and two copies can silently drift. This test defends the contract
that they never do: it evaluates the objective builders from both
modules on a fixed, deterministic grid of inputs and asserts the
outputs are *exactly* equal (these are deterministic functions -- any
difference at all, however small, is a genuine divergence, not noise
to tolerate with an approximate comparison).

Skips (rather than fails) when `machinery` is not importable, so the
suite still passes for an installed user without the research tree.
"""
import itertools
import os
import sys
import unittest

import numpy as np

from bo_audit import benchmarks_h1 as h1

MACHINERY_DIR = os.path.join(
    os.path.dirname(__file__), "..", "..",
    "article_bo_machinery", "research", "article_loop", "experiments",
)


def _load_machinery():
    """Import the original machinery module, or raise ImportError."""
    sys.path.insert(0, os.path.abspath(MACHINERY_DIR))
    try:
        import machinery  # noqa: the module under test
    finally:
        # Leave the path entry in place only if the import actually
        # succeeded; on failure, don't pollute sys.path for other tests.
        if os.path.abspath(MACHINERY_DIR) in sys.path and "machinery" not in sys.modules:
            sys.path.remove(os.path.abspath(MACHINERY_DIR))
    return machinery


def _grid(d, points=(0.02, 0.18, 0.5, 0.82, 0.98)):
    """Deterministic cartesian grid over the d-dimensional unit cube."""
    return np.array(list(itertools.product(points, repeat=d)), dtype=float)


class TestBenchmarksH1Parity(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        try:
            cls.machinery = _load_machinery()
        except ImportError as exc:
            # Report the real cause. A skip here means the divergence
            # guard did NOT run, and the two most common causes look
            # nothing alike: the research tree may be absent (expected
            # for an installed user), or it may be present while one of
            # machinery's own dependencies -- scipy, notably -- is
            # missing (a developer environment problem worth fixing).
            # Attributing both to "tree not on path" would let a silent
            # non-run pass for an expected one.
            present = os.path.isdir(os.path.abspath(MACHINERY_DIR))
            raise unittest.SkipTest(
                "machinery module not importable, so the vendored copy "
                "was NOT compared against its source. Research tree at "
                f"{MACHINERY_DIR} {'present' if present else 'absent'}; "
                f"import failed with: {exc!r}")

    def _assert_exact_match(self, name_a, obj_a, obj_b, X):
        y_a = obj_a["fn"](X)
        y_b = obj_b["fn"](X)
        np.testing.assert_array_equal(
            y_a, y_b,
            err_msg=f"{name_a}: vendored bo_audit.benchmarks_h1 output "
                    f"diverges from machinery.py on the fixed test grid")
        self.assertEqual(obj_a["d"], obj_b["d"])
        self.assertEqual(obj_a["optimum"], obj_b["optimum"])
        self.assertEqual(obj_a["schema"].types, obj_b["schema"].types)
        self.assertEqual(obj_a["schema"].levels, obj_b["schema"].levels)

    def test_func2C_matches(self):
        obj_a = self.machinery.OBJECTIVES["func2C"]()
        obj_b = h1.OBJECTIVES["func2C"]()
        self._assert_exact_match("func2C", obj_a, obj_b, _grid(4))

    def test_func3C_matches(self):
        obj_a = self.machinery.OBJECTIVES["func3C"]()
        obj_b = h1.OBJECTIVES["func3C"]()
        self._assert_exact_match("func3C", obj_a, obj_b, _grid(5))

    def test_cat_ackley_d3_L5_matches(self):
        obj_a = self.machinery.make_cat_ackley(3, 5)
        obj_b = h1.make_cat_ackley(3, 5)
        self._assert_exact_match("cat_ackley_d3_L5", obj_a, obj_b, _grid(3))

    def test_cat_ackley_d5_L5_matches(self):
        obj_a = self.machinery.make_cat_ackley(5, 5)
        obj_b = h1.make_cat_ackley(5, 5)
        self._assert_exact_match("cat_ackley_d5_L5", obj_a, obj_b, _grid(5))

    def test_cat_ackley_d6_L11_matches(self):
        obj_a = self.machinery.make_cat_ackley(6, 11)
        obj_b = h1.make_cat_ackley(6, 11)
        self._assert_exact_match("cat_ackley_d6_L11", obj_a, obj_b, _grid(6))

    def test_decode_levels_matches_on_dense_1d_grid(self):
        """Boundary-heavy check of the shared cat-encoding helper alone."""
        u = np.linspace(0.0, 1.0, 2001)
        for L in (3, 4, 5, 11):
            a = self.machinery.decode_levels(u, L)
            b = h1.decode_levels(u, L)
            np.testing.assert_array_equal(a, b)


if __name__ == "__main__":
    unittest.main()
