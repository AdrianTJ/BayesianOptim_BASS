# /// script
# dependencies = ["numpy", "matplotlib"]
# ///
"""
Regenerates the hinge-collection figure for the MARS section
(written_files/tesis_escrito/Figures/hinge_collection.png).

Replaces the old 3hinge.png. The advisor's markup asked for the knot labels
to sit at the knots themselves rather than floating over the curve arms:
each pair is now annotated at its vertex on the axis.
"""

import numpy as np
import matplotlib.pyplot as plt

knots = [(-3.0, "#4a52e0", r"$x_{1,j}$"),
         (-2.0, "#3d3d9e", r"$x_{2,j}$"),
         (3.0,  "#4d4d4d", r"$x_{N,j}$")]
half = 1.0  # horizontal half-width of each V at height 2

fig, ax = plt.subplots(figsize=(9.6, 4.6))
for k, color, label in knots:
    xs = np.linspace(k - half, k + half, 200)
    ax.plot(xs, 2 * np.abs(xs - k) / half * 1.0, color=color, linewidth=2.6)
    ax.plot([k], [0], marker="o", markersize=6, color=color, zorder=5)
    ax.annotate(label, xy=(k, 0), xytext=(k, -0.42), fontsize=14,
                ha="center", color=color,
                arrowprops=dict(arrowstyle="-", linewidth=0.7, color=color))

ax.text(0.5, 1.0, r"$\cdots$", fontsize=26, ha="center", va="center")

ax.set_xlabel(r"$X_j$", fontsize=14)
ax.set_ylabel("y", fontsize=13)
ax.set_xlim(-5.2, 5.2)
ax.set_ylim(-0.62, 2.1)
ax.spines[["top", "right"]].set_visible(False)

plt.tight_layout()
plt.savefig("../../written_files/tesis_escrito/Figures/hinge_collection.png",
            dpi=300, bbox_inches="tight")
print("wrote Figures/hinge_collection.png")
