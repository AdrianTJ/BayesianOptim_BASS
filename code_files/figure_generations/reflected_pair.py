# /// script
# dependencies = ["numpy", "matplotlib"]
# ///
"""
Regenerates the reflected-pair figure for the MARS section
(written_files/tesis_escrito/Figures/reflected_pair.png).

Replaces the old 3ref_pair.png, which had the x-axis label cut off and no
marking of the knot itself (both flagged in the advisor's markup). One knot
t = x_ij, both hinge functions labelled, the knot annotated on the axis.
"""

import numpy as np
import matplotlib.pyplot as plt

t = -3.0
x = np.linspace(-5, -1, 400)
h1 = np.maximum(0, x - t)      # (X_j - x_ij)_+ : opens to the right
h2 = np.maximum(0, t - x)      # (x_ij - X_j)_+ : opens to the left

fig, ax = plt.subplots(figsize=(8.6, 5.2))
DARK, TEAL = "#3d3d9e", "#4fa8a8"

ax.plot(x[x >= t], h1[x >= t], color=DARK, linewidth=3.2)
ax.plot(x[x <= t], np.zeros_like(x[x <= t]), color=DARK, linewidth=3.2,
        linestyle=(0, (5, 4)))
ax.plot(x[x <= t], h2[x <= t], color=TEAL, linewidth=3.2)
ax.plot(x[x >= t], np.zeros_like(x[x >= t]), color=TEAL, linewidth=3.2,
        linestyle=(0, (5, 4)))

ax.text(-2.05, 1.72, r"$h_1 = (X_j - x_{ij})_+$", fontsize=15, color=DARK,
        ha="center")
ax.text(-4.05, 1.72, r"$h_2 = (x_{ij} - X_j)_+$", fontsize=15, color=TEAL,
        ha="center")

# the knot, marked explicitly on the axis
ax.plot([t], [0], marker="o", markersize=7, color="black", zorder=5)
ax.annotate(r"$t = x_{ij}$", xy=(t, 0), xytext=(t, -0.28),
            fontsize=13, ha="center",
            arrowprops=dict(arrowstyle="-", linewidth=0.8))

ax.set_xlabel(r"$X_j$", fontsize=14)
ax.set_ylabel("y", fontsize=13)
ax.set_xlim(-5.15, -0.85)
ax.set_ylim(-0.42, 2.05)
ax.spines[["top", "right"]].set_visible(False)

plt.tight_layout()
plt.savefig("../../written_files/tesis_escrito/Figures/reflected_pair.png",
            dpi=300, bbox_inches="tight")
print("wrote Figures/reflected_pair.png")
