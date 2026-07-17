# /// script
# dependencies = ["matplotlib"]
# ///
"""
Generates the decision-tree illustration for the BASS chapter
(written_files/tesis_escrito/Figures/decision_tree.png).

The tree mirrors the chapter's regression example: predicting house price
from floor area and neighbourhood, two splits deep, with leaf predictions
equal to the mean sale price of the training homes in each leaf.
"""

import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

NODE_STYLE = dict(boxstyle="round,pad=0.42", linewidth=1.4,
                  facecolor="#f2f2f2", edgecolor="black")
LEAF_STYLE = dict(boxstyle="round,pad=0.42", linewidth=1.4,
                  facecolor="#ffd54d", edgecolor="black")

# (x, y, text, is_leaf)
nodes = {
    "root":  (0.50, 0.86, "floor area $>$ 120 m$^2$?", False),
    "left":  (0.26, 0.52, "neighbourhood\nin {A, B}?", False),
    "right": (0.74, 0.52, "$\\hat{y} = 415$", True),
    "ll":    (0.12, 0.16, "$\\hat{y} = 320$", True),
    "lr":    (0.40, 0.16, "$\\hat{y} = 205$", True),
}

edges = [
    ("root", "left",  "no"),
    ("root", "right", "yes"),
    ("left", "ll",    "yes"),
    ("left", "lr",    "no"),
]

fig, ax = plt.subplots(figsize=(7.2, 5.0))
ax.set_xlim(0, 1)
ax.set_ylim(0, 1)
ax.axis("off")

for a, b, lab in edges:
    xa, ya = nodes[a][0], nodes[a][1]
    xb, yb = nodes[b][0], nodes[b][1]
    ax.plot([xa, xb], [ya - 0.05, yb + 0.06], color="black", linewidth=1.3, zorder=1)
    ax.text((xa + xb) / 2 + 0.025, (ya + yb) / 2 + 0.01, lab,
            fontsize=11, style="italic", ha="left", va="center")

for key, (x, y, text, is_leaf) in nodes.items():
    ax.text(x, y, text, ha="center", va="center", fontsize=12, zorder=3,
            bbox=LEAF_STYLE if is_leaf else NODE_STYLE)

ax.text(0.5, 0.005,
        "Leaf values are the mean sale price (thousands) of the training homes in the leaf.",
        ha="center", va="bottom", fontsize=9.5, color="#444444")

plt.tight_layout()
plt.savefig("../../written_files/tesis_escrito/Figures/decision_tree.png",
            dpi=300, bbox_inches="tight")
print("wrote ../../written_files/tesis_escrito/Figures/decision_tree.png")
