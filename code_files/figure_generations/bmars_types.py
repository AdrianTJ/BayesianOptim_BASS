# /// script
# dependencies = ["numpy", "matplotlib"]
# ///
"""
Generates the BMARS basis-types illustration
(written_files/tesis_escrito/Figures/bmars_types.png).

Three panels, one per type in the running two-predictor example:
type 1 splits on x1 only, type 2 on x2 only, type 3 is their interaction.
Surfaces are the example's own terms, so the text and figure agree.
"""

import numpy as np
import matplotlib.pyplot as plt

x1 = np.linspace(0, 1, 120)
x2 = np.linspace(0, 1, 120)
X1, X2 = np.meshgrid(x1, x2)

panels = [
    (np.maximum(0, X1 - 0.3), r"Type 1: $(x_1 - 0.3)_+$"),
    (np.maximum(0, 0.7 - X2), r"Type 2: $(0.7 - x_2)_+$"),
    (np.maximum(0, X1 - 0.3) * np.maximum(0, 0.7 - X2),
     r"Type 3: $(x_1 - 0.3)_+ \, (0.7 - x_2)_+$"),
]

fig = plt.figure(figsize=(12.5, 4.2))
for i, (Z, title) in enumerate(panels, 1):
    ax = fig.add_subplot(1, 3, i, projection="3d")
    ax.plot_surface(X1, X2, Z, cmap="viridis", linewidth=0, antialiased=True)
    ax.set_title(title, fontsize=12, pad=10)
    ax.set_xlabel(r"$x_1$", fontsize=11)
    ax.set_ylabel(r"$x_2$", fontsize=11)
    ax.tick_params(labelsize=8)
    ax.view_init(elev=24, azim=-135)

plt.tight_layout()
plt.savefig("../../written_files/tesis_escrito/Figures/bmars_types.png",
            dpi=300, bbox_inches="tight")
print("wrote Figures/bmars_types.png")
