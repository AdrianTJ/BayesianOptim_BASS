# /// script
# dependencies = ["numpy", "matplotlib"]
# ///

import numpy as np
import matplotlib.pyplot as plt

n_simulations = 100
n_steps = 100

t = np.arange(n_steps + 1)
steps = np.random.normal(0, 1, size=(n_simulations, n_steps))
paths = np.concatenate([np.zeros((n_simulations, 1)), np.cumsum(steps, axis=1)], axis=1)

plt.figure(figsize=(10, 6))
for path in paths:
    plt.plot(t, path, linewidth=0.7, alpha=0.8)

plt.axhline(0, color='black', linewidth=1.2)
plt.xlabel('t', fontsize=13)
plt.ylabel('X_t', fontsize=13)
plt.title('100 Simulations of a Gaussian Random Walk', fontsize=14)
plt.tight_layout()
plt.show()