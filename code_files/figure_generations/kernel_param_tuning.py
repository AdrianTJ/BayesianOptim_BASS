# /// script
# dependencies = ["numpy", "matplotlib"]
# ///

"""
GP Hyperparameter Sensitivity Visualization
==========================================

This script demonstrates how the length-scale hyperparameter (ell) affects
the posterior mean and uncertainty of a Gaussian Process Regression model.
It illustrates underfitting (ell too large) and overfitting (ell too small).

Author: Adrian TJ
Date: June 2026
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec

# ==============================================================================
# GP Math Utilities
# ==============================================================================

def kernel_rbf(x1, x2, ell=1.0, sigma_f=1.0):
    """Squared Exponential Kernel with length-scale ell and signal variance sigma_f."""
    sq_dist = (x1[:, None] - x2[None, :]) ** 2
    return sigma_f**2 * np.exp(-sq_dist / (2 * ell**2))

def gp_posterior(x_train, y_train, x_test, ell, sigma_f=1.0, sigma_n=0.2):
    """
    Compute GP posterior mean and standard deviation.
    
    Implementation uses Cholesky decomposition for numerical stability.
    """
    # Covariance of training points (with noise nugget)
    K      = kernel_rbf(x_train, x_train, ell, sigma_f) + sigma_n**2 * np.eye(len(x_train))
    # Cross-covariance
    K_s    = kernel_rbf(x_train, x_test,  ell, sigma_f)
    # Covariance of test points
    K_ss   = kernel_rbf(x_test,  x_test,  ell, sigma_f) + 1e-6 * np.eye(len(x_test))
    
    # Solve system using Cholesky
    L      = np.linalg.cholesky(K)
    alpha  = np.linalg.solve(L.T, np.linalg.solve(L, y_train))
    mu     = K_s.T @ alpha
    
    # Predictive variance
    v      = np.linalg.solve(L, K_s)
    cov    = K_ss - v.T @ v
    std    = np.sqrt(np.maximum(np.diag(cov), 0))
    
    return mu, std

# ==============================================================================
# Plot Generation
# ==============================================================================

# Data generation
np.random.seed(7)
x_train = np.array([-4.0, -2.5, -1.0, 0.5, 1.5, 3.0, 4.5])
y_train = np.sin(x_train) + 0.15 * np.random.randn(len(x_train))
x_test  = np.linspace(-6, 6, 400)

# Length-scale variants to test
ell_values = [0.2, 1.0, 3.0, 10.0]
titles = [
    r"$\ell = 0.2$ (Overfitting - high complexity)",
    r"$\ell = 1.0$ (Near Optimal)",
    r"$\ell = 3.0$ (Underfitting - too smooth)",
    r"$\ell = 10.0$ (Underfitting - rigid)",
]

fig = plt.figure(figsize=(14, 9))
fig.suptitle(
    r"Effect of Length-Scale Hyperparameter $\ell$ on GP Regression"
    "\n(Squared Exponential Kernel, $\\sigma_f = 1$, $\\sigma_n = 0.2$)",
    fontsize=13, fontweight="bold", y=1.01
)

gs = GridSpec(2, 2, figure=fig, hspace=0.45, wspace=0.3)
axes = [fig.add_subplot(gs[i // 2, i % 2]) for i in range(4)]

for ax, ell, title in zip(axes, ell_values, titles):
    mu, std = gp_posterior(x_train, y_train, x_test, ell=ell)

    # Plot 95% confidence interval
    ax.fill_between(x_test, mu - 2*std, mu + 2*std,
                    alpha=0.25, color="steelblue", label=r"$\pm 2\sigma$")
    ax.plot(x_test, mu, color="steelblue", linewidth=2.0, label="Posterior mean")
    ax.scatter(x_train, y_train, color="black", zorder=5,
               s=40, label="Observations")

    ax.set_title(title, fontsize=11)
    ax.set_xlabel("$x$", fontsize=10)
    ax.set_ylabel("$f(x)$", fontsize=10)
    ax.set_xlim(-6, 6)
    ax.set_ylim(-3, 3)
    ax.legend(fontsize=8, loc="upper right")
    ax.tick_params(labelsize=8)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

plt.savefig("gp_regression_ell.pdf", bbox_inches="tight", dpi=300)
plt.savefig("gp_regression_ell.png", bbox_inches="tight", dpi=300)
plt.show()
print("Saved: gp_regression_ell.pdf and gp_regression_ell.png")