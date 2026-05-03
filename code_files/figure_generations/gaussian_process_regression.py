# /// script
# dependencies = ["numpy", "matplotlib"]
# ///


import numpy as np
import matplotlib.pyplot as plt

def kernel_rbf(x1, x2, ell=1.0, sigma_f=1.0):
    sq_dist = (x1[:, None] - x2[None, :]) ** 2
    return sigma_f**2 * np.exp(-sq_dist / (2 * ell**2))

def gp_exact_samples(x_train, y_train, x_test, ell, sigma_f=1.0, n_samples=1, noise=1e-8):
    K      = kernel_rbf(x_train, x_train, ell, sigma_f) + noise * np.eye(len(x_train))
    K_s    = kernel_rbf(x_train, x_test,  ell, sigma_f)
    K_ss   = kernel_rbf(x_test,  x_test,  ell, sigma_f) + noise * np.eye(len(x_test))
    L      = np.linalg.cholesky(K)
    alpha  = np.linalg.solve(L.T, np.linalg.solve(L, y_train))
    mu     = K_s.T @ alpha
    v      = np.linalg.solve(L, K_s)
    cov    = K_ss - v.T @ v
    cov   += noise * np.eye(len(x_test))
    L2     = np.linalg.cholesky(cov)
    samples = mu[:, None] + L2 @ np.random.randn(len(x_test), n_samples)
    return mu, samples

# --- Setup ---
np.random.seed(3)
x_train = np.array([-3.0, -1.0, 0.5, 2.0, 3.5])
y_train = np.array([ 0.5,  1.2, -0.3, 0.9, -0.5])
x_test  = np.linspace(-5.5, 5.5, 500)

ell_values = [0.3, 0.4, 0.75, 0.6, 1.0, 1.8, 3.5, 6.0, 0.45, 0.9, 2.5, 4.5]
colors     = plt.cm.viridis(np.linspace(0.05, 0.92, len(ell_values)))

fig, ax = plt.subplots(figsize=(8, 5))
ax.set_title("GP Samples Conditioned on Observations\n(varying length-scale $\\ell$)",
             fontsize=12, fontweight="bold")

np.random.seed(12)
for ell, color in zip(ell_values, colors):
    _, samples = gp_exact_samples(x_train, y_train, x_test,
                                  ell=ell, sigma_f=1.2, n_samples=1)
    ax.plot(x_test, samples[:, 0], color=color, linewidth=1.4,
            alpha=0.5, label=f"$\\ell = {ell}$")

ax.scatter(x_train, y_train, color="black", zorder=6, s=55, label="Observations")
ax.set_xlim(-5.5, 5.5)
ax.set_ylim(-4, 4)
ax.set_xlabel("$x$", fontsize=10)
ax.set_ylabel("$f(x)$", fontsize=10)
ax.legend(fontsize=7.5, loc="upper right", ncol=2)
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
ax.tick_params(labelsize=8)

plt.tight_layout()
plt.savefig("function_space.pdf", bbox_inches="tight", dpi=300)
plt.savefig("function_space.png", bbox_inches="tight", dpi=300)
plt.show()
print("Saved: function_space.pdf and function_space.png")