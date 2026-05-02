import numpy as np
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec

# --- Kernel definitions ---
def kernel_rbf(x1, x2, length_scale=1.0, variance=1.0):
    sq_dist = (x1[:, None] - x2[None, :]) ** 2
    return variance * np.exp(-0.5 * sq_dist / length_scale**2)

def kernel_matern12(x1, x2, length_scale=1.0, variance=1.0):
    dist = np.abs(x1[:, None] - x2[None, :])
    return variance * np.exp(-dist / length_scale)

def kernel_matern32(x1, x2, length_scale=1.0, variance=1.0):
    dist = np.abs(x1[:, None] - x2[None, :])
    r = np.sqrt(3) * dist / length_scale
    return variance * (1 + r) * np.exp(-r)

def kernel_periodic(x1, x2, length_scale=1.0, variance=1.0, period=2.0):
    dist = np.abs(x1[:, None] - x2[None, :])
    return variance * np.exp(-2 * np.sin(np.pi * dist / period)**2 / length_scale**2)

def kernel_linear(x1, x2, variance=1.0, offset=0.0):
    return variance * (x1[:, None] - offset) * (x2[None, :] - offset)

# --- GP sampler ---
def sample_gp(kernel, x, n_samples=5, noise=1e-6, **kwargs):
    K = kernel(x, x, **kwargs)
    K += noise * np.eye(len(x))  # jitter for numerical stability
    L = np.linalg.cholesky(K)
    samples = L @ np.random.randn(len(x), n_samples)
    return samples

# --- Plot setup ---
np.random.seed(42)
x = np.linspace(-5, 5, 300)
n_samples = 5
colors = plt.cm.viridis(np.linspace(0.1, 0.9, n_samples))

kernels = [
    (kernel_rbf,       "RBF (Squared Exponential)",  {"length_scale": 1.0, "variance": 1.0}),
    (kernel_matern12,  "Matérn $\\nu = 1/2$",         {"length_scale": 1.0, "variance": 1.0}),
    (kernel_matern32,  "Matérn $\\nu = 3/2$",         {"length_scale": 1.0, "variance": 1.0}),
    (kernel_periodic,  "Periodic",                    {"length_scale": 1.0, "variance": 1.0, "period": 2.0}),
    (kernel_linear,    "Linear",                      {"variance": 0.3,     "offset": 0.0}),
]

fig = plt.figure(figsize=(16, 10))
fig.suptitle("Gaussian Process Samples Under Different Kernel Functions",
             fontsize=15, fontweight="bold", y=1.01)

gs = GridSpec(2, 3, figure=fig, hspace=0.45, wspace=0.3)
axes = [fig.add_subplot(gs[i // 3, i % 3]) for i in range(5)]

for ax, (kernel_fn, title, kwargs) in zip(axes, kernels):
    samples = sample_gp(kernel_fn, x, n_samples=n_samples, **kwargs)
    for i in range(n_samples):
        ax.plot(x, samples[:, i], color=colors[i], linewidth=1.2, alpha=0.85)
    ax.axhline(0, color="black", linewidth=0.6, linestyle="--", alpha=0.4)
    ax.set_title(title, fontsize=11)
    ax.set_xlabel("$x$", fontsize=10)
    ax.set_ylabel("$f(x)$", fontsize=10)
    ax.set_xlim(-5, 5)
    ax.tick_params(labelsize=8)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

plt.savefig("gp_kernels.pdf", bbox_inches="tight", dpi=300)
plt.savefig("gp_kernels.png", bbox_inches="tight", dpi=300)
plt.show()
print("Saved: gp_kernels.pdf and gp_kernels.png")