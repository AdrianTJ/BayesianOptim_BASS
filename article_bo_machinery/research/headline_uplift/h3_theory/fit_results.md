# H3 decay-law fit (protocol: DESIGN.md)

| cell | m0 (σ=0) | m∞ (fit) | s (half-loss) | γ | R² fit (E8) | R² held-out (E7) | Spearman ρ(σ, median) E8 |
|---|---|---|---|---|---|---|---|
| func2C/flip/n50 | -0.1919 | -0.0106 | 1.12 | 0.68 | 0.758 | 0.117 | +0.771 |
| func2C/flip/n1000 | -0.2053 | -0.0178 | 1.29 | 1.64 | 0.988 | 0.307 | +0.771 |
| func2C/keep/n50 | -0.2063 | -0.0455 | 3.09 | 18.33 | 0.621 | 0.630 | +0.943 |
| func2C/keep/n1000 | -0.2063 | -0.0033 | 3.40 | 18.37 | 0.998 | -1.275 | +0.943 |
| func3C/flip/n50 | -0.6651 | -0.0937 | 4.85 | 0.89 | 0.977 | 0.810 | +1.000 |
| func3C/flip/n1000 | -0.7154 | -0.1527 | 3.23 | 1.22 | 0.891 | 0.878 | +0.943 |
| func3C/keep/n50 | -0.7221 | -0.3077 | 7.80 | 3.32 | 1.000 | 0.658 | +1.000 |
| func3C/keep/n1000 | -0.7221 | -0.0256 | 22.25 | 1.76 | 1.000 | 0.863 | +1.000 |

Cells with NOT perfectly monotone-worsening medians (ρ<1): ['func2C/flip/n50', 'func2C/flip/n1000', 'func2C/keep/n50', 'func2C/keep/n1000', 'func3C/flip/n1000'] — disclosed per DESIGN, not smoothed.

Median fitted half-loss dial s across cells: 3.3 (per-cell values above).
