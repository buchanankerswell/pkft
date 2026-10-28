"""Density-curve helpers mirroring the R ``p.data`` / ``p.pks`` overlays.

All curves are plain Gaussians evaluated in numpy so the FastAPI tier and the
vanilla-JS frontend (which reimplements ``normal_pdf`` in JS) agree. Counts
follow the R convention: per-observation PDFs are divided by ``n`` so the
summed PDF integrates to ~1.
"""

import math

import numpy as np


def normal_pdf(x: np.ndarray, mean: float, standard_deviation: float) -> np.ndarray:
    x = np.asarray(x, dtype=float)
    if standard_deviation <= 0:
        raise ValueError("standard deviation must be > 0")
    return np.exp(-0.5 * ((x - mean) / standard_deviation) ** 2) / (standard_deviation * math.sqrt(2.0 * math.pi))


def grid_for(values: np.ndarray, errors: np.ndarray, sigma_range: float, n: int = 400) -> np.ndarray:
    lo = float(np.min(values) - sigma_range * np.mean(errors))
    hi = float(np.max(values) + sigma_range * np.mean(errors))
    if not math.isfinite(lo) or not math.isfinite(hi) or lo == hi:
        lo, hi = float(np.min(values)) - 1.0, float(np.max(values)) + 1.0
    return np.linspace(lo, hi, n)


def sumpdf(x: np.ndarray, values: np.ndarray, errors: np.ndarray) -> np.ndarray:
    """Mean of per-observation Gaussians (R ``d.s`` / ``sumd``)."""
    x = np.asarray(x, dtype=float)
    total = np.zeros_like(x, dtype=float)
    for v, e in zip(values, errors):
        total += normal_pdf(x, float(v), float(e))
    return total / max(len(values), 1)


def normal_curve(x: np.ndarray, values: np.ndarray, errors: np.ndarray) -> np.ndarray:
    """Single Gaussian at the sample mean/sd (R ``d.n`` overlay)."""
    if len(values) < 2:
        return np.zeros_like(np.asarray(x, dtype=float))
    m = float(np.mean(values))
    sd = float(np.std(values, ddof=1))
    if sd <= 0:
        return np.zeros_like(np.asarray(x, dtype=float))
    return normal_pdf(x, m, sd)


def peak_curve(mean: float, standard_error: float, proportion: float, sigma_range: float) -> tuple[np.ndarray, np.ndarray]:
    """Weighted component curve over :math:`\\mu \\pm \\mathrm{sigrange} \\cdot \\mathrm{se}` (R ``d.norm``)."""
    step = max(standard_error / 10.0, 1e-12)
    lo, hi = mean - sigma_range * standard_error, mean + sigma_range * standard_error
    if lo == hi:
        lo, hi = mean - 1.0, mean + 1.0
    x = np.arange(lo, hi + step / 2.0, step)
    return x, normal_pdf(x, mean, max(standard_error, 1e-12)) * proportion
