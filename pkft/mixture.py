"""Uncertainty-aware discrete mixture (Galbraith & Laslett 1993, IsoplotR ``peakfit`` spirit).

Each observation :math:`x_i` carries a known analytical variance :math:`s_i^2`.
Component :math:`j` is a discrete age :math:`\\mu_j` with mixing proportion :math:`\\pi_j`;
the log-likelihood is :math:`\\ell = \\sum_i \\log \\sum_j \\pi_j \\, N(x_i \\mid \\mu_j, s_i^2)`.
There is no per-component overdispersion term, so -- unlike a standard GMM --
the model cannot collapse to zero variance and needs only numpy.

``peak_count=0`` means automatic selection over 1..min(5, n) by BIC with
:math:`p = 2k - 1` free parameters (:math:`k` means + :math:`k-1` proportions), matching the
IsoplotR ``k='auto'`` convention (maximum 5). Results are not bit-identical
to IsoplotR (different optimiser/initialisation) but maximise the same
likelihood.
"""

import math

import numpy as np

Z_VALUES = {
    "90%": 1.6448536269514729,
    "95%": 1.959963984540054,
    "98%": 2.32634787404084,
    "99%": 2.5758293035489004,
}

AUTO_PEAK_MAX = 5
PEAK_HARD_MAX = 25
N_INIT = 10
MAX_ITER = 500
TOL = 1e-8


def _norm_pdf(x: np.ndarray, mean: float, variance: np.ndarray) -> np.ndarray:
    return np.exp(-0.5 * ((x - mean) ** 2) / variance) / np.sqrt(2.0 * math.pi * variance)


def _fit_fixed_count(
    values: np.ndarray,
    errors: np.ndarray,
    peak_count: int,
    seed: int = 0,
    n_init: int = N_INIT,
) -> dict:
    count = values.shape[0]
    if peak_count < 1:
        raise ValueError("peak_count must be >= 1")
    if peak_count > count:
        raise ValueError(f"peak_count={peak_count} exceeds observation count n={count}")
    if np.any(errors <= 0):
        raise ValueError("all standard errors must be > 0")

    variances = errors**2
    rng = np.random.default_rng(seed)
    sort_order = np.argsort(values)
    sorted_values = values[sort_order]

    best_log_likelihood = -math.inf
    best: dict | None = None

    for init in range(n_init):
        if init == 0:
            # Quantile spread: deterministic, reproducible start.
            positions = np.linspace(0, count - 1, peak_count).round().astype(int)
            means = sorted_values[positions].astype(float) + rng.normal(0, 1e-9, size=peak_count)
        else:
            choice = rng.choice(count, size=peak_count, replace=False)
            means = values[choice].astype(float) + rng.normal(0, np.mean(errors) * 0.05, size=peak_count)
        proportions = np.full(peak_count, 1.0 / peak_count)
        previous_log_likelihood = -math.inf
        responsibilities = np.full((count, peak_count), 1.0 / peak_count)
        for _ in range(MAX_ITER):
            # E-step
            component_likelihoods = np.empty((count, peak_count))
            for j in range(peak_count):
                component_likelihoods[:, j] = proportions[j] * _norm_pdf(values, means[j], variances)
            row_totals = component_likelihoods.sum(axis=1, keepdims=True)
            row_totals = np.maximum(row_totals, 1e-300)
            responsibilities = component_likelihoods / row_totals
            log_likelihood = float(np.sum(np.log(row_totals)))
            # M-step
            component_counts = responsibilities.sum(axis=0)  # (k,)
            # Guard against empty components: reseed to a random point.
            for j in range(peak_count):
                if component_counts[j] < 1e-12:
                    means[j] = float(rng.choice(values))
                    component_counts[j] = 1e-12
            proportions = component_counts / count
            weights = responsibilities / variances[:, None]  # (n, k)
            means = (weights * values[:, None]).sum(axis=0) / weights.sum(axis=0)
            if abs(log_likelihood - previous_log_likelihood) < TOL * max(1.0, abs(log_likelihood)):
                previous_log_likelihood = log_likelihood
                break
            previous_log_likelihood = log_likelihood
        # Final likelihood at converged parameters.
        component_likelihoods = np.empty((count, peak_count))
        for j in range(peak_count):
            component_likelihoods[:, j] = proportions[j] * _norm_pdf(values, means[j], variances)
        log_likelihood = float(np.sum(np.log(np.maximum(component_likelihoods.sum(axis=1), 1e-300))))
        if log_likelihood > best_log_likelihood:
            best_log_likelihood = log_likelihood
            best = {
                "means": means.copy(),
                "proportions": proportions.copy(),
                "responsibilities": responsibilities.copy(),
                "log_likelihood": log_likelihood,
            }

    assert best is not None
    # Standard errors: means from weighted precision, proportions from multinomial.
    weights = best["responsibilities"] / variances[:, None]
    mean_standard_errors = 1.0 / np.sqrt(np.maximum(weights.sum(axis=0), 1e-300))
    proportion_standard_errors = np.sqrt(np.maximum(best["proportions"] * (1.0 - best["proportions"]) / count, 0.0))
    # Sort by ascending mean for stable peak numbering.
    sort_order = np.argsort(best["means"])
    return {
        "means": best["means"][sort_order],
        "proportions": best["proportions"][sort_order],
        "mean_standard_errors": mean_standard_errors[sort_order],
        "proportion_standard_errors": proportion_standard_errors[sort_order],
        "log_likelihood": best["log_likelihood"],
    }


def bic(log_likelihood: float, peak_count: int, count: int) -> float:
    """Bayes information criterion; :math:`p = 2k - 1` free parameters."""
    params = 2 * peak_count - 1
    return -2.0 * log_likelihood + params * math.log(count)


def fit_mixture(
    values: list[float] | np.ndarray,
    errors: list[float] | np.ndarray,
    peak_count: int = 0,
    seed: int = 0,
) -> dict:
    """Fit 1..k discrete components. ``peak_count=0`` auto-selects by BIC over 1..min(5, n)."""
    values_array = np.asarray(values, dtype=float)
    error_array = np.asarray(errors, dtype=float)
    count = values_array.shape[0]
    if count == 0:
        raise ValueError("no observations")
    if np.any(~np.isfinite(values_array)) or np.any(~np.isfinite(error_array)):
        raise ValueError("values and errors must be finite")
    if np.any(error_array <= 0):
        raise ValueError("all standard errors must be > 0")
    if peak_count < 0 or peak_count > PEAK_HARD_MAX:
        raise ValueError(f"peak_count must be in 0..{PEAK_HARD_MAX}")
    if peak_count > count:
        raise ValueError(f"peak_count={peak_count} exceeds observation count n={count}")

    if peak_count == 0:
        candidates = list(range(1, min(AUTO_PEAK_MAX, count) + 1))
        table = []
        fits = {}
        for candidate in candidates:
            fit = _fit_fixed_count(values_array, error_array, candidate, seed=seed)
            score = bic(fit["log_likelihood"], candidate, count)
            table.append({"peak_count": candidate, "bic": score, "log_likelihood": fit["log_likelihood"]})
            fits[candidate] = fit
        best_count = min(table, key=lambda r: r["bic"])["peak_count"]
        out = fits[best_count]
        return {
            "peak_count": best_count,
            "bic": next(r["bic"] for r in table if r["peak_count"] == best_count),
            "log_likelihood": out["log_likelihood"],
            "means": out["means"],
            "proportions": out["proportions"],
            "mean_standard_errors": out["mean_standard_errors"],
            "proportion_standard_errors": out["proportion_standard_errors"],
            "bic_table": table,
            "method": "discrete-mixture-em(auto-bic)",
        }
    fit = _fit_fixed_count(values_array, error_array, peak_count, seed=seed)
    return {
        "peak_count": peak_count,
        "bic": bic(fit["log_likelihood"], peak_count, count),
        "log_likelihood": fit["log_likelihood"],
        "means": fit["means"],
        "proportions": fit["proportions"],
        "mean_standard_errors": fit["mean_standard_errors"],
        "proportion_standard_errors": fit["proportion_standard_errors"],
        "bic_table": None,
        "method": "discrete-mixture-em",
    }


def format_significant_digits(value: float, significant_digits: int) -> str:
    """Format to ``significant_digits`` significant digits (R ``sigdig`` convention)."""
    if not math.isfinite(value):
        return str(value)
    if value == 0:
        return "0"
    return f"{value:.{significant_digits}g}"
