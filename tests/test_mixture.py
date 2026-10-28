"""Unit tests for the discrete-mixture engine and curve helpers."""

import numpy as np

from pkft import curves as curve_lib
from pkft import mixture as mix


def test_two_clusters_recovered():
    rng = np.random.default_rng(42)
    a = rng.normal(10.0, 0.05, 20)
    b = rng.normal(12.0, 0.05, 20)
    x = np.concatenate([a, b])
    s = np.full_like(x, 0.05)
    out = mix.fit_mixture(x, s, peak_count=2, seed=0)
    assert out["peak_count"] == 2
    means = sorted(float(v) for v in out["means"])
    assert abs(means[0] - 10.0) < 0.1
    assert abs(means[1] - 12.0) < 0.1
    assert abs(float(out["proportions"][0]) - 0.5) < 0.15


def test_auto_selects_two_over_one():
    rng = np.random.default_rng(7)
    x = np.concatenate([rng.normal(10.0, 0.05, 25), rng.normal(12.0, 0.05, 25)])
    s = np.full_like(x, 0.05)
    out = mix.fit_mixture(x, s, peak_count=0, seed=0)
    assert out["bic_table"] is not None
    assert out["peak_count"] == 2
    bics = {r["peak_count"]: r["bic"] for r in out["bic_table"]}
    assert bics[2] < bics[1]


def test_single_observation():
    out = mix.fit_mixture([17.6], [0.02], peak_count=1)
    assert abs(float(out["means"][0]) - 17.6) < 1e-9
    assert abs(float(out["mean_standard_errors"][0]) - 0.02) < 1e-9


def test_curves_agree_with_r_convention():
    x = np.array([17.6, 17.7])
    e = np.array([0.02, 0.02])
    grid = curve_lib.grid_for(x, e, 3.0, n=50)
    total = curve_lib.sumpdf(grid, x, e)
    assert np.all(total >= 0)
    # Mean of per-point pdfs integrates to ~1 over a wide grid.
    assert abs(float(np.trapezoid(total, grid)) - 1.0) < 0.05
    px, pd = curve_lib.peak_curve(17.65, 0.01, 0.5, 3.0)
    assert len(px) == 61  # +/-3 SE at SE/10 steps, mirroring R seq()
    assert all(v >= 0 for v in pd)
