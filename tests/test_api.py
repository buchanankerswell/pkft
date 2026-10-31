"""Smoke + golden tests for the pkft API (mirrors rxnDB's test_api.py scope)."""

import csv
from pathlib import Path

from fastapi.testclient import TestClient

from pkft.api import app

client = TestClient(app)
DEMO = Path(__file__).parent.parent / "pkft" / "data" / "demo-trayler-2020.csv"


def _demo_observations():
    with open(DEMO, newline="") as fh:
        return [{"sample": r["sample"], "value": float(r["value"]), "standard_error": float(r["standard_error"])} for r in csv.DictReader(fh)]


def test_health():
    assert client.get("/health").json() == {"status": "ok"}


def test_demo_csv():
    res = client.get("/demo")
    assert res.status_code == 200
    assert res.headers["content-type"].startswith("text/csv")
    lines = res.text.strip().splitlines()
    assert lines[0] == "sample,value,standard_error"
    assert len(lines) == 49  # header + 48 Trayler rows


def test_fit_fixed_count_two_points():
    body = {
        "observations": [
            {"value": 17.634, "standard_error": 0.016},
            {"value": 17.629, "standard_error": 0.025},
        ],
        "peak_count": 1,
    }
    res = client.post("/fit", json=body)
    assert res.status_code == 200, res.text
    data = res.json()
    assert data["peak_count"] == 1
    assert len(data["peaks"]) == 1
    peak = data["peaks"][0]
    assert abs(peak["value"] - 17.632) < 0.005
    assert peak["legend"].startswith("1,")
    assert len(data["curves"][0]["x_values"]) > 10


def test_fit_auto_on_demo():
    obs = _demo_observations()
    assert len(obs) == 48
    res = client.post("/fit", json={"observations": obs, "peak_count": 0})
    assert res.status_code == 200, res.text
    data = res.json()
    assert 1 <= data["peak_count"] <= 5
    assert data["bic_table"] is not None and len(data["bic_table"]) == min(5, len(obs))
    total = sum(p["proportion"] for p in data["peaks"])
    assert abs(total - 1.0) < 1e-6
    values = [p["value"] for p in data["peaks"]]
    assert values == sorted(values)
    assert all(p["uncertainty"] > p["standard_error"] for p in data["peaks"])  # 95% CI wider than 1-SE


def test_fit_validation_errors():
    assert client.post("/fit", json={"observations": [], "peak_count": 1}).status_code == 422
    bad = {"observations": [{"value": 1.0, "standard_error": 0.0}], "peak_count": 1}
    assert client.post("/fit", json=bad).status_code == 422
    many = {"observations": [{"value": 1.0, "standard_error": 0.1}], "peak_count": 2}
    assert client.post("/fit", json=many).status_code == 422
    bigk = {"observations": [{"value": 1.0, "standard_error": 0.1}], "peak_count": 26}
    assert client.post("/fit", json=bigk).status_code == 422


def test_validate_echo():
    body = {"observations": [{"value": 17.6, "standard_error": 0.02}], "peak_count": 0}
    res = client.post("/validate", json=body)
    assert res.status_code == 200
    assert res.json()["peak_count"] == 0


def test_ui_serves_without_plotly():
    for path in ("/", "/mixture"):
        res = client.get(path)
        assert res.status_code == 200
        html = res.text
        assert "plot1" in html and "plot2" in html
        assert "Plotly" not in html and "plotly" not in html
        assert "Mixture plot" in html and "Peaks" in html
        assert 'id="settings"' in html and 'id="help"' in html
    css = client.get("/static/app.css").text
    assert "--link: #3b5cf6" in css and "--hairline" in css
    js = client.get("/static/app.js").text
    assert "toggleSettings" in js and "toggleHelp" in js and "initTheme" in js
