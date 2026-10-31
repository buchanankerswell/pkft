"""FastAPI tier. Browsers and agents read the same endpoints."""

from pathlib import Path

from fastapi import FastAPI, HTTPException
from fastapi.responses import FileResponse, PlainTextResponse
from fastapi.staticfiles import StaticFiles

from pkft import curves as curve_lib
from pkft import mixture as mix
from pkft.models import BicEntry, FitRequest, FitResponse, Peak, PeakCurve

app = FastAPI(
    title="pkft",
    version="1.0.0",
    description="Agent-facing JSON API. Humans: use the web UI served at /.",
)
app.mount("/static", StaticFiles(directory=Path(__file__).parent / "static"), name="static")


@app.get("/health")
def health() -> dict:
    return {"status": "ok"}


@app.get("/demo")
def demo() -> FileResponse:
    path = Path(__file__).parent / "data" / "demo-trayler-2020.csv"
    return FileResponse(path, media_type="text/csv", filename="demo-trayler-2020.csv")


def _fit_response(request: FitRequest) -> FitResponse:
    values = [o.value for o in request.observations]
    # Input errors reach the fit in one-sigma units.
    errors = [o.standard_error / request.input_sigma for o in request.observations]
    try:
        result = mix.fit_mixture(values, errors, peak_count=request.peak_count)
    except ValueError as exc:
        raise HTTPException(status_code=422, detail=str(exc)) from exc

    z = mix.Z_VALUES[request.confidence]
    peaks: list[Peak] = []
    curve_objs: list[PeakCurve] = []
    for i, (mean, standard_error, proportion, proportion_error) in enumerate(
        zip(result["means"], result["mean_standard_errors"], result["proportions"], result["proportion_standard_errors"]),
        start=1,
    ):
        uncertainty = float(z * float(standard_error))
        legend = (
            f"{i},{mix.format_significant_digits(float(mean), request.significant_digits)},"
            f"{mix.format_significant_digits(uncertainty, request.significant_digits)}"
        )
        peaks.append(
            Peak(
                peak=i,
                value=float(mean),
                standard_error=float(standard_error),
                uncertainty=uncertainty,
                proportion=float(proportion),
                proportion_standard_error=float(proportion_error),
                legend=legend,
            )
        )
        grid, curve = curve_lib.peak_curve(float(mean), max(float(standard_error), 1e-12), float(proportion), request.sigma_range)
        curve_objs.append(PeakCurve(peak=i, x_values=[float(v) for v in grid], y_values=[float(v) for v in curve]))

    table = (
        [BicEntry(peak_count=r["peak_count"], bic=r["bic"], log_likelihood=r["log_likelihood"]) for r in result["bic_table"]]
        if result["bic_table"] is not None
        else None
    )
    return FitResponse(
        peak_count=result["peak_count"],
        bic=result["bic"],
        log_likelihood=result["log_likelihood"],
        confidence=request.confidence,
        method=result["method"],
        peaks=peaks,
        curves=curve_objs,
        bic_table=table,
    )


@app.post("/fit", response_model=FitResponse)
def fit(request: FitRequest) -> FitResponse:
    return _fit_response(request)


@app.post("/validate", response_model=FitRequest)
def validate(request: FitRequest) -> FitRequest:
    return request


@app.get("/peaks.csv")
def peaks_csv_placeholder() -> PlainTextResponse:
    # Real CSV download is client-side from the last /fit response;
    # this route exists so a stale bookmark explains itself.
    return PlainTextResponse("Run POST /fit first; the UI offers Download Peaks (csv).\n", media_type="text/plain")


@app.get("/")
def index() -> FileResponse:
    return FileResponse(Path(__file__).parent / "static" / "index.html")


@app.get("/mixture")
def mixture_page() -> FileResponse:
    return FileResponse(Path(__file__).parent / "static" / "index.html")


def main() -> None:
    import uvicorn

    uvicorn.run("pkft.api:app", host="127.0.0.1", port=8000, reload=True)


if __name__ == "__main__":
    main()
