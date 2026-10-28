"""Pydantic models for peak-fit requests and responses."""

from typing import Literal

from pydantic import BaseModel, Field

ConfidenceLevel = Literal["90%", "95%", "98%", "99%"]


class Observation(BaseModel):
    sample: str | None = None
    value: float = Field(description="Measured value in the data's own units")
    standard_error: float = Field(gt=0, description="Analytical standard error (one sigma) in the same units as value")


class FitRequest(BaseModel):
    observations: list[Observation] = Field(min_length=1)
    peak_count: int = Field(default=0, ge=0, le=25, description="Number of peaks; 0 asks for automatic selection by BIC (compares 1..5 peaks)")
    significant_digits: int = Field(default=2, ge=1, le=6)
    confidence: ConfidenceLevel = "95%"
    input_sigma: int = Field(
        default=1,
        ge=1,
        le=3,
        description="Sigma level the input errors are quoted at (1, 2, or 3); each error is divided by this to reach one-sigma errors",
    )
    sigma_range: float = Field(default=3.0, ge=0.5, le=10.0, description="Half-width of returned peak curves in standard errors")


class Peak(BaseModel):
    peak: int
    value: float
    standard_error: float = Field(description="One-sigma standard error of the peak value")
    uncertainty: float = Field(
        description="Half-width of the confidence interval at the requested confidence level (z times the one-sigma standard error)"
    )
    proportion: float = Field(ge=0, le=1)
    proportion_standard_error: float = Field(ge=0)
    legend: str = Field(description="CSV line 'peak,value,uncertainty' rounded to significant digits")


class PeakCurve(BaseModel):
    peak: int
    x_values: list[float]
    y_values: list[float]


class BicEntry(BaseModel):
    peak_count: int
    bic: float
    log_likelihood: float


class FitResponse(BaseModel):
    peak_count: int
    bic: float
    log_likelihood: float
    confidence: ConfidenceLevel
    method: str
    peaks: list[Peak]
    curves: list[PeakCurve]
    bic_table: list[BicEntry] | None = None
