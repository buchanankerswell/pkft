# pkft: Peakfit Explorer

[![PyPI](https://img.shields.io/pypi/v/pkft)](https://pypi.org/project/pkft/)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23735940.svg)](https://doi.org/10.5281/zenodo.23735940)

📚 See the [documentation](https://pkft.readthedocs.io/en/latest/) for details.

Peak finding for 1D data with uncertainties by discrete Gaussian mixture
modeling (Galbraith & Laslett, 1993) in the spirit of IsoplotR
`peakfit` (Vermeesch, 2018), served as a FastAPI service with a minimal
dependency-free web UI.

## Prerequisite software

This project is written in [python](https://www.python.org). For most users, I recommend installing the [miniforge](https://github.com/conda-forge/miniforge) python distribution. This distributions includes a minimal installation of python and the package manager [conda](https://docs.conda.io/en/latest/), which is required to build the necessary python environment for this software.

For macOS users, miniforge can be installed with [homebrew](https://brew.sh):

```bash
brew install miniforge
```

For Window's users, miniforge can be installed from a binary .exe. Follow the instructions for miniconda, anaconda, or miniforge (recommended) [here](https://docs.conda.io/projects/conda/en/stable/user-guide/install/windows.html).

## Installation Guide

To install with Conda (recommended):

``` bash
# Create conda environment
conda create -n pkft python=3.13 pip

# Activate conda environment
conda activate pkft

# Install pkft
pip install pkft
```

## Usage Guide

After installing pkft, launch the server from the command line:

``` bash
# Activate conda environment
conda activate pkft

# Launch server (UI at http://127.0.0.1:8000/)
pkft
```

The same JSON serves browsers and agents:

``` bash
curl -X POST http://127.0.0.1:8000/fit \
  -H 'Content-Type: application/json' \
  -d '{"observations": [{"value": 17.634, "standard_error": 0.016}], "peak_count": 1}'
```

Open http://127.0.0.1:8000/ for the data explorer, http://127.0.0.1:8000/mixture for the mixture tab, or click **Load demo** for the bundled Trayler et al. 2020 dataset.

## Method

The likelihood is the Galbraith & Laslett (1993) discrete mixture. Each observation $x_i$ with analytical variance $s_i^2$ contributes a mixture density over $k$ discrete ages $\mu_j$ with proportions $\pi_j$:

$$\ell = \sum_i \log \sum_{j=1}^{k} \pi_j \, N(x_i \mid \mu_j, s_i^2)$$

Expectation–maximisation alternates between assigning points to peaks and moving the peaks, restarting 10 times from spread-out and random starting points and keeping the best likelihood. BIC with $p = 2k - 1$ parameters selects $k$. The fit maximises the same likelihood as IsoplotR `peakfit` with `log=FALSE`. Numbers can differ in the last digits because the optimiser and starting points differ. Component curves use the peak standard error for width and are weighted by each peak's share of the data. See the docs for the full API reference.

## Citation

```bibtex
@software{pkft,
  author       = {Kerswell, Buchanan},
  title        = {pkft: Peakfit Explorer},
  month        = oct,
  year         = 2026,
  publisher    = {Zenodo},
  version      = {latest},
  doi          = {10.5281/zenodo.23735940},
  url          = {https://doi.org/10.5281/zenodo.23735940},
}
```

## Acknowledgement

I gratefully acknowledge feedback and suggestions provided by
[Matthew Kohn](https://scholar.google.com/citations?user=xSyB1KQAAAAJ&hl=en&oi=ao)
and Kayleigh Harvey.
Peak modeling follows [IsoplotR](https://github.com/pvermees/IsoplotR/)
(Vermeesch, 2018); demo data from Trayler et al. (2020).
