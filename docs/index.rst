pkft: Peakfit Explorer
=======================

.. container:: .large

    pkft finds peaks in one-dimensional datasets using a Gaussian Mixture Model that accounts for measurement uncertainties. Paste data, fit the mixture, share the peaks.

Quickstart
----------

.. code-block:: bash

    pip install pkft
    pkft

Fit the data from your browser or a script:

.. code-block:: bash

    curl -X POST http://127.0.0.1:8000/fit -H 'Content-Type: application/json' \
        -d '{"observations": [{"sample": "CV-13", "value": 17.634, "standard_error": 0.016}], "peak_count": 1}'

That example fits a single peak to a single measurement. Open http://127.0.0.1:8000/ for the data explorer, http://127.0.0.1:8000/mixture for the mixture tab, or click Load demo for the bundled Trayler et al. 2020 dataset. See :doc:`installation` for environment setup and :doc:`usage` for the full walkthrough.

Citation
--------

.. image:: https://zenodo.org/badge/DOI/10.5281/zenodo.23735940.svg
   :target: https://doi.org/10.5281/zenodo.23735940

Cite this release:

.. code-block:: bibtex

    @software{kerswell_2026_23735941,
      author       = {Kerswell, Buchanan},
      title        = {pkft: Peakfit Explorer},
      month        = oct,
      year         = 2026,
      publisher    = {Zenodo},
      version      = {latest},
      doi          = {10.5281/zenodo.23735940},
      url          = {https://doi.org/10.5281/zenodo.23735940},
    }

.. toctree::
   :maxdepth: 2
   :caption: Documentation
   :hidden:

   installation
   usage
   api
   autoapi/index
