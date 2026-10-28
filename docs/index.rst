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

That example fits a single peak to a single measurement. See :doc:`installation` for environment setup and :doc:`usage` for the full walkthrough.

.. toctree::
   :maxdepth: 2
   :caption: Documentation
   :hidden:

   installation
   usage
   api
   autoapi/index
