Agent API
=========

For autonomous agents. Humans: start with :doc:`usage`.

Base URL defaults to ``http://127.0.0.1:8000/``. All request and response bodies are JSON with ``Content-Type: application/json``, except ``GET /demo`` (CSV) and ``GET /`` and ``GET /mixture`` (HTML). The service is stateless. Every ``POST`` carries its full dataset and nothing is stored server-side. Errors return the cause in the response body. 422 means an invalid payload. 404 means an unknown identifier.

Data model
----------

Minimal accepted payload (three observations, automatic peak count):

.. code-block:: json

    {
      "observations": [
        {"sample": "A", "value": 10.02, "standard_error": 0.05},
        {"sample": "A", "value": 10.05, "standard_error": 0.04},
        {"sample": "B", "value": 12.01, "standard_error": 0.06}
      ],
      "peak_count": 0
    }

Field rules:

* ``observations``: non-empty array of ``{"sample": str | null, "value": float, "standard_error": float}``. ``standard_error`` must be above zero and is read as a one-sigma error.
* ``peak_count``: 0..25. 0 asks for automatic selection over 1..5 by BIC.
* ``significant_digits``: 1..6. Rounds the ``legend`` strings only.
* ``confidence``: one of ``"90%"``, ``"95%"``, ``"98%"``, ``"99%"``. Sets the interval behind ``uncertainty``.
* ``input_sigma``: 1..3. Divisor applied to each error first, so two-sigma labs pass 2.
* ``sigma_range``: 0.5..10. Half-width of returned peak curves in standard errors.

The web UI imports CSV with header ``sample,value,standard_error``. Rows map one-to-one onto ``observations``.

Endpoints
---------

========================== ====================================================
Endpoint                   Contract
========================== ====================================================
``GET /health``            Returns ``{"status": "ok"}``.
``GET /demo``              Bundled demo CSV (``sample,value,standard_error``).
``POST /fit``              Body: the data model above. Returns the fit below. 422 on empty observations, non-positive ``standard_error``, ``peak_count`` above the row count, non-finite numbers, or bad enums.
``POST /validate``         Body: the data model above. Echoes it back. Same 422 cases as ``/fit``.
``GET /``                  Human data explorer (HTML).
``GET /mixture``           Human mixture tab (HTML).
========================== ====================================================

``POST /fit`` responds with ``peak_count``, ``bic``, ``log_likelihood``, ``confidence``, and ``method``. ``peaks`` holds one entry per component with ``value``, one-sigma ``standard_error``, ``uncertainty`` (confidence-interval half-width at ``confidence``), ``proportion``, ``proportion_standard_error``, and a ``"peak,value,uncertainty"`` ``legend`` line. ``curves`` holds the plotted component densities as ``x_values``/``y_values`` pairs. ``bic_table`` (automatic counts only) lists the BIC per candidate count.

Example call and first response lines:

.. code-block:: bash

    curl -X POST http://127.0.0.1:8000/fit \
        -H 'Content-Type: application/json' \
        -d '{"observations": [{"value": 17.634, "standard_error": 0.016}, {"value": 17.629, "standard_error": 0.025}], "peak_count": 1}'

.. code-block:: json

    {
      "peak_count": 1,
      "peaks": [
        {"peak": 1, "value": 17.632547105561862, "legend": "1,18,0.026"}
      ]
    }
