Usage
=====

Start the server
----------------

After :doc:`installation`, start the server with:

.. code-block:: bash

    pkft

The command launches the app on http://127.0.0.1:8000/.

Open the interface
------------------

Visit http://127.0.0.1:8000/. Two tabs share the page. ``Visualize Data`` holds an editable table and a plain SVG plot. ``Gaussian Mixture Model`` holds the fit controls and the mixture plot. ``Load demo`` fills the table with the bundled Trayler et al. 2020 dataset. ``Import CSV`` takes any ``sample,value,standard_error`` file.

Data format
-----------

The app takes a table of observations. Each row holds a sample label, a value, and the value's one-sigma standard error:

.. code-block:: text

    sample,value,standard_error
    A,10.02,0.05
    A,10.05,0.04
    B,12.01,0.06

Paste rows into the table, import a CSV file in this shape, or click ``Load demo`` for the bundled dataset. Scripts can send the same rows as JSON. The exact format lives in :doc:`api`.

Worked example: demo data
-------------------------

1. Open http://127.0.0.1:8000/
2. Click ``Load demo`` (48 rows appear, all ticked)
3. Switch to the ``Gaussian Mixture Model`` tab (or open http://127.0.0.1:8000/mixture)
4. Leave ``peak_count = 0`` so the app picks the number of peaks itself; the fit updates on its own

The result lists ``peak,value,standard_error`` lines, a table comparing one to five peaks, and the mixture plot with green peak areas. The comparison uses the Bayesian information criterion (BIC), which trades fit quality against the number of peaks. ``Download peaks (csv)`` in the top bar saves the peak list.

Tabs
----

* ``Visualize Data`` (``/``): data table with per-row toggles, plot type (histogram or point), ordering, bin count, density-curve overlays (norm/kernel/sumpdf/stackpdf/indv), and sigma range. Scroll to zoom around the cursor, double-click to reset.
* ``Gaussian Mixture Model`` (``/mixture``): peak count (0 asks for an automatic count, 1..25 sets it), significant digits, confidence level (90/95/98/99%), sigma range, input sigma, peak scale, and overlays. Fits update automatically as you edit. The peak text and BIC table update on every fit.

Only ticked rows are plotted and fitted. The fit expects one-sigma errors (standard errors). If your lab reports two-sigma errors, set input sigma to 2. The app divides each error by that number first.

Mixture Model
-------------

The likelihood is the Galbraith & Laslett (1993) discrete mixture. Each observation :math:`x_i` with analytical variance :math:`s_i^2` contributes a mixture density over :math:`k` discrete ages :math:`\mu_j` with proportions :math:`\pi_j`:

.. math::

   \ell = \sum_i \log \sum_{j=1}^{k} \pi_j \, N(x_i \mid \mu_j, s_i^2)

Expectation-maximization alternates between assigning points to peaks and moving the peaks, restarting 10 times from spread-out and random starting points and keeping the best likelihood. BIC with :math:`p = 2k - 1` parameters selects :math:`k`. The fit maximizes the same likelihood as IsoplotR ``peakfit`` with ``log=FALSE``. Numbers can differ in the last digits because the optimizer and starting points differ. Component curves use the peak standard error for width and are weighted by each peak's share of the data.

Troubleshooting
---------------

* ``/fit`` returns 422. Read the error message: empty observations, non-positive ``standard_error``, ``peak_count`` above the row count, or a non-finite value.
* The page loads but the plot is empty. No rows are ticked, or all rows have invalid numbers. Click ``Load demo``.
* Port 8000 is busy. Start the app on another port: ``python -m uvicorn pkft.api:app --port 8001``.

Call the API
------------

Scripts and agents can call the same URLs the browser uses. The full technical reference lives in :doc:`api`.
