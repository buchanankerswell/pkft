/* pkft UI: vanilla JS + inline SVG, zero dependencies. Fits update automatically. */
"use strict";
const $ = (id) => document.getElementById(id);
const state = { rows: [], fit: null, xlim1: null, xlim2: null, fitToken: 0 };

function normPdf(x, mu, sd) {
  sd = Math.max(sd, 1e-12);
  return Math.exp(-0.5 * ((x - mu) / sd) ** 2) / (sd * Math.sqrt(2 * Math.PI));
}
function mean(a) {
  return a.reduce((s, v) => s + v, 0) / a.length;
}
function sd(a) {
  const m = mean(a);
  return Math.sqrt(a.reduce((s, v) => s + (v - m) ** 2, 0) / Math.max(a.length - 1, 1));
}
function checkedValues(id) {
  return [...document.querySelectorAll(`#${id} input:checked`)].map((e) => e.value);
}
function padTable(headers, rows) {
  const widths = headers.map((h, i) => Math.max(h.length, ...rows.map((r) => String(r[i]).length)));
  const line = (cells) => `| ${cells.map((c, i) => String(c).padEnd(widths[i])).join(" | ")} |`;
  return [line(headers), `| ${widths.map((w) => "-".repeat(w)).join(" | ")} |`, ...rows.map(line)].join("\n");
}

/* ---------- data table ---------- */
function blankRow() {
  return { sample: "", value: "", standard_error: "", on: true };
}
function setRows(rows) {
  state.rows = rows;
  renderTable();
  draw();
  scheduleFit();
}
function renderTable() {
  const tb = document.querySelector("#table tbody");
  tb.innerHTML = "";
  state.rows.forEach((r, i) => {
    const tr = document.createElement("tr");
    if (r.on) tr.classList.add("ticked");
    tr.innerHTML = `<td><input type="checkbox" data-i="${i}" data-f="on" ${r.on ? "checked" : ""} aria-label="include row ${i + 1}" /></td>
      <td><input data-i="${i}" data-f="sample" value="${r.sample ?? ""}" /></td>
      <td><input data-i="${i}" data-f="value" value="${r.value ?? ""}" /></td>
      <td><input data-i="${i}" data-f="standard_error" value="${r.standard_error ?? ""}" /></td>
      <td><button data-del="${i}">×</button></td>`;
    tb.appendChild(tr);
  });
}
document.querySelector("#table tbody").addEventListener("change", (e) => {
  const t = e.target;
  if (!t.dataset.i) return;
  const r = state.rows[+t.dataset.i];
  const f = t.dataset.f;
  if (f === "on") {
    r.on = t.checked;
    t.closest("tr").classList.toggle("ticked", t.checked);
  } else if (f === "sample") r.sample = t.value;
  else if (f === "value") r.value = parseFloat(t.value);
  else if (f === "standard_error") r.standard_error = parseFloat(t.value);
  draw();
  scheduleFit();
});
document.querySelector("#table tbody").addEventListener("click", (e) => {
  if (e.target.dataset.del !== undefined) {
    state.rows.splice(+e.target.dataset.del, 1);
    renderTable();
    draw();
    scheduleFit();
  }
});
$("add_row").onclick = () => {
  state.rows.push({ sample: `S-${state.rows.length + 1}`, value: 0, standard_error: 0.01, on: true });
  renderTable();
  const wrap = $("tablewrap");
  wrap.scrollTop = wrap.scrollHeight;
  const inputs = wrap.querySelectorAll("tbody tr:last-child input");
  if (inputs[1]) inputs[1].focus();
  draw();
  scheduleFit();
};
$("clear").onclick = () => setRows([blankRow()]);
$("toggle_all").onchange = (e) => {
  state.rows.forEach((r) => (r.on = e.target.checked));
  renderTable();
  draw();
  scheduleFit();
};

function activeRows() {
  return state.rows
    .filter((r) => r.on && Number.isFinite(+r.value) && Number.isFinite(+r.standard_error) && +r.standard_error > 0)
    .map((r) => ({ sample: String(r.sample ?? ""), value: +r.value, standard_error: +r.standard_error }));
}

/* ---------- CSV ---------- */
function parseCSV(text) {
  const lines = text.trim().split(/\r?\n/);
  const head = lines[0].split(",").map((s) => s.trim());
  const vi = head.indexOf("value"),
    si = head.indexOf("standard_error"),
    ni = head.indexOf("sample");
  return lines
    .slice(1)
    .filter((l) => l.trim())
    .map((l) => {
      const c = l.split(",");
      return { sample: ni >= 0 ? c[ni].trim() : "", value: parseFloat(c[vi]), standard_error: parseFloat(c[si]), on: true };
    });
}
async function loadDemo() {
  const res = await fetch("/demo");
  setRows(parseCSV(await res.text()));
}
$("load_demo").onclick = loadDemo;
$("file").onchange = async (e) => {
  const f = e.target.files[0];
  if (!f) return;
  setRows(parseCSV(await f.text()));
  e.target.value = "";
};

/* ---------- SVG helpers ---------- */
const W = 720,
  H = 420,
  ML = 56,
  MR = 56,
  MT = 12,
  MB = 34;
function axes(svg, x0, x1, y1, label) {
  const iw = W - ML - MR,
    ih = H - MT - MB;
  const X = (v) => ML + ((v - x0) / Math.max(x1 - x0, 1e-12)) * iw;
  const Y = (v) => MT + ih - (v / Math.max(y1, 1e-12)) * ih;
  let s = `<line class="axis" x1="${ML}" y1="${MT + ih}" x2="${ML + iw}" y2="${MT + ih}" />`;
  for (let i = 0; i <= 5; i++) {
    const v = x0 + ((x1 - x0) * i) / 5;
    s += `<text class="tick" x="${X(v)}" y="${MT + ih + 16}" text-anchor="middle">${+v.toFixed(3)}</text>`;
  }
  if (label) s += `<text class="tick" x="${ML - 8}" y="${MT + 8}" text-anchor="end">${label}</text>`;
  return { X, Y, iw, ih, head: s };
}
function pathOf(pts, X, Y) {
  return pts.map((p, i) => `${i ? "L" : "M"}${X(p[0]).toFixed(1)},${Y(p[1]).toFixed(1)}`).join(" ");
}
function areaOf(pts, X, Y) {
  const base = Y(0);
  return pathOf(pts, X, Y) + ` L${X(pts[pts.length - 1][0]).toFixed(1)},${base.toFixed(1)} L${X(pts[0][0]).toFixed(1)},${base.toFixed(1)} Z`;
}

function densityCurves(vals, errs, sigma_range, grid) {
  const n = vals.length;
  const sum = grid.map((x) => vals.reduce((s, v, i) => s + normPdf(x, v, errs[i]), 0) / n);
  let norm = null;
  if (n > 1 && sd(vals) > 0) norm = grid.map((x) => normPdf(x, mean(vals), sd(vals)));
  // Gaussian kernel smooth with Silverman bandwidth for the kernel overlay.
  let kern = null;
  if (n > 1 && sd(vals) > 0) {
    const h = Math.max(1.06 * sd(vals) * Math.pow(n, -0.2), 1e-9);
    kern = grid.map((x) => vals.reduce((s, v) => s + normPdf(x, v, h), 0) / n);
  }
  const per = vals.map((v, i) => grid.map((x) => normPdf(x, v, errs[i]) / n));
  return { sum, norm, kern, per };
}

function drawExplore() {
  const svg = $("plot1");
  const rows = activeRows();
  const type = $("type").value;
  if (!rows.length) {
    svg.innerHTML = `<text x="20" y="30">No data</text>`;
    return;
  }
  const vals = rows.map((r) => r.value),
    errs = rows.map((r) => r.standard_error);
  const sr = +$("sigma_range_plot").value || 3;
  if (type === "point") return drawPoints(svg, rows, $("arrange").value);
  const bins = +$("bins").value || 30;
  let x0 = Math.min(...vals) - sr * mean(errs),
    x1 = Math.max(...vals) + sr * mean(errs);
  x1 += (x1 - x0) * 0.04;
  if (state.xlim1) [x0, x1] = state.xlim1;
  const bw = (x1 - x0) / bins;
  const counts = new Array(bins).fill(0);
  vals.forEach((v) => {
    const b = Math.min(bins - 1, Math.max(0, Math.floor((v - x0) / bw)));
    counts[b]++;
  });
  const dens = counts.map((c) => c / (vals.length * bw));
  const grid = Array.from({ length: 400 }, (_, i) => x0 + ((x1 - x0) * i) / 399);
  const dc = densityCurves(vals, errs, sr, grid);
  const yTop = Math.max(...dens, ...dc.sum, ...(dc.norm || [0]), ...(dc.kern || [0])) * 1.1 || 1;
  const { X, Y, head } = axes(svg, x0, x1, yTop, "");
  let s = head;
  const iw = W - ML - MR;
  dens.forEach((d, b) => {
    const bx = (ML + (b * iw) / bins).toFixed(1);
    const bw = (iw / bins).toFixed(1);
    s += `<rect class="hist" x="${bx}" y="${Y(d).toFixed(1)}" width="${bw}" height="${(Y(0) - Y(d)).toFixed(1)}" />`;
  });
  const curves = checkedValues("curves1");
  if (curves.includes("norm") && dc.norm)
    s += `<path d="${areaOf(
      grid.map((x, i) => [x, dc.norm[i]]),
      X,
      Y,
    )}" class="area-norm" />`;
  if (curves.includes("kernel") && dc.kern)
    s += `<path d="${areaOf(
      grid.map((x, i) => [x, dc.kern[i]]),
      X,
      Y,
    )}" fill="#666" opacity="0.15" />`;
  if (curves.includes("sumpdf"))
    s += `<path d="${areaOf(
      grid.map((x, i) => [x, dc.sum[i]]),
      X,
      Y,
    )}" class="area-sum" />`;
  if (curves.includes("stackpdf")) {
    let acc = new Array(grid.length).fill(0);
    dc.per.forEach((p) => {
      acc = acc.map((a, i) => a + p[i]);
      s += `<path d="${areaOf(
        grid.map((x, i) => [x, acc[i]]),
        X,
        Y,
      )}" fill="none" stroke="#888" stroke-width="0.5" />`;
    });
  }
  if (curves.includes("indv"))
    dc.per.forEach((p) => {
      s += `<path class="indv" d="${pathOf(
        grid.map((x, i) => [x, p[i]]),
        X,
        Y,
      )}" />`;
    });
  vals.forEach((v) => {
    s += `<line class="rug" x1="${X(v).toFixed(1)}" y1="${Y(0).toFixed(1)}" x2="${X(v).toFixed(1)}" y2="${(Y(0) - 12).toFixed(1)}" />`;
  });
  svg.innerHTML = s;
}

function drawPoints(svg, rows, arrange) {
  const key = arrange === "sample" ? "sample" : arrange;
  const sorted = rows.slice().sort((a, b) => (key === "sample" ? String(a.sample).localeCompare(String(b.sample)) : a[key] - b[key]));
  const vals = sorted.map((r) => r.value);
  let x0 = Math.min(...vals) - 1 * Math.max(...sorted.map((r) => r.standard_error));
  let x1 = Math.max(...vals) + 1 * Math.max(...sorted.map((r) => r.standard_error));
  x1 += (x1 - x0) * 0.04;
  if (state.xlim1) [x0, x1] = state.xlim1;
  const n = sorted.length,
    rowH = Math.min(24, (H - MT - MB) / Math.max(n, 1));
  const X = (v) => ML + ((v - x0) / Math.max(x1 - x0, 1e-12)) * (W - ML - MR);
  let s = `<line class="axis" x1="${ML}" y1="${H - MB}" x2="${W - MR}" y2="${H - MB}" />`;
  for (let i = 0; i <= 5; i++) {
    const v = x0 + ((x1 - x0) * i) / 5;
    s += `<text class="tick" x="${X(v)}" y="${H - MB + 16}" text-anchor="middle">${+v.toFixed(3)}</text>`;
  }
  sorted.forEach((r, i) => {
    const y = MT + 10 + i * rowH;
    s += `<line class="eb" x1="${X(r.value - r.standard_error).toFixed(1)}" y1="${y}" x2="${X(r.value + r.standard_error).toFixed(1)}" y2="${y}" />`;
    s += `<circle class="pt" cx="${X(r.value).toFixed(1)}" cy="${y}" r="3"><title>${r.sample}: ${r.value} ± ${r.standard_error}</title></circle>`;
    s += `<text class="tick" x="${ML - 6}" y="${y + 3}" text-anchor="end">${String(r.sample).slice(0, 18)}</text>`;
  });
  svg.innerHTML = s;
}

function drawMixture() {
  const svg = $("plot2");
  const rows = activeRows();
  const fit = state.fit;
  if (!rows.length) {
    svg.innerHTML = `<text x="20" y="30">No data</text>`;
    return;
  }
  const vals = rows.map((r) => r.value),
    errs = rows.map((r) => r.standard_error);
  const sr = +$("sigma_range").value || 3;
  const bins = +$("bins").value || 30;
  let x0 = Math.min(...vals) - sr * mean(errs),
    x1 = Math.max(...vals) + sr * mean(errs);
  if (state.xlim2) [x0, x1] = state.xlim2;
  if (fit && fit.peaks.length) {
    fit.curves.forEach((c) => {
      c.x_values.forEach((v) => {
        if (v < x0) x0 = v;
        if (v > x1) x1 = v;
      });
    });
  }
  if (!state.xlim2) x1 += (x1 - x0) * 0.04;
  const bw = (x1 - x0) / bins;
  const counts = new Array(bins).fill(0);
  vals.forEach((v) => {
    const b = Math.min(bins - 1, Math.max(0, Math.floor((v - x0) / bw)));
    counts[b]++;
  });
  const dens = counts.map((c) => c / (vals.length * bw));
  const grid = Array.from({ length: 400 }, (_, i) => x0 + ((x1 - x0) * i) / 399);
  const dc = densityCurves(vals, errs, sr, grid);
  const scale = +$("peak_scale").value || 3;
  let yTop = Math.max(...dens, ...dc.sum) * 1.1 || 1;
  if (fit)
    fit.curves.forEach((c) =>
      c.y_values.forEach((d) => {
        yTop = Math.max(yTop, (d / scale) * 1.15);
      }),
    );
  const { X, Y, head } = axes(svg, x0, x1, yTop, "");
  let s = head;
  const iw = W - ML - MR;
  dens.forEach((d, b) => {
    const bx = (ML + (b * iw) / bins).toFixed(1);
    const bw = (iw / bins).toFixed(1);
    s += `<rect class="hist" x="${bx}" y="${Y(d).toFixed(1)}" width="${bw}" height="${(Y(0) - Y(d)).toFixed(1)}" />`;
  });
  const curves = checkedValues("curves2");
  if (curves.includes("norm") && dc.norm)
    s += `<path d="${areaOf(
      grid.map((x, i) => [x, dc.norm[i]]),
      X,
      Y,
    )}" class="area-norm" />`;
  if (curves.includes("kernel") && dc.kern)
    s += `<path d="${areaOf(
      grid.map((x, i) => [x, dc.kern[i]]),
      X,
      Y,
    )}" fill="#666" opacity="0.15" />`;
  if (curves.includes("sumpdf"))
    s += `<path d="${areaOf(
      grid.map((x, i) => [x, dc.sum[i]]),
      X,
      Y,
    )}" class="area-sum" />`;
  if (fit) {
    const palette = ["#2f81f7", "#39c5cf", "#3fb950", "#9ece6a", "#e3b341"];
    fit.curves.forEach((c, j) => {
      const pts = c.x_values.map((x, i) => [x, c.y_values[i] / scale]);
      s += `<path class="pk" d="${areaOf(pts, X, Y)}" fill="${palette[j % palette.length]}" />`;
    });
  }
  vals.forEach((v) => {
    s += `<line class="rug" x1="${X(v).toFixed(1)}" y1="${Y(0).toFixed(1)}" x2="${X(v).toFixed(1)}" y2="${(Y(0) - 12).toFixed(1)}" />`;
  });
  svg.innerHTML = s;
}

function draw() {
  drawExplore();
  drawMixture();
}
["type", "arrange", "bins", "sigma_range_plot", "peak_scale"].forEach((id) => {
  $(id)?.addEventListener("input", () => {
    if (id === "bins") $("bins_v").textContent = $("bins").value;
    if (id === "sigma_range_plot") $("sigma_range_plot_v").textContent = $("sigma_range_plot").value;
    if (id === "peak_scale") $("peak_scale_v").textContent = $("peak_scale").value;
    draw();
  });
});
["peak_count", "significant_digits", "confidence", "sigma_range", "input_sigma"].forEach((id) => {
  $(id)?.addEventListener("input", () => {
    draw();
    scheduleFit();
  });
});
document.querySelectorAll("#curves1 input, #curves2 input").forEach((e) => e.addEventListener("change", draw));

/* zoom: wheel on x, dblclick reset */
function bindZoom(id, key) {
  const svg = $(id);
  svg.addEventListener(
    "wheel",
    (e) => {
      e.preventDefault();
      const rows = activeRows();
      if (!rows.length) return;
      const vals = rows.map((r) => r.value);
      let [x0, x1] = state[key] || [Math.min(...vals) - 1, Math.max(...vals) + 1];
      const f = e.deltaY > 0 ? 1.15 : 1 / 1.15;
      const mid = (x0 + x1) / 2,
        half = ((x1 - x0) / 2) * f;
      state[key] = [mid - half, mid + half];
      draw();
    },
    { passive: false },
  );
  svg.addEventListener("dblclick", () => {
    state[key] = null;
    draw();
  });
}
bindZoom("plot1", "xlim1");
bindZoom("plot2", "xlim2");

/* ---------- automatic fitting (debounced, stale responses ignored) ---------- */
let fitTimer = null;
function scheduleFit() {
  clearTimeout(fitTimer);
  fitTimer = setTimeout(runFit, 400);
}
async function runFit() {
  const token = ++state.fitToken;
  const rows = activeRows();
  if (rows.length < 1) {
    state.fit = null;
    $("info").textContent = "No data";
    $("bic").innerHTML = "";
    $("dl_csv").disabled = true;
    drawMixture();
    return;
  }
  const body = {
    observations: rows,
    peak_count: +$("peak_count").value || 0,
    significant_digits: +$("significant_digits").value || 2,
    confidence: $("confidence").value,
    input_sigma: +$("input_sigma").value || 1,
    sigma_range: +$("sigma_range").value || 3,
  };
  const res = await fetch("/fit", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(body) });
  if (token !== state.fitToken) return;
  if (!res.ok) {
    $("info").textContent = `Error ${res.status}: ${(await res.text()).slice(0, 300)}`;
    return;
  }
  const fit = await res.json();
  if (token !== state.fitToken) return;
  state.fit = fit;
  const noun = fit.peaks.length === 1 ? "peak" : "peaks";
  const scores = `log-likelihood ${(+fit.log_likelihood).toFixed(2)}, BIC ${(+fit.bic).toFixed(2)} (${fit.method})`;
  const summary = `${fit.peaks.length} ${noun}, ${scores}`;
  $("info").textContent =
    padTable(
      ["peak", "value", "standard error"],
      fit.peaks.map((p) => p.legend.split(",")),
    ) + `\n\n${summary}`;
  $("bic").innerHTML = fit.bic_table
    ? `<pre>${padTable(
        ["peaks", "BIC", "log-likelihood"],
        fit.bic_table.map((r) => [`${r.peak_count}`, (+r.bic).toFixed(2), (+r.log_likelihood).toFixed(2)]),
      )}</pre>`
    : "";
  $("dl_csv").disabled = false;
  drawMixture();
}
$("dl_csv").onclick = () => {
  if (!state.fit) return;
  const csv = "peak,value,standard_error\n" + state.fit.peaks.map((p) => p.legend).join("\n") + "\n";
  const a = document.createElement("a");
  a.href = URL.createObjectURL(new Blob([csv], { type: "text/csv" }));
  a.download = "pkft-peaks.csv";
  a.click();
};
function dlSVG(id, name) {
  const svg = $(id);
  const blob = new Blob([`<?xml version="1.0"?>\n${svg.outerHTML}`], { type: "image/svg+xml" });
  const a = document.createElement("a");
  a.href = URL.createObjectURL(blob);
  a.download = name;
  a.click();
}
$("dl_svg").onclick = () => {
  const mixtureActive = !$("view-mixture").classList.contains("hidden");
  if (mixtureActive) dlSVG("plot2", "pkft-mixture.svg");
  else dlSVG("plot1", "pkft-explore.svg");
};

/* ---------- dialogs (settings + help) ---------- */
const settings = wireDialog("settings", "settings_btn");
const help = wireDialog("help", "help_btn");
// Dialogs stay mutually exclusive: opening one closes the other.
$("settings_btn").addEventListener("click", () => {
  if (help.isOpen) help.close();
});
$("help_btn").addEventListener("click", () => {
  if (settings.isOpen) settings.close();
});
document.addEventListener("keydown", (e) => {
  if (e.key === "Escape" && (settings.isOpen || help.isOpen)) {
    settings.close();
    help.close();
    return;
  }
  const tag = e.target.tagName;
  if (tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT" || e.target.isContentEditable) return;
  if (e.key === "s") {
    e.preventDefault();
    if (help.isOpen) help.close();
    settings.toggle();
  }
  if (e.key === "?") {
    e.preventDefault();
    if (settings.isOpen) settings.close();
    help.toggle();
  }
});
initTheme(".actions");

/* ---------- tabs ---------- */
function showTab(name) {
  document.querySelectorAll(".tabs a").forEach((a) => a.classList.toggle("active", a.dataset.tab === name));
  $("view-explore").classList.toggle("hidden", name !== "explore");
  $("view-mixture").classList.toggle("hidden", name !== "mixture");
  const ind = $("tab_indicator");
  const active = document.querySelector(".tabs a.active");
  if (active) {
    ind.style.left = active.offsetLeft + "px";
    ind.style.width = active.offsetWidth + "px";
  }
}
document.querySelectorAll(".tabs a").forEach((a) =>
  a.addEventListener("click", (e) => {
    e.preventDefault();
    history.pushState({}, "", a.getAttribute("href"));
    showTab(a.dataset.tab);
  }),
);
addEventListener("popstate", () => showTab(location.pathname === "/mixture" ? "mixture" : "explore"));
showTab(location.pathname === "/mixture" ? "mixture" : "explore");
loadDemo();
