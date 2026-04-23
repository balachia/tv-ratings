// Crameri palettes — perceptually uniform, colorblind-safe
// managua (dark midpoint) for light mode, roma (light midpoint) for dark mode
var MANAGUA = ["#ffcf67","#d38c50","#a5563f","#6c2d3a","#4d3463","#5267a6","#67a0d3","#80e7fe"];
var ROMA    = ["#7d1700","#a05e1a","#bd9a3c","#d1de98","#9ce2d4","#45accc","#2472b4","#023098"];

function getPalette() {
  return document.documentElement.classList.contains("dark") ? ROMA : MANAGUA;
}

function interpolateColor(palette, t) {
  t = Math.max(0, Math.min(1, t));
  var n = palette.length - 1;
  var i = Math.floor(t * n);
  var f = t * n - i;
  if (i >= n) return palette[n];
  var c0 = hexToRgb(palette[i]);
  var c1 = hexToRgb(palette[i + 1]);
  return "rgb(" +
    Math.round(c0.r + f * (c1.r - c0.r)) + "," +
    Math.round(c0.g + f * (c1.g - c0.g)) + "," +
    Math.round(c0.b + f * (c1.b - c0.b)) + ")";
}

function hexToRgb(hex) {
  var n = parseInt(hex.slice(1), 16);
  return { r: (n >> 16) & 255, g: (n >> 8) & 255, b: n & 255 };
}

function ratingToPercentile(rating) {
  if (typeof PCTILE_RATINGS === "undefined") return (rating - 1) / 9;
  for (var i = 0; i < PCTILE_RATINGS.length - 1; i++) {
    if (rating <= PCTILE_RATINGS[i + 1]) {
      var f = (rating - PCTILE_RATINGS[i]) / (PCTILE_RATINGS[i + 1] - PCTILE_RATINGS[i]);
      return PCTILE_VALUES[i] + f * (PCTILE_VALUES[i + 1] - PCTILE_VALUES[i]);
    }
  }
  return 1;
}

function absoluteColor(rating) {
  if (rating === null || rating === undefined) return null;
  return interpolateColor(getPalette(), ratingToPercentile(rating));
}

function relativeColor(rank, maxRank) {
  if (rank === null || rank === undefined) return null;
  var t = (rank - 1) / Math.max(1, maxRank - 1);
  return interpolateColor(getPalette(), t);
}

function rgbToRgba(rgb, alpha) {
  return rgb.replace("rgb(", "rgba(").replace(")", "," + alpha + ")");
}

var currentMode = "absolute";
var currentData = null;
var MINI_THRESHOLD = 4;

// ---- dark/light mode ----

function isDarkMode() {
  return document.documentElement.classList.contains("dark");
}

function setTheme(dark) {
  document.documentElement.classList.toggle("dark", dark);
  var btn = document.getElementById("btn-theme");
  if (btn) btn.textContent = dark ? "\u2600 Light" : "\u263e Dark";
  if (currentData) {
    rebuildColors();
  }
}

function rebuildColors() {
  var cells = document.querySelectorAll(".heatmap-cell:not(.heatmap-cell-empty)");
  cells.forEach(function(cell) {
    var rating = parseFloat(cell.dataset.rating);
    var rank = parseInt(cell.dataset.rank);
    var maxRank = parseInt(cell.dataset.maxRank);
    var absColor = absoluteColor(rating);
    var relColor = relativeColor(rank, maxRank);
    cell.dataset.absColor = absColor;
    cell.dataset.relColor = relColor;
    var color = currentMode === "absolute" ? absColor : relColor;
    cell.style.backgroundColor = rgbToRgba(color, 0.35);
    cell.style.borderLeft = "4px solid " + color;
  });

  var miniCanvas = document.querySelector(".mini-canvas");
  if (miniCanvas && currentData) {
    redrawMiniCanvas(miniCanvas);
  }
}

function redrawMiniCanvas(miniCanvas) {
  var eps = currentData.episodes;
  var maxSeason = 0, maxEp = 0, maxRank = 0;
  var lookup = {};
  for (var i = 0; i < eps.length; i++) {
    if (eps[i].season > maxSeason) maxSeason = eps[i].season;
    if (eps[i].episode > maxEp) maxEp = eps[i].episode;
    if (eps[i].ratingRank > maxRank) maxRank = eps[i].ratingRank;
    lookup[eps[i].season + "," + eps[i].episode] = eps[i];
  }
  var ctx = miniCanvas.getContext("2d");
  ctx.clearRect(0, 0, miniCanvas.width, miniCanvas.height);
  var cellSize = 6;
  for (var s = 1; s <= maxSeason; s++) {
    for (var ep = 1; ep <= maxEp; ep++) {
      var d = lookup[s + "," + ep];
      if (!d || d.rating === null) continue;
      ctx.fillStyle = currentMode === "absolute"
        ? absoluteColor(d.rating)
        : relativeColor(d.ratingRank, maxRank);
      ctx.fillRect((s - 1) * cellSize, (ep - 1) * cellSize, cellSize - 1, cellSize - 1);
    }
  }
}

// ---- heatmap building ----

function buildHeatmap(data) {
  currentData = data;
  var container = document.getElementById("heatmap-container");
  container.innerHTML = "";

  var oldMini = document.getElementById("mini-view-wrapper");
  if (oldMini) oldMini.remove();

  var eps = data.episodes;
  if (!eps || eps.length === 0) return;

  var maxSeason = 0, maxEp = 0;
  for (var i = 0; i < eps.length; i++) {
    if (eps[i].season > maxSeason) maxSeason = eps[i].season;
    if (eps[i].episode > maxEp) maxEp = eps[i].episode;
  }

  var maxRank = 0;
  for (var i = 0; i < eps.length; i++) {
    if (eps[i].ratingRank !== null && eps[i].ratingRank > maxRank)
      maxRank = eps[i].ratingRank;
  }

  var lookup = {};
  for (var i = 0; i < eps.length; i++) {
    lookup[eps[i].season + "," + eps[i].episode] = eps[i];
  }

  var isMini = maxSeason >= MINI_THRESHOLD;

  if (isMini) {
    var miniWrapper = document.createElement("div");
    miniWrapper.id = "mini-view-wrapper";
    var header = document.querySelector(".detail-header");
    if (header) miniWrapper.style.top = header.offsetHeight + "px";
    container.parentNode.insertBefore(miniWrapper, container);
    buildMiniView(miniWrapper, eps, lookup, maxSeason, maxEp, maxRank);
  }

  buildFullGrid(container, lookup, maxSeason, maxEp, maxRank, isMini);
}

function buildMiniView(container, eps, lookup, maxSeason, maxEp, maxRank) {
  var miniWrap = document.createElement("div");
  miniWrap.className = "mini-view";

  var label = document.createElement("div");
  label.className = "mini-label";
  label.textContent = maxSeason + " seasons, " + eps.length + " episodes";
  miniWrap.appendChild(label);

  var outerWrap = document.createElement("div");
  outerWrap.className = "mini-scroll-wrapper";

  var fadeTop = document.createElement("div");
  fadeTop.className = "mini-fade-top";
  var fadeBottom = document.createElement("div");
  fadeBottom.className = "mini-fade-bottom";

  var scrollWrap = document.createElement("div");
  scrollWrap.className = "mini-scroll";

  var canvas = document.createElement("canvas");
  var cellSize = 6;
  canvas.width = maxSeason * cellSize;
  canvas.height = maxEp * cellSize;
  canvas.className = "mini-canvas";

  var ctx = canvas.getContext("2d");
  for (var s = 1; s <= maxSeason; s++) {
    for (var ep = 1; ep <= maxEp; ep++) {
      var d = lookup[s + "," + ep];
      if (!d || d.rating === null || d.rating === undefined) continue;
      var color = currentMode === "absolute"
        ? absoluteColor(d.rating)
        : relativeColor(d.ratingRank, maxRank);
      ctx.fillStyle = color;
      ctx.fillRect((s - 1) * cellSize, (ep - 1) * cellSize, cellSize - 1, cellSize - 1);
    }
  }

  scrollWrap.appendChild(canvas);
  outerWrap.appendChild(fadeTop);
  outerWrap.appendChild(scrollWrap);
  outerWrap.appendChild(fadeBottom);
  miniWrap.appendChild(outerWrap);
  container.appendChild(miniWrap);

  function updateFades() {
    var atTop = scrollWrap.scrollTop <= 2;
    var atBottom = scrollWrap.scrollTop + scrollWrap.clientHeight >= scrollWrap.scrollHeight - 2;
    fadeTop.classList.toggle("visible", !atTop);
    fadeBottom.classList.toggle("visible", !atBottom);
  }
  scrollWrap.addEventListener("scroll", updateFades);
  setTimeout(updateFades, 0);
}

function buildFullGrid(container, lookup, maxSeason, maxEp, maxRank, hasMini) {
  var grid = document.createElement("div");
  grid.className = "heatmap-grid";
  grid.style.gridTemplateColumns = "repeat(" + maxSeason + ", minmax(90px, 1fr))";
  grid.style.gridTemplateRows = "repeat(" + maxEp + ", 42px)";

  if (hasMini) {
    var expandLabel = document.createElement("div");
    expandLabel.className = "expand-label";
    expandLabel.textContent = "Full detail view \u2193";
    container.appendChild(expandLabel);
  }

  for (var ep = 1; ep <= maxEp; ep++) {
    for (var s = 1; s <= maxSeason; s++) {
      var key = s + "," + ep;
      var d = lookup[key];

      var cell = document.createElement("div");
      cell.className = "heatmap-cell";
      cell.style.gridColumn = s;
      cell.style.gridRow = ep;

      if (!d || d.rating === null || d.rating === undefined) {
        cell.classList.add("heatmap-cell-empty");
        if (d) {
          cell.innerHTML = '<span class="cell-code">' + s + "x" + pad2(ep) + "</span>";
        }
        grid.appendChild(cell);
        continue;
      }

      var absColor = absoluteColor(d.rating);
      var relColor = relativeColor(d.ratingRank, maxRank);
      cell.dataset.absColor = absColor;
      cell.dataset.relColor = relColor;
      cell.dataset.season = d.season;
      cell.dataset.episode = d.episode;
      cell.dataset.title = d.title || "";
      cell.dataset.rating = d.rating;
      cell.dataset.votes = d.votes;
      cell.dataset.year = d.year || "";
      cell.dataset.tconst = d.tconst || "";
      cell.dataset.rank = d.ratingRank;
      cell.dataset.maxRank = maxRank;

      var color = currentMode === "absolute" ? absColor : relColor;
      cell.style.backgroundColor = rgbToRgba(color, 0.35);
      cell.style.borderLeft = "4px solid " + color;

      var titleText = d.title || "";
      if (titleText.length > 22) titleText = titleText.substring(0, 20) + "\u2026";

      cell.innerHTML =
        '<span class="cell-code">' + s + "x" + pad2(ep) + "</span>" +
        '<span class="cell-title">' + escapeHtml(titleText) + "</span>" +
        '<span class="cell-rating">' + d.rating.toFixed(1) + "</span>";

      cell.addEventListener("mouseenter", showTooltip);
      cell.addEventListener("mousemove", moveTooltip);
      cell.addEventListener("mouseleave", hideTooltip);
      cell.addEventListener("click", handleCellTap);

      grid.appendChild(cell);
    }
  }

  container.appendChild(grid);
}

function recolor(mode) {
  currentMode = mode;
  var cells = document.querySelectorAll(".heatmap-cell:not(.heatmap-cell-empty)");
  cells.forEach(function(cell) {
    var color = mode === "absolute" ? cell.dataset.absColor : cell.dataset.relColor;
    if (color) {
      cell.style.backgroundColor = rgbToRgba(color, 0.35);
      cell.style.borderLeft = "4px solid " + color;
    }
  });

  var miniCanvas = document.querySelector(".mini-canvas");
  if (miniCanvas && currentData) {
    redrawMiniCanvas(miniCanvas);
  }

  document.getElementById("btn-absolute").classList.toggle("active", mode === "absolute");
  document.getElementById("btn-relative").classList.toggle("active", mode === "relative");
}

function pad2(n) {
  return n < 10 ? "0" + n : "" + n;
}

function escapeHtml(s) {
  var div = document.createElement("div");
  div.textContent = s;
  return div.innerHTML;
}

// tooltip
var _activeTooltipCell = null;
var _hideTooltipTimer = null;
var _isTouch = false;

function populateTooltip(d) {
  var tip = document.getElementById("tooltip");
  var votes = parseInt(d.votes);
  var votesStr = votes >= 1000 ? (votes / 1000).toFixed(1) + "K" : "" + votes;
  var yearStr = d.year ? " (" + d.year + ")" : "";
  var imdbUrl = d.tconst ? "https://www.imdb.com/title/" + d.tconst + "/" : "";
  var linkStart = imdbUrl ? '<a href="' + imdbUrl + '" target="_blank" class="tooltip-link">' : "";
  var linkEnd = imdbUrl ? "</a>" : "";
  tip.innerHTML =
    "<strong>" + d.season + "x" + pad2(parseInt(d.episode)) + " " +
    linkStart + escapeHtml(d.title) + linkEnd +
    "</strong>" + yearStr + "<br>" +
    d.rating + "/10 &middot; " + votesStr + " votes";
}

function showTooltip(e) {
  if (_isTouch) return;
  clearTimeout(_hideTooltipTimer);
  _activeTooltipCell = e.currentTarget;
  populateTooltip(e.currentTarget.dataset);
  var tip = document.getElementById("tooltip");
  var rect = e.currentTarget.getBoundingClientRect();
  tip.style.left = rect.left + "px";
  tip.style.top = (window.scrollY + rect.top - tip.offsetHeight - 6) + "px";
  tip.style.display = "block";
  tip.style.top = (window.scrollY + rect.top - tip.offsetHeight - 6) + "px";
}

function moveTooltip(e) {}

function hideTooltip() {
  if (_isTouch) return;
  _hideTooltipTimer = setTimeout(function() {
    _activeTooltipCell = null;
    document.getElementById("tooltip").style.display = "none";
  }, 300);
}

function keepTooltip() {
  clearTimeout(_hideTooltipTimer);
}

function dismissTooltip() {
  _activeTooltipCell = null;
  document.getElementById("tooltip").style.display = "none";
}

function handleCellTap(e) {
  _isTouch = true;
  e.preventDefault();
  var cell = e.currentTarget;
  var tip = document.getElementById("tooltip");
  if (_activeTooltipCell === cell) {
    dismissTooltip();
    return;
  }
  _activeTooltipCell = cell;
  populateTooltip(cell.dataset);
  var rect = cell.getBoundingClientRect();
  tip.style.display = "block";
  tip.style.left = rect.left + "px";
  tip.style.top = (window.scrollY + rect.top - tip.offsetHeight - 6) + "px";
}

// ---- export ----

function exportImage() {
  if (!currentData || !currentData.episodes || currentData.episodes.length === 0) return;

  var eps = currentData.episodes;
  var title = currentData.title || "Untitled";
  var dark = isDarkMode();
  var palette = getPalette();

  var maxSeason = 0, maxEp = 0, maxRank = 0;
  var lookup = {};
  for (var i = 0; i < eps.length; i++) {
    if (eps[i].season > maxSeason) maxSeason = eps[i].season;
    if (eps[i].episode > maxEp) maxEp = eps[i].episode;
    if (eps[i].ratingRank > maxRank) maxRank = eps[i].ratingRank;
    lookup[eps[i].season + "," + eps[i].episode] = eps[i];
  }

  var cellW = 110;
  var cellH = 48;
  var barW = 5;
  var gap = 1;
  var titleH = 50;
  var padX = 16;
  var padY = 12;

  var gridW = maxSeason * (cellW + gap) - gap;
  var gridH = maxEp * (cellH + gap) - gap;
  var canvasW = gridW + padX * 2;
  var canvasH = titleH + gridH + padY * 2;

  var canvas = document.createElement("canvas");
  canvas.width = canvasW;
  canvas.height = canvasH;
  var ctx = canvas.getContext("2d");

  ctx.fillStyle = dark ? "#1a1a2e" : "#f8f9fa";
  ctx.fillRect(0, 0, canvasW, canvasH);

  ctx.fillStyle = dark ? "#e0e0e0" : "#212529";
  ctx.font = "bold 20px -apple-system, BlinkMacSystemFont, sans-serif";
  ctx.textBaseline = "middle";
  ctx.fillText(title, padX, titleH / 2 + padY / 2);

  var ox = padX;
  var oy = titleH;

  for (var s = 1; s <= maxSeason; s++) {
    for (var ep = 1; ep <= maxEp; ep++) {
      var d = lookup[s + "," + ep];
      var cx = ox + (s - 1) * (cellW + gap);
      var cy = oy + (ep - 1) * (cellH + gap);

      ctx.fillStyle = dark ? "#252540" : "#ffffff";
      ctx.fillRect(cx, cy, cellW, cellH);

      if (!d || d.rating === null || d.rating === undefined) continue;

      var color = currentMode === "absolute"
        ? absoluteColor(d.rating)
        : relativeColor(d.ratingRank, maxRank);

      ctx.fillStyle = rgbToRgba(color, 0.35);
      ctx.fillRect(cx, cy, cellW, cellH);

      ctx.fillStyle = color;
      ctx.fillRect(cx, cy, barW, cellH);

      ctx.fillStyle = dark ? "#e0e0e0" : "#212529";

      ctx.font = "bold 11px -apple-system, BlinkMacSystemFont, sans-serif";
      ctx.textBaseline = "top";
      ctx.fillText(s + "x" + pad2(ep), cx + barW + 3, cy + 3);

      var epTitle = d.title || "";
      if (epTitle.length > 18) epTitle = epTitle.substring(0, 16) + "\u2026";
      ctx.font = "9px -apple-system, BlinkMacSystemFont, sans-serif";
      ctx.globalAlpha = 0.7;
      ctx.fillText(epTitle, cx + barW + 3, cy + 17);
      ctx.globalAlpha = 1;

      ctx.font = "bold 11px -apple-system, BlinkMacSystemFont, sans-serif";
      ctx.textBaseline = "top";
      var ratingText = d.rating.toFixed(1);
      var ratingW = ctx.measureText(ratingText).width;
      ctx.fillText(ratingText, cx + cellW - ratingW - 4, cy + 3);
    }
  }

  var a = document.createElement("a");
  var slug = title.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/(^-|-$)/g, "");
  a.download = slug + ".png";
  a.href = canvas.toDataURL("image/png");
  a.click();
}

// init
document.addEventListener("DOMContentLoaded", function() {
  var tip = document.getElementById("tooltip");
  tip.addEventListener("mouseenter", keepTooltip);
  tip.addEventListener("mouseleave", dismissTooltip);
  document.addEventListener("click", function(e) {
    if (_activeTooltipCell && !e.target.closest(".heatmap-cell") && !e.target.closest(".heatmap-tooltip")) {
      dismissTooltip();
    }
  });

  var prefersDark = window.matchMedia("(prefers-color-scheme: dark)").matches;
  setTheme(prefersDark);

  window.matchMedia("(prefers-color-scheme: dark)").addEventListener("change", function(e) {
    setTheme(e.matches);
  });

  document.getElementById("btn-theme").addEventListener("click", function() {
    setTheme(!isDarkMode());
  });
  document.getElementById("btn-absolute").addEventListener("click", function() { recolor("absolute"); });
  document.getElementById("btn-relative").addEventListener("click", function() { recolor("relative"); });
  document.getElementById("btn-export").addEventListener("click", exportImage);
  document.getElementById("btn-back").addEventListener("click", goBack);
});
