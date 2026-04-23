// Crameri palettes for sparklines
var SPARK_MANAGUA = ["#ffcf67","#d38c50","#a5563f","#6c2d3a","#4d3463","#5267a6","#67a0d3","#80e7fe"];
var SPARK_ROMA    = ["#7d1700","#a05e1a","#bd9a3c","#d1de98","#9ce2d4","#45accc","#2472b4","#023098"];

function sparkGetPalette() {
  return document.documentElement.classList.contains("dark") ? SPARK_ROMA : SPARK_MANAGUA;
}

function sparkInterpolateColor(palette, t) {
  t = Math.max(0, Math.min(1, t));
  var n = palette.length - 1;
  var i = Math.floor(t * n);
  var f = t * n - i;
  if (i >= n) return palette[n];
  var c0 = sparkHexToRgb(palette[i]);
  var c1 = sparkHexToRgb(palette[i + 1]);
  return "rgb(" +
    Math.round(c0.r + f * (c1.r - c0.r)) + "," +
    Math.round(c0.g + f * (c1.g - c0.g)) + "," +
    Math.round(c0.b + f * (c1.b - c0.b)) + ")";
}

function sparkHexToRgb(hex) {
  var n = parseInt(hex.slice(1), 16);
  return { r: (n >> 16) & 255, g: (n >> 8) & 255, b: n & 255 };
}

function sparkAbsoluteT(rating) {
  if (typeof PCTILE_RATINGS === "undefined") return (rating - 1) / 9;
  for (var i = 0; i < PCTILE_RATINGS.length - 1; i++) {
    if (rating <= PCTILE_RATINGS[i + 1]) {
      var f = (rating - PCTILE_RATINGS[i]) / (PCTILE_RATINGS[i + 1] - PCTILE_RATINGS[i]);
      return PCTILE_VALUES[i] + f * (PCTILE_VALUES[i + 1] - PCTILE_VALUES[i]);
    }
  }
  return 1;
}

function drawSparkline(canvas, ratings, seasonBreaks) {
  var ctx = canvas.getContext("2d");
  var w = canvas.width;
  var h = canvas.height;
  var pad = 2;
  var palette = sparkGetPalette();

  var pts = [];
  for (var i = 0; i < ratings.length; i++) {
    if (ratings[i] !== null && ratings[i] !== undefined) {
      pts.push({ x: i, y: ratings[i] });
    }
  }
  if (pts.length < 2) return;

  var minR = Infinity, maxR = -Infinity;
  for (var i = 0; i < pts.length; i++) {
    if (pts[i].y < minR) minR = pts[i].y;
    if (pts[i].y > maxR) maxR = pts[i].y;
  }
  if (maxR - minR < 0.5) { minR -= 0.25; maxR += 0.25; }

  var xScale = (w - 2 * pad) / (ratings.length - 1);
  var yScale = (h - 2 * pad) / (maxR - minR);

  function xPos(idx) { return pad + idx * xScale; }
  function yPos(val) { return h - pad - (val - minR) * yScale; }

  ctx.clearRect(0, 0, w, h);

  // season break lines
  if (seasonBreaks && seasonBreaks.length > 0) {
    var isDark = document.documentElement.classList.contains("dark");
    ctx.strokeStyle = isDark ? "rgba(255,255,255,0.15)" : "rgba(0,0,0,0.12)";
    ctx.lineWidth = 1;
    for (var i = 0; i < seasonBreaks.length; i++) {
      var bx = xPos(seasonBreaks[i] - 0.5);
      ctx.beginPath();
      ctx.moveTo(bx, 0);
      ctx.lineTo(bx, h);
      ctx.stroke();
    }
  }

  // fill area to median baseline
  var mid = typeof GLOBAL_MEDIAN !== "undefined" ? GLOBAL_MEDIAN : 7.6;
  var midY = yPos(Math.max(minR, Math.min(maxR, mid)));

  ctx.beginPath();
  ctx.moveTo(xPos(pts[0].x), midY);
  for (var i = 0; i < pts.length; i++) {
    ctx.lineTo(xPos(pts[i].x), yPos(pts[i].y));
  }
  ctx.lineTo(xPos(pts[pts.length - 1].x), midY);
  ctx.closePath();

  var avgRating = 0;
  for (var i = 0; i < pts.length; i++) avgRating += pts[i].y;
  avgRating /= pts.length;
  var fillColor = sparkInterpolateColor(palette, sparkAbsoluteT(avgRating));
  ctx.fillStyle = fillColor.replace("rgb(", "rgba(").replace(")", ",0.2)");
  ctx.fill();

  // colored line segments
  ctx.lineWidth = 1.5;
  for (var i = 0; i < pts.length - 1; i++) {
    var segRating = (pts[i].y + pts[i + 1].y) / 2;
    ctx.strokeStyle = sparkInterpolateColor(palette, sparkAbsoluteT(segRating));
    ctx.beginPath();
    ctx.moveTo(xPos(pts[i].x), yPos(pts[i].y));
    ctx.lineTo(xPos(pts[i + 1].x), yPos(pts[i + 1].y));
    ctx.stroke();
  }
}

// search results handler
Shiny.addCustomMessageHandler("searchResults", function(msg) {
  var data = typeof msg === "string" ? JSON.parse(msg) : msg;
  var container = document.getElementById("search-results");
  container.innerHTML = "";

  if (!data || data.length === 0) {
    container.style.display = "none";
    return;
  }
  container.style.display = "block";

  data.forEach(function(show) {
    var row = document.createElement("div");
    row.className = "search-result-row";
    row.dataset.id = show.parentTconst;

    var info = document.createElement("div");
    info.className = "search-result-info";
    var yearStr = "";
    if (show.yearMin && show.yearMax) {
      yearStr = show.yearMin === show.yearMax
        ? " (" + show.yearMin + ")"
        : " (" + show.yearMin + "\u2013" + show.yearMax + ")";
    }
    info.innerHTML =
      '<div class="search-result-title">' + escapeHtmlSpark(show.showTitle) + yearStr + "</div>" +
      '<div class="search-result-meta">' +
        show.nEpisodes + " eps &middot; " + show.avgRating + " avg" +
      "</div>";

    var canvas = document.createElement("canvas");
    canvas.className = "search-result-sparkline";
    canvas.width = 140;
    canvas.height = 32;
    drawSparkline(canvas, show.ratings, show.seasonBreaks);

    row.appendChild(info);
    row.appendChild(canvas);
    container.appendChild(row);

    row.addEventListener("click", function() {
      Shiny.setInputValue("selectedShow", show.parentTconst, { priority: "event" });
    });
  });
});

function escapeHtmlSpark(s) {
  var div = document.createElement("div");
  div.textContent = s;
  return div.innerHTML;
}
