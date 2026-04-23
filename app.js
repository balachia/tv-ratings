// ---- globals (set after shows.json loads) ----
var GLOBAL_MEDIAN;
var PCTILE_RATINGS;
var PCTILE_VALUES;

var showIndex = [];     // full show list
var showLookup = {};    // id -> show object
var loadedTiers = {};   // tier number -> {id: columnar data}
var loadingTiers = {};  // tier number -> Promise (in-flight loads)

// ---- data loading ----

function loadData() {
  var input = document.getElementById("search-input");
  input.placeholder = "Loading show database...";
  input.disabled = true;

  fetch("data/shows.json")
    .then(function(r) { return r.json(); })
    .then(function(data) {
      GLOBAL_MEDIAN  = data.config.globalMedian;
      PCTILE_RATINGS = data.config.pctileRatings;
      PCTILE_VALUES  = data.config.pctileValues;

      showIndex = data.shows;
      for (var i = 0; i < showIndex.length; i++) {
        showIndex[i]._titleLower = showIndex[i].t.toLowerCase();
        showLookup[showIndex[i].i] = showIndex[i];
      }

      input.placeholder = "Search for a TV show...";
      input.disabled = false;

      // preload tier 0
      loadTier(0);

      // check URL for deep link
      var linked = getShowFromURL();
      if (linked && showLookup[linked]) {
        selectShow(linked);
      } else {
        input.focus();
      }
    });
}

function loadTier(tierNum) {
  if (loadedTiers[tierNum]) return Promise.resolve();
  if (loadingTiers[tierNum]) return loadingTiers[tierNum];

  loadingTiers[tierNum] = fetch("data/episodes-" + tierNum + ".json")
    .then(function(r) { return r.json(); })
    .then(function(tierData) {
      loadedTiers[tierNum] = tierData;
      delete loadingTiers[tierNum];
    });

  return loadingTiers[tierNum];
}

function getEpisodes(showId) {
  var show = showLookup[showId];
  if (!show) return Promise.resolve(null);

  var tier = show.tier;
  return loadTier(tier).then(function() {
    var col = loadedTiers[tier][showId];
    if (!col) return null;

    var episodes = [];
    for (var i = 0; i < col.s.length; i++) {
      episodes.push({
        tconst:  col.c[i],
        season:  col.s[i],
        episode: col.e[i],
        title:   col.t[i],
        year:    col.y[i],
        rating:  col.r[i],
        votes:   col.v[i]
      });
    }
    return episodes;
  });
}

// ---- URL routing ----

function getShowFromURL() {
  var hash = window.location.hash;
  if (hash && hash.length > 1) {
    return hash.substring(1); // #tt0903747 -> tt0903747
  }
  return null;
}

function setURLForShow(id) {
  history.pushState(null, "", "#" + id);
}

function clearURL() {
  history.pushState(null, "", window.location.pathname);
}

// ---- search ----

var _searchTimer = null;
var _selectedIndex = -1;
var _currentMatches = [];

function searchDebounce(val) {
  clearTimeout(_searchTimer);
  _searchTimer = setTimeout(function() {
    performSearch(val);
  }, 300);
}

function clearSearch() {
  var inp = document.getElementById("search-input");
  inp.value = "";
  inp.focus();
  performSearch("");
}

function performSearch(query) {
  _selectedIndex = -1;
  if (!query || query.trim().length < 2) {
    _currentMatches = [];
    renderSearchResults([]);
    return;
  }
  var q = query.trim().toLowerCase();
  var matches = showIndex.filter(function(s) {
    return s._titleLower.indexOf(q) !== -1;
  });
  matches.sort(function(a, b) { return b.v - a.v; });
  matches = matches.slice(0, 10);
  _currentMatches = matches;
  renderSearchResults(matches);
}

function handleSearchKeydown(e) {
  var rows = document.querySelectorAll(".search-result-row");
  if (rows.length === 0) return;

  if (e.key === "ArrowDown") {
    e.preventDefault();
    _selectedIndex = Math.min(_selectedIndex + 1, rows.length - 1);
    updateSelectedRow(rows);
  } else if (e.key === "ArrowUp") {
    e.preventDefault();
    _selectedIndex = Math.max(_selectedIndex - 1, -1);
    updateSelectedRow(rows);
  } else if (e.key === "Enter") {
    e.preventDefault();
    if (_selectedIndex >= 0 && _selectedIndex < _currentMatches.length) {
      selectShow(_currentMatches[_selectedIndex].i);
    } else if (_currentMatches.length > 0) {
      selectShow(_currentMatches[0].i);
    }
  } else if (e.key === "Escape") {
    clearSearch();
  }
}

function updateSelectedRow(rows) {
  for (var i = 0; i < rows.length; i++) {
    rows[i].classList.toggle("search-result-selected", i === _selectedIndex);
  }
  if (_selectedIndex >= 0 && rows[_selectedIndex]) {
    rows[_selectedIndex].scrollIntoView({ block: "nearest" });
  }
}

function renderSearchResults(matches) {
  var container = document.getElementById("search-results");
  container.innerHTML = "";

  if (!matches || matches.length === 0) {
    container.style.display = "none";
    return;
  }
  container.style.display = "block";

  matches.forEach(function(s, idx) {
    var row = document.createElement("div");
    row.className = "search-result-row";

    var info = document.createElement("div");
    info.className = "search-result-info";
    var yearStr = "";
    if (s.y1 && s.y2) {
      yearStr = s.y1 === s.y2
        ? " (" + s.y1 + ")"
        : " (" + s.y1 + "\u2013" + s.y2 + ")";
    }
    info.innerHTML =
      '<div class="search-result-title">' + escapeHtml(s.t) + yearStr + "</div>" +
      '<div class="search-result-meta">' +
        s.n + " eps &middot; " + s.r + " avg" +
      "</div>";

    var canvas = document.createElement("canvas");
    canvas.className = "search-result-sparkline";
    canvas.width = 140;
    canvas.height = 32;
    drawSparkline(canvas, s.rs, s.sb);

    row.appendChild(info);
    row.appendChild(canvas);
    container.appendChild(row);

    row.addEventListener("click", function() {
      selectShow(s.i);
    });
  });
}

// ---- show detail ----

function selectShow(id) {
  setURLForShow(id);

  getEpisodes(id).then(function(episodes) {
    if (!episodes) return;

    computeRatingRank(episodes);

    var years = episodes.map(function(e) { return e.year; }).filter(function(y) { return y; });
    var yearRange = "";
    if (years.length > 0) {
      var minY = Math.min.apply(null, years);
      var maxY = Math.max.apply(null, years);
      yearRange = minY === maxY ? " (" + minY + ")" : " (" + minY + "\u2013" + maxY + ")";
    }

    var show = showLookup[id];
    var title = show ? show.t : "Unknown";

    document.getElementById("detail-title").textContent = title + yearRange;
    document.getElementById("detail-view").style.display = "block";
    document.getElementById("search-results").style.display = "none";

    buildHeatmap({
      title: title,
      parentTconst: id,
      episodes: episodes
    });
  });
}

function goBack() {
  clearURL();
  document.getElementById("detail-view").style.display = "none";
  var oldMini = document.getElementById("mini-view-wrapper");
  if (oldMini) oldMini.remove();
  document.getElementById("search-results").style.display = "block";
  document.getElementById("search-input").focus();
}

function computeRatingRank(episodes) {
  var rated = [];
  for (var i = 0; i < episodes.length; i++) {
    if (episodes[i].rating !== null && episodes[i].rating !== undefined) {
      rated.push(episodes[i].rating);
    }
  }
  rated.sort(function(a, b) { return a - b; });

  var unique = [];
  for (var i = 0; i < rated.length; i++) {
    if (i === 0 || rated[i] !== rated[i - 1]) unique.push(rated[i]);
  }
  var rankMap = {};
  for (var i = 0; i < unique.length; i++) {
    rankMap[unique[i]] = i + 1;
  }

  for (var i = 0; i < episodes.length; i++) {
    episodes[i].ratingRank = episodes[i].rating != null ? rankMap[episodes[i].rating] : null;
  }
}

// ---- init ----

document.addEventListener("DOMContentLoaded", function() {
  // keyboard nav in search
  document.getElementById("search-input").addEventListener("keydown", handleSearchKeydown);

  // "/" to focus search
  document.addEventListener("keydown", function(e) {
    if (e.key === "/" && document.activeElement !== document.getElementById("search-input")) {
      e.preventDefault();
      goBack();
    }
  });

  // browser back/forward
  window.addEventListener("popstate", function() {
    var id = getShowFromURL();
    if (id && showLookup[id]) {
      selectShow(id);
    } else {
      goBack();
    }
  });

  loadData();
});
