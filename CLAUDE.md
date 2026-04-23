# TV Ratings

Personal tool for visualizing IMDb episode ratings as heatmaps.

## Structure

- `show-ids.csv` — database of show short-names → IMDb title IDs (tt-codes)
- `get-id.R` — search IMDb for a show and add it to show-ids.csv (v1, broken by IMDb WAF)
- `get-reviews.R` — scrape IMDb episode ratings via Selenium and generate heatmap plots (v1)
- `get-reviews-v2.R` — fetch ratings from IMDb bulk datasets (TSVs) instead of scraping (v2)
- `plots/` — full-size PNG heatmaps (season × episode grid, color-coded by rating)
- `minis/` — compact PNG heatmaps (no text, just colored grid)
- `htmls/` — cached IMDb HTML pages (v1 only, gitignored)

## v1 vs v2

v1 (`get-reviews.R`) scrapes IMDb with Selenium/Firefox. Fragile: CSS selectors break on
IMDb redesigns, WAF blocks non-browser requests, slow (5s sleep per page).

v2 (`get-reviews-v2.R`) uses IMDb's published bulk datasets (https://datasets.imdbws.com/)
— no scraping, no Selenium, no bot detection. Downloads TSV files, filters to the target
show, and plots. Same output format (plots/ and minis/).

## v3: Shiny app

`app/` contains a Shiny web app for interactive browsing of all ~45K TV shows on IMDb.

- `app/preprocess.R` — downloads IMDb datasets, joins episodes+ratings+titles, saves to `app/data/episodes.rds`
- `app/app.R` — Shiny app: live search with sparklines, interactive heatmap detail view
- `app/www/` — client-side JS (sparkline.js, heatmap.js) and CSS (style.css)
- `app/data/episodes.rds` — preprocessed data (~19MB, gitignored)

Run: `cd app && Rscript preprocess.R && Rscript -e 'shiny::runApp(".")'`

Search is debounced client-side (300ms), results sorted by total votes. Sparklines and
heatmaps rendered client-side in JS for speed. Heatmap supports absolute (RdYlBu) and
relative (Blues) color modes via toggle.

## v4: Static site

`site/` contains a fully static version — no server needed.

- `site/preprocess-static.R` — generates JSON data files from the RDS
- `site/index.html` — single page app
- `site/app.js` — orchestrator: data loading, search, show selection
- `site/sparkline.js` — sparkline rendering (from v3, Shiny handlers removed)
- `site/heatmap.js` — heatmap rendering (from v3, Shiny handlers removed)
- `site/style.css` — same as v3
- `site/data/shows.json` — 45K show index with sparkline data (~7.7MB, gitignored)
- `site/data/episodes/{id}.json` — per-show episode detail (~45K files, gitignored)

Run: `cd site && Rscript preprocess-static.R && python3 -m http.server 8080`
Deploy: any static host (GitHub Pages, Netlify, S3, etc.)

## Conventions

- R with data.table, ggplot2, cowplot
- Show tags are short kebab-case names (e.g. `breaking-bad`, `bob-burgers`)
- Output filenames: `{show-tag}-{imdb-id}.png`
- Plots use RdYlBu palette (stable/absolute mode) or Blues palette (relative/rank mode)
