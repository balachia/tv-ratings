library(shiny)
library(data.table)
library(jsonlite)

############################################################
##### global: load data once at startup

episodes <- readRDS("data/episodes.rds")
setkey(episodes, parentTconst)

# compute global median and percentile breakpoints for color mapping
GLOBAL_MEDIAN_RATING <- median(episodes$rating, na.rm=TRUE)
# fine-grained percentile table: rating → percentile (0-1)
# use ecdf for exact mapping
rating_ecdf <- ecdf(episodes$rating[!is.na(episodes$rating)])
# sample at every 0.1 increment from 1.0 to 10.0
RATING_PCTILE_RATINGS <- seq(1.0, 10.0, by=0.1)
RATING_PCTILE_VALUES  <- rating_ecdf(RATING_PCTILE_RATINGS)

# pre-compute show index for search
# include season boundaries for sparkline season-split lines
show_index <- episodes[order(season, episode), .(
    showTitle  = showTitle[1],
    nEpisodes  = .N,
    avgRating  = round(mean(rating, na.rm=TRUE), 1),
    totalVotes = sum(votes, na.rm=TRUE),
    yearMin    = min(year, na.rm=TRUE),
    yearMax    = max(year, na.rm=TRUE),
    ratings    = list(rating),
    seasons    = list(season)
), by = parentTconst]
# fix Inf/-Inf from shows with no year data
show_index[is.infinite(yearMin), `:=`(yearMin=NA_integer_, yearMax=NA_integer_)]
show_index[, titleLower := tolower(showTitle)]

cat(sprintf("Loaded %s episodes across %s shows (median rating: %.1f)\n",
    format(nrow(episodes), big.mark=","),
    format(nrow(show_index), big.mark=","),
    GLOBAL_MEDIAN_RATING))

############################################################
##### UI

ui <- fluidPage(
    tags$head(
        tags$link(rel="stylesheet", href="style.css"),
        tags$script(src="sparkline.js"),
        tags$script(src="heatmap.js")
    ),

    div(class="top-bar",
        h2("TV Ratings Explorer"),
        tags$button(id="btn-theme", class="btn-theme")
    ),

    div(class="search-wrapper",
        tags$input(
            id = "search-input",
            type = "text",
            placeholder = "Search for a TV show...",
            oninput = "searchDebounce(this.value)"
        ),
        tags$button(id="btn-clear", class="btn-clear", type="button",
                    onclick="clearSearch()", "\u00d7")
    ),

    tags$script(HTML(sprintf("
        var GLOBAL_MEDIAN = %.2f;
        var PCTILE_RATINGS = %s;
        var PCTILE_VALUES  = %s;
        function clearSearch() {
            var inp = document.getElementById('search-input');
            inp.value = '';
            inp.focus();
            Shiny.setInputValue('searchQuery', '', {priority: 'event'});
        }
        var _searchTimer = null;
        function searchDebounce(val) {
            clearTimeout(_searchTimer);
            _searchTimer = setTimeout(function() {
                Shiny.setInputValue('searchQuery', val, {priority: 'event'});
            }, 300);
        }
    ", GLOBAL_MEDIAN_RATING,
       toJSON(RATING_PCTILE_RATINGS),
       toJSON(RATING_PCTILE_VALUES)))),

    div(id="search-results", class="search-results-container"),

    div(id="detail-view", class="detail-container", style="display:none;",
        div(class="detail-header",
            tags$button(id="btn-back", "\u2190 Back"),
            h3(id="detail-title"),
            div(class="toggle-group",
                tags$button(id="btn-absolute", class="btn active", "Absolute"),
                tags$button(id="btn-relative", class="btn", "Relative"),
                tags$button(id="btn-export", class="btn", "\u2913 Export")
            )
        ),
        div(id="heatmap-container", class="heatmap-container"),
        div(id="tooltip", class="heatmap-tooltip", style="display:none;")
    )
)

############################################################
##### server

server <- function(input, output, session) {

    observeEvent(input$searchQuery, {
        query <- input$searchQuery
        if (is.null(query) || nchar(trimws(query)) < 2) {
            session$sendCustomMessage("searchResults", "[]")
            return()
        }

        q <- tolower(trimws(query))
        matches <- show_index[grepl(q, titleLower, fixed=TRUE)]
        matches <- head(matches[order(-totalVotes)], 10)

        result <- lapply(seq_len(nrow(matches)), function(i) {
            # compute season boundary indices for sparkline
            s <- matches$seasons[[i]]
            boundaries <- which(diff(s) != 0)
            list(
                parentTconst = matches$parentTconst[i],
                showTitle    = matches$showTitle[i],
                nEpisodes    = matches$nEpisodes[i],
                avgRating    = matches$avgRating[i],
                yearMin      = matches$yearMin[i],
                yearMax      = matches$yearMax[i],
                ratings      = matches$ratings[[i]],
                seasonBreaks = boundaries
            )
        })
        session$sendCustomMessage("searchResults", toJSON(result, auto_unbox=TRUE))
    })

    observeEvent(input$selectedShow, {
        show_id <- input$selectedShow
        eps <- episodes[.(show_id)]
        eps <- eps[!is.na(rating)]
        eps[, ratingRank := frank(rating, ties.method="dense", na.last="keep")]

        show_title <- eps$showTitle[1]

        detail <- list(
            title = show_title,
            parentTconst = show_id,
            medianRating = GLOBAL_MEDIAN_RATING,
            episodes = eps[order(season, episode),
                          .(tconst, season, episode, title, year, rating, votes, ratingRank)]
        )
        session$sendCustomMessage("showDetail", toJSON(detail, auto_unbox=TRUE))
    })
}

shinyApp(ui, server)
