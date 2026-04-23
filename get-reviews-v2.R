#!/usr/bin/env Rscript
suppressMessages({
    library(optparse)
    library(data.table)
    library(ggplot2)
    library(cowplot)
    library(purrr)
    library(stringr)
})

# load show ids
shows.dt <- fread("./show-ids.csv")

option.list <- list(
    make_option(c("-r", "--reload"), action="store_true", default=FALSE,
                help="Re-download IMDb dataset files"),
    make_option(c("-s", "--stable"), action="store_true", default=FALSE,
                help="Use absolute rating scale (1-10) instead of relative rank"),
    make_option(c("-t", "--text"), action="store_true", default=FALSE,
                help="Print text plot to terminal"),
    make_option(c("-m", "--mini"), action="store_true", default=FALSE,
                help="Generate mini plot"),
    make_option(c("-a", "--all"), action="store_true", default=FALSE,
                help="Generate plots for all shows in database")
    )
parser <- OptionParser(option_list=option.list)
arguments <- parse_args(parser, positional_arguments = c(0, Inf))
opts <- arguments$options
show.args <- arguments$args

# if text plotting, check for 'crayon'
if (opts$text && !(suppressMessages(require("crayon", quietly=TRUE)))) {
    stop("can't load 'crayon' package for text plotting")
}

# resolve which shows to process
if (opts$all) {
    show.list <- shows.dt$show
} else if (length(show.args) > 0) {
    show.list <- show.args
} else {
    stop("provide show name(s) or use --all")
}

############################################################
##### IMDb dataset download + caching

dataset.dir <- "./datasets"
if (!dir.exists(dataset.dir)) dir.create(dataset.dir)

dataset.files <- list(
    episodes = "title.episode.tsv.gz",
    ratings  = "title.ratings.tsv.gz",
    basics   = "title.basics.tsv.gz"
)
dataset.base <- "https://datasets.imdbws.com/%s"

download.datasets <- function(reload=FALSE) {
    for (name in names(dataset.files)) {
        fname <- dataset.files[[name]]
        local.path <- file.path(dataset.dir, fname)
        if (reload || !file.exists(local.path)) {
            cat(sprintf("Downloading %s...\n", fname))
            download.file(sprintf(dataset.base, fname), local.path, mode="wb", quiet=TRUE)
        }
    }
}

load.datasets <- function() {
    cat("Loading datasets... ")
    episodes <- fread(
        cmd = sprintf("gunzip -c %s", file.path(dataset.dir, dataset.files$episodes)),
        na.strings = "\\N"
    )
    ratings <- fread(
        cmd = sprintf("gunzip -c %s", file.path(dataset.dir, dataset.files$ratings)),
        na.strings = "\\N"
    )
    basics <- fread(
        cmd = sprintf("gunzip -c %s", file.path(dataset.dir, dataset.files$basics)),
        select = c("tconst", "primaryTitle"),
        na.strings = "\\N",
        quote = ""
    )
    cat("done\n")
    list(episodes=episodes, ratings=ratings, basics=basics)
}

get.show.data <- function(show.id, datasets) {
    eps <- datasets$episodes[parentTconst == show.id]
    eps <- eps[!is.na(seasonNumber) & !is.na(episodeNumber)]
    eps[, seasonNumber := as.numeric(seasonNumber)]
    eps[, episodeNumber := as.numeric(episodeNumber)]

    # join ratings
    eps <- datasets$ratings[eps, on=c(tconst="tconst")]

    # join episode titles
    eps <- datasets$basics[eps, on="tconst"]

    # rename to match v1 conventions
    setnames(eps, c("seasonNumber", "episodeNumber", "averageRating", "numVotes", "primaryTitle"),
                  c("season", "num", "rating", "votes", "name"))

    # get show title
    show.title <- datasets$basics[tconst == show.id, primaryTitle]

    list(dat=eps, title=show.title)
}

############################################################
##### utility (shared with v1)

wrap.text <- function(x, n=10, split='\n    ') {
    x %>% map(strwrap, width=n, exdent=2) %>% map(paste, collapse='\n')
}

############################################################
##### plotting

plot.show <- function(show.dat, show.title, show.show, show.id, opts) {
    # add plot columns
    show.dat[, wrapped.title := name %>% map(strwrap, width=20, exdent=2) %>% map(paste, collapse='\n')]
    show.dat[, season.label := sprintf('%dx%02d', season, num)]
    show.dat[, rating.label := sprintf('%0.1f', rating)]
    show.dat[, rating.rank := frank(rating, ties.method='dense', na.last='keep')]
    show.dat[, `:=`(sl=season-0.5, sr=season+0.5, nt=num-0.5, nb=num+0.5)]
    show.dat[, season.nna := sum(!is.na(rating)), by=season]

    # drop episode 0s and empty seasons
    show.dat <- show.dat[num >= 1]
    show.dat <- show.dat[season.nna > 0]

    if (nrow(show.dat) == 0) {
        cat(" no rated episodes, skipping\n")
        return(invisible(NULL))
    }

    nseason <- show.dat[, max(season)]
    neps <- show.dat[, max(num)]

    show.title.wrapped <- strwrap(show.title, 10*nseason) %>% paste0(collapse='\n')

    # fill settings
    if(opts$stable) {
        fill.var <- as.name('rating')
        fill.palette <- 'RdYlBu'
        fill.values <- c(0, ( (6.5-1) / (10-1) ), 1)
        fill.limits <- c(1,10)
    } else {
        fill.var <- as.name('rating.rank')
        fill.palette <- 'Blues'
        fill.values <- c(0,1)
        fill.limits <- NULL
    }

    lbar <- 0.1
    rect.border.size <- 0.5
    text.nudge.x <- 0.02
    text.nudge.y <- -0.05
    text.size <- 3
    ggp <- ggplot(show.dat, aes(season, y=num, fill=!!fill.var)) +
        theme_void() +
        coord_cartesian(expand=FALSE) +
        geom_rect(aes(xmin=sl, xmax=sl+lbar, ymin=nt, ymax=nb), color='white', size=0) +
        geom_rect(aes(xmin=sl, xmax=sr, ymin=nt, ymax=nb), alpha=0.5, color='white', size=rect.border.size) +
        geom_text(aes(x=sl+lbar, y=nt, label=season.label), hjust=0, vjust=1, nudge_x=text.nudge.x, nudge_y=text.nudge.y, lineheight=0.9, size=text.size) +
        geom_text(aes(x=sl+lbar, y=nt, label=wrapped.title), hjust=0, vjust=1, nudge_x=text.nudge.x, nudge_y=text.nudge.y-0.25, lineheight=0.9, size=0.75*text.size) +
        geom_text(aes(x=sr, y=nt, label=rating.label), hjust=1, vjust=1, nudge_x=-text.nudge.x, nudge_y=text.nudge.y, lineheight=0.9, size=text.size) +
        scale_y_reverse() +
        scale_fill_distiller(type='seq', direction=1, palette=fill.palette, values=fill.values, limits=fill.limits) +
        guides(fill="none") +
        NULL

    ggp.title <- ggdraw() +
        draw_label(show.title.wrapped, fontface='bold', x=0, hjust=0) +
        theme(plot.margin=margin(0, 0, 0, 0.5, 'cm')) +
        NULL
    ggp.combined <- plot_grid(ggp.title, ggp, ncol=1, rel_heights=c(1, neps))

    ggsave(sprintf('plots/%s-%s.png', show.show, show.id), ggp.combined, height=0.5+neps*0.5, width=nseason*1, limitsize = FALSE)

    # mini
    if(opts$mini) {
        mini.rect.border.size <- 0.25
        ggp.mini <- ggplot(show.dat, aes(season, y=num, fill=!!fill.var)) +
            theme_void() +
            coord_cartesian(expand=FALSE) +
            geom_rect(aes(xmin=sl, xmax=sr, ymin=nt, ymax=nb), color='white', size=mini.rect.border.size) +
            scale_y_reverse() +
            scale_fill_distiller(type='seq', direction=1, palette=fill.palette, values=fill.values, limits=fill.limits) +
            guides(fill="none") +
            NULL
        ggsave(sprintf('minis/%s-%s.png', show.show, show.id), ggp.mini, height=neps*0.25, width=nseason*0.25, units='cm', limitsize = FALSE)
    }

    # text plot
    if(opts$text) {
        fill.var.char <- as.character(fill.var)
        if(opts$stable) {
            gradient.limits <- c(1, 6.5, 10)
        } else {
            data.range <- show.dat[, ..fill.var.char] %>% range(na.rm=TRUE)
            gradient.limits <- data.range
        }
        pal <- scales::gradient_n_pal(scales::brewer_pal(palette=fill.palette, direction=1)(7), values=gradient.limits)
        show.dat[, char := '\u2588']
        show.dat[, text.color := pal(get(fill.var.char))]
        show.dat[is.na(text.color), `:=`(text.color='grey50', char='\u2591')]
        show.dat$text.color %>%
            imap_chr(~ make_style(.x)(show.dat$char[.y])) %>%
            set(show.dat, j='text', value=.)
        disp.dat <- CJ(season=1:nseason, num=1:neps)[show.dat, text := i.text]
        disp.dat[is.na(text), text := " "]
        disp.dat <- disp.dat[, .(text=paste0(text, collapse="")), by=season]
        disp.dat$text %>%
            paste0(collapse='\n') %>%
            cat()
        cat("\n")
    }
}

############################################################
##### main

download.datasets(reload=opts$reload)
datasets <- load.datasets()

for (show.show in show.list) {
    show.id <- shows.dt[J(show.show), id, on="show"]
    if (is.na(show.id)) {
        cat(sprintf("'%s' not in database, skipping\n", show.show))
        next
    }
    cat(sprintf("%s", show.show))

    result <- get.show.data(show.id, datasets)
    if (nrow(result$dat) == 0) {
        cat(" — no episodes found, skipping\n")
        next
    }

    show.title <- result$title
    cat(sprintf(" (%s, %d eps)\n", show.title, nrow(result$dat)))

    plot.show(result$dat, show.title, show.show, show.id, opts)
}
