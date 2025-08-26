#!/usr/bin/env Rscript
suppressMessages({
    library(optparse)
    library(rvest)
    library(purrr)
    library(lubridate)
    library(stringr)
    library(data.table)
    library(ggplot2)
    library(cowplot)
    library(xml2)
    library(RSelenium)
})

# load show ids
shows.dt <- fread("./show-ids.csv")

option.list <- list(
    make_option(c("-r", "--reload"), action="store_true", default=FALSE),
    make_option(c("-s", "--stable"), action="store_true", default=FALSE),
    make_option(c("-t", "--text"), action="store_true", default=FALSE),
    make_option(c("-m", "--mini"), action="store_true", default=FALSE)
    )
parser <- OptionParser(option_list=option.list)
arguments <- parse_args(parser, positional_arguments = 1)
# arguments <- parse_args(parser, args=c("-smt", "brooklyn-99"), positional_arguments=1)
# arguments <- parse_args(parser, args=c("-smt", "vikings"), positional_arguments=1)
# arguments <- parse_args(parser, args=c("-smt", "simpsons"), positional_arguments=1)
# arguments <- parse_args(parser, args=c("-smt", "mare-of-easttown"), positional_arguments=1)
# arguments <- parse_args(parser, args=c("-smt", "bluey"), positional_arguments=1)
opts <- arguments$options
show.show <- arguments$args

# if text plotting, check for 'crayon'
if (opts$text && !(suppressMessages(require("crayon", quietly=TRUE)))) {
    stop("can't load 'crayon' package for text plotting")
}


# usage: ./get-reviews.R SHOW-TAG
#args <- commandArgs(trailingOnly=TRUE)
#show.show <- args[1]
show.id <- shows.dt[J(show.show), id, on="show"]

if(is.na(show.id)) {
    stop(sprintf("Show '%s' not in database", show.show))
}

imdb.base <- "http://www.imdb.com%s"
title.base <- "http://www.imdb.com/title/%s/"
episode.base <- "http://www.imdb.com/title/%s/episodes/?season=%d"
local.title.base <- "./htmls/%s.html"
local.episode.base <- "./htmls/%s.%d.html"

# download helpers
download.show <- function(url, driver = NULL, verbose=TRUE) {
    if(verbose) {
        cat("Downloading show page\n")
    }
    if(!is.null(driver)) {
        driver$navigate(url)
        Sys.sleep(5)
        html <- driver$getPageSource()[[1]]
        html <- read_html(html)
    } else {
        html <- read_html(url)
    }
    html
}

download.season <- function(url, season, driver = NULL, verbose=TRUE) {
    if(verbose) {
        #cat(sprintf('Downloading season %s\n', season))
        cat(sprintf("%s ", season))
    }
    if(!is.null(driver)) {
        driver$navigate(url)
        Sys.sleep(5)
        # try paginate
        paginate <- driver$findElements(using = "css", ".ipc-see-more__button")
        while(length(paginate) > 0) {
            paginate[[1]]$clickElement()
            Sys.sleep(5)
            paginate <- driver$findElements(using = "css", ".ipc-see-more__button")
        }
        # finally, download the page
        html <- driver$getPageSource()[[1]]
        html <- read_html(html)
    } else {
        html <- read_html(url)
    }
    html
}

pull.max.season <- function(show.page) {
    # show.page %>%
    #     html_nodes("#title-episode-widget .seasons-and-year-nav div") %>% 
    #     `[[`(3) %>%
    #     html_nodes("a") %>%
    #     html_text() %>%
    #     .[[1]] %>%
    #     as.numeric
    try.selector <- show.page |> 
        html_nodes("#browse-episodes-season > option")
    if(length(try.selector) > 0) {
        try.selector |>
            html_attr('value') |>
            sapply(\(.) suppressWarnings(as.integer(.))) |>
            Filter(f = \(.) !is.na(.), x = _) |>
            max()
    } else {
        # if we don't find a selector, assume we're dealing with single season show
        1
    }
}

pull.season.episode.pages <- function(show.page) {
    seasons <- show.page %>% 
        html_nodes("#title-episode-widget .seasons-and-year-nav div") %>% `[[`(3)
    season.order <- seasons %>% 
        html_nodes("a") %>% 
        html_text %>% as.numeric

    # extract and format urls
    season.pages <- seasons %>% html_nodes("a") %>% html_attr("href")
    season.pages <- sprintf(imdb.base, season.pages)

    # reset order and names
    names(season.pages) <- season.order
    season.pages <- season.pages[order(season.order)]
    season.pages
}

pull.title <- function(show.page) {
    show.page %>% 
        # html_node(".titleBar h1") %>%
        html_node("h1") %>%
        html_text() %>%
        trimws()
}

parse.vote.number <- function(x) {
    mult <- c('NA' = 1, K = 1e3, M = 1e6)
    matches <- str_match(x, '(\\d+\\.?\\d*)([MK])?$')
    mults <- matches[,3]
    mults[is.na(mults)] <- 'NA'
    mults <- mult[mults]
    as.numeric(matches[,2]) * mults
}

pull.episode.stats <- function(season.page, season=NA) {
    empty.to.na <- function(x) if(is_empty(x)) NA else x
    # episodes <- season.page %>% html_nodes("#episodes_content .clear .list.detail.eplist .list_item .info")
    episodes <- season.page %>% html_nodes(".episode-item-wrapper")

    # ep.nums <- episodes %>% html_nodes('meta[itemprop="episodeNumber"]') %>% html_attr("content") %>% as.numeric
    ep.nums <- seq_along(episodes)

    # ep.names <- episodes %>% html_nodes('strong a[itemprop="name"]') %>% html_text
    ep.names <- episodes |>
        html_nodes('.ipc-title__text') |>
        html_text() |>
        str_split(' ∙ ') |>
        sapply(\(.) .[2])

    try.parse.airdates.ratings <- function(x) {
        if(length(x) == 2) {
            rvs <- str_match(trimws(html_text(x[[2]])),
                             "(\\d\\.\\d)/10\\s\\((.+)\\)")
            list(airdate = trimws(html_text(x[[1]])),
                 rating = as.numeric(rvs[, 2]),
                 votes = parse.vote.number(rvs[, 3]))
        } else {
            list(airdate = NA, rating = NA, votes = NA)
        }
    }

    # ep.airdates <- episodes %>% html_nodes(".airdate") %>% html_text %>% trimws
    ep.airdates.ratings <- episodes |>
        lapply(\(.) html_elements(., "div > div > div > div > span"))
    ep.airdates.ratings <- ep.airdates.ratings |>
        lapply(try.parse.airdates.ratings) |>
        purrr::transpose() |>
        lapply(unlist)

    # TODO: something seriously wrong with the dates imdb reports
    ep.airdates <- ep.airdates.ratings$airdate
    ep.ratings <- ep.airdates.ratings$rating
    ep.votes <- ep.airdates.ratings$votes

    # ep.ratings <- episodes %>%
    #     map(~html_nodes(., ".ipl-rating-widget .ipl-rating-star.small .ipl-rating-star__rating")) %>%
    #     map(html_text) %>%
    #     map(empty.to.na) %>%
    #     map_dbl(as.numeric)
    # ep.votes <- episodes %>%
    #     map(~html_nodes(., ".ipl-rating-widget .ipl-rating-star.small .ipl-rating-star__total-votes")) %>%
    #     map(html_text) %>%
    #     map(~str_remove_all(., "[(),]")) %>%
    #     map(empty.to.na) %>%
    #     map_dbl(as.numeric)
    data.table(season=as.numeric(season), num=ep.nums, name=ep.names, airdate=ep.airdates, rating=ep.ratings, votes=ep.votes)
}

############################################################
##### utility

word.splits <- function(xls, n=20) {
    last.break <- 0
    if(length(xls)>1) {
        cuts <- numeric(length(xls))
        for(idx in seq_along(xls)) {
            if(xls[idx] - last.break > n) { last.break <- xls[idx-1] }
            cuts[idx] <- last.break
        }
    } else {
        cuts <- 0
    }
    cuts
}

wrap.text <- function(x, n=10, split='\n    ') {
    split.merge <- function(x, at, split) {
        if(length(at) > 0) {
            map2_chr(c(0, at)+1, c(at, nchar(x)+1)-1, ~substr(x, .x, .y)) %>% paste(collapse=split)
        } else {
            x
        }
    }
    breaks <- x %>% str_locate_all('[ ]') %>% map(~.[,1]) %>% map2(map(x, nchar), ~ c(.x, .y))
    #cuts <- breaks %>% map(~ word.splits(., n)) %>% map(unique) %>% map(rev)
    cuts <- breaks %>% map(~ word.splits(., n)) %>% map(~ keep(., ~ . > 0)) %>% map(unique) %>% map(sort)
    x %>% map2(cuts, ~ split.merge(.x, .y, split))
    #words <- x %>% str_split(' ')
    #word.cuts <- words %>% map(nchar) %>% map(~ .+1) %>% map(cumsum) %>% map(splitter)
}



############################################################
##### plotting



############################################################
##### download

# TODO: be verbose about what we're downloading

remDr <- NULL

# try local title page
title.local <- sprintf(local.title.base, show.id)
must.dl <- opts$reload || (!file.exists(title.local))
title.url <- if(must.dl) sprintf(title.base, show.id) else title.local

if(must.dl && is.null(remDr)) {
    rD <- rsDriver(browser="firefox",
                   port = 4567L + sample(0:9, 1),
                   verbose=F,
                   phantomver=NULL, chromever=NULL, geckover = "latest")
    remDr <- rD[["client"]]
}

# get show page, and save if needed
title.page <- title.url |>
    download.show(driver = remDr)
res <- if(must.dl) { write_html(title.page, title.local) }

# get title
show.title <- title.page %>% pull.title()
cat('\t', show.title, '\n', sep='')

# get season pages
max.season <- title.page %>% pull.max.season()
# try local season pages
seasons.local <- sprintf(local.episode.base, show.id, 1:max.season)
must.dl.eps <- opts$reload || (!all(file.exists(seasons.local)))
season.page.urls <- if(must.dl.eps) sprintf(episode.base, show.id, 1:max.season) else seasons.local

if(must.dl.eps && is.null(remDr)) {
    rD <- rsDriver(browser="firefox",
                   port = 4567L + sample(0:9, 1),
                   verbose=F)
    remDr <- rD[["client"]]
}

cat(sprintf('%d season%s, downloading ', max.season, if(max.season==1) '' else 's'))
season.pages <- season.page.urls |>
    imap(download.season, driver = remDr)
res <- if(must.dl.eps) { map2(season.pages, seasons.local, ~ write_html(.x, .y)) }
cat('\n')

# get season pages
#season.pages <- title.page %>% pull.season.episode.pages %>% imap(download.season)

# show.dat <- season.pages %>% imap(pull.episode.stats) %>% rbindlist
show.dat <- season.pages |>
    imap(pull.episode.stats) |> 
    rbindlist()

############################################################
##### plot

# drop empty episodes
#show.dat <- show.dat[!is.na(rating)]

#show.dat[, wrapped.title := wrap.text(name, 20, '\n  ')]
show.dat[, wrapped.title := name %>% map(strwrap, width=20, exdent=2) %>% map(paste, collapse='\n')]
show.dat[, label := sprintf('S%dE%d\n%s', season, num, wrapped.title)]
show.dat[, season.label := sprintf('%dx%02d', season, num)]
#show.dat[, title.label := sprintf('S%dE%d\n%s', season, num, wrapped.title)]
show.dat[, rating.label := sprintf('%0.1f', rating)]
show.dat[, rating.rank := frank(rating, ties.method='dense', na.last='keep')]
show.dat[, `:=`(sl=season-0.5, sr=season+0.5, nt=num-0.5, nb=num+0.5)]

show.dat[, season.nna := sum(!is.na(rating)), by=season]

# drop empty seasons, and episode 0s
show.dat <- show.dat[num >= 1]
show.dat <- show.dat[season.nna > 0]

nseason <- show.dat[, max(season)]
neps <- show.dat[, max(num)]

# wrapped show title
show.title.wrapped <- strwrap(show.title, 10*nseason) %>% paste0(collapse='\n')
#print(show.title)
#print(show.title.wrapped)

# global rating ('stable') vs rank visualization
if(opts$stable) {
    # set rating variable
    #fill.var <- if(opts$stable) as.name('rating') else as.name('rating.rank')
    fill.var <- as.name('rating')
    #fill.palette <- 'RdBu'
    fill.palette <- 'RdYlBu'
    # midpoint at 6.5 ~ global mean/median rating
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
ggp <- ggplot(show.dat, aes_(~season, y=~num, fill=fill.var)) +
    theme_void() +
    coord_cartesian(expand=FALSE) +
    geom_rect(aes(xmin=sl, xmax=sl+lbar, ymin=nt, ymax=nb), color='white', size=0) +
    geom_rect(aes(xmin=sl, xmax=sr, ymin=nt, ymax=nb), alpha=0.5, color='white', size=rect.border.size) +
    #geom_text(aes(x=sl+lbar, y=nt), hjust=0, vjust=1, nudge_x=text.nudge.x, nudge_y=text.nudge.y, lineheight=0.9, size=text.size) +
    geom_text(aes(x=sl+lbar, y=nt, label=season.label), hjust=0, vjust=1, nudge_x=text.nudge.x, nudge_y=text.nudge.y, lineheight=0.9, size=text.size) +
    geom_text(aes(x=sl+lbar, y=nt, label=wrapped.title), hjust=0, vjust=1, nudge_x=text.nudge.x, nudge_y=text.nudge.y-0.25, lineheight=0.9, size=0.75*text.size) +
    geom_text(aes(x=sr, y=nt, label=rating.label), hjust=1, vjust=1, nudge_x=-text.nudge.x, nudge_y=text.nudge.y, lineheight=0.9, size=text.size) +
    scale_y_reverse() +
    scale_fill_distiller(type='seq', direction=1, palette=fill.palette, values=fill.values, limits=fill.limits) +
    #scale_fill_distiller(type='seq', direction=1, palette='Blues', values=c(0,1)) +
    guides(fill="none") +
    #labs(title=show.title) +
    NULL

#if(opts$stable) {
#    # midpoint at 6.5 ~ global mean/median rating
#    ggp <- ggp + scale_fill_distiller(type='seq', direction=1, palette='RdBu', values=c(0, ((6.5-1)/9), 1), limits=c(1,10))
#} else {
#    ggp <- ggp + scale_fill_distiller(type='seq', direction=1, palette='Blues', values=c(0,1))
#}

ggp.title <- ggdraw() +
    draw_label(show.title.wrapped, fontface='bold', x=0, hjust=0) +
    theme(plot.margin=margin(0, 0, 0, 0.5, 'cm')) +
    NULL
ggp.combined <- plot_grid(ggp.title, ggp, ncol=1, rel_heights=c(1, neps))

ggsave(sprintf('plots/%s-%s.png', show.show, show.id), ggp.combined, height=0.5+neps*0.5, width=nseason*1, limitsize = FALSE)


# mini
if(opts$mini) {
    lbar <- 0.1
    mini.rect.border.size <- 0.25
    ggp.mini <- ggplot(show.dat, aes_(~season, y=~num, fill=fill.var)) +
        theme_void() +
        coord_cartesian(expand=FALSE) +
        #geom_rect(aes(xmin=sl, xmax=sl+lbar, ymin=nt, ymax=nb), color='white', size=0) +
        geom_rect(aes(xmin=sl, xmax=sr, ymin=nt, ymax=nb), color='white', size=mini.rect.border.size) +
        scale_y_reverse() +
        scale_fill_distiller(type='seq', direction=1, palette=fill.palette, values=fill.values, limits=fill.limits) +
        guides(fill="none") +
        NULL
    ggsave(sprintf('minis/%s-%s.png', show.show, show.id), ggp.mini, height=neps*0.25, width=nseason*0.25, units='cm', limitsize = FALSE)
}

# text plot
if(opts$text) {
    # set up gradient
    fill.var.char <- as.character(fill.var)
    if(opts$stable) {
        gradient.limits <- c(1, 6.5, 10)
    } else {
        data.range <- show.dat[, ..fill.var.char] %>% range(na.rm=TRUE)
        gradient.limits <- data.range
    }
    pal <- scales::gradient_n_pal(scales::brewer_pal(palette=fill.palette, direction=1)(7), values=gradient.limits)
    # set up color styles
    show.dat[, char := '\u2588']
    show.dat[, text.color := pal(get(fill.var.char))]
    show.dat[is.na(text.color), `:=`(text.color='grey50', char='\u2591')]
    show.dat$text.color %>%
        imap_chr(~ make_style(.x)(show.dat$char[.y])) %>%
        set(show.dat, j='text', value=.)
    # expand to 
    disp.dat <- CJ(season=1:nseason, num=1:neps)[show.dat, text := i.text]
    disp.dat[is.na(text), text := " "]
    #disp.dat <- disp.dat[, .(text=paste0(text, collapse="")), by=num]
    disp.dat <- disp.dat[, .(text=paste0(text, collapse="")), by=season]
    disp.dat$text %>%
        paste0(collapse='\n') %>%
        cat()
    cat("\n")
}
