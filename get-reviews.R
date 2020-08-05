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
})

# load show ids
shows.dt <- fread('./show-ids.csv')

option.list <- list(
    make_option(c('-r', '--reload'), action='store_true', default=FALSE),
    make_option(c('-s', '--stable'), action='store_true', default=FALSE),
    make_option(c('-m', '--mini'), action='store_true', default=FALSE)
    )
parser <- OptionParser(option_list=option.list)
arguments <- parse_args(parser, positional_arguments = 1)
opts <- arguments$options
show.show <- arguments$args

# usage: ./get-reviews.R SHOW-TAG
#args <- commandArgs(trailingOnly=TRUE)
#show.show <- args[1]
show.id <- shows.dt[J(show.show), id, on='show']

if(is.na(show.id)) {
    stop(sprintf('Show "%s" not in database', show.show))
}

imdb.base <- 'http://www.imdb.com%s'
title.base <- 'http://www.imdb.com/title/%s/'
episode.base <- 'http://www.imdb.com/title/%s/episodes?season=%d'
local.title.base <- './htmls/%s.html'
local.episode.base <- './htmls/%s.%d.html'

# download helpers
download.show <- function(url, verbose=TRUE) {
    if(verbose) {
        cat('Downloading show page\n')
    }
    read_html(url)
}

download.season <- function(url, season, verbose=TRUE) {
    if(verbose) {
        #cat(sprintf('Downloading season %s\n', season))
        cat(sprintf('%s ', season))
    }
    read_html(url)
}

pull.max.season <- function(show.page) {
    show.page %>% 
        html_nodes('#title-episode-widget .seasons-and-year-nav div') %>% 
        `[[`(3) %>%
        html_nodes('a') %>%
        html_text() %>%
        .[[1]] %>%
        as.numeric
}

pull.season.episode.pages <- function(show.page) {
    seasons <- show.page %>% 
        html_nodes('#title-episode-widget .seasons-and-year-nav div') %>% `[[`(3)
    season.order <- seasons %>% 
        html_nodes('a') %>% 
        html_text %>% as.numeric

    # extract and format urls
    season.pages <- seasons %>% html_nodes('a') %>% html_attr('href')
    season.pages <- sprintf(imdb.base, season.pages)

    # reset order and names
    names(season.pages) <- season.order
    season.pages <- season.pages[order(season.order)]
    season.pages
}

pull.title <- function(show.page) {
    show.page %>% 
        html_node('.titleBar h1') %>%
        html_text() %>%
        trimws()
}

pull.episode.stats <- function(season.page, season=NA) {
    empty.to.na <- function(x) if(is_empty(x)) NA else x
    episodes <- season.page %>% html_nodes('#episodes_content .clear .list.detail.eplist .list_item .info')
    ep.nums <- episodes %>% html_nodes('meta[itemprop="episodeNumber"]') %>% html_attr('content') %>% as.numeric
    ep.names <- episodes %>% html_nodes('strong a[itemprop="name"]') %>% html_text
    ep.airdates <- episodes %>% html_nodes('.airdate') %>% html_text %>% trimws
    ep.ratings <- episodes %>%
        map(~html_nodes(., '.ipl-rating-widget .ipl-rating-star.small .ipl-rating-star__rating')) %>%
        map(html_text) %>%
        map(empty.to.na) %>%
        map_dbl(as.numeric)
    ep.votes <- episodes %>%
        map(~html_nodes(., '.ipl-rating-widget .ipl-rating-star.small .ipl-rating-star__total-votes')) %>%
        map(html_text) %>%
        map(~str_remove_all(., '[(),]')) %>%
        map(empty.to.na) %>%
        map_dbl(as.numeric)
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

# try local title page
title.local <- sprintf(local.title.base, show.id)
must.dl <- opts$reload || (!file.exists(title.local))
title.url <- if(must.dl) sprintf(title.base, show.id) else title.local

# get show page, and save if needed
title.page <- title.url %>% download.show
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

cat(sprintf('%d season%s, downloading ', max.season, if(max.season==1) '' else 's'))
season.pages <- season.page.urls %>% imap(download.season)
res <- if(must.dl.eps) { map2(season.pages, seasons.local, ~ write_html(.x, .y)) }
cat('\n')

# get season pages
#season.pages <- title.page %>% pull.season.episode.pages %>% imap(download.season)

show.dat <- season.pages %>% imap(pull.episode.stats) %>% rbindlist

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
    guides(fill=FALSE) +
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

ggsave(sprintf('plots/%s-%s.png', show.show, show.id), ggp.combined, height=0.5+neps*0.5, width=nseason*1)


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
        guides(fill=FALSE) +
        NULL
    ggsave(sprintf('minis/%s-%s.png', show.show, show.id), ggp.mini, height=neps*0.25, width=nseason*0.25, units='cm')
}


