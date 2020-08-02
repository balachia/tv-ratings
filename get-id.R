#!/usr/bin/env Rscript

suppressMessages({
    library(rvest)
    library(data.table)
})

args <- commandArgs(trailingOnly=TRUE)

build.query <- function(x) {
    # build + standardize query
    query <- paste(x, collapse=' ')
    query <- chartr(' ', '+', query)
    query
}

if(args[1]=='search') {
    query <- build.query(tail(args, -1))
    add <- NULL
    cat(sprintf('searching: %s\n', query))
} else if(args[1]=='add') {
    query <- build.query(tail(args, -2))
    add <- args[2]
    cat(sprintf('adding "%s" from: %s\n', add, query))
} else {
    stop('usage: get-id.R [add|search] query terms')
}

#query <- 'avatar airbender'

search.base <- 'https://www.imdb.com/find?s=tt&q=%s'
url.base <- 'https://www.imdb.com/title/%s'

# get page
search.page <- read_html(sprintf(search.base, query))

# results table
search.texts <- search.page %>%
    html_nodes('.findList .result_text') %>%
    html_text()
search.links <- search.page %>%
    html_nodes('.findList td.result_text>a') %>%
    html_attr('href') %>%
    substring(8, 16)
show.id <- search.links[1]

ep.url <- sprintf(url.base, show.id)
cat(sprintf('%s\n%s\n', search.texts[1], ep.url))

# if adding
if(!is.null(add)) {
    show.dt <- fread('./show-ids.csv')
    if(add %in% show.dt$show) {
        show.dt.id <- show.dt[J(add), id, on='show']
        if(show.dt.id == show.id) {
            cat('show already in database\n')
        } else {
            cat(sprintf('not replacing item already in database\n%s\n', sprintf(url.base, show.dt.id)))
        }
    } else {
        show.dt.show <- show.dt[J(show.id), show, on='id']
        if(!is.na(show.dt.show)) {
            cat(sprintf('link already in database as %s\n', show.dt.show))
        } else {
            show.dt <- rbind(show.dt,
                             data.table(show=add, id=show.id))
            fwrite(show.dt, './show-ids.csv')
        }
    }

}
