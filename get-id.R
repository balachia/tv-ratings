#!/usr/bin/env Rscript

suppressMessages({
    library(rvest)
    library(optparse)
    library(data.table)
    library(stringr)
})

option.list <- list(
    make_option(c('-n', '--noconfirm'), action='store_true', default=FALSE),
    make_option(c('-r', '--replace'), action='store_true', default=FALSE)
    )
parser <- OptionParser(option_list=option.list)
arguments <- parse_args(parser, positional_arguments = c(2, Inf))
opts <- arguments$options
add <- arguments$args[1]
query <- tail(arguments$args, -1)

#args <- commandArgs(trailingOnly=TRUE)

build.query <- function(x) {
    # build + standardize query
    query <- paste(x, collapse=' ')
    query <- chartr(' ', '+', query)
    query
}

# bind query terms into search string
query <- build.query(query)
cat(sprintf('%s "%s" from: %s\n', if(opts$replace) 'replacing' else 'adding', add, query))

#if(args[1]=='search') {
#    query <- build.query(tail(args, -1))
#    add <- NULL
#    cat(sprintf('searching: %s\n', query))
#} else if(args[1]=='add') {
#    query <- build.query(tail(args, -2))
#    add <- args[2]
#    cat(sprintf('adding "%s" from: %s\n', add, query))
#} else {
#    stop('usage: get-id.R [add|search] query terms')
#}

pretty.print.result <- function(x, id = NULL) {
    # url <- x |>
    #     html_nodes('a') |>
    #     html_attr('href') |> 
    #     substring(8, 16)
    url <- x |>
        html_nodes('a') |>
        html_attr('href') |>
        str_match('/title/(tt\\d+)/')
    url <- url[, 2]
    title <- x |>
        html_nodes('a') |>
        html_text()
    details <- x |>
        html_nodes('ul') |>
        (`[[`)(1) |>
        html_nodes('li') |> 
        sapply(html_text)
    if(!is.null(title)) title <- paste0(id, ') ', title)
    cat(sprintf('%s\n\t%s\n\t%s\n',
                title,
                paste(details, collapse = ' -- '),
                sprintf(url.base, url)
                ))
}

#query <- 'avatar airbender'
# query <- 'avatar airbender' |> build.query()

# search.base <- 'https://www.imdb.com/find?s=tt&q=%s'
# url.base <- 'https://www.imdb.com/title/%s'

search.base <- 'https://www.imdb.com/find/?q=%s'
url.base <- 'https://www.imdb.com/title/%s'

# get page
search.page <- read_html(sprintf(search.base, query))

# results table
# search.texts <- search.page %>%
#     html_nodes('.ipc-metadata-list-summary-item__c') %>%
#     html_text()
search.texts <- search.page %>%
    html_nodes('.ipc-metadata-list-summary-item__tc')
search.texts <- search.page %>%
    html_nodes('.ipc-metadata-list-summary-item__tc') |>
    (function(.) mapply(FUN = pretty.print.result, x = ., id = seq_along(.)))()
search.links <- search.page %>%
    html_nodes('.ipc-metadata-list-summary-item__t') %>%
    # html_nodes('.findList td.result_text>a') %>%
    html_attr('href') %>%
    str_match('/title/(tt\\d+)/')
search.links <- search.links[, 2]
# show.id <- search.links[1]

# confirm add
result.select <- 1
if(!opts$noconfirm) {
    # cat('correct? [Yn]')
    # confirmed <- readLines(con='stdin', n=1) %>%
    #     grepl('(^$)|(^[Yy])', .)
    # if(!confirmed) { quit() }
    cat('choice: ')
    selected <- readLines(con='stdin', n=1) |>
        as.integer()
    if(selected %in% seq_along(search.links)) {
        result.select <- selected
    } else {
        quit()
    }
}
show.id <- search.links[result.select]

ep.url <- sprintf(url.base, show.id)
# cat(sprintf('%s\n%s\n', search.texts[1], ep.url))
cat(sprintf('\n%s\n', ep.url))

# try to add
# is show or id in database already?
dt.fp <- './show-ids.csv'
show.dt <- fread(dt.fp)
show.tag.exists <- show.dt[J(add), id, on='show']
show.id.exists <- show.dt[J(show.id), show, on='id']

# show tag already in database
if(!is.na(show.tag.exists)) {
    if(show.tag.exists == show.id) {
        cat(sprintf('"%s" already in database\n', add))
    } else {
        prior.url <- sprintf(url.base, show.tag.exists)
        cat(sprintf('different "%s" already in database\n\t%s\n\t%sreplacing\n', add, prior.url, if(opts$replace) '' else 'not '))
        if(opts$replace) {
            show.dt[J(add), id := show.id, on='id']
            fwrite(show.dt, dt.fp)
        }
    }
    quit()
}
# show id already in database
if(!is.na(show.id.exists)) {
    cat(sprintf('link already in database as "%s"\n', show.id.exists))
    quit()
}
# else, add show
show.dt <- rbind(show.dt,
                 data.table(show=add, id=show.id))
fwrite(show.dt, dt.fp)

## if adding
#if(!is.null(add)) {
#    show.dt <- fread('./show-ids.csv')
#    if(add %in% show.dt$show) {
#        show.dt.id <- show.dt[J(add), id, on='show']
#        if(show.dt.id == show.id) {
#            cat('show already in database\n')
#        } else {
#            cat(sprintf('not replacing item already in database\n%s\n', sprintf(url.base, show.dt.id)))
#        }
#    } else {
#        show.dt.show <- show.dt[J(show.id), show, on='id']
#        if(!is.na(show.dt.show)) {
#            cat(sprintf('link already in database as %s\n', show.dt.show))
#        } else {
#            show.dt <- rbind(show.dt,
#                             data.table(show=add, id=show.id))
#            fwrite(show.dt, './show-ids.csv')
#        }
#    }

#}
