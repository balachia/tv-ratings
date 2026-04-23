#!/usr/bin/env Rscript
suppressMessages({
    library(optparse)
    library(data.table)
})

option.list <- list(
    make_option(c("-r", "--reload"), action="store_true", default=FALSE,
                help="Re-download IMDb dataset files")
)
parser <- OptionParser(option_list=option.list)
opts <- parse_args(parser)

############################################################
##### download datasets

dataset.dir <- "../datasets"
if (!dir.exists(dataset.dir)) dir.create(dataset.dir)

dataset.files <- list(
    episodes = "title.episode.tsv.gz",
    ratings  = "title.ratings.tsv.gz",
    basics   = "title.basics.tsv.gz"
)
dataset.base <- "https://datasets.imdbws.com/%s"

for (name in names(dataset.files)) {
    fname <- dataset.files[[name]]
    local.path <- file.path(dataset.dir, fname)
    if (opts$reload || !file.exists(local.path)) {
        cat(sprintf("Downloading %s... ", fname))
        download.file(sprintf(dataset.base, fname), local.path, mode="wb", quiet=TRUE)
        cat("done\n")
    }
}

############################################################
##### load and join

cat("Loading episodes... ")
episodes <- fread(
    cmd = sprintf("gunzip -c %s", file.path(dataset.dir, dataset.files$episodes)),
    na.strings = "\\N"
)
cat(sprintf("%s rows\n", format(nrow(episodes), big.mark=",")))

cat("Loading ratings... ")
ratings <- fread(
    cmd = sprintf("gunzip -c %s", file.path(dataset.dir, dataset.files$ratings)),
    na.strings = "\\N"
)
cat(sprintf("%s rows\n", format(nrow(ratings), big.mark=",")))

cat("Loading basics... ")
basics <- fread(
    cmd = sprintf("gunzip -c %s", file.path(dataset.dir, dataset.files$basics)),
    select = c("tconst", "primaryTitle", "startYear"),
    na.strings = "\\N",
    quote = ""
)
cat(sprintf("%s rows\n", format(nrow(basics), big.mark=",")))

# inner join: only episodes that have ratings
cat("Joining... ")
dat <- episodes[ratings, on="tconst", nomatch=0]

# filter to valid season/episode numbers
dat <- dat[!is.na(seasonNumber) & !is.na(episodeNumber)]
dat[, seasonNumber := as.integer(seasonNumber)]
dat[, episodeNumber := as.integer(episodeNumber)]
dat <- dat[episodeNumber >= 1]

# join episode titles + year
dat <- basics[dat, on="tconst"]
setnames(dat, c("primaryTitle", "startYear"), c("title", "year"))
dat[, year := as.integer(year)]

# join show titles (don't need year for the parent show here)
show.titles <- basics[dat[, .(parentTconst = unique(parentTconst))], on=c(tconst="parentTconst")]
setnames(show.titles, c("tconst", "primaryTitle"), c("parentTconst", "showTitle"))
show.titles[, startYear := NULL]
dat <- show.titles[dat, on="parentTconst"]

# clean up columns
dat <- dat[, .(parentTconst, tconst, showTitle, season=seasonNumber, episode=episodeNumber,
               title, year, rating=averageRating, votes=numVotes)]

cat(sprintf("done: %s episodes across %s shows\n",
    format(nrow(dat), big.mark=","),
    format(uniqueN(dat$parentTconst), big.mark=",")))

############################################################
##### save

out.path <- "data/episodes.rds"
saveRDS(dat, out.path)
cat(sprintf("Saved to %s (%.1f MB)\n", out.path, file.size(out.path) / 1e6))
