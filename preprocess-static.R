#!/usr/bin/env Rscript
suppressMessages({
    library(optparse)
    library(data.table)
    library(jsonlite)
})

option.list <- list(
    make_option(c("-r", "--reload"), action="store_true", default=FALSE,
                help="Re-download IMDb dataset files")
)
parser <- OptionParser(option_list=option.list)
opts <- parse_args(parser)

############################################################
##### ensure RDS exists

rds.path <- "app/data/episodes.rds"
if (!file.exists(rds.path)) rds.path <- "../app/data/episodes.rds"
if (!file.exists(rds.path)) {
    stop("episodes.rds not found. Run app/preprocess.R first.")
}

cat("Loading data... ")
episodes <- readRDS(rds.path)
cat(sprintf("%s episodes\n", format(nrow(episodes), big.mark=",")))

############################################################
##### compute global stats

GLOBAL_MEDIAN <- median(episodes$rating, na.rm=TRUE)
rating_ecdf <- ecdf(episodes$rating[!is.na(episodes$rating)])
PCTILE_RATINGS <- seq(1.0, 10.0, by=0.1)
PCTILE_VALUES  <- rating_ecdf(PCTILE_RATINGS)

cat(sprintf("Global median: %.1f\n", GLOBAL_MEDIAN))

############################################################
##### build show index

cat("Building show index... ")
show_index <- episodes[order(season, episode), .(
    t  = showTitle[1],
    ot = showOrigTitle[1],
    n  = .N,
    r  = round(mean(rating, na.rm=TRUE), 1),
    v  = sum(votes, na.rm=TRUE),
    y1 = min(year, na.rm=TRUE),
    y2 = max(year, na.rm=TRUE),
    rs = list(rating),
    sb = list(which(diff(season) != 0))
), by=.(i = parentTconst)]

# drop ot if same as t (saves space — most shows are English)
show_index[ot == t, ot := NA_character_]

show_index[is.infinite(y1), `:=`(y1=NA_integer_, y2=NA_integer_)]
setorder(show_index, -v)

# estimate columnar bytes per show: ~46 bytes/ep + 50 overhead
show_index[, estBytes := n * 46 + 50]

cat(sprintf("%s shows\n", format(nrow(show_index), big.mark=",")))

############################################################
##### assign tiers: preload (tier 0) + ~1MB chunks

PRELOAD_COUNT <- 50
CHUNK_TARGET  <- 1000000  # ~1MB per chunk

# tier 0: top 50 shows
show_index[, tier := NA_integer_]
show_index[1:min(PRELOAD_COUNT, .N), tier := 0L]

# remaining shows: fill chunks of ~1MB
current_tier <- 1L
current_size <- 0
for (idx in (PRELOAD_COUNT + 1):nrow(show_index)) {
    if (current_size >= CHUNK_TARGET) {
        current_tier <- current_tier + 1L
        current_size <- 0
    }
    set(show_index, idx, "tier", current_tier)
    current_size <- current_size + show_index$estBytes[idx]
}

n_tiers <- max(show_index$tier) + 1
cat(sprintf("Assigned %d tiers (preload + %d chunks)\n", n_tiers, n_tiers - 1))

############################################################
##### write shows.json

cat("Writing shows.json... ")
shows_json <- list(
    config = list(
        globalMedian = GLOBAL_MEDIAN,
        pctileRatings = PCTILE_RATINGS,
        pctileValues = PCTILE_VALUES
    ),
    shows = lapply(seq_len(nrow(show_index)), function(idx) {
        row <- show_index[idx]
        list(
            i  = row$i,
            t  = row$t,
            ot = row$ot,
            n  = row$n,
            r  = row$r,
            v  = row$v,
            y1 = row$y1,
            y2 = row$y2,
            tier = row$tier,
            rs = row$rs[[1]],
            sb = row$sb[[1]]
        )
    })
)
write(toJSON(shows_json, auto_unbox=TRUE, na="null", digits=4), "data/shows.json")
cat(sprintf("%.1f MB\n", file.size("data/shows.json") / 1e6))

############################################################
##### write tiered episode files (columnar format)

cat("Writing episode tiers...\n")

for (tier_num in sort(unique(show_index$tier))) {
    ids <- show_index[tier == tier_num, i]
    tier_data <- list()
    for (id in ids) {
        eps <- episodes[parentTconst == id][order(season, episode)]
        tier_data[[id]] <- list(
            s = eps$season,
            e = eps$episode,
            t = eps$title,
            y = eps$year,
            r = eps$rating,
            v = eps$votes,
            c = eps$tconst
        )
    }
    fname <- sprintf("data/episodes-%d.json", tier_num)
    write(toJSON(tier_data, auto_unbox=TRUE, na="null", digits=2), fname)
    cat(sprintf("  tier %2d: %5d shows, %6.0f KB\n", tier_num,
        length(ids), file.size(fname) / 1000))
}

############################################################
##### write locale files from title.akas

datasets.dir <- "datasets"
if (!dir.exists(datasets.dir)) datasets.dir <- "../datasets"
akas.gz <- file.path(datasets.dir, "title.akas.tsv.gz")

if (file.exists(akas.gz)) {
    cat("Generating locale files...\n")

    # fast filter with python
    ids_file <- tempfile()
    writeLines(show_index$i, ids_file)
    filtered_file <- tempfile(fileext=".tsv")
    system2("python3", c("-c", shQuote(sprintf(
        "import gzip\nids=set(open('%s').read().strip().split('\\n'))\nwith gzip.open('%s','rt',encoding='utf-8',errors='replace') as f:\n h=f.readline()\n with open('%s','w') as o:\n  o.write(h.replace('\\x00',''))\n  [o.write(l.replace('\\x00','')) for l in f if l.split('\\t',1)[0] in ids]",
        ids_file, akas.gz, filtered_file))))

    # strip NULs and read
    clean_file <- tempfile(fileext=".tsv")
    system2("tr", c("-d", r"('\0')"), stdin=filtered_file, stdout=clean_file)
    sa <- fread(clean_file, na.strings=r"(\N)", quote="", fill=TRUE)
    unlink(c(ids_file, filtered_file, clean_file))

    # non-ASCII titles that differ from primary
    na_rows <- sa[!is.na(title) & title != "" & grepl("[^[:ascii:]]", title, perl=TRUE)]
    primary <- unique(episodes[, .(parentTconst, showTitle)])
    na_rows <- primary[na_rows, on=c(parentTconst="titleId")]
    na_rows <- na_rows[tolower(title) != tolower(showTitle)]

    # region -> locale
    r2l <- c(RU="ru", JP="ja", KR="ko", CN="zh", TW="zh",
             DE="de", FR="fr", UA="uk", BG="bg", GR="el",
             IL="he", IN="hi", IR="fa", TR="tr", HU="hu",
             CZ="cs", PL="pl", TH="th", VN="vi", BR="pt",
             SA="ar", AE="ar", EG="ar", RS="sr", HR="hr",
             RO="ro", SE="sv", FI="fi", DK="da", NO="nb")
    na_rows[, locale := r2l[region]]
    na_rows <- na_rows[!is.na(locale)]

    dir.create("data/locale", showWarnings=FALSE)
    for (loc in unique(na_rows$locale)) {
        best <- na_rows[locale == loc, .(title = title[which.min(nchar(title))]), by=parentTconst]
        out <- setNames(as.list(best$title), best$parentTconst)
        fname <- sprintf("data/locale/%s.json", loc)
        write(toJSON(out, auto_unbox=TRUE), fname)
        cat(sprintf("  %s: %5d shows, %4.0f KB\n", loc, nrow(best), file.size(fname)/1000))
    }
} else {
    cat("Skipping locales (title.akas.tsv.gz not found)\n")
}

cat("Done.\n")
