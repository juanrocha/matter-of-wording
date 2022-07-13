# correct tags
# I made a mistake in 01-read_data.R: line 82 assign wrong tags to files. Recreating
# the index and correcting tags. I cannot re-run from script 01 because then I lose
# all downloaded abstracts.

library(tidyverse)
library(here)
library(fs)
library(tictoc)

fls <- dir_ls(path = "data/", recurse = TRUE) |> 
    str_subset( ".csv")

tags <- fls |> 
    str_replace_all(pattern = "data/regime_shifts_lit/", replacement = "rs-") |>
    str_replace_all(pattern = "data/", replacement = "es-") |> 
    str_remove_all(pattern = "[:digit:]") |> 
    str_remove("_") |> # only the first one
    str_remove(".csv") |> 
    str_remove("_no-abstracts")

# remove the tags of the regime shifts and ecosystem service general searches, 
# not useful for matching

tags[c(1,34,35)] <- NA

tic()
all_files <- map(
    fls,
    function(x) {
        x |> read_csv(col_types = "cccccccccccccc") |> janitor::clean_names()
    })
toc()

all_files <- all_files |> 
    map(function(x) select(x, doi)) |> 
    map2( .y = tags, function(x,y) {
        x$tag <- y
        return(x)} ) |> 
    bind_rows()
