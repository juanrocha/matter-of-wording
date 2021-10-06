library(tidyverse)
library(rscopus)
library(fs)
library(here)

## ES data does not have abstracts
dat <- read_csv(
    file = "data/210609_Scopus_ecosystem-servic*_OR_nature-contributions-to-people_1-20k.csv") %>% 
    janitor::clean_names()

## RS data has no abstracts
rs_dat <- read_csv(
    file = "data/20210609_regime-shifts.csv"
)


## Read the API key settings and throttling rates: https://dev.elsevier.com/api_key_settings.html
## For abstract retrival 10k per week at max 9 requests per second.
names(dat)

txt <- dat %>% select(link, eid, doi) %>% head(1) %>% 
    pull(eid) %>% 
    abstract_retrieval(identifier = "eid")


### It does not return abstracts! I probably need the institutional authentication

class(txt)
names(txt)
txt$content$`abstracts-retrieval-response`$item$bibrecord$head$abstracts 

## For it to work, it needs to be part of a list!
z <- list(txt)

purrr::map_chr(z, function(x){ x$content$`abstracts-retrieval-response`$item$bibrecord$head$abstracts })
## It does work from the university!!


#### Compile all data and extract paper ids for recovering abstracts ####

fls <- dir_ls(path = "data/")

dat <- map(
    fls,
    function(x) {
        x |> read_csv() |> janitor::clean_names()
    }
)

abst <- dat |> map(ncol) > 19 # data frames with 19 columns do not have abstracts

tags <- names(dat[abst]) |> 
    str_remove(pattern = "data/") |> 
    str_remove_all(pattern = "[:digit:]") |> 
    str_remove("_") |> # only the first one
    str_remove(".csv")

df_abs <- dat[abst] |> 
    map(function(x) select(x, doi, abstract)) |> 
    map2( .y = tags, function(x,y) {
        x$tag <- y
        return(x)} ) |> 
    bind_rows()

df_abs |> nest(tags = tag) # 7346 unique abstracts


tags_no <- names(dat[!abst]) |> 
    str_remove(pattern = "data/") |> 
    str_remove_all(pattern = "[:digit:]") |> 
    str_remove("_") |> # only the first one
    str_remove(".csv")

df_lack <- dat[!abst] |> 
    map(function(x) select(x, doi)) |> 
    map2( .y = tags_no, function(x,y) {
        x$tag <- y
        return(x)} ) |> 
    bind_rows()

df_lack <- df_lack |> nest(tags = tag) # 46374 unique papers without abstracts

save(df_lack, file = "data/missing_abstracts.RData")

