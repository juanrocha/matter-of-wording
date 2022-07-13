## Recover missing abstracts
## This script only works from an IP based on campus
## Juan Rocha
## 211006

library(tidyverse)
library(fs)
library(here)
library(rscopus)
library(tictoc)

#J211103: Try to collect all missing values within a list of dois. There is ~6k
# for records after 40k, but there is NAs in previous rounds.


load("data/missing_abstracts.RData")


res <- list() # response list

dois <- df_lack |> 
    slice(40001:46374) |> # API allows 10k per week, change index every week
    pull(doi)

## Run again with missing
# dois <- df_30_40k |> 
#     filter(is.na(abstract)) |> 
#     pull(doi)


tic()
for (i in seq_along(dois)){
    res[[i]] <- abstract_retrieval(id = dois[i], identifier = "doi") 
    Sys.sleep(0.2)
}
toc() #6845s / 2hrs | 9598.665 / 2.6hrs

## The res (response) object is over 8.7GB, not point of saving it all, only the
## abstracts which are already >20MB for 1000 records. It does however return
## full reference information. Good to know for future work.

## abs_safe extract the abstract safely, if there was an error in the call, it returns null.
abs_safe <- safely(
    function(x){ 
        x$content$`abstracts-retrieval-response`$item$bibrecord$head$abstracts 
    }
)


abs_safe(res[[597]])

abstracts <- res |> 
    map(abs_safe)
    
abstracts[[597]] # an example of an error, it should be stored as NULL

abstracts <- transpose(abstracts)

df_extra2 <- tibble(
    doi = dois,
    abstract = abstracts$result[1:length(dois)]) |> 
    unnest(abstract, keep_empty = TRUE) ## only 3857 records :(

df_extra2 |> filter(!is.na(abstract)) # 9829 records ;) | 29 records on the 30-40k dataset!
# J211125: still 1013 missing 

save(df_extra2, file = "data/abstracts_extra2.RData")

### Collecting missing values ###
load("data/abstracts_1_10k.RData")
load("data/abstracts_10_20k.RData")
load("data/abstracts_20_30k.RData")
load("data/abstracts_30_40k.RData")
load("data/abstracts_40_46k.RData")
load("data/abstracts_4extra1.RData")
load("data/abstracts_extra2.RData")
load("data/abstracts_scopus.RData")

df_1_10k |> filter(is.na(abstract)) #63
df_10_20k |> filter(is.na(abstract)) #>6k
df_20_30k |> filter(is.na(abstract)) # 171
df_30_40k |> filter(is.na(abstract)) #200
df_40_46k |> filter(is.na(abstract)) # 384
df_extra |> filter(is.na(abstract)) # 0

## Lets recover missing abstracts:
# dois <- df_10_20k |>
#     filter(is.na(abstract))  |> 
#     pull(doi)

dois <- bind_rows(
    df_1_10k |> filter(is.na(abstract)), #63
    df_10_20k |> filter(is.na(abstract)), #>6k
    df_20_30k |> filter(is.na(abstract)), # 171
    df_30_40k |> filter(is.na(abstract)), #200
    df_40_46k |> filter(is.na(abstract)), # 384
    ) |> pull(doi) # 6961

## Once everything is downloaded, the following is use to compile a unique dataset:
df_abstracts <- bind_rows(
    df_1_10k, df_10_20k, df_20_30k, df_30_40k, df_40_46k,
    df_extra, df_extra2) |> 
    filter(!is.na(abstract))

## Need to recover the tags:
df_abstracts <- df_abstracts |> 
    left_join(df_lack) |> 
    bind_rows(df_abs |> nest(tags =  tag)) |> 
    filter(!is.na(abstract))

## I need to add id column to abstracts because there are duplicates but useful: 
## there are book chapters coded with tags that all share the same doi
df_abstracts |> 
    group_by(doi) |> 
    add_count() |> 
    arrange(desc(n)) |> 
    filter(n > 1) |> print(n=100) |> 
    unnest(cols)
#filter(abstract == "[No abstract available]")
## I cant discard by doi because some are books where a doc is a book chapter.
df_abstracts |> filter(abstract == "[No abstract available]")

df_abstracts <- df_abstracts |> 
    mutate(id = row_number())

save(df_abstracts, file = "data/abstracts_combined.RData")


df_abstracts
