# libraries
library(tidytext)
library(tidyverse)
library(tictoc)

load("data/abstracts_combined_220708.RData")

l <- df_abstracts |> 
    filter(is.na(doi)) |> # there are 1057 docs with missing doi
    nrow()

# This creates a provisional ID that enable matching later the results.
df_abstracts$doi[is.na(df_abstracts$doi)] <- paste("NA", 1:l, sep = "_")

## extra words
too_words <- tibble(
    word = c("paper", "study", "aim", "aims", "objective", "purpose", "elsevier", "taylor", "francis", "©", "john", "wiley", "sons", "springer", "verlag", "b.v", "abstract", "press", "reserved", "rights", "author", "taylor", "francis", "elsevier", "i.e.", "e.g.", "publisher", "publishers", "published", "publishing", "ii", "iv", "mdpi", "copyright", "journal", "auhors", "blackwell", "oxford", "cambridge", "publisher", "university", "book", "volume", "gmbh", "The Author(s).")
) |> 
    ## add stop words spanish
    bind_rows(tibble(word = tm::stopwords("spanish")))

## Document term matrix:
tic()
dtm <- df_abstracts %>% 
    filter(abstract != "[No abstract available]") |> # some missing values coded differently
    select(-tags) |> # this is the tag column
    unnest_tokens(word, abstract) %>% 
    filter(!is.na(word)) %>% ## 
    anti_join(stop_words) %>% 
    anti_join(too_words) %>%
    filter(!str_detect(word, "[:digit:]")) %>%
    #mutate(word = textstem::lemmatize_words(word)) %>% 
    group_by(id) %>%
    count(word, sort = TRUE) %>%
    bind_tf_idf(term = word, document = id, n = n) |> 
    cast_dtm(document = id, term = word, value = n) #, weighting = tm::weightTfIdf
toc() # 150 sec

dtm # documents: 62240, terms: 118466

save(dtm, file = "data/dtm.RData")
