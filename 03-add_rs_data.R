library(tidyverse)
library(here)
library(fs)


# all_fls <- dir_ls("data/", recurse = TRUE) |> 
#     str_subset(".csv")
# 
# dat <-  map(
#     all_fls,
#     function(x) {
#         x |> read_csv(col_types = "cccccccccccccc") |> janitor::clean_names()
#     })
# 
# dat <- dat |> bind_rows() #140563 papers
# 
# dat <-  dat |> unique() # 83984 papers
# 
# tags <- all_fls |> 
#     str_remove(pattern = "data/") |> 
#     str_remove(pattern = "regime_shifts_lit/") |> 
#     str_remove_all(pattern = "[:digit:]") |> 
#     str_remove("_") |> # only the first one
#     str_remove(".csv")

## On June 2022 I realize many regime shift abstracts were not analyzed.

load("data/abstracts_combined_211125.RData")

rs_fls <- dir_ls("data/regime_shifts_lit/")

rs_abs <- map(
    rs_fls,
    function(x) {
        x |> read_csv(col_types = "cccccccccccccc") |> janitor::clean_names()
    })


tags <- rs_fls |> 
    str_remove(pattern = "data/regime_shifts_lit/") |> 
    str_remove_all(pattern = "[:digit:]") |> 
    str_remove("_") |> # only the first one
    str_remove(".csv")

rs_abs <- rs_abs |> 
    map(function(x) select(x, doi, abstract)) |> 
    map2( .y = tags, function(x,y) {
        x$tag <- y
        return(x)} ) |> 
    bind_rows()

rs_abs |> skimr::skim()

rs_abs |> pull(abstract) |> unique() |> length() # 9555 unique abstracts

df_abstracts |> pull(abstract) |> unique() |> length() #51149 unique abstracts

sum(!rs_abs$abstract %in% df_abstracts$abstract) # this means I need to run the topic model again.
# there are 9356 abstracts missing from RS that exist in df_abstracts
full_join(df_abstracts, rs_abs |> nest(tags = tag))  |>  # 68,639 rows
    unique()  # 62293

## There is still many documents wihtout doi, I need other field to do the matching
rs_abs |> 
    filter(is.na(doi))

rs_abs |> filter(is.na(abstract))

# another test, same result:  62293 papers total
df_abstracts <- bind_rows(df_abstracts, rs_abs |> nest(tags = tag)) |> 
    unique()

df_abstracts |> filter(is.na(abstract)) # this should be used for the topic model because it does 
# repeat rows if a paper has more than one tag.

df_abstracts <- df_abstracts |> filter(abstract != "[No abstract available]") # 62240

df_abstracts <- df_abstracts |> 
    mutate(id = row_number())

save(df_abstracts, file = "data/abstracts_combined_220708.RData")


