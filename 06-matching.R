library(tidytext)
library(tidyverse)
library(tm)
library (topicmodels)
library(lda)

load("data/models_gibbs.RData")
load("data/abstracts_combined_220708.RData")
# # documents: 62240, terms: 118466
k <- c(25,50,100,250,500) # best model was 250 topics = 4

df_gamma <- tidy(topicNumber.TM[[4]], matrix = "gamma") # document topics
df_beta <- tidy(topicNumber.TM[[4]], matrix = "beta") # topics terms

rm(topicNumber.TM) # you only need beta and gamma of the 4th model, after load you
# can remove to save memory

# documents and words (terms)
d <- 62240 
w <-  118466

# The sum of probabilities across topics is 1, and the sum of the matrix is 250 topics
# so the prob is 250/250 = 1 / number of words
tau_beta <- 1/w # probability of topic being random
# the sum of probabilities across documents is 1, and the sum of the full matrix is then 
# the number of rows (documents). thus a fair probability is the sum of gammas / the size of the matrix 
tau_gamma <- d/(d*250) # probability of document belonging to topic randomly
# reduce size in memory while matching
df_abstracts <- df_abstracts |> 
    select(-abstract) |> 
    unnest(tags)

tags <- df_abstracts |> pull(tag) |> unique()

# example
focus_docs <- df_abstracts |> 
    filter(tag == "culture") |> 
    pull(id) |> unique()

top5 <- df_gamma |> 
    filter(document %in% focus_docs) |> 
    group_by(topic) |> 
    summarize(mean_topic = mean(gamma)) |> 
    arrange(desc(mean_topic)) |> 
    ungroup() |> 
    top_n(n = 5, wt = mean_topic) |> 
    pull(topic)

df_gamma |> 
    filter(document %in% focus_docs) |> 
    filter(topic %in% top5) |> 
    mutate(topic = as_factor(topic)) |> 
    ggplot(aes(gamma, topic)) +
    geom_vline(xintercept = tau_gamma, color = "orange", linetype = 2) +
    geom_violin() + 
    scale_x_log10()

df_gamma |> 
    filter(document %in% focus_docs) |> 
    filter(topic %in% top5) |> 
    mutate(topic = as_factor(topic)) |> 
    ggplot(aes(document, topic)) +
    geom_tile(aes(fill = gamma)) +
    scale_fill_gradient2(name = expression(gamma), 
        low = "red", mid = "white", high = "blue", 
        midpoint = log10(tau_gamma), trans = "log10") +
    theme_light()


df_beta |> 
    filter(topic %in% top5) |> 
    group_by(topic) |> 
    top_n(n = 25, wt = beta) |> 
    ungroup() |> 
    arrange(topic, -beta) |> 
    mutate(term = reorder_within(term, beta, topic)) |> 
    ggplot(aes(beta,term)) + 
    geom_col() +
    #geom_vline(xintercept = tau_beta, color = "orange", linetype = 2) +
    scale_y_reordered() +
    facet_wrap(~topic, scales = "free_y")

## characteristic words
df_beta |>
    filter(topic %in% top5) |> 
    group_by(topic) |> 
    filter(beta > tau_beta) |> 
    summarize(words = n())
## so roughly speaking there is 2-3k words that characterize topics with prob > tau_beta == random.
## out of the 118k words in the vocab.

## characteristic topics
df_gamma |> 
    filter(document %in% focus_docs) |> 
    group_by(topic) |> 
    summarize(mean_topic = mean(gamma)) |> 
    arrange(desc(mean_topic)) |> 
    ungroup() |> 
    #filter(mean_topic > tau_gamma)   # 63 topics are characteristic... 
    ggplot(aes(mean_topic)) + geom_density() + 
    geom_vline(xintercept = tau_gamma, color = "red") +
    scale_x_log10()

# that is the probability that a word belongs to a topic at random
df_beta |> 
    ggplot(aes(beta)) + 
    geom_vline(xintercept = 1/w, color = "red") +
    geom_density() + 
    scale_x_log10() 

# and the probability that a topic was assigned to a document at random.
df_gamma |> 
    ggplot(aes(gamma)) +
    geom_density() +
    geom_vline(xintercept = 1/250, color = "red") +
    scale_x_log10()

#### Matching ####
# Only use top 10 topics, roughly speaking that is 3 times tau_gamma
tags
rs_tags <- tags[c(7,36:65)]
es_tags <- tags[-c(7,36:65,16, 1,3)]

matches <- matrix(nrow = length(es_tags), ncol = length(rs_tags))

tic()
for (i in seq_along(es_tags)){
    # extract documents of interest
    es_docs <- df_abstracts |> 
        filter(tag == es_tags[[i]]) |> 
        pull(id) |> unique()
    
    # extract top 10 topics
    top_es10 <- df_gamma |> 
        filter(document %in% es_docs) |> 
        group_by(topic) |> 
        summarize(mean_topic = mean(gamma)) |> 
        ungroup() |> 
        top_n(n = 10, wt = mean_topic) |> 
        pull(topic)
    
    for (j in seq_along(rs_tags)){
        # extract documents of interest
        rs_docs <- df_abstracts |> 
            filter(tag == rs_tags[[j]]) |> 
            pull(id) |> unique()
        
        # extract top 10 topics
        top_rs10 <- df_gamma |> 
            filter(document %in% rs_docs) |> 
            group_by(topic) |> 
            summarize(mean_topic = mean(gamma)) |> 
            ungroup() |> 
            top_n(n = 10, wt = mean_topic) |> 
            pull(topic)
        
        matches[i,j] <- sum(top_es10 %in% top_rs10)
    }
}
toc() #298.786s

rownames(matches) <- es_tags
colnames(matches) <- rs_tags

df_match <- matches |> 
    as_tibble(rownames = "es_tags") |> 
    pivot_longer(cols = fisheries:last_col(), names_to = "rs_tags", values_to = "n_topics")

df_match |> 
    #filter(n_topics > 0) |> 
    ggplot(aes(es_tags, rs_tags)) +
    geom_tile(aes(fill = n_topics)) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90))

save(df_match, file = "data/ES_detected.RData")

## Example with one
# search_docs <- df_abstracts |> 
#     filter(tag == "fisheries") |> 
#     pull(id) |> unique()
#     
# 
# res25 <- df_gamma |> 
#     filter(document %in% search_docs) |> 
#     group_by(topic) |> 
#     summarize(mean_topic = mean(gamma)) |> 
#     arrange(desc(mean_topic)) |> 
#     ungroup() |> 
#     top_n(n = 25, wt = mean_topic) |> 
#     pull(topic)
# 
# top5 %in% res25
