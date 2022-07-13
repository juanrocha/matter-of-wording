library(tidytext)
library(tidyverse)
library(tm)
library (topicmodels)
library(lda)
library(patchwork)
library(png)
library(gg3D)

load("data/models_gibbs.RData")
load("data/abstracts_combined_220708.RData")

#Finding number of topics
k <- c(25,50,100,250,500)

## visualizations:
df_topic_number <- tibble(
    topic_number = k,
    entropy = map_dbl (topicNumber.TM, function (x)
        mean(apply(posterior(x)$topics, 1, function (z) - sum(z * log(z)))) # maximize Entropy
    ),
    alpha = map_dbl(topicNumber.TM, slot, "alpha"),
    log_lik = map_dbl(topicNumber.TM, logLik) #,  #maximize loglik
    #perplexity = map_dbl(topicNumber.TM, perplexity) #minimize perplexity
)

#### algorithm selection ####
df_stats <- tibble(
    model = names(lapply(tset.TM, logLik)),
    loglik = as.numeric(lapply(tset.TM, logLik)), #maximize loglik
    entropy = lapply (tset.TM, function (x) 
        mean(apply(posterior(x)$topics,
                   1, function (z) - sum(z * log(z))))) %>% as.numeric()#maximize ENTROPY
    
)


perp <-  lapply(tset.TM[c(1,2,4)], perplexity)
perp$Gibbs <- NA
# pretty names:
df_stats$model <- c("VEM alpha", "VEM fixed", "Gibbs", "CTM")

g1 <- df_stats %>%
    add_column(perplexity = as.numeric(perp[c(1,2,4,3)])) %>% #minimize perplexity
    pivot_longer(cols = 2:4, names_to = "measure", values_to = "value") %>%
    ggplot(aes(model, value)) + 
    geom_col() + 
    # scale_y_continuous(labels = scales::label_scientific) +
    facet_wrap(.~measure, scales = "free_y") +
    labs(x = "Algorithm", y = "Value", tag = "A") +
    theme_light(base_size = 8) + 
    theme(axis.text.x = element_text(size = 5))
g1

#### number of topics ####
g2 <- df_topic_number %>%
    # mutate(alpha_log = log10(alpha)) %>%
    pivot_longer(cols = 2:last_col(), names_to = "measure", values_to = "value") %>%
    # filter(measure != "alpha") %>%
    ggplot(aes(as.factor(topic_number), value)) +
    geom_col() + 
    # scale_y_continuous(labels = scales::label_scientific) +
    labs(x = "Number of topics", y = "Value", tag = "B") +
    facet_wrap(.~measure, scales = "free", ncol = 4, nrow = 1) +
    theme_light(base_size = 8)

g1/g2



ggsave(
    filename = "topicstats.png",
    path = "figures/",
    plot = last_plot(),
    device = "png",
    width = 6, height = 3, units = "in",
    dpi = 400,
    bg = "white"
)

#### topics ####


## visualization of topics
# g_50 <- df_topics1 %>%
#     group_by(topic) %>%
#     top_n(10, beta) %>% 
#     ungroup() %>%
#     arrange(topic, - beta) %>%
#     mutate(term = reorder_within(term, beta, topic)) %>%
#     ggplot(aes(term, beta)) +
#     geom_col(aes(fill = as.factor(topic)),show.legend = FALSE) +
#     coord_flip() + 
#     scale_x_reordered() +
#     labs(tag = "D", y = "Probability of word explaining the topic", x = "Word ranking") +
#     facet_wrap(.~ topic, scales = "free_y", ncol = 5) +
#     theme_light(base_size = 6) + 
#     theme(axis.text.x = element_text(size = 5))
# 
# 
# g_50
# 
# ggsave(
#     file = "topics50.png", 
#     path = "figures/",
#     device = "png", 
#     width = unit(7,"in"), height = unit(7,"in"),
#     bg = "white", dpi = 400)
# 
