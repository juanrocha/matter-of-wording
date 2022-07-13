# libraries for modeling
library(tidytext)
library(tidyverse)
library(tm)
library (topicmodels)
library(lda)
library(tictoc)
load("data/dtm.Rdata")

SEED <- 2022

## Choosing best algorithm
k <- 10
tic()
tset.TM <- list (
    VEM0 = LDA(dtm, k=k, control = list (seed = SEED)),
    VEM_fixed= LDA(dtm, k=k, control= list (estimate.alpha = F, seed = SEED)),
    Gibbs = LDA (dtm, k=k, method ="Gibbs", control = list (seed = SEED, burnin= 1000, thin = 100, iter= 1000)),
    CTM = CTM (dtm, k=k, control = list(seed = SEED, var= list (tol= 10^-4), em= list (tol = 10^-3))))
toc() # 2732.134s
sapply (tset.TM[1:3], slot, "alpha")

## Gibbs beats them all

#Finding number of topics
k <- c(25,50,100,250,500)
tic()
topicNumber.TM <- map(
    .x = k,
    .f = function(x) {
        LDA(dtm, k = x, control= list (seed = SEED), method = "Gibbs")
    })
toc() # 47920.944 sec elapsed | 13.3 hrs



save(tset.TM, topicNumber.TM, file = "data/models_gibbs.RData")
