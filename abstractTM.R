library(bibliometrix)
library(tidyverse)
library(tidytext)
library(topicmodels)

## read in Web of Science exports and convert to datraframe
filePaths = dir("./data", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
D <- do.call("readFiles", as.list(filePaths))
M <- convert2df(D, dbsource = "isi", format = "bibtex")

## keep only document identifier (UT) and abstracts (AB), removing rows where abstract is missing
abstracts <- M %>% 
    select(UT, AB) %>%
    filter(!is.na(AB))

## convert to one-term-per-document-per-row format
tidy_abstracts <- abstracts %>%
    unnest_tokens(word, AB)

## remove stopwords
cleaned_abstracts <- tidy_abstracts %>%
    anti_join(get_stopwords())

## find most-used words
pop_words <- cleaned_abstracts %>%
    count(word, sort = TRUE)

## create document-word-count table (stopwords included)
## freq_abstracts <- tidy_abstracts %>%
##     count(UT, word, sort = TRUE)

## create document-word-count table (stopwords removed)
dwc_abstracts <- cleaned_abstracts %>%
    count(UT, word, sort = TRUE)

## convert tidy format to Document-Term-Matrix needed for LDA
DTM_abstracts <- dwc_abstracts %>%
    cast_dtm(UT, word, n)

##  create LDA model assuming 30 topics.
AB_LDA <- LDA(DTM_abstracts, 30, method = "VEM")

## Top terms per topic
AB_topics <- tidy(AB_LDA, matrix = "beta") 
top_terms <- AB_topics %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
top_terms






