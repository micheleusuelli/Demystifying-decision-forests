

# load the data

rm(list = ls())


# @knitr config -----------------------------------------------------------
opts_chunk$set(out.width='750px', dpi=200)


# @knitr load-packages ----------------------------------------------------
library(magrittr)
library(proxy)
library(tm)
library(ggplot2)
library(ggdendro)

# @knitr load-data --------------------------------------------------------
file_doc <- file.path("data", "doc.RData")
load(file = file_doc)



# @knitr define-parameters ------------------------------------------------
topics <- names(docs_final$content)
labels_articles <- c(
  rep("uk_city", 9),
  rep("machine_learning", 18),
  rep("coffee", 16)
)


# @knitr build-term-document-matrix ---------------------------------------
docs_tdm <- docs_final %>%
  tm::TermDocumentMatrix() %>%
  as.matrix()
dim(docs_tdm)
head(docs_tdm[, 1:4])


# @knitr clean-tdm --------------------------------------------------------
min_count <- 10
word_counts <- rowSums(docs_tdm)
docs_tdm <- docs_tdm[word_counts > min_count, ]



# @knitr compute-distance -------------------------------------------------
dist_doc <- proxy::dist(t(docs_tdm), method = "cosine")
dist_doc %>%
  as.matrix() %>%
  image()


# @knitr cluster-tdm ------------------------------------------------------
hclust_model <- hclust(dist_doc, method = "ward.D")
ggdendrogram(hclust_model, theme_dendro = FALSE)


