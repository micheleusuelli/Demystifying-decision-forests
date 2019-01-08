


library(dplyr)
library(data.table)
# classification
iris.sub <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
iris.dforest <- rxDForest(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                          data = iris[iris.sub, ])

df_importance <- iris.dforest$forest %>% 
  lapply("[[", "frame") %>% 
  lapply("[[", "var") %>% 
  lapply(as.character) %>% 
  unlist %>% 
  table %>%
  data.table %>% 
  setnames(1, "feature") %>% 
  filter(!feature %in% "<leaf>") %>% 
  arrange(-N)

df_importance



