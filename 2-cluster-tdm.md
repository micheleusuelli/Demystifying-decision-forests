
Wordcount-based clustering
========================================================
author: Michele Usuelli
date: 2016-01-29
width: 1680
height: 1050
css: css/custom.css









Overview
====



```r
print(1)
```

```
[1] 1
```

```r
# configure ---------------------------------------------------------------

rm(list = ls())
source("0-config.R")
# load(file = par$rdata_sample)



# build-tree --------------------------------------------------------------

# regression
df_in <- rxReadXdf(file = file.path(rxGetOption("sampleDataDir"),
                                    "mortDefaultSmall.xdf"))
```

```
Rows Processed: 100000
Time to read data file: 0.03 secs.
Time to convert to data frame: 0.00 secs.
```

```r
target_attribute <- "default"
predictors <- names(df_in) %>% setdiff(target_attribute)
formula_predict <- paste(target_attribute, "~",
                         paste(predictors, collapse = " + ")) %>% 
  formula()

rows_train <- sample(nrow(df_in), floor(nrow(df_in) * 0.8))
df_train <- df_in[rows_train, ]
n_defaults <- df_train$default %>% sum
i_to_keep <- c(
  (df_train$default == 0) %>% which %>% sample(n_defaults),
  (df_train$default == 1) %>% which %>% sample(n_defaults)
)
df_train <- df_train[i_to_keep, ]
df_test <- df_in[-rows_train, ]

model_tree <- rxDTree(
# model_tree <- rpart(
  formula = formula_predict, 
  data = df_train, 
  # cp = 0.01,
  maxDepth = 5)
```

```
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.009 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.016 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.017 seconds 
Rows Read: 2, Total Rows Processed: 2, Total Chunk Time: 0.007 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.026 seconds 
Rows Read: 4, Total Rows Processed: 4, Total Chunk Time: 0.013 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.029 seconds 
Rows Read: 8, Total Rows Processed: 8, Total Chunk Time: 0.022 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.029 seconds 
Rows Read: 12, Total Rows Processed: 12, Total Chunk Time: 0.014 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.025 seconds 
Rows Read: 8, Total Rows Processed: 8, Total Chunk Time: 0.003 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.015 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.017 seconds 
Rows Read: 2, Total Rows Processed: 2, Total Chunk Time: 0.006 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.025 seconds 
Rows Read: 4, Total Rows Processed: 4, Total Chunk Time: 0.011 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.024 seconds 
Rows Read: 8, Total Rows Processed: 8, Total Chunk Time: 0.007 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.025 seconds 
Rows Read: 4, Total Rows Processed: 4, Total Chunk Time: 0.002 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.003 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.028 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.018 seconds 
Rows Read: 2, Total Rows Processed: 2, Total Chunk Time: 0.006 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.025 seconds 
Rows Read: 4, Total Rows Processed: 4, Total Chunk Time: 0.012 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.025 seconds 
Rows Read: 8, Total Rows Processed: 8, Total Chunk Time: 0.013 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.024 seconds 
Rows Read: 4, Total Rows Processed: 4, Total Chunk Time: 0.006 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.025 seconds 
Rows Read: 2, Total Rows Processed: 2, Total Chunk Time: Less than .001 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.002 seconds 
Rows Read: 748, Total Rows Processed: 748, Total Chunk Time: 0.002 seconds 

Elapsed time for DTreeEstimation: 0.896 secs.

Elapsed time for BxDTreeBase: 0.898 secs.
 
```

```r
RevoTreeView::createTreeView(model_tree) %>% plot



# extract-frame -----------------------------------------------------------

# extract frame
df_frame <- model_tree %>% extractFrameTree

# define test set
df_test$id <- 1:nrow(df_test)

# define path for all the test set
list_i <- 1:nrow(df_test)

# prepare a report about the splits
df_splits <- list_i %>% 
      head(20) %>%
      lapply(function(i) df_test[i, ]) %>% 
      lapply(findPathTest, df_frame = df_frame) %>% 
      rbindlist() %>% 
      filter(!var == "<leaf>")

# findPathTest(df_test[1, ], df_frame = df_frame)
# findPathTest(df_test[2, ], df_frame = df_frame)



# measure-relevance -------------------------------------------------------

# measure the relevance by sample
df_relevance <- df_splits %>%
  data.frame %>% 
  group_by(id, var) %>% 
  summarise(ydiff = sum(ydiff)) %>% 
  arrange(abs(ydiff))

# reshape into the wide format
df_wide <- df_relevance %>% 
  spread(var, ydiff)
# df_wide %>% View

# build heatmap
n_samples <- 20

# define chart pars
size_text <- ifelse(n_samples <= 20, 10, 200 / n_samples)

# build chart
p <- df_relevance %>% 
  group_by() %>% 
  mutate(id = id %>% 
           as.character() %>% factor) %>% 
  filter(as.numeric(id) <= n_samples) %>%
  ggplot(aes(x = var, y = id, fill = ydiff))+
  geom_tile() +
  scale_fill_gradient(low = "red", high = "blue") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, 
                                   size = size_text),
        axis.text.y = element_text(size = size_text))
plot(p)
```

![plot of chunk config](2-cluster-tdm-figure/config-1.png) 

```r
# analise-threshold -------------------------------------------------------

df_thresholds <- df_splits %>% 
  group_by(var, index) %>% 
  summarise(ydiff_mean = mean(ydiff), 
            multi_split = var(ydiff) > 0) %>% 
  arrange(-abs(ydiff_mean))
df_thresholds
```

```
Source: local data table [3 x 4]
Groups: var

       var index ydiff_mean multi_split
     (chr) (dbl)      (dbl)       (lgl)
1   ccDebt     0 0.03415245        TRUE
2 houseAge     0 0.02629638       FALSE
3     year     0 0.36284289       FALSE
```


