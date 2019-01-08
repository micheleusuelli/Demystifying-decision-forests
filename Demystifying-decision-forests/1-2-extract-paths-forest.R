
# configure ---------------------------------------------------------------

rm(list = ls())
source("0-config.R")
# load(file = par$rdata_sample)




# prepare-data ------------------------------------------------------------

# regression
df_in <- rxReadXdf(file = file.path(rxGetOption("sampleDataDir"),
                                    "mortDefaultSmall.xdf")) %>% 
  balance_set()
target_attribute <- "default"

# define predictors
predictors <- names(df_in) %>% setdiff(target_attribute)
formula_predict <- paste(target_attribute, "~",
                         paste(predictors, collapse = " + ")) %>% 
  formula()

# split train test
rows_train <- sample(nrow(df_in), floor(nrow(df_in) * 0.8))
df_train <- df_in[rows_train, ] 
df_test <- df_in[-rows_train, ] %>% head(20)



# build-model --------------------------------------------------------------

# model_forest <- randomForest::randomForest(
model_forest <- rxDForest(
  formula = formula_predict, 
  # cp = 0.01,
  maxDepth = 4,
  nTree = 10,
  # keep.forest = TRUE,
  data = df_train) 
# model_forest$forest[[1]] %>% class
# model_forest$forest %>% sapply(class)



# extract-frame -----------------------------------------------------------

# extract frame
list_df_frame <- model_forest$forest %>% lapply(extractFrameTree)

# prepare a report about the splits
df_frame <- list_df_frame[[1]]
df_splits <- list_df_frame %>% 
  lapply(function(df_frame) {
    (1:nrow(df_test)) %>% 
      head(20) %>%
      lapply(function(i) df_test[i, ]) %>% 
      lapply(findPathTest, df_frame = df_frame) %>% 
      rbindlist() %>% 
      filter(!var == "<leaf>")
  }) %>% 
  rbindlist



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

# build heatmap
p <- plot_relevance_tree(df_splits, n_samples = 20)
plot(p)



# analise-threshold -------------------------------------------------------

df_thresholds <- df_splits %>% 
  group_by(var, index) %>% 
  summarise(ydiff_mean = mean(ydiff), 
            multi_split = var(ydiff) > 0) %>% 
  arrange(-abs(ydiff_mean))
df_thresholds

