
# @knitr configure --------------------------------------------------------

rm(list = ls())
source("0-config.R")
# load(file = par$rdata_sample)
rxOptions(reportProgress = 0)



# @knitr prepare-data -----------------------------------------------------

# regression
df_in <- rxReadXdf(file = file.path(rxGetOption("sampleDataDir"),
                                    "mortDefaultSmall.xdf")) %>% 
  balance_set()
df_in$id <- rownames(df_in)
rownames(df_in) <- NULL
col_id <- "id"
target_attribute <- "default"

# define predictors
predictors <- names(df_in) %>% setdiff(c(col_id, target_attribute))
formula_predict <- paste(target_attribute, "~",
                         paste(predictors, collapse = " + ")) %>% 
  formula()

# split train test
rows_train <- sample(nrow(df_in), floor(nrow(df_in) * 0.8))
df_train <- df_in[rows_train, ] 
df_test <- df_in[-rows_train, ] %>% head(20)

# display
df_in %>% head



# @knitr build-tree -------------------------------------------------------

model_tree <- rxDTree(
  formula = formula_predict, 
  data = df_train
)



# @knitr plot-tree --------------------------------------------------------
model_plot <- model_tree
class(model_plot) <- "rpart"
model_plot %>% 
  prune(cp = 0.01) %>% 
  rpart.plot()



# @knitr extract-frame ----------------------------------------------------

# extract frame
df_frame <- model_tree %>% extractFrameTree

df_frame %>% 
  select(var, index, yval) %>% 
  setnames(1:3, c("predictor", "value", "score")) %>% 
  head(10) %>% 
  print



# @knitr define-paths -----------------------------------------------------

# prepare a report about the splits
df_splits <- (1:nrow(df_test)) %>% 
  lapply(function(i) df_test[i, ]) %>% 
  lapply(findPathTest, df_frame = df_frame) %>% 
  rbindlist() %>% 
  filter(!var == "<leaf>")

ids_to_keep <- df_splits$id %>% unique %>% head(2)
df_splits %>% 
  filter(id %in% ids_to_keep) %>% 
  select(id, var, index, yval, go_left) %>% 
  setnames(2:4, c("predictor", "value", "score")) %>% 
  print



# @knitr measure-relevance ------------------------------------------------

# build heatmap
df_relevance <- prepare_relevance(df_splits)
plot_relevance_tree(df_relevance, n_samples = 20)



# @knitr analise-threshold ------------------------------------------------

df_thresholds <- df_splits %>% 
  group_by(var, index) %>% 
  summarise(ydiff = mean(ydiff), 
            n_splits = ydiff %>% unique %>% length) %>% 
  arrange(-abs(ydiff))
df_thresholds

