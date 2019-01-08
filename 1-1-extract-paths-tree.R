


# @knitr configure --------------------------------------------------------

rm(list = ls())
source("0-config.R")
# load(file = par$rdata_sample)



# @knitr prepare-data -----------------------------------------------------

# regression
df_in <- rxReadXdf(file = file.path(rxGetOption("sampleDataDir"),
                                    "mortDefaultSmall.xdf")) %>% 
  balance_set()
df_in$id <- rownames(df_in)
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



# build-model -------------------------------------------------------------

model_tree <- rxDTree(
# model_tree <- rpart(
  formula = formula_predict, 
  data = df_train
  )
# RevoTreeView::createTreeView(model_tree) %>% plot



# @knitr extract-frame -----------------------------------------------------------

# extract frame
df_frame <- model_tree %>% extractFrameTree

# prepare a report about the splits
# df_record = df_test[1, ]
df_splits <- (1:nrow(df_test)) %>% 
      lapply(function(i) df_test[i, ]) %>% 
      lapply(findPathTest, df_frame = df_frame) %>% 
      rbindlist() %>% 
      filter(!var == "<leaf>")



# @knitr measure-relevance -------------------------------------------------------

# build heatmap
p <- plot_relevance_tree(df_splits, n_samples = 20)
plot(p)



# @knitr analise-threshold -------------------------------------------------------

df_thresholds <- df_splits %>% 
  group_by(var, index) %>% 
  summarise(ydiff_mean = mean(ydiff), 
            multi_split = var(ydiff) > 0) %>% 
  arrange(-abs(ydiff_mean))
df_thresholds


