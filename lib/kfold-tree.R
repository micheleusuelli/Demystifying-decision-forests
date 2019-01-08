

kfoldTree <- function(
  df_in,
  target,
  features,
  n_fold = 5,
  n_features = Inf,
  fun_model = rxDTree,
  ...
) {
  
  # run the kfold for a decisoin tree
  
  kfoldRx(
    df_in,
    target,
    features,
    n_fold = 5,
    n_features = Inf,
    fun_model = rxDTree,
    ...
  )
}



kfoldRx <- function(
  df_in,
  target,
  features,
  n_fold = 5,
  n_features = Inf,
  fun_model = rxDTree,
  # fun_predict = rxPredict,
  ...
) {
  
  # run the kfold for any RevoScaleR predictive model
  
  # clean data
  for(this_feature in c(features, target)) {
    if(class(df_in[[this_feature]]) == "character") {
      df_in[[this_feature]] <- factor(df_in[[this_feature]])
    }
  }
  
  # prepare data
  df_in$fold <- sample(1:n_fold, nrow(df_in), TRUE)
  df_in$predicted <- 0
  
  # prepare formula
  formula_predict <- paste(target, "~", 
                           paste(features, collapse = " + ")) %>% 
    formula
  
  # select features
  if(n_features < Inf) {
    inf_gain <- attrEval(formula = formula_predict, 
                         data = df_in,
                         estimator = "InfGain")
    features <- inf_gain %>% sort(decreasing = TRUE) %>%
      head(n_features) %>% names
  }
  
  # run kfold
  this_fold <- 1
  
  for(this_fold in 1:n_fold) {
    
    # split train and test
    df_train <- df_in %>% filter(fold != this_fold)
    df_test <- df_in %>% filter(fold == this_fold)
    
    # balance classes
    n <- df_train[[target]] %>% table %>% max
    df_train <- df_train[[target]] %>% 
      unique %>% 
      lapply(function(this_level) {
        which(df_train[[target]] == this_level)
        }) %>% 
      lapply(sample, size = n * 10, replace = TRUE) %>% 
      lapply(function(i_rows) df_train[i_rows, ]) %>% 
      rbindlist()
    
    # build model
    df_train[[target]] %>% table %>% print
    model_tree <- fun_model(formula = formula_predict, 
                            data = df_train,
                            ...)
    
    # predict
    df_pred <- rxPredict(modelObject = model_tree, data = df_test)
    
    if(class(df_pred[[1]]) == "factor") {
      these_predicted <- df_pred[[1]]
    } else {
      these_predicted <- df_pred %>% apply(1, which.max)
    }
    
    # assign prediction label
    df_in <- df_in %>% 
      mutate(predicted = ifelse(fold == this_fold, 
                                these_predicted, 
                                predicted))
    
  }
  
  # assign predicted levels
  levels_target <- levels(df_in[[target]])
  df_in <- df_in %>% 
    mutate(predicted = factor(predicted, 
                              levels = seq_along(levels_target),
                              labels = levels_target))
  
  df_in
}
