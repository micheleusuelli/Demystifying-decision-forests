
plotExploratoryTree <- function(
  df_in, # training set
  target, # target attribute
  features # predictors
  ) {
  
  # build and display an exploratory decision tree
  
  # convert characters into factors
  for(this_feature in c(features, target)) {
    if(class(df_in[[this_feature]]) == "character") {
      df_in[[this_feature]] <- factor(df_in[[this_feature]])
    }
  }
  
  # build the tree
  predict_tree <- paste(target, "~", 
                        paste(features, collapse = " + ")) %>% 
    formula %>% 
    rxDTree(data = df_in)
  
  # plot tree
  class(predict_tree) <- "rpart"
  rpart.plot(predict_tree)
  
}
