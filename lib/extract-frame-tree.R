

extractFrameTree <- function(model_tree) {
  
  # extract splits
  df_splits <- model_tree$splits %>% data.frame()
  df_splits$variable <- rownames(model_tree$splits)
  
  # extract frame
  df_frame <- model_tree$frame
  df_frame$id <- df_frame %>% rownames %>% as.numeric
  df_frame <- df_frame %>% 
    mutate(var = as.character(var))
  
  # combine splits and frame
  
  if(length(df_splits$index) == sum(df_frame$var != "<leaf>")) {
    
    # for rxDTree
    df_frame[df_frame$var != "<leaf>", "index"] <- df_splits$index
    
  } else if (nrow(df_splits) > nrow(df_frame)) {
    
    # for rpart
    df_to_join <- df_splits %>% 
      select(variable, count, index) %>% 
      setnames("count", "n") %>% 
      setnames("variable", "var")
    df_frame <- df_frame %>% 
      left_join(df_to_join, by = c("var", "n"))
  } else {
    
    df_frame[, "index"] <- NA
    
  }
  
  df_frame
}
