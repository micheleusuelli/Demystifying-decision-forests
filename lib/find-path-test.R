

findPathTest <- function(
  df_record,
  df_frame, 
  col_id = "id") {
  
  # browser()
  
  # initialise the parameters
  df_path <- NULL
  
  # define max number of iterations
  n_iter <- df_frame %>% nrow %>% log2 %>% ceiling()
  
  # define the path
  for(i in 1:n_iter) {
    
    df_path <- findPathTestIter(
      df_record = df_record,
      df_frame = df_frame,
      df_path = df_path)
    
  }
  
  # output the table
  df_path <- df_path %>% 
    # select(var, index, yval) %>%
    mutate(ydiff = c(yval[-1] - yval[-n()], 0))
  
  # include the ID
  if(col_id %in% names(df_record)) {
    df_path[[col_id]] <- df_record[[col_id]]
  }
  
  df_path
  
}



findPathTestIter <- function(
  df_record,
  df_frame,
  df_path) {
  
  # browser()
  
  # define the row to extract to determine the next split
  
  if(is.null(df_path %>% nrow)) {
    
    # top of the tree
    id_split <- 1
    
  } else if(df_path$var %>% tail(1) == "<leaf>") {
    
    # leaf
    return(df_path)
    
  } else {
    
    # other split
    n_levels <- df_path %>% nrow
    i_start <- 2 ^ n_levels
    n_right <- 2 ^ ((n_levels - 1):0)
    go_right <- !df_path[, "go_left"]
    i_right <- sum(n_right * go_right)
    id_split <- i_start + i_right
  }
  # print(id_split)
  # if(id_split == 62) browser()
  
  # extract the split row from the frame
  df_split <- df_frame %>% 
    filter(id == id_split)
  
  # add info to the split
  df_split$id_split <- id_split
  
  # if it's a not a leaf, define the direction
  if(df_split$var == "<leaf>") {
    df_split$go_left <- NA
  } else {
    df_split$go_left <- df_record[[df_split$var]] < df_split$index
  }
  
  # add the new record to df path
  df_path <- rbind(df_path, df_split)
  
  df_path
  
}


