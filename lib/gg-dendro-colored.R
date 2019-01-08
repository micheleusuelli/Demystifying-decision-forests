
ggDendroColored <- function(
  model_clust, # output of hclust
  df_info, # data frame containing the variable used for the color
  col_info = "id", # name of the column to use to join the clustering model with df_info
  col_color = "model" # column with the color
) {
  
  # build a dendrogram coloring its labels
  
  # extract the dataset with the label coming from the dendrogram
  df_hclust <- model_clust %>% 
    as.dendrogram %>% 
    dendro_data %>% 
    label
  
  # rename the label column
  if("label" %in% names(df_hclust)) {
    setnames(df_hclust, "label", col_info)
  }
  
  # clean the dataset and join it to the 1d features
  df_plot <- df_hclust %>% 
    mutate(id = id %>% as.character %>% as.numeric) %>% 
    left_join(df_info, by = col_info)
  
  # build a colored dendrogram
  ggdendrogram(model_clust, rotate=TRUE, leaf_labels = FALSE) +
    geom_text(data = df_plot,
              aes_string(label = "x", 
                         x = "x", 
                         y = 0, 
                         colour = col_color))
  
}
