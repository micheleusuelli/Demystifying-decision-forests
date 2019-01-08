
heatmapClusters <- function(
  df_clusters, 
  df_info = NULL, 
  col_cluster = "cluster",
  col_group = "group",
  col_id = "id"
) {
  
  # build a heatmap from the output of a clusering (or classification) model
  
  # define the dataset
  if(!is.null(df_info)) {
    
    df_joined <- df_clusters %>% 
      data.frame %>% 
      left_join(df_info, by = "id")
    
  } else {
    
    df_joined <- df_clusters
    
  }
  
  # build heatmap
  df_joined %>% 
    group_by_(col_cluster, col_group) %>% 
    summarise(n = n()) %>% 
    ggplot(aes_string(x = col_cluster, y = col_group, fill = "n")) +
    geom_tile() +
    theme(axis.text.x = element_text(
      angle = ifelse(df_clusters[[col_cluster]] %>% is.factor, 30,0), 
      hjust = 1)) +
    ggtitle("Confusion matrix of the results")
  
}
