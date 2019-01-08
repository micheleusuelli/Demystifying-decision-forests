
clusterCars <- function(
  df_in, 
  features,
  n_clusters = 5,
  method = "complete",
  distance = "euclidean",
  normalise = TRUE) {
  
  # build a hierarchical clustering model and define the clusters
  
  # remove records having missing values
  n_na <- df_in %>% select(one_of(features)) %>% is.na %>% rowSums()
  df_in <- df_in %>% 
    filter(n_na == 0)
  
  # divide the features by their mean
  if(normalise) {
    for(this_feature in features) {
      x <- df_in[[this_feature]]
      x <- x / mean(x, na.rm = TRUE)
      x[is.na(x)] <- 0
      df_in[[this_feature]] <- x
    }
  }
  
  # build the hierarchical clustering model
  model_hclust <- buildDendroCars(
    df_in = df_in, 
    features = features,
    distance = distance,
    method = method)
  
  # determine the clusters
  defineClustersCars(
    df_in = df_in, 
    model_hclust = model_hclust,
    n_clusters = n_clusters)
  
}


buildDendroCars <- function(
  df_in, 
  features,
  method = "complete",
  distance = "euclidean") {
  
  # build a hierarchical clustering model
  
  rownames(df_in) <- df_in$id
  
  model_hclust <- df_in %>% 
    select(one_of(features)) %>% 
    dist(method = distance) %>% 
    hclust(method = method)
  
  model_hclust
  
}



defineClustersCars <- function(
  df_in, 
  model_hclust,
  n_clusters = 5) {
  
  # starting from a hierarchical clustering model, define the clusters
  
  df_in$cluster <- model_hclust %>% 
    cutree(k = n_clusters)
  
  df_in
  
}


