

prepare_relevance <- function(
  df_splits
) {
  df_splits %>%
    data.frame %>% 
    group_by(id, var, go_left) %>% 
    summarise(ydiff = sum(ydiff)) %>% 
    arrange(abs(ydiff))
}

plot_relevance_tree <- function(
  df_relevance, 
  n_samples = 20) {
  
  # define chart pars
  size_text <- ifelse(n_samples <= 20, 10, 200 / n_samples)
  
  # build chart
  p <- df_relevance %>% 
    mutate(var_dir = paste(var, 
                           ifelse(go_left, "<", ">"), 
                           # ydiff %>% substring(1, 6)
                           "thr"
    )) %>% 
    group_by() %>% 
    mutate(id = id %>% 
             as.character() %>% factor) %>% 
    filter(as.numeric(id) <= n_samples) %>%
    mutate(impact = ifelse(ydiff > 0, "+", "-"),
           variation = abs(ydiff)) %>% 
    ggplot(aes(x = var_dir, y = id, 
               fill = impact, 
               alpha = variation))+
    geom_tile() +
    # scale_fill_gradient(low = "red", high = "blue") +
    scale_fill_discrete(h = c(240, 0)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, 
                                     size = size_text),
          axis.text.y = element_text(size = size_text))
  p
  
}