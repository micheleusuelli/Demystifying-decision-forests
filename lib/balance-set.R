
balance_set <- function(df_in) {
  n_defaults <- df_in$default %>% sum
  i_to_keep <- c(
    (df_in$default == 0) %>% which %>% sample(n_defaults),
    (df_in$default == 1) %>% which %>% sample(n_defaults)
  )
  df_in <- df_in[i_to_keep, ]
  df_in
}
