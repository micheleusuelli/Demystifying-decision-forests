
library(dplyr)

path_script_in <- "base-1-paths-tree.R"
path_script_out <- "pres-1-paths-tree.R"


# add @knitr --------------------------------------------------------------

add_knitr_prefix <- function(path_script_in, path_script_out) {
  
  x <- readLines(path_script_in)
  is_header <- grepl("-{4,}", x)
  headers_knitr <- x[is_header] %>% 
    gsub(pattern = "^# ", replacement = "# @knitr ") %>% 
    gsub(pattern = "\\s-+$", replacement = "") %>% 
    gsub(pattern = "\\s-+$", replacement = "") %>% 
    paste(rep("-", 75) %>% paste(collapse = "-")) %>% 
    substring(1, 75)
  x[is_header] <- headers_knitr
  x %>% paste(collapse = "\n") %>% cat(file = path_script_out)

}



# define_markdown_sections ------------------------------------------------


define_markdown_sections <- function(path_script_r, file_out = "") {
  
  x <- readLines(path_script_r)
  x <- x[grep("^# @knit", x)]
  x <- x %>% 
    gsub(pattern = "# @knitr ", replacement = "") %>% 
    gsub(pattern = "\\s-*$", replacement = "")
  sprintf(
    "
%s
====

```{r %s}
```
",
    x, x
  ) %>% paste(collapse = "\n\n") %>% cat(file = file_out)

}


add_knitr_prefix(path_script_in, path_script_out)
# define_markdown_sections(path_script_r = path_script_out)

