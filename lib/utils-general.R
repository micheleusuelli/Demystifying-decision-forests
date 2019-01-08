
sourceDirectory <- function(path_dir) {
  
  # source all the files from a directory
  
  files_to_source <- list.files("shiny",
                                full.names = TRUE,
                                recursive = TRUE,
                                pattern = "[.]R$")
  invisible(sapply(files_to_source, source))
  
}
