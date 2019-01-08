
#' Override the defaults in a list
#' Given a list of default parameters and a list of new parameters, override the defaults. Useful when parsing arguments from the terminal.
#'
#' @param defaults List of default parametsr
#' @param parameters List of new parameters
#'
#'
#' @return
#' @export

assignArgNames <- function(defaults, parameters) {
  
  for(arg in names(parameters)) {
    if(is.logical(defaults[[arg]])) {
      defaults[[arg]] <- parameters[[arg]] %>% as.logical
    } else if(is.numeric(defaults[[arg]])) {
      defaults[[arg]] <- parameters[[arg]] %>% as.numeric
    } else {
      defaults[[arg]] <- parameters[[arg]]
    }
  }
  defaults
}





#'
#' parse named arguments from terminal. The syntax is Rscript script.R args1 val1 arg2 val2. All the arguments are parsed as characters.
#'
#'
#'
#' @return
#' @export


parseArgNamed <- function() {
  args = commandArgs(trailingOnly=TRUE)
  n_args <- floor((length(args) / 2))
  if(n_args == 0) return()
  list_args <- args[(1:n_args) * 2]
  names(list_args) <- args[(1:n_args) * 2 - 1]
  list_args
}
