

# define ui
combineShinyUi <- function(...) {
  do.call(what = fluidPage, args = list(...))
}



# define server
combineShinyServer <- function(...) {
  server <- function(input, output) {
    for(this_server in list(...)) {
      this_server(input, output)
    }
  }
}
