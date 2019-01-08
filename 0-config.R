

# general-config ----------------------------------------------------------

set.seed(1)
rxSetComputeContext("localpar")



# load-packages -----------------------------------------------------------

# load the packages
packages_to_load <- intersect(readLines("config/0-packages.txt"),
                                rownames(installed.packages()))
invisible(sapply(packages_to_load, library, character.only = TRUE))

# load the personal library
files_to_source <- list.files("lib",
                              full.names = TRUE,
                              recursive = TRUE,
                              pattern = "[.]R$")
invisible(sapply(files_to_source, source))



# load-parameters ---------------------------------------------------------

par <- jsonlite::fromJSON("config/1-parameters.json")



# define-cols -------------------------------------------------------------

cols_id <- "R_SAMPLE_NUMBER"
target_attribute <- "Severity_Bin"



# define-xdf --------------------------------------------------------------

xdf_sample <- par$xdf_sample %>% RxXdfData()
xdf_granular <- par$xdf_granular %>% RxXdfData()
xdf_tmp <- par$xdf_tmp %>% RxXdfData()

