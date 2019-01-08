
# set-up the environment: install the packages


# install-cran-packages ---------------------------------------------------

# install the missing packages
packages_to_install <- setdiff(readLines("config/0-packages.txt"),
                               rownames(installed.packages()))

for(package in packages_to_install) {
  install.packages(package)
}
