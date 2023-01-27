#!/usr/bin/env R

#
# .Rprofile for interactive analysis
#
# Do not load libraries here. Only basic configurations
#

# Library for external site libraries
.libPaths(c("/analysis/src/RServer/R-site-library/", .libPaths()))

# Global options
options(
  mc.cores = parallel::detectCores()
)

# fix BLAS multithread issues
RhpcBLASctl::blas_set_num_threads(2)

# force R to use analysis directory as home
Sys.setenv(HOME = "/analysis")

# set repository
CRAN_repo="https://packagemanager.rstudio.com/all/__linux__/focal/2022-07-05+Y3JhbiwyOjQ1MjYyMTU7RTRCNjFBNjg"

Sys.setenv(CRAN = CRAN_repo)
Sys.setenv(BioC_mirror = "https://packagemanager.rstudio.com/bioconductor")

options(BioC_mirror = 'https://packagemanager.rstudio.com/bioconductor')
options(repos = c(CRAN = CRAN_repo), download.file.method = 'libcurl')

# set working directory to the current directory
setwd("/analysis")

# Auto start project
setHook("rstudio.sessionInit", function(newSession) {
  if (newSession && is.null(rstudioapi::getActiveProject())) {
    rstudioapi::openProject(".")
  }
}, action = "append")
