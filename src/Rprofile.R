#!/usr/bin/env R

#
# .Rprofile for interactive analysis
#
# Do not load libraries here. Only basic configurations
#

# Library for external site libraries
.libPaths(c("/analysis/src/R-site-library/", .libPaths()))

# force R to use analysis directory as home
Sys.setenv(HOME = "/analysis")

# set repository
assign("CRAN_repo", "https://packagemanager.rstudio.com/all/__linux__/focal/2022-07-05+Y3JhbiwyOjQ1MjYyMTU7RTRCNjFBNjg", envir=globalenv())
assign("CRAN", CRAN_repo, envir = globalenv())
assign("BioC_mirror", "https://packagemanager.rstudio.com/bioconductor", envir = globalenv())

options(BioC_mirror = 'https://packagemanager.rstudio.com/bioconductor')
options(repos = c(CRAN = CRAN_repo), download.file.method = 'libcurl')

# set working directory to the current directory
setwd("/analysis")
