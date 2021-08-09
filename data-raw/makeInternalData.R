#!/usr/bin/Rscript

# This script must be run from package root!

source("data-raw/countyCensus.R")
source("data-raw/abbr2state.R")

usethis::use_data(Census, StateCensus, abbr2state, internal = TRUE, overwrite = TRUE)
