library(data.table)
library(tidyverse)

fivethirtyeight_nfl_url <- "https://projects.fivethirtyeight.com/2018-nfl-predictions/games/"
input_file <- fs::path("input/NFL Pool.xlsm")

# setwd(here::here())
# source("R/fivethirtyeight.R")
