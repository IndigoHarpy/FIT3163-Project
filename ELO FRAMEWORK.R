# ELO Framework (Baseline)

# Packages
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
})

DATA = "mens_singles.csv"
INITIAL_RATING = 1500
K_FACTOR = 32
WRITE_HISTORY <- TRUE             # set FALSE to skip writing history
LEADERBOARD_N <- 30               # top N players to print/save

df <- read_csv(data, show_col_types = FALSE)
