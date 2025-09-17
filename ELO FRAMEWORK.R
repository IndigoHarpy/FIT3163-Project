# ELO Framework (Baseline)

# Packages
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
})

DATA_PATH = "mens_singles.csv"
INITIAL_RATING = 1500
K_FACTOR = 32
WRITE_HISTORY <- TRUE             # set FALSE to skip writing history
LEADERBOARD_N <- 30               # top N players to print/save

# Optional: round-based K weighting (kept simple; toggle with USE_ROUND_WEIGHTS)
USE_ROUND_WEIGHTS <- FALSE
round_k <- function(rnd) {
  # map common round labels -> multiplier
  # tweak as you like; defaults to 1 for anything unlisted
  m <- c(
    "R128"=1.0, "R64"=1.0, "R32"=1.0, "R16"=1.05, "QF"=1.1, "SF"=1.15, "F"=1.2
  )
  if (!is.na(rnd) && rnd %in% names(m)) m[[rnd]] else 1.0
}

# === 1) Load & basic schema check ===
stopifnot(file.exists(DATA_PATH))
df <- read_csv(DATA_PATH, show_col_types = FALSE)

required <- c("tourney_date", "winner_id", "loser_id")
missing_cols <- setdiff(required, names(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# Minimal columns for Elo (+ optional round/best_of for weighting if desired)
matches <- df %>%
  select(tourney_date, winner_id, loser_id, round = any_of("round"), best_of = any_of("best_of"))

# Parse date (dataset uses int like 20240115)
matches <- matches %>%
  mutate(tourney_date = ymd(as.character(tourney_date))) %>%
  arrange(tourney_date, row_number())

# === 2) Elo helpers ===
expected_score <- function(rA, rB) {
  1.0 / (1.0 + 10^((rB - rA) / 400))
}

elo_update_pair <- function(r_w, r_l, k) {
  # winner gets score 1, loser 0
  Ew <- expected_score(r_w, r_l)
  El <- 1 - Ew
  r_w_new <- r_w + k * (1 - Ew)
  r_l_new <- r_l + k * (0 - El)
  list(w_new = r_w_new, l_new = r_l_new, Ew = Ew)
}

# === 3) Run Elo over matches ===
ratings <- new.env(hash = TRUE, parent = emptyenv())  # player_id -> rating

get_rating <- function(pid) {
  key <- as.character(pid)
  if (!exists(key, envir = ratings, inherits = FALSE)) {
    assign(key, INITIAL_RATING, envir = ratings)
  }
  get(key, envir = ratings, inherits = FALSE)
}

set_rating <- function(pid, val) {
  assign(as.character(pid), val, envir = ratings)
}

# Optional: collect history per match
history <- vector("list", nrow(matches))

for (i in seq_len(nrow(matches))) {
  m <- matches[i, ]
  
  w <- m$winner_id
  l <- m$loser_id
  rw_pre <- get_rating(w)
  rl_pre <- get_rating(l)
  
  k_here <- K_FACTOR
  if (USE_ROUND_WEIGHTS && "round" %in% names(m)) {
    k_here <- K_FACTOR * round_k(m$round)
  }
  
  up <- elo_update_pair(rw_pre, rl_pre, k_here)
  
  set_rating(w, up$w_new)
  set_rating(l, up$l_new)
  
  # record (optional but useful)
  history[[i]] <- data.frame(
    idx = i,
    date = m$tourney_date,
    round = if ("round" %in% names(m)) m$round else NA_character_,
    best_of = if ("best_of" %in% names(m)) m$best_of else NA_integer_,
    winner_id = w,
    loser_id = l,
    winner_pre = rw_pre,
    loser_pre  = rl_pre,
    winner_post = up$w_new,
    loser_post  = up$l_new,
    expected_win_prob = up$Ew,
    k_used = k_here,
    stringsAsFactors = FALSE
  )
}

history_df <- bind_rows(history)

# === 4) Final ratings & leaderboard (with names if available) ===

# Try common player files in the working directory
candidate_files <- c("atp_players.csv", "players.csv", "players_atp.csv", "player_lookup.csv")
player_path <- candidate_files[file.exists(candidate_files)][1]

name_map <- NULL
if (!is.na(player_path)) {
  pm <- readr::read_csv(player_path, show_col_types = FALSE)
  
  stopifnot("player_id" %in% names(pm))
  if (all(c("name_first","name_last") %in% names(pm))) {
    name_map <- pm |>
      dplyr::mutate(player_name = paste(name_first, name_last)) |>
      dplyr::select(player_id, player_name)
  } else if ("name" %in% names(pm)) {
    name_map <- pm |>
      dplyr::rename(player_name = name) |>
      dplyr::select(player_id, player_name)
  } else {
    warning("Players file found but no usable name columns; names will remain NA.")
  }
}

# Gather final ratings
all_players <- unique(c(history_df$winner_id, history_df$loser_id))
final_ratings <- data.frame(
  player_id = as.integer(all_players),
  rating    = sapply(as.character(all_players), function(p) get(p, envir = ratings)),
  stringsAsFactors = FALSE
) |>
  dplyr::arrange(dplyr::desc(rating))

# Attach names if available
if (!is.null(name_map)) {
  final_ratings <- final_ratings |>
    dplyr::left_join(name_map, by = "player_id") |>
    dplyr::select(player_id, player_name, rating)
} else {
  final_ratings <- final_ratings |>
    dplyr::mutate(player_name = NA_character_) |>
    dplyr::select(player_id, player_name, rating)
}

# Print and save Top 30
LEADERBOARD_N <- 30
cat("\nTop", LEADERBOARD_N, "players by Elo:\n")
print(head(dplyr::arrange(final_ratings, dplyr::desc(rating)), LEADERBOARD_N))

readr::write_csv(final_ratings, "elo_leaderboard.csv")
cat("Saved: elo_leaderboard.csv\n")

# === 6) Sanity pings ===
cat("\nSanity checks:\n")
cat("- Ratings count:", nrow(final_ratings), "\n")
cat("- Min/Mean/Max rating:",
    round(min(final_ratings$rating),1), "/",
    round(mean(final_ratings$rating),1), "/",
    round(max(final_ratings$rating),1), "\n")

# Done.
