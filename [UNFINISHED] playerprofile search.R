# ---------------------------
# Minimal Glicko-2 per-match engine (global updates)
# Input: Player name (or id) → Process: run Glicko-2 over the entire dataset, match-by-match → Output: that player’s final rating
# ---------------------------
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
})

# === Config ===
DATA_PATH <- "mens_singles.csv"   # change if needed
INITIAL_R <- 1500
INITIAL_RD <- 350
INITIAL_SIGMA <- 0.06
TAU <- 0.5         # volatility constraint (typical: 0.3 ~ 1.2)
EPS <- 1e-6

# Optional: if you have a player-name -> id table, set it here (two columns: player_id, player_name)
players_map <- NULL  # e.g., read_csv("players_map.csv")

# === Read data ===
df <- read_csv(DATA_PATH, show_col_types = FALSE)

# Basic schema check
stopifnot(all(c("tourney_date","winner_id","loser_id") %in% names(df)))

# Parse date & order. (If tourney_date is like 20240115 int)
df <- df %>%
  mutate(tourney_date = ymd(as.character(tourney_date))) %>%
  arrange(tourney_date, row_number())

# === Glicko-2 helpers ===
to_mu_phi <- function(r, RD) list(mu = (r - 1500)/173.7178, phi = RD/173.7178)
from_mu_phi <- function(mu, phi) list(r = 173.7178*mu + 1500, RD = 173.7178*phi)

g_fun <- function(phi) 1 / sqrt(1 + 3*phi^2/pi^2)
E_fun <- function(mu, mu_j, phi_j) 1 / (1 + exp(-g_fun(phi_j) * (mu - mu_j)))

# Update volatility via the Illinois/regula-falsi scheme (Glickman 2001/2013 notes)
update_sigma <- function(phi, delta, v, sigma, tau = TAU, eps = EPS) {
  a <- log(sigma^2)
  f <- function(x) {
    ex <- exp(x)
    num <- ex * (delta^2 - phi^2 - v - ex)
    den <- 2*(phi^2 + v + ex)^2
    (num/den) - ((x - a)/tau^2)
  }
  # bracket
  if (delta^2 > (phi^2 + v)) {
    B <- log(delta^2 - phi^2 - v)
  } else {
    k <- 1
    while (f(a - k*tau) > 0) k <- k + 1
    B <- a - k*tau
  }
  A <- a
  fA <- f(A); fB <- f(B)
  # iterate
  while (abs(B - A) > eps) {
    C <- A + (A - B) * fA / (fB - fA)
    fC <- f(C)
    if (fC * fB < 0) { A <- B; fA <- fB } else { fA <- fA/2 }
    B <- C; fB <- fC
  }
  exp(A/2)  # sigma'
}

# Single-player update given an opponent (per-match rating period)
glicko2_update_one_side <- function(r, RD, sigma, r_opp, RD_opp, score,
                                    tau = TAU, eps = EPS) {
  # Convert to Glicko-2 scale
  s <- to_mu_phi(r, RD); mu <- s$mu; phi <- s$phi
  so <- to_mu_phi(r_opp, RD_opp); mu_j <- so$mu; phi_j <- so$phi
  
  # m = 1 opponent (v, delta)
  E <- E_fun(mu, mu_j, phi_j)
  g <- g_fun(phi_j)
  v <- 1 / (g^2 * E * (1 - E))
  delta <- v * g * (score - E)
  
  # sigma'
  sigma_p <- update_sigma(phi, delta, v, sigma, tau, eps)
  
  # phi*
  phi_star <- sqrt(phi^2 + sigma_p^2)
  
  # new phi, mu
  phi_prime <- 1 / sqrt( (1/phi_star^2) + (1/v) )
  mu_prime <- mu + phi_prime^2 * g * (score - E)
  
  out <- from_mu_phi(mu_prime, phi_prime)
  list(r = out$r, RD = out$RD, sigma = sigma_p,
       expected = E)
}

# Symmetric two-sided update for a single match
glicko2_update_match <- function(stateA, stateB, scoreA) {
  # scoreA: 1 if A wins, 0 if A loses
  # Update A vs B
  updA <- glicko2_update_one_side(stateA$r, stateA$RD, stateA$sigma,
                                  stateB$r, stateB$RD, stateB$sigma, scoreA)
  # Update B vs A (scoreB = 1 - scoreA) using A's *pre-match* state
  scoreB <- 1 - scoreA
  updB <- glicko2_update_one_side(stateB$r, stateB$RD, stateB$sigma,
                                  stateA$r, stateA$RD, stateA$sigma, scoreB)
  list(A = updA, B = updB)
}

# Store player states in an environment
mk_state <- function() list(r = INITIAL_R, RD = INITIAL_RD, sigma = INITIAL_SIGMA)
states <- new.env(parent = emptyenv())

get_state <- function(pid) {
  key <- as.character(pid)
  if (!exists(key, envir = states, inherits = FALSE)) assign(key, mk_state(), envir = states)
  get(key, envir = states, inherits = FALSE)
}
set_state <- function(pid, st) assign(as.character(pid), st, envir = states)

# === Main: run global updates then query a player's final rating ===
# player_input: can be numeric id or character name (if you provide players_map or name columns exist)
glicko2_final_for_player <- function(player_input) {
  # Resolve id if a name is given
  resolve_id <- function(x) {
    if (is.numeric(x)) return(as.integer(x))
    if (is.character(x)) {
      # Try to resolve via dataset columns if available
      if ("winner_name" %in% names(df) && "loser_name" %in% names(df)) {
        # collect candidate ids by name appearances
        w_ids <- df %>% filter(winner_name == x) %>% pull(winner_id)
        l_ids <- df %>% filter(loser_name  == x) %>% pull(loser_id)
        ids <- unique(c(w_ids, l_ids))
        if (length(ids) == 1) return(ids[[1]])
      }
      # Try a provided map
      if (!is.null(players_map)) {
        hit <- players_map %>% filter(player_name == x)
        if (nrow(hit) == 1) return(hit$player_id[[1]])
      }
      stop("Could not resolve player name to a unique id. Provide players_map or name columns.")
    }
    stop("player_input must be numeric id or player name.")
  }
  
  target_id <- resolve_id(player_input)
  
  # Iterate through matches; update both sides each time
  for (i in seq_len(nrow(df))) {
    w <- df$winner_id[[i]]
    l <- df$loser_id[[i]]
    
    sW <- get_state(w)
    sL <- get_state(l)
    
    upd <- glicko2_update_match(sW, sL, scoreA = 1)  # winner gets 1
    set_state(w, upd$A)
    set_state(l, upd$B)
  }
  
  # Return final for the player
  st <- get_state(target_id)
  tibble::tibble(
    player_id = as.integer(target_id),
    rating = round(st$r, 2),
    RD = round(st$RD, 2),
    sigma = round(st$sigma, 5)
  )
}

# === Example usage ===
# If your input is a numeric player id:
# glicko2_final_for_player(103819)

# If you only know a name, supply a players_map first or ensure your df has winner_name/loser_name:
# players_map <- read_csv("players_map.csv") # columns: player_id, player_name
# glicko2_final_for_player("Novak Djokovic")
