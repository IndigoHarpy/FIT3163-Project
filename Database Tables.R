library(tidyverse)

results <- read.csv("mens_singles.csv")
w_results <- read.csv("womens_singles.csv")
men <- read.csv("current_men.csv", row.names = 1)
women <- read.csv("current_women.csv", row.names = 1)

results$tourney_date <- as.Date(as.character(results$tourney_date), format = "%Y%m%d")
w_results$tourney_date <- as.Date(as.character(w_results$tourney_date), format = "%Y%m%d")

results$year <- format(results$tourney_date, "%Y")
w_results$year <- format(w_results$tourney_date, "%Y")

# Win Loss Summary
yearly_wins <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(matches_won = n())

yearly_losses <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(matches_lost = n())

win_loss_summary <- merge(x = yearly_wins, y = yearly_losses, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

win_loss_summary[is.na(win_loss_summary)] <- 0

win_loss_summary$total_matches <- win_loss_summary$matches_won + win_loss_summary$matches_lost

win_loss_summary$matches_lost <- NULL

colnames(win_loss_summary)[2] <- "player_id"

win_loss_summary <- merge(win_loss_summary, men)[1:4]

yearly_wins <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(matches_won = n())

yearly_losses <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(matches_lost = n())

w_win_loss_summary <- merge(x = yearly_wins, y = yearly_losses, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

w_win_loss_summary[is.na(w_win_loss_summary)] <- 0

w_win_loss_summary$total_matches <- w_win_loss_summary$matches_won + w_win_loss_summary$matches_lost

w_win_loss_summary$matches_lost <- NULL

colnames(w_win_loss_summary)[2] <- "player_id"

w_win_loss_summary <- merge(w_win_loss_summary, women)[1:4]

win_loss_summary <- rbind(win_loss_summary, w_win_loss_summary)

write.csv(win_loss_summary, "win_loss.csv", row.names = FALSE)

# Double faults
df_serves_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(double_fault = sum(w_df, na.rm = TRUE),
            service_points = sum(w_svpt, na.rm = TRUE))

df_serves_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(double_fault = sum(l_df, na.rm = TRUE),
            service_points = sum(l_svpt, na.rm = TRUE))

df_serves <- merge(x = df_serves_w, y = df_serves_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

df_serves$double_fault <- df_serves$double_fault.x + df_serves$double_fault.y

df_serves$service_points <- df_serves$service_points.x + df_serves$service_points.y

df_serves[, 3:6] <- list(NULL)

colnames(df_serves)[2] <- "player_id"

df_serves <- merge(df_serves, men)[1:4]

df_serves_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(double_fault = sum(w_df, na.rm = TRUE),
            service_points = sum(w_svpt, na.rm = TRUE))

df_serves_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(double_fault = sum(l_df, na.rm = TRUE),
            service_points = sum(l_svpt, na.rm = TRUE))

w_df_serves <- merge(x = df_serves_w, y = df_serves_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

w_df_serves$double_fault <- w_df_serves$double_fault.x + w_df_serves$double_fault.y

w_df_serves$service_points <- w_df_serves$service_points.x + w_df_serves$service_points.y

w_df_serves[, 3:6] <- list(NULL)

colnames(w_df_serves)[2] <- "player_id"

w_df_serves <- merge(w_df_serves, women)[1:4]

df_serves <- rbind(df_serves, w_df_serves)

na_presence <- df_serves %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n(),
            na_presence = sum(is.na(double_fault)))

absent_players <- c()

for (i in 1:nrow(na_presence)) {
  if (na_presence$total_matches[i] <= 2 * na_presence$na_presence[i]) {
    absent_players <- c(absent_players, i)
  }
}

player_ids <- na_presence$player_id[absent_players]

df_serves <- df_serves[!df_serves$player_id %in% player_ids,]

write.csv(df_serves, "df_serves.csv", row.names = FALSE)

# 1st Serves Won
first_serves_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_serve = sum(w_1stIn, na.rm = TRUE),
            first_won = sum(w_1stWon, na.rm = TRUE))

first_serves_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_serve = sum(l_1stIn, na.rm = TRUE),
            first_won = sum(l_1stWon, na.rm = TRUE))

first_serves <- merge(x = first_serves_w, y = first_serves_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

first_serves$first_serve <- first_serves$first_serve.x + first_serves$first_serve.y

first_serves$first_won <- first_serves$first_won.x + first_serves$first_won.y

first_serves[, 3:6] <- list(NULL)

colnames(first_serves)[2] <- "player_id"

first_serves <- merge(first_serves, men)[1:4]

first_serves_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_serve = sum(w_1stIn, na.rm = TRUE),
            first_won = sum(w_1stWon, na.rm = TRUE))

first_serves_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_serve = sum(l_1stIn),
            first_won = sum(l_1stWon))

w_first_serves <- merge(x = first_serves_w, y = first_serves_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

w_first_serves$first_serve <- w_first_serves$first_serve.x + w_first_serves$first_serve.y

w_first_serves$first_won <- w_first_serves$first_won.x + w_first_serves$first_won.y

w_first_serves[, 3:6] <- list(NULL)

colnames(w_first_serves)[2] <- "player_id"

w_first_serves <- merge(w_first_serves, women)[1:4]

first_serves <- rbind(first_serves, w_first_serves)

na_presence <- first_serves %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n(),
            na_presence = sum(is.na(first_serve)))

absent_players <- c()

for (i in 1:nrow(na_presence)) {
  if (na_presence$total_matches[i] <= 2 * na_presence$na_presence[i]) {
    absent_players <- c(absent_players, i)
  }
}

player_ids <- na_presence$player_id[absent_players]

first_serves <- first_serves[!first_serves$player_id %in% player_ids,]

write.csv(first_serves, "first_serves.csv", row.names = FALSE)

# Second Serves Won
second_serves_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_serve = sum(w_1stIn, na.rm = TRUE),
            second_won = sum(w_2ndWon, na.rm = TRUE),
            serve_point = sum(w_svpt, na.rm = TRUE),
            double_fault = sum(w_df, na.rm = TRUE))

second_serves_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_serve = sum(l_1stIn, na.rm = TRUE),
            second_won = sum(l_2ndWon, na.rm = TRUE),
            serve_point = sum(l_svpt, na.rm = TRUE),
            double_fault = sum(l_df, na.rm = TRUE))

second_serves <- merge(x = second_serves_w, y = second_serves_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

second_serves$first_serve <- second_serves$first_serve.x + second_serves$first_serve.y

second_serves$second_won <- second_serves$second_won.x + second_serves$second_won.y

second_serves$serve_point <- second_serves$serve_point.x + second_serves$serve_point.y

second_serves$double_fault <- second_serves$double_fault.x + second_serves$double_fault.y

second_serves[, 3:10] <- list(NULL)

colnames(second_serves)[2] <- "player_id"

second_serves$second_serve <- second_serves$serve_point - second_serves$first_serve - second_serves$double_fault

second_serves$serve_point <- NULL
second_serves$double_fault <- NULL
second_serves$first_serve <- NULL

second_serves <- merge(second_serves, men)[1:4]

second_serves_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_serve = sum(w_1stIn, na.rm = TRUE),
            second_won = sum(w_2ndWon, na.rm = TRUE),
            serve_point = sum(w_svpt, na.rm = TRUE),
            double_fault = sum(w_df, na.rm = TRUE))

second_serves_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_serve = sum(l_1stIn, na.rm = TRUE),
            second_won = sum(l_2ndWon, na.rm = TRUE),
            serve_point = sum(l_svpt, na.rm = TRUE),
            double_fault = sum(l_df, na.rm = TRUE))

w_second_serves <- merge(x = second_serves_w, y = second_serves_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

w_second_serves$first_serve <- w_second_serves$first_serve.x + w_second_serves$first_serve.y

w_second_serves$second_won <- w_second_serves$second_won.x + w_second_serves$second_won.y

w_second_serves$serve_point <- w_second_serves$serve_point.x + w_second_serves$serve_point.y

w_second_serves$double_fault <- w_second_serves$double_fault.x + w_second_serves$double_fault.y

w_second_serves[, 3:10] <- list(NULL)

colnames(w_second_serves)[2] <- "player_id"

w_second_serves$second_serve <- w_second_serves$serve_point - w_second_serves$first_serve - w_second_serves$double_fault

w_second_serves$serve_point <- NULL
w_second_serves$double_fault <- NULL
w_second_serves$first_serve <- NULL

w_second_serves <- merge(w_second_serves, women)[1:4]

second_serves <- rbind(second_serves, w_second_serves)

na_presence <- second_serves %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n(),
            na_presence = sum(is.na(second_serve)))

absent_players <- c()

for (i in 1:nrow(na_presence)) {
  if (na_presence$total_matches[i] <= 2 * na_presence$na_presence[i]) {
    absent_players <- c(absent_players, i)
  }
}

player_ids <- na_presence$player_id[absent_players]

second_serves <- second_serves[!second_serves$player_id %in% player_ids,]

write.csv(second_serves, "second_serves.csv", row.names = FALSE)

# Break Points
bp_save_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(bp_faced = sum(w_bpFaced, na.rm = TRUE),
            bp_saved = sum(w_bpSaved, na.rm = TRUE))

bp_save_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(bp_faced = sum(l_bpFaced, na.rm = TRUE),
            bp_saved = sum(l_bpSaved, na.rm = TRUE))

bp_save <- merge(x = bp_save_w, y = bp_save_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

bp_save$bp_faced <- bp_save$bp_faced.x + bp_save$bp_faced.y

bp_save$bp_saved <- bp_save$bp_saved.x + bp_save$bp_saved.y

bp_save[, 3:6] <- list(NULL)

colnames(bp_save)[2] <- "player_id"

bp_save <- merge(bp_save, men)[1:4]

bp_save_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(bp_faced = sum(w_bpFaced, na.rm = TRUE),
            bp_saved = sum(w_bpSaved, na.rm = TRUE))

bp_save_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(bp_faced = sum(l_bpFaced, na.rm = TRUE),
            bp_saved = sum(l_bpSaved, na.rm = TRUE))

w_bp_save <- merge(x = bp_save_w, y = bp_save_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

w_bp_save$bp_faced <- w_bp_save$bp_faced.x + w_bp_save$bp_faced.y

w_bp_save$bp_saved <- w_bp_save$bp_saved.x + w_bp_save$bp_saved.y

w_bp_save[, 3:6] <- list(NULL)

colnames(w_bp_save)[2] <- "player_id"

w_bp_save <- merge(w_bp_save, women)[1:4]

bp_save <- rbind(bp_save, w_bp_save)

na_presence <- bp_save %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n(),
            na_presence = sum(is.na(bp_faced)))

absent_players <- c()

for (i in 1:nrow(na_presence)) {
  if (na_presence$total_matches[i] <= 2 * na_presence$na_presence[i]) {
    absent_players <- c(absent_players, i)
  }
}

player_ids <- na_presence$player_id[absent_players]

bp_save <- bp_save[!bp_save$player_id %in% player_ids,]

write.csv(bp_save, "bp_save.csv", row.names = FALSE)

# Aces
aces_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(aces = sum(w_ace, na.rm = TRUE),
            serves = sum(w_svpt, na.rm = TRUE))

aces_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(aces = sum(l_ace, na.rm = TRUE),
            serves = sum(l_svpt, na.rm = TRUE))

aces <- merge(x = aces_w, y = aces_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

aces$aces <- aces$aces.x + aces$aces.y

aces$serves <- aces$serves.x + aces$serves.y

aces[, 3:6] <- list(NULL)

colnames(aces)[2] <- "player_id"

aces <- merge(aces, men)[1:4]

aces_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(aces = sum(w_ace, na.rm = TRUE),
            serves = sum(w_svpt, na.rm = TRUE))

aces_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(aces = sum(l_ace, na.rm = TRUE),
            serves = sum(l_svpt, na.rm = TRUE))

w_aces <- merge(x = aces_w, y = aces_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

w_aces$aces <- w_aces$aces.x + w_aces$aces.y

w_aces$serves <- w_aces$serves.x + w_aces$serves.y

w_aces[, 3:6] <- list(NULL)

colnames(w_aces)[2] <- "player_id"

w_aces <- merge(w_aces, women)[1:4]

aces <- rbind(aces, w_aces)

na_presence <- aces %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n(),
            na_presence = sum(is.na(aces)))

absent_players <- c()

for (i in 1:nrow(na_presence)) {
  if (na_presence$total_matches[i] <= 2 * na_presence$na_presence[i]) {
    absent_players <- c(absent_players, i)
  }
}

player_ids <- na_presence$player_id[absent_players]

aces <- aces[!aces$player_id %in% player_ids,]

write.csv(aces, "aces.csv", row.names = FALSE)

# Serves Won
serve_point_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_serves_won = sum(w_1stWon, na.rm = TRUE),
            second_serves_won = sum(w_2ndWon, na.rm = TRUE),
            serves = sum(w_svpt, na.rm = TRUE))

serve_point_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_serves_won = sum(l_1stWon, na.rm = TRUE),
            second_serves_won = sum(l_2ndWon, na.rm = TRUE),
            serves = sum(l_svpt, na.rm = TRUE))

serve_point <- merge(x = serve_point_w, y = serve_point_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

serve_point$first_serves_won <- serve_point$first_serves_won.x + serve_point$first_serves_won.y

serve_point$second_serves_won <- serve_point$second_serves_won.x + serve_point$second_serves_won.y

serve_point$serves <- serve_point$serves.x + serve_point$serves.y

serve_point[, 3:8] <- list(NULL)

colnames(serve_point)[2] <- "player_id"

serve_point$serves_won <- serve_point$first_serves_won + serve_point$second_serves_won

serve_point$first_serves_won <- NULL
serve_point$second_serves_won <- NULL

serve_point <- merge(serve_point, men)[1:4]

serve_point_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_serves_won = sum(w_1stWon, na.rm = TRUE),
            second_serves_won = sum(w_2ndWon, na.rm = TRUE),
            serves = sum(w_svpt, na.rm = TRUE))

serve_point_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_serves_won = sum(l_1stWon, na.rm = TRUE),
            second_serves_won = sum(l_2ndWon, na.rm = TRUE),
            serves = sum(l_svpt, na.rm = TRUE))

w_serve_point <- merge(x = serve_point_w, y = serve_point_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

w_serve_point$first_serves_won <- w_serve_point$first_serves_won.x + w_serve_point$first_serves_won.y

w_serve_point$second_serves_won <- w_serve_point$second_serves_won.x + w_serve_point$second_serves_won.y

w_serve_point$serves <- w_serve_point$serves.x + w_serve_point$serves.y

w_serve_point[, 3:8] <- list(NULL)

colnames(w_serve_point)[2] <- "player_id"

w_serve_point$serves_won <- w_serve_point$first_serves_won + w_serve_point$second_serves_won

w_serve_point$first_serves_won <- NULL
w_serve_point$second_serves_won <- NULL

w_serve_point <- merge(w_serve_point, women)[1:4]

serve_point <- rbind(serve_point, w_serve_point)

na_presence <- serve_point %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n(),
            na_presence = sum(is.na(serves)))

absent_players <- c()

for (i in 1:nrow(na_presence)) {
  if (na_presence$total_matches[i] <= 2 * na_presence$na_presence[i]) {
    absent_players <- c(absent_players, i)
  }
}

player_ids <- na_presence$player_id[absent_players]

serve_point <- serve_point[!serve_point$player_id %in% player_ids,]

write.csv(serve_point, "serves_won.csv", row.names = FALSE)

# First Serve Returns
first_returns_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_return = sum(l_1stIn, na.rm = TRUE),
            first_lost = sum(l_1stWon, na.rm = TRUE))

first_returns_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_return = sum(w_1stIn, na.rm = TRUE),
            first_lost = sum(w_1stWon, na.rm = TRUE))

first_returns <- merge(x = first_returns_w, y = first_returns_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

first_returns$first_return <- first_returns$first_return.x + first_returns$first_return.y

first_returns$first_lost <- first_returns$first_lost.x + first_returns$first_lost.y

first_returns[, 3:6] <- list(NULL)

colnames(first_returns)[2] <- "player_id"

first_returns$first_won <- first_returns$first_return - first_returns$first_lost

first_returns$first_lost <- NULL

first_returns <- merge(first_returns, men)[1:4]

first_returns_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_return = sum(l_1stIn, na.rm = TRUE),
            first_lost = sum(l_1stWon, na.rm = TRUE))

first_returns_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_return = sum(w_1stIn, na.rm = TRUE),
            first_lost = sum(w_1stWon, na.rm = TRUE))

w_first_returns <- merge(x = first_returns_w, y = first_returns_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

w_first_returns$first_return <- w_first_returns$first_return.x + w_first_returns$first_return.y

w_first_returns$first_lost <- w_first_returns$first_lost.x + w_first_returns$first_lost.y

w_first_returns[, 3:6] <- list(NULL)

colnames(w_first_returns)[2] <- "player_id"

w_first_returns$first_won <- w_first_returns$first_return - w_first_returns$first_lost

w_first_returns$first_lost <- NULL

w_first_returns <- merge(w_first_returns, women)[1:4]

first_returns <- rbind(first_returns, w_first_returns)

na_presence <- first_returns %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n(),
            na_presence = sum(is.na(first_return)))

absent_players <- c()

for (i in 1:nrow(na_presence)) {
  if (na_presence$total_matches[i] <= 2 * na_presence$na_presence[i]) {
    absent_players <- c(absent_players, i)
  }
}

player_ids <- na_presence$player_id[absent_players]

first_returns <- first_returns[!first_returns$player_id %in% player_ids,]

write.csv(first_returns, "first_returns.csv", row.names = FALSE)

# Second Serve Returns
second_returns_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_return = sum(l_1stIn, na.rm = TRUE),
            second_lost = sum(l_2ndWon, na.rm = TRUE),
            return_point = sum(l_svpt, na.rm = TRUE),
            double_fault = sum(l_df, na.rm = TRUE))

second_returns_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_return = sum(w_1stIn, na.rm = TRUE),
            second_lost = sum(w_2ndWon, na.rm = TRUE),
            return_point = sum(w_svpt, na.rm = TRUE),
            double_fault = sum(w_df, na.rm = TRUE))

second_returns <- merge(x = second_returns_w, y = second_returns_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

second_returns$first_return <- second_returns$first_return.x + second_returns$first_return.y

second_returns$second_lost <- second_returns$second_lost.x + second_returns$second_lost.y

second_returns$return_point <- second_returns$return_point.x + second_returns$return_point.y

second_returns$double_fault <- second_returns$double_fault.x + second_returns$double_fault.y

second_returns[, 3:10] <- list(NULL)

colnames(second_returns)[2] <- "player_id"

second_returns$second_return <- second_returns$return_point - second_returns$first_return - second_returns$double_fault

second_returns$second_won <- second_returns$second_return - second_returns$second_lost

second_returns$return_point <- NULL
second_returns$double_fault <- NULL
second_returns$first_return <- NULL
second_returns$second_lost <- NULL

second_returns <- merge(second_returns, men)[1:4]

second_returns_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_return = sum(l_1stIn, na.rm = TRUE),
            second_lost = sum(l_2ndWon, na.rm = TRUE),
            return_point = sum(l_svpt, na.rm = TRUE),
            double_fault = sum(l_df, na.rm = TRUE))

second_returns_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_return = sum(w_1stIn, na.rm = TRUE),
            second_lost = sum(w_2ndWon, na.rm = TRUE),
            return_point = sum(w_svpt, na.rm = TRUE),
            double_fault = sum(w_df, na.rm = TRUE))

w_second_returns <- merge(x = second_returns_w, y = second_returns_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

w_second_returns$first_return <- w_second_returns$first_return.x + w_second_returns$first_return.y

w_second_returns$second_lost <- w_second_returns$second_lost.x + w_second_returns$second_lost.y

w_second_returns$return_point <- w_second_returns$return_point.x + w_second_returns$return_point.y

w_second_returns$double_fault <- w_second_returns$double_fault.x + w_second_returns$double_fault.y

w_second_returns[, 3:10] <- list(NULL)

colnames(w_second_returns)[2] <- "player_id"

w_second_returns$second_return <- w_second_returns$return_point - w_second_returns$first_return - w_second_returns$double_fault

w_second_returns$second_won <- w_second_returns$second_return - w_second_returns$second_lost

w_second_returns$return_point <- NULL
w_second_returns$double_fault <- NULL
w_second_returns$first_return <- NULL
w_second_returns$second_lost <- NULL

w_second_returns <- merge(w_second_returns, women)[1:4]

second_returns <- rbind(second_returns, w_second_returns)

na_presence <- second_returns %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n(),
            na_presence = sum(is.na(second_return)))

absent_players <- c()

for (i in 1:nrow(na_presence)) {
  if (na_presence$total_matches[i] <= 2 * na_presence$na_presence[i]) {
    absent_players <- c(absent_players, i)
  }
}

player_ids <- na_presence$player_id[absent_players]

second_returns <- second_returns[!second_returns$player_id %in% player_ids,]

write.csv(second_returns, "second_returns.csv", row.names = FALSE)

# BP Conversion
bp_convert_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(bp = sum(l_bpFaced, na.rm = TRUE),
            bp_saved = sum(l_bpSaved, na.rm = TRUE))

bp_convert_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(bp = sum(w_bpFaced, na.rm = TRUE),
            bp_saved = sum(w_bpSaved, na.rm = TRUE))

bp_convert <- merge(x = bp_convert_w, y = bp_convert_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

bp_convert$bp <- bp_convert$bp.x + bp_convert$bp.y

bp_convert$bp_saved <- bp_convert$bp_saved.x + bp_convert$bp_saved.y

bp_convert[, 3:6] <- list(NULL)

colnames(bp_convert)[2] <- "player_id"

bp_convert$bp_converted <- bp_convert$bp - bp_convert$bp_saved

bp_convert$bp_saved <- NULL

bp_convert <- merge(bp_convert, men)[1:4]

bp_convert_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(bp = sum(l_bpFaced, na.rm = TRUE),
            bp_saved = sum(l_bpSaved, na.rm = TRUE))

bp_convert_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(bp = sum(w_bpFaced, na.rm = TRUE),
            bp_saved = sum(w_bpSaved, na.rm = TRUE))

w_bp_convert <- merge(x = bp_convert_w, y = bp_convert_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

w_bp_convert$bp <- w_bp_convert$bp.x + w_bp_convert$bp.y

w_bp_convert$bp_saved <- w_bp_convert$bp_saved.x + w_bp_convert$bp_saved.y

w_bp_convert[, 3:6] <- list(NULL)

colnames(w_bp_convert)[2] <- "player_id"

w_bp_convert$bp_converted <- w_bp_convert$bp - w_bp_convert$bp_saved

w_bp_convert$bp_saved <- NULL

w_bp_convert <- merge(w_bp_convert, women)[1:4]

bp_convert <- rbind(bp_convert, w_bp_convert)

na_presence <- bp_convert %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n(),
            na_presence = sum(is.na(bp)))

absent_players <- c()

for (i in 1:nrow(na_presence)) {
  if (na_presence$total_matches[i] <= 2 * na_presence$na_presence[i]) {
    absent_players <- c(absent_players, i)
  }
}

player_ids <- na_presence$player_id[absent_players]

bp_convert <- bp_convert[!bp_convert$player_id %in% player_ids,]

write.csv(bp_convert, "bp_convert.csv", row.names = FALSE)

# Return Points Won
returns_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_returns_lost = sum(l_1stWon, na.rm = TRUE),
            second_returns_lost = sum(l_2ndWon, na.rm = TRUE),
            returns = sum(l_svpt, na.rm = TRUE))

returns_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_returns_lost = sum(w_1stWon, na.rm = TRUE),
            second_returns_lost = sum(w_2ndWon, na.rm = TRUE),
            returns = sum(w_svpt, na.rm = TRUE))

returns <- merge(x = returns_w, y = returns_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

returns$first_returns_lost <- returns$first_returns_lost.x + returns$first_returns_lost.y

returns$second_returns_lost <- returns$second_returns_lost.x + returns$second_returns_lost.y

returns$returns <- returns$returns.x + returns$returns.y

returns[, 3:8] <- list(NULL)

colnames(returns)[2] <- "player_id"

returns$returns_lost <- returns$first_returns_lost + returns$second_returns_lost

returns$returns_won <- returns$returns - returns$returns_lost

returns$first_returns_lost <- NULL
returns$second_returns_lost <- NULL
returns$returns_lost <- NULL

returns <- merge(returns, men)[1:4]

returns_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_returns_lost = sum(l_1stWon, na.rm = TRUE),
            second_returns_lost = sum(l_2ndWon, na.rm = TRUE),
            returns = sum(l_svpt, na.rm = TRUE))

returns_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_returns_lost = sum(w_1stWon, na.rm = TRUE),
            second_returns_lost = sum(w_2ndWon, na.rm = TRUE),
            returns = sum(w_svpt, na.rm = TRUE))

w_returns <- merge(x = returns_w, y = returns_l, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

w_returns$first_returns_lost <- w_returns$first_returns_lost.x + w_returns$first_returns_lost.y

w_returns$second_returns_lost <- w_returns$second_returns_lost.x + w_returns$second_returns_lost.y

w_returns$returns <- w_returns$returns.x + w_returns$returns.y

w_returns[, 3:8] <- list(NULL)

colnames(w_returns)[2] <- "player_id"

w_returns$returns_lost <- w_returns$first_returns_lost + w_returns$second_returns_lost

w_returns$returns_won <- w_returns$returns - w_returns$returns_lost

w_returns$first_returns_lost <- NULL
w_returns$second_returns_lost <- NULL
w_returns$returns_lost <- NULL

w_returns <- merge(w_returns, women)[1:4]

returns <- rbind(returns, w_returns)

na_presence <- returns %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n(),
            na_presence = sum(is.na(returns)))

absent_players <- c()

for (i in 1:nrow(na_presence)) {
  if (na_presence$total_matches[i] <= 2 * na_presence$na_presence[i]) {
    absent_players <- c(absent_players, i)
  }
}

player_ids <- na_presence$player_id[absent_players]

returns <- returns[!returns$player_id %in% player_ids,]

write.csv(returns, "returns_won.csv", row.names = FALSE)

# Points Won
serves <- read.csv("serves_won.csv")
returns <- read.csv("returns_won.csv")

total_points <- merge(x = serves, y = returns, by.x = c("year", "player_id"), by.y = c("year", "player_id"), all = TRUE)

total_points$total_points <- total_points$serves + total_points$returns

total_points$total_won <- total_points$serves_won + total_points$returns_won

total_points$serves <- NULL
total_points$serves_won <- NULL
total_points$returns <- NULL
total_points$returns_won <- NULL

write.csv(total_points, "total_points.csv", row.names = FALSE)
