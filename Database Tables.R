library(tidyverse)

results <- read.csv("mens_singles.csv")
w_results <- read.csv("womens_singles.csv")
men <- read.csv("current_men.csv")
women <- read.csv("current_women.csv")

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

write.csv(win_loss_summary, "win_loss.csv")

# Double faults
df_serves_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(double_fault = sum(w_df),
            service_points = sum(w_svpt))

df_serves_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(double_fault = sum(l_df),
            service_points = sum(l_svpt))

df_serves <- merge(x = df_serves_w, y = df_serves_l, by.x = c("year", "winner_id", "double_fault", "service_points"), by.y = c("year", "loser_id", "double_fault", "service_points"), all = TRUE)

colnames(df_serves)[2] <- "player_id"

df_serves <- merge(df_serves, men)[1:4]

df_serves_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(double_fault = sum(w_df),
            service_points = sum(w_svpt))

df_serves_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(double_fault = sum(l_df),
            service_points = sum(l_svpt))

w_df_serves <- merge(x = df_serves_w, y = df_serves_l, by.x = c("year", "winner_id", "double_fault", "service_points"), by.y = c("year", "loser_id", "double_fault", "service_points"), all = TRUE)

colnames(w_df_serves)[2] <- "player_id"

w_df_serves <- merge(w_df_serves, women)[1:4]

df_serves <- rbind(df_serves, w_df_serves)

na_presence <- aggregate(double_fault ~ player_id, data=df_serves, function(x) {sum(is.na(x))}, na.action = NULL)

data_presence <- df_serves %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n())

na_in_data <- merge(na_presence, data_presence)

absent_players <- c()

for (i in 1:nrow(na_in_data)) {
  if (na_in_data$total_matches[i]/2 >= na_in_data$double_fault[i]) {
    absent_players <- c(absent_players, i)
  }
}

df_serves <- df_serves[-absent_players, ]

write.csv(df_serves, "df_serves.csv")

