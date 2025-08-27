setwd("Mens singles")

results_2005 <- read.csv("atp_matches_2005.csv", row.names = NULL)
results_2006 <- read.csv("atp_matches_2006.csv", row.names = NULL)
results_2007 <- read.csv("atp_matches_2007.csv", row.names = NULL)
results_2008 <- read.csv("atp_matches_2008.csv", row.names = NULL)
results_2009 <- read.csv("atp_matches_2009.csv", row.names = NULL)
results_2010 <- read.csv("atp_matches_2010.csv", row.names = NULL)
results_2011 <- read.csv("atp_matches_2011.csv", row.names = NULL)
results_2012 <- read.csv("atp_matches_2012.csv", row.names = NULL)
results_2013 <- read.csv("atp_matches_2013.csv", row.names = NULL)
results_2014 <- read.csv("atp_matches_2014.csv", row.names = NULL)
results_2015 <- read.csv("atp_matches_2015.csv", row.names = NULL)
results_2016 <- read.csv("atp_matches_2016.csv", row.names = NULL)
results_2017 <- read.csv("atp_matches_2017.csv", row.names = NULL)
results_2018 <- read.csv("atp_matches_2018.csv", row.names = NULL)
results_2019 <- read.csv("atp_matches_2019.csv", row.names = NULL)
results_2020 <- read.csv("atp_matches_2020.csv", row.names = NULL)
results_2021 <- read.csv("atp_matches_2021.csv", row.names = NULL)
results_2022 <- read.csv("atp_matches_2022.csv", row.names = NULL)
results_2023 <- read.csv("atp_matches_2023.csv", row.names = NULL)
results_2024 <- read.csv("atp_matches_2024.csv", row.names = NULL)

results <- rbind(results_2005, results_2006, results_2007, results_2008, results_2009, results_2010, results_2011, results_2012, results_2013, results_2014, results_2015, results_2016, results_2017, results_2018, results_2019, results_2020, results_2021, results_2022, results_2023, results_2024)

rm(results_2005, results_2006, results_2007, results_2008, results_2009, results_2010, results_2011, results_2012, results_2013, results_2014, results_2015, results_2016, results_2017, results_2018, results_2019, results_2020, results_2021, results_2022, results_2023, results_2024)

setwd("..")
setwd("Womens singles")

w_results_2005 <- read.csv("wta_matches_2005.csv", row.names = NULL)
w_results_2006 <- read.csv("wta_matches_2006.csv", row.names = NULL)
w_results_2007 <- read.csv("wta_matches_2007.csv", row.names = NULL)
w_results_2008 <- read.csv("wta_matches_2008.csv", row.names = NULL)
w_results_2009 <- read.csv("wta_matches_2009.csv", row.names = NULL)
w_results_2010 <- read.csv("wta_matches_2010.csv", row.names = NULL)
w_results_2011 <- read.csv("wta_matches_2011.csv", row.names = NULL)
w_results_2012 <- read.csv("wta_matches_2012.csv", row.names = NULL)
w_results_2013 <- read.csv("wta_matches_2013.csv", row.names = NULL)
w_results_2014 <- read.csv("wta_matches_2014.csv", row.names = NULL)
w_results_2015 <- read.csv("wta_matches_2015.csv", row.names = NULL)
w_results_2016 <- read.csv("wta_matches_2016.csv", row.names = NULL)
w_results_2017 <- read.csv("wta_matches_2017.csv", row.names = NULL)
w_results_2018 <- read.csv("wta_matches_2018.csv", row.names = NULL)
w_results_2019 <- read.csv("wta_matches_2019.csv", row.names = NULL)
w_results_2020 <- read.csv("wta_matches_2020.csv", row.names = NULL)
w_results_2021 <- read.csv("wta_matches_2021.csv", row.names = NULL)
w_results_2022 <- read.csv("wta_matches_2022.csv", row.names = NULL)
w_results_2023 <- read.csv("wta_matches_2023.csv", row.names = NULL)
w_results_2024 <- read.csv("wta_matches_2024.csv", row.names = NULL)

w_results <- rbind(w_results_2005, w_results_2006, w_results_2007, w_results_2008, w_results_2009, w_results_2010, w_results_2011, w_results_2012, w_results_2013, w_results_2014, w_results_2015, w_results_2016, w_results_2017, w_results_2018, w_results_2019, w_results_2020, w_results_2021, w_results_2022, w_results_2023, w_results_2024)

rm(w_results_2005, w_results_2006, w_results_2007, w_results_2008, w_results_2009, w_results_2010, w_results_2011, w_results_2012, w_results_2013, w_results_2014, w_results_2015, w_results_2016, w_results_2017, w_results_2018, w_results_2019, w_results_2020, w_results_2021, w_results_2022, w_results_2023, w_results_2024)

setwd("..")
setwd("Mens doubles")

doubles_2005 <- read.csv("atp_matches_doubles_2005.csv", row.names = NULL)
doubles_2006 <- read.csv("atp_matches_doubles_2006.csv", row.names = NULL)
doubles_2007 <- read.csv("atp_matches_doubles_2007.csv", row.names = NULL)
doubles_2008 <- read.csv("atp_matches_doubles_2008.csv", row.names = NULL)
doubles_2009 <- read.csv("atp_matches_doubles_2009.csv", row.names = NULL)
doubles_2010 <- read.csv("atp_matches_doubles_2010.csv", row.names = NULL)
doubles_2011 <- read.csv("atp_matches_doubles_2011.csv", row.names = NULL)
doubles_2012 <- read.csv("atp_matches_doubles_2012.csv", row.names = NULL)
doubles_2013 <- read.csv("atp_matches_doubles_2013.csv", row.names = NULL)
doubles_2014 <- read.csv("atp_matches_doubles_2014.csv", row.names = NULL)
doubles_2015 <- read.csv("atp_matches_doubles_2015.csv", row.names = NULL)
doubles_2016 <- read.csv("atp_matches_doubles_2016.csv", row.names = NULL)
doubles_2017 <- read.csv("atp_matches_doubles_2017.csv", row.names = NULL)
doubles_2018 <- read.csv("atp_matches_doubles_2018.csv", row.names = NULL)
doubles_2019 <- read.csv("atp_matches_doubles_2019.csv", row.names = NULL)
doubles_2020 <- read.csv("atp_matches_doubles_2020.csv", row.names = NULL)

doubles_results <- rbind(doubles_2005, doubles_2006, doubles_2007, doubles_2008, doubles_2009, doubles_2010, doubles_2011, doubles_2012, doubles_2013, doubles_2014, doubles_2015, doubles_2016, doubles_2017, doubles_2018, doubles_2019, doubles_2020)

rm(doubles_2005, doubles_2006, doubles_2007, doubles_2008, doubles_2009, doubles_2010, doubles_2011, doubles_2012, doubles_2013, doubles_2014, doubles_2015, doubles_2016, doubles_2017, doubles_2018, doubles_2019, doubles_2020)

setwd("..")

men <- read.csv("atp_players.csv")
women <- read.csv("wta_players.csv")

men$wikidata_id <- NULL
women$wikidata_id <- NULL

keep <- c("tourney_name", "surface", "tourney_date", "winner_id", "winner_seed", "loser_id", "loser_seed", "score", "best_of", "round", "minutes", "w_ace", "w_df", "w_svpt", "w_1stIn", "w_1stWon", "w_2ndWon", "w_SvGms", "w_bpSaved", "w_bpFaced", "l_ace", "l_df", "l_svpt", "l_1stIn", "l_1stWon", "l_2ndWon", "l_SvGms", "l_bpSaved", "l_bpFaced", "winner_rank", "winner_rank_points", "loser_rank", "loser_rank_points")

doub_keep <- c("tourney_name", "surface", "tourney_date", "winner1_id", "winner2_id", "winner_seed", "loser1_id", "loser2_id", "loser_seed", "score", "best_of", "round", "minutes", "w_ace", "w_df", "w_svpt", "w_1stIn", "w_1stWon", "w_2ndWon", "w_SvGms", "w_bpSaved", "w_bpFaced", "l_ace", "l_df", "l_svpt", "l_1stIn", "l_1stWon", "l_2ndWon", "l_SvGms", "l_bpSaved", "l_bpFaced", "winner1_rank", "winner1_rank_points", "loser1_rank", "loser1_rank_points", "winner2_rank", "winner2_rank_points", "loser2_rank", "loser2_rank_points")

results <- results[keep]
w_results <- w_results[keep]
doubles_results <- doubles_results[doub_keep]

old_men <- c()
old_women <- c()

for (i in 1:nrow(men)) {
  id <- men$player_id[i]
  if (id %in% results$winner_id | id %in% results$loser_id | id %in% doubles_results$winner1_id | id %in% doubles_results$winner2_id | id %in% doubles_results$loser1_id | id %in% doubles_results$loser2_id) {
    next
  } else {
    old_men <- c(old_men, i)
  }
}

for (i in 1:nrow(women)) {
  id <- women$player_id[i]
  if (id %in% w_results$winner_id | id %in% w_results$loser_id) {
    next
  } else {
    old_women <- c(old_women, i)
  }
}

men <- men[-old_men, ]
women <- women[-old_women, ]

write.csv(men, "atp_players.csv", row.names = FALSE)
write.csv(women, "wta_players.csv", row.names = FALSE)

write.csv(results, "mens_singles.csv", row.names = FALSE)
write.csv(w_results, "womens_singles.csv", row.names = FALSE)
write.csv(doubles_results, "mens_doubles.csv", row.names = FALSE)