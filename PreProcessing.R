setwd("Mens singles")

results_1968 <- read.csv("atp_matches_1968.csv", row.names = NULL)
results_1969 <- read.csv("atp_matches_1969.csv", row.names = NULL)
results_1970 <- read.csv("atp_matches_1970.csv", row.names = NULL)
results_1971 <- read.csv("atp_matches_1971.csv", row.names = NULL)
results_1972 <- read.csv("atp_matches_1972.csv", row.names = NULL)
results_1973 <- read.csv("atp_matches_1973.csv", row.names = NULL)
results_1974 <- read.csv("atp_matches_1974.csv", row.names = NULL)
results_1975 <- read.csv("atp_matches_1975.csv", row.names = NULL)
results_1976 <- read.csv("atp_matches_1976.csv", row.names = NULL)
results_1977 <- read.csv("atp_matches_1977.csv", row.names = NULL)
results_1978 <- read.csv("atp_matches_1978.csv", row.names = NULL)
results_1979 <- read.csv("atp_matches_1979.csv", row.names = NULL)
results_1980 <- read.csv("atp_matches_1980.csv", row.names = NULL)
results_1981 <- read.csv("atp_matches_1981.csv", row.names = NULL)
results_1982 <- read.csv("atp_matches_1982.csv", row.names = NULL)
results_1983 <- read.csv("atp_matches_1983.csv", row.names = NULL)
results_1984 <- read.csv("atp_matches_1984.csv", row.names = NULL)
results_1985 <- read.csv("atp_matches_1985.csv", row.names = NULL)
results_1986 <- read.csv("atp_matches_1986.csv", row.names = NULL)
results_1987 <- read.csv("atp_matches_1987.csv", row.names = NULL)
results_1988 <- read.csv("atp_matches_1988.csv", row.names = NULL)
results_1989 <- read.csv("atp_matches_1989.csv", row.names = NULL)
results_1990 <- read.csv("atp_matches_1990.csv", row.names = NULL)
results_1991 <- read.csv("atp_matches_1991.csv", row.names = NULL)
results_1992 <- read.csv("atp_matches_1992.csv", row.names = NULL)
results_1993 <- read.csv("atp_matches_1993.csv", row.names = NULL)
results_1994 <- read.csv("atp_matches_1994.csv", row.names = NULL)
results_1995 <- read.csv("atp_matches_1995.csv", row.names = NULL)
results_1996 <- read.csv("atp_matches_1996.csv", row.names = NULL)
results_1997 <- read.csv("atp_matches_1997.csv", row.names = NULL)
results_1998 <- read.csv("atp_matches_1998.csv", row.names = NULL)
results_1999 <- read.csv("atp_matches_1999.csv", row.names = NULL)
results_2000 <- read.csv("atp_matches_2000.csv", row.names = NULL)
results_2001 <- read.csv("atp_matches_2001.csv", row.names = NULL)
results_2002 <- read.csv("atp_matches_2002.csv", row.names = NULL)
results_2003 <- read.csv("atp_matches_2003.csv", row.names = NULL)
results_2004 <- read.csv("atp_matches_2004.csv", row.names = NULL)
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

results <- rbind(results_1968, results_1969, results_1970, results_1971, results_1972, results_1973, results_1974, results_1975, results_1976, results_1977, results_1978, results_1979, results_1980, results_1981, results_1982, results_1983, results_1984, results_1985, results_1986, results_1987, results_1988, results_1989, results_1990, results_1991, results_1992, results_1993, results_1994, results_1995, results_1996, results_1997, results_1998, results_1999, results_2000, results_2001, results_2002, results_2003, results_2004, results_2005, results_2006, results_2007, results_2008, results_2009, results_2010, results_2011, results_2012, results_2013, results_2014, results_2015, results_2016, results_2017, results_2018, results_2019, results_2020, results_2021, results_2022, results_2023, results_2024)

rm(results_1968, results_1969, results_1970, results_1971, results_1972, results_1973, results_1974, results_1975, results_1976, results_1977, results_1978, results_1979, results_1980, results_1981, results_1982, results_1983, results_1984, results_1985, results_1986, results_1987, results_1988, results_1989, results_1990, results_1991, results_1992, results_1993, results_1994, results_1995, results_1996, results_1997, results_1998, results_1999, results_2000, results_2001, results_2002, results_2003, results_2004, results_2005, results_2006, results_2007, results_2008, results_2009, results_2010, results_2011, results_2012, results_2013, results_2014, results_2015, results_2016, results_2017, results_2018, results_2019, results_2020, results_2021, results_2022, results_2023, results_2024)

setwd("..")
setwd("Womens singles")

w_results_1968 <- read.csv("wta_matches_1968.csv", row.names = NULL)
w_results_1969 <- read.csv("wta_matches_1969.csv", row.names = NULL)
w_results_1970 <- read.csv("wta_matches_1970.csv", row.names = NULL)
w_results_1971 <- read.csv("wta_matches_1971.csv", row.names = NULL)
w_results_1972 <- read.csv("wta_matches_1972.csv", row.names = NULL)
w_results_1973 <- read.csv("wta_matches_1973.csv", row.names = NULL)
w_results_1974 <- read.csv("wta_matches_1974.csv", row.names = NULL)
w_results_1975 <- read.csv("wta_matches_1975.csv", row.names = NULL)
w_results_1976 <- read.csv("wta_matches_1976.csv", row.names = NULL)
w_results_1977 <- read.csv("wta_matches_1977.csv", row.names = NULL)
w_results_1978 <- read.csv("wta_matches_1978.csv", row.names = NULL)
w_results_1979 <- read.csv("wta_matches_1979.csv", row.names = NULL)
w_results_1980 <- read.csv("wta_matches_1980.csv", row.names = NULL)
w_results_1981 <- read.csv("wta_matches_1981.csv", row.names = NULL)
w_results_1982 <- read.csv("wta_matches_1982.csv", row.names = NULL)
w_results_1983 <- read.csv("wta_matches_1983.csv", row.names = NULL)
w_results_1984 <- read.csv("wta_matches_1984.csv", row.names = NULL)
w_results_1985 <- read.csv("wta_matches_1985.csv", row.names = NULL)
w_results_1986 <- read.csv("wta_matches_1986.csv", row.names = NULL)
w_results_1987 <- read.csv("wta_matches_1987.csv", row.names = NULL)
w_results_1988 <- read.csv("wta_matches_1988.csv", row.names = NULL)
w_results_1989 <- read.csv("wta_matches_1989.csv", row.names = NULL)
w_results_1990 <- read.csv("wta_matches_1990.csv", row.names = NULL)
w_results_1991 <- read.csv("wta_matches_1991.csv", row.names = NULL)
w_results_1992 <- read.csv("wta_matches_1992.csv", row.names = NULL)
w_results_1993 <- read.csv("wta_matches_1993.csv", row.names = NULL)
w_results_1994 <- read.csv("wta_matches_1994.csv", row.names = NULL)
w_results_1995 <- read.csv("wta_matches_1995.csv", row.names = NULL)
w_results_1996 <- read.csv("wta_matches_1996.csv", row.names = NULL)
w_results_1997 <- read.csv("wta_matches_1997.csv", row.names = NULL)
w_results_1998 <- read.csv("wta_matches_1998.csv", row.names = NULL)
w_results_1999 <- read.csv("wta_matches_1999.csv", row.names = NULL)
w_results_2000 <- read.csv("wta_matches_2000.csv", row.names = NULL)
w_results_2001 <- read.csv("wta_matches_2001.csv", row.names = NULL)
w_results_2002 <- read.csv("wta_matches_2002.csv", row.names = NULL)
w_results_2003 <- read.csv("wta_matches_2003.csv", row.names = NULL)
w_results_2004 <- read.csv("wta_matches_2004.csv", row.names = NULL)
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

w_results <- rbind(w_results_1968, w_results_1969, w_results_1970, w_results_1971, w_results_1972, w_results_1973, w_results_1974, w_results_1975, w_results_1976, w_results_1977, w_results_1978, w_results_1979, w_results_1980, w_results_1981, w_results_1982, w_results_1983, w_results_1984, w_results_1985, w_results_1986, w_results_1987, w_results_1988, w_results_1989, w_results_1990, w_results_1991, w_results_1992, w_results_1993, w_results_1994, w_results_1995, w_results_1996, w_results_1997, w_results_1998, w_results_1999, w_results_2000, w_results_2001, w_results_2002, w_results_2003, w_results_2004, w_results_2005, w_results_2006, w_results_2007, w_results_2008, w_results_2009, w_results_2010, w_results_2011, w_results_2012, w_results_2013, w_results_2014, w_results_2015, w_results_2016, w_results_2017, w_results_2018, w_results_2019, w_results_2020, w_results_2021, w_results_2022, w_results_2023, w_results_2024)

rm(w_results_1968, w_results_1969, w_results_1970, w_results_1971, w_results_1972, w_results_1973, w_results_1974, w_results_1975, w_results_1976, w_results_1977, w_results_1978, w_results_1979, w_results_1980, w_results_1981, w_results_1982, w_results_1983, w_results_1984, w_results_1985, w_results_1986, w_results_1987, w_results_1988, w_results_1989, w_results_1990, w_results_1991, w_results_1992, w_results_1993, w_results_1994, w_results_1995, w_results_1996, w_results_1997, w_results_1998, w_results_1999, w_results_2000, w_results_2001, w_results_2002, w_results_2003, w_results_2004, w_results_2005, w_results_2006, w_results_2007, w_results_2008, w_results_2009, w_results_2010, w_results_2011, w_results_2012, w_results_2013, w_results_2014, w_results_2015, w_results_2016, w_results_2017, w_results_2018, w_results_2019, w_results_2020, w_results_2021, w_results_2022, w_results_2023, w_results_2024)

setwd("..")
setwd("Mens doubles")

doubles_2000 <- read.csv("atp_matches_doubles_2000.csv", row.names = NULL)
doubles_2001 <- read.csv("atp_matches_doubles_2001.csv", row.names = NULL)
doubles_2002 <- read.csv("atp_matches_doubles_2002.csv", row.names = NULL)
doubles_2003 <- read.csv("atp_matches_doubles_2003.csv", row.names = NULL)
doubles_2004 <- read.csv("atp_matches_doubles_2004.csv", row.names = NULL)
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

doubles_results <- rbind(doubles_2000, doubles_2001, doubles_2002, doubles_2003, doubles_2004, doubles_2005, doubles_2006, doubles_2007, doubles_2008, doubles_2009, doubles_2010, doubles_2011, doubles_2012, doubles_2013, doubles_2014, doubles_2015, doubles_2016, doubles_2017, doubles_2018, doubles_2019, doubles_2020)

rm(doubles_2000, doubles_2001, doubles_2002, doubles_2003, doubles_2004, doubles_2005, doubles_2006, doubles_2007, doubles_2008, doubles_2009, doubles_2010, doubles_2011, doubles_2012, doubles_2013, doubles_2014, doubles_2015, doubles_2016, doubles_2017, doubles_2018, doubles_2019, doubles_2020)

setwd("..")
setwd("Mens singles")

men <- read.csv("atp_players.csv")

setwd("..")
setwd("Womens singles")

women <- read.csv("wta_players.csv")

setwd("..")

men$wikidata_id <- NULL
women$wikidata_id <- NULL

results$ht_diff <- results$winner_ht - results$loser_ht
results$age_diff <- results$winner_age - results$loser_age
results$rank_diff <- results$winner_rank - results$loser_rank
w_results$ht_diff <- w_results$winner_ht - w_results$loser_ht
w_results$age_diff <- w_results$winner_age - w_results$loser_age
w_results$rank_diff <- w_results$winner_rank - w_results$loser_rank

remove <- c("tourney_id", "tourney_name", "draw_size", "match_num", "winner_entry", "winner_name", "winner_ht", "winner_ioc", "winner_age", "loser_entry", "loser_name", "loser_ht", "loser_ioc", "loser_age", "best_of", "winner_rank", "winner_rank_points", "loser_rank", "loser_rank_points")

doub_remove <- c("tourney_id", "tourney_name", "draw_size", "match_num", "winner1_entry", "winner1_name", "winner1_ht", "winner1_ioc", "winner1_age", "loser1_entry", "loser1_name", "loser1_ht", "loser1_ioc", "loser1_age", "winner2_entry", "winner2_name", "winner2_ht", "winner2_ioc", "winner2_age", "loser2_entry", "loser2_name", "loser2_ht", "loser2_ioc", "loser2_age", "best_of", "winner1_rank", "winner1_rank_points", "loser1_rank", "loser1_rank_points", "winner2_rank", "winner2_rank_points", "loser2_rank", "loser2_rank_points")

results <- results[, !(colnames(results) %in% remove) ]
w_results <- w_results[, !(colnames(w_results) %in% remove) ]
doubles_results <- doubles_results[, !(colnames(doubles_results) %in% doub_remove) ]

keep_rows <- c()

for (i in 1:nrow(results)) {
  if (results$winner_id[i] %in% men$player_id | results$loser_id[i] %in% men$player_id) {
    keep_rows <- c(keep_rows, i)
  }
}

results <- results[keep_rows , ]

keep_rows <- c()

for (i in 1:nrow(w_results)) {
  if (w_results$winner_id[i] %in% women$player_id | w_results$loser_id[i] %in% women$player_id) {
    keep_rows <- c(keep_rows, i)
  }
}

w_results <- w_results[keep_rows , ]

keep_rows <- c()

for (i in 1:nrow(doubles_results)) {
  if (doubles_results$winner1_id[i] %in% men$player_id | doubles_results$loser1_id[i] %in% men$player_id | doubles_results$winner2_id[i] %in% men$player_id | doubles_results$loser2_id[i] %in% men$player_id) {
    keep_rows <- c(keep_rows, i)
  }
}

doubles_results <- doubles_results[keep_rows , ]

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

recent_results <- rbind(results_2005, results_2006, results_2007, results_2008, results_2009, results_2010, results_2011, results_2012, results_2013, results_2014, results_2015, results_2016, results_2017, results_2018, results_2019, results_2020, results_2021, results_2022, results_2023, results_2024)

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

w_recent_results <- rbind(w_results_2005, w_results_2006, w_results_2007, w_results_2008, w_results_2009, w_results_2010, w_results_2011, w_results_2012, w_results_2013, w_results_2014, w_results_2015, w_results_2016, w_results_2017, w_results_2018, w_results_2019, w_results_2020, w_results_2021, w_results_2022, w_results_2023, w_results_2024)

rm(w_results_2005, w_results_2006, w_results_2007, w_results_2008, w_results_2009, w_results_2010, w_results_2011, w_results_2012, w_results_2013, w_results_2014, w_results_2015, w_results_2016, w_results_2017, w_results_2018, w_results_2019, w_results_2020, w_results_2021, w_results_2022, w_results_2023, w_results_2024)

setwd("..")

row.names(men) <- men$player_id
row.names(women) <- women$player_id

colnames(recent_results)[8] <- "player_id"

win_men <- merge(men, recent_results)[1:7]
win_men <- win_men[!duplicated(win_men), ]

recent_results$player_id <- NULL

colnames(recent_results)[15] <- "player_id"

lose_men <- merge(men, recent_results)[1:7]
lose_men <- lose_men[!duplicated(lose_men), ]

men <- rbind(win_men, lose_men)
men <- men[!duplicated(men), ]

colnames(w_recent_results)[8] <- "player_id"

win_women <- merge(women, w_recent_results)[1:7]
win_women <- win_women[!duplicated(win_women), ]

w_recent_results$player_id <- NULL

colnames(w_recent_results)[15] <- "player_id"

lose_women <- merge(women, w_recent_results)[1:7]
lose_women <- lose_women[!duplicated(lose_women), ]

women <- rbind(win_women, lose_women)
women <- women[!duplicated(women), ]

write.csv(men, "current_men.csv")
write.csv(women, "current_women.csv")

write.csv(results, "mens_singles.csv", row.names = FALSE)
write.csv(w_results, "womens_singles.csv", row.names = FALSE)
write.csv(doubles_results, "mens_doubles.csv", row.names = FALSE)