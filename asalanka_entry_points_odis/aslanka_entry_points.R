library(tidyverse)
library(ggplot2)

odi_data <- list.files(
  path = "C:/Users/thevi/Downloads/odis_nov/",
  pattern = ".*[^_info]\\.csv",
  full.names = TRUE
) %>%
  map_df(~ read.csv(., colClasses = c("season" = "character"))) %>%
  mutate(start_date = as.Date(start_date, format = "%Y-%m-%d"))

find_position <- function(id, pteam, pplayer) {
  pos <- read.csv(
    paste("C:/Users/thevi/Downloads/odis_nov/", id, "_info.csv", sep = ""),
    header = FALSE,
    col.names = c("type", "type1", "team", "player", "id")
  ) %>%
    filter(type == "info" & type1 == "player", team == pteam) %>%
    mutate(pos = 1:n()) %>%
    filter(player == pplayer)

  return(pos$pos)
}

asalanka <- odi_data %>%
  group_by(match_id, striker, batting_team, bowling_team) %>%
  summarise(
    runs = sum(runs_off_bat),
    balls = sum(is.na(wides)),
    dismissed = ifelse(
      any(wicket_type != "" & player_dismissed == striker),
      "Yes", "No"
    ),
    entry = first(ball),
  ) %>%
  mutate(entry_over = (entry %/% 5) * 5) %>%
  arrange(match_id, batting_team, entry) %>%
  filter(striker == "KIC Asalanka") %>%
  mutate(pos = find_position(match_id, batting_team, striker))

asalanka_final <- asalanka %>%
  group_by(entry_over) %>%
  summarise(
    Average = sum(runs) / sum(ifelse(dismissed == "Yes", 1, 0)),
    "Strike Rate" = sum(runs) / sum(balls) * 100,
    count = n()
  ) %>%
  mutate(
    "Entry Over" = paste(entry_over, entry_over + 5, sep = "-"),
    entry_over = NULL
  ) %>%
  write.csv("C:/Users/thevi/Desktop/asalanka.csv")

as <- asalanka %>%
  filter(entry_over >= 25 & bowling_team != "United Arab Emirates")
average <- sum(as$runs) / sum(ifelse(as$dismissed == "Yes", 1, 0))
sr <- sum(as$runs) / sum(as$balls) * 100
c <- count(as)
