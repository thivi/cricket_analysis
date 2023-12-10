library(tidyverse)

file_path <- ""

odi_data <- list.files(
  path = file_path,
  pattern = ".*[^_info]\\.csv",
  full.names = TRUE
) %>%
  map_df(~ read.csv(., colClasses = c("season" = "character"))) %>%
  mutate(start_date = as.Date(start_date, format = "%Y-%m-%d"))

wc_cycle <- odi_data %>%
  filter(start_date > as.Date("2019-07-14"))

wickets <- wc_cycle %>%
  filter(wicket_type != "") %>%
  group_by(match_id, batting_team) %>%
  mutate(wkt_no = seq_len(n())) %>%
  complete(wkt_no = 1:10, fill = list(ball = 50)) %>%
  mutate(ball = ifelse(
    floor(ball) != 0,
    floor(ball) + ((ball %% floor(ball) * 10) / 6),
    floor(ball) + (ball * 10 / 6)
  ))

teams <- c(
  "Sri Lanka",
  "India",
  "Pakistan",
  "England",
  "West Indies",
  "South Africa",
  "New Zealand",
  "Bangladesh",
  "Australia",
  "Afghanistan",
  "Zimbabwe"
)
entry_point <- wickets %>%
  group_by(batting_team, wkt_no) %>%
  summarise_at(vars(ball), list(ball = mean)) %>%
  mutate(role = wkt_no + 2) %>%
  mutate(ball = ifelse(
    floor(ball) != 0,
    floor(ball) + (ball %% floor(ball) * 6) / 10,
    floor(ball) + ((ball * 6) / 10)
  )) %>%
  mutate(ball = round(ball, digits = 1)) %>%
  mutate(ball = ifelse(
    round(ball %% floor(ball), digits = 1) == 0.6,
    floor(ball) + 1,
    ball
  )) %>%
  filter(role < 12) %>%
  filter(batting_team %in% teams) %>%
  write.csv("./entry_points.csv")
