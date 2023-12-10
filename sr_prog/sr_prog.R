library(tidyverse)
library(ggplot2)

file_path <- ""

odi_data <- list.files(
  path = file_path,
  pattern = ".*[^_info]\\.csv",
  full.names = TRUE
) %>%
  map_df(~ read.csv(., colClasses = c("season" = "character"))) %>%
  mutate(start_date = as.Date(start_date, format = "%Y-%m-%d"))

sl <- odi_data %>%
  filter(batting_team == "Sri Lanka") %>%
  filter(start_date > as.Date("2023-01-01")) %>%
  group_by(match_id, striker) %>%
  mutate(runs = cumsum(runs_off_bat)) %>%
  mutate(balls = cumsum(ifelse(is.na(wides), 1, 0))) %>%
  mutate(sr = runs / balls * 100)


sr_prog <- sl %>%
  group_by(striker) %>%
  filter(n() > 200) %>%
  group_by(striker, balls) %>%
  summarise(sr = mean(sr))

sr_prog %>%
  write.csv("./sr_prog.csv")

sr_prog %>%
  filter(striker != "M Theekshana") %>%
  filter(striker != "PVD Chameera") %>%
  ggplot(aes(x = balls, y = sr, color = striker)) +
  geom_line(size = 1) +
  labs(x = "Ball", y = "Strike Rate", color = "Batsman")
