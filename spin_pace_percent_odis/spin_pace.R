library(tidyverse)
library(ggplot2)

# Match no: 1395700 first 5 overs missing

file_path <- ""

odi_data <- read.csv(file = file_path) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

since2019 <- odi_data %>%
  filter(date > as.Date("2019-07-15"))

bowler_kind <- since2019 %>%
  group_by(p_match, inns, over) %>%
  summarise(bowl_kind = head(bowl_kind, 1)) %>%
  group_by(over) %>%
  summarise(
    total = n(),
    spin = sum(ifelse(bowl_kind == "spin bowler", 1, 0)),
    pace = sum(ifelse(bowl_kind == "pace bowler", 1, 0)),
    others = sum(ifelse(bowl_kind == "mixture/unknown", 1, 0))
  ) %>%
  mutate(
    spin_percent = spin / total * 100,
    pace_percent = pace / total * 100,
    others_percent = others / total * 100
  )

bowler_type_percent <- data.frame(
  bowler_kind$over,
  bowler_kind$spin_percent,
  bowler_kind$pace_percent,
  bowler_kind$others_percent
)

write.csv(bowler_type_percent,
  file = "C:/Users/thevi/Desktop/bowler_type_percent.csv"
)

bowler_kind_middle <- since2019 %>%
  group_by(p_match, inns, over) %>%
  summarise(bowl_kind = head(bowl_kind, 1)) %>%
  group_by(over) %>%
  filter(over >= 15 & over <= 37) %>%
  summarise(
    total = n(),
    spin = sum(ifelse(bowl_kind == "spin bowler", 1, 0)),
    pace = sum(ifelse(bowl_kind == "pace bowler", 1, 0)),
    others = sum(ifelse(bowl_kind == "mixture/unknown", 1, 0))
  ) %>%
  mutate(
    spin_percent = spin / total * 100,
    pace_percent = pace / total * 100,
    others_percent = others / total * 100
  )

spin_average_middle <-
  sum(bowler_kind_middle$spin) / sum(bowler_kind_middle$total) * 100
