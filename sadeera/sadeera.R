library(tidyverse)
library(ggplot2)

# Match no: 1395700 first 5 overs missing

odi_data <- read.csv(file = "C:/Users/thevi/Downloads/odi_bbb.csv") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

since2023 <- odi_data %>%
  filter(date > as.Date("2023-01-01"))

wc2023 <- odi_data %>%
  filter(competition == "World Cup 2023")

sadeera <- since2023 %>%
  filter(p_bat == 629076)

sadeera_wc <- wc2023 %>%
  filter(p_bat == 629076)

# Entry point
entry_point <- sadeera %>%
  filter(wide == 0 & byes == 0 & legbyes == 0) %>%
  arrange(p_match, ball_id) %>%
  group_by(p_match) %>%
  summarise(
    entry_over = head(over, 1),
    opposition = head(team_bowl, 1),
    date = head(date, 1), position = head(inns_wkts, 1) + 2,
    spin = sum(ifelse(bowl_kind == "spin bowler", 1, 0)),
    pace = sum(ifelse(bowl_kind == "pace bowler", 1, 0)),
    runs = sum(score),
    outs = sum(ifelse(out == "True" & dismissal != "run out", 1, 0)),
    balls = n()
  ) %>%
  mutate(
    spin_p = spin / (spin + pace) * 100,
    pace_p = pace / (spin + pace) * 100,
    sr = runs / balls * 100
  )

entry_point_sum <- entry_point %>%
  group_by(entry_over) %>%
  summarise(
    avg = sum(runs) / (sum(ifelse(outs == 1, 1, 0))),
    spin = sum(spin) / sum(balls) * 100,
    pace = sum(pace) / sum(balls) * 100,
    sr = sum(runs) / sum(balls) * 100
  )

entry_point_5 <- entry_point %>%
  filter(entry_over <= 5) %>%
  summarise(
    avg = sum(runs) / (sum(ifelse(outs == 1, 1, 0))),
    spin = sum(spin) / sum(balls) * 100,
    pace = sum(pace) / sum(balls) * 100,
    sr = sum(runs) / sum(balls) * 100
  )

entry_point_10 <- entry_point %>%
  filter(entry_over <= 10 & entry_over > 5) %>%
  summarise(
    avg = sum(runs) / (sum(ifelse(outs == 1, 1, 0))),
    spin = sum(spin) / sum(balls) * 100,
    pace = sum(pace) / sum(balls) * 100,
    sr = sum(runs) / sum(balls) * 100
  )

entry_point_15 <- entry_point %>%
  filter(entry_over <= 15 & entry_over > 10) %>%
  summarise(
    avg = sum(runs) / (sum(ifelse(outs == 1, 1, 0))),
    spin = sum(spin) / sum(balls) * 100,
    pace = sum(pace) / sum(balls) * 100,
    sr = sum(runs) / sum(balls) * 100
  )

entry_point_15_50 <- entry_point %>%
  filter(entry_over <= 50 & entry_over > 15) %>%
  summarise(
    avg = sum(runs) / (sum(ifelse(outs == 1, 1, 0))),
    spin = sum(spin) / sum(balls) * 100,
    pace = sum(pace) / sum(balls) * 100,
    sr = sum(runs) / sum(balls) * 100
  )

pak <- sadeera %>%
  filter(wide == 0 & byes == 0 & legbyes == 0) %>%
  filter(p_match == 1384399 & bowl_kind == "pace bowler") %>%
  group_by(bowl_kind) %>%
  summarise(runs = sum(score), balls = n(), sr = sum(score) / n() * 100)

ban <- sadeera %>%
  filter(wide == 0 & byes == 0 & legbyes == 0) %>%
  filter(p_match == 1388403 & bowl_kind == "pace bowler") %>%
  group_by(bowl_kind) %>%
  summarise(runs = sum(score), balls = n(), sr = sum(score) / n() * 100)

ban_bowl <- sadeera %>%
  filter(wide == 0 & byes == 0 & legbyes == 0) %>%
  filter(p_match == 1388403 & bowl_kind == "pace bowler") %>%
  group_by(p_bowl, bowl) %>%
  summarise(runs = sum(score), balls = n(), sr = sum(score) / n() * 100)

mean_position <- mean(entry_point$entry_over)
median_position <- median(entry_point$entry_over)

# Spin vs pace
sadeera_runs <- sadeera %>%
  filter(wide == 0 & byes == 0 & legbyes == 0)

sadeera_wc_runs <- sadeera_wc %>%
  filter(wide == 0 & byes == 0 & legbyes == 0)

summarize_data <- function(p_data) {
  total_runs <- sum(data$score)
  total_balls <- length(data$score)
  dismissals <- p_data %>%
    filter(out == "True" & dismissal != "run out") %>%
    nrow()
  dots <- p_data %>%
    filter(score == 0) %>%
    nrow()
  fours <- p_data %>%
    filter(outcome == "four") %>%
    nrow()
  sixes <- p_data %>%
    filter(outcome == "six") %>%
    nrow()
  nbb <- total_balls - fours + sixes
  nbr <- total_runs - (4 * fours) + (6 * sixes)
  data_summary <- data.frame(
    metric = c(
      "avg",
      "sr",
      "4s",
      "6s",
      "dot%",
      "NBSR",
      "balls",
      "runs",
      "outs"
    ),
    value = c(
      total_balls / dismissals,
      total_runs / total_balls * 100,
      fours,
      sixes,
      dots / total_balls * 100,
      nbr / nbb * 100,
      total_balls,
      total_runs,
      dismissals
    )
  )

  return(data_summary)
}

spin <- sadeera_runs %>%
  filter(bowl_kind == "spin bowler")

spin_summary <- summarize_data(spin)

pace <- sadeera_runs %>%
  filter(bowl_kind == "pace bowler")

pace_summary <- summarize_data(pace)

spin_wc <- sadeera_wc_runs %>%
  filter(bowl_kind == "spin bowler")

spin_summary_wc <- summarize_data(spin_wc)

lb_wc <- spin_wc %>%
  filter(
    bowl_style == "LB" |
      bowl_style == "LBG" |
      bowl_style == "OB/LB" |
      bowl_style == "LWS"
  )

lb_wc_sum <- summarize_data(lb_wc)

sla_wc <- spin_wc %>%
  filter(bowl_style == "SLA")

sla_wc_sum <- summarize_data(sla_wc)

ob_wc <- spin_wc %>%
  filter(bowl_style == "OB")

ob_wc_sum <- summarize_data(ob_wc)

pace_wc <- sadeera_wc_runs %>%
  filter(bowl_kind == "pace bowler")

pace_summary_wc <- summarize_data(pace_wc)

sadeera_0_15_data <- sadeera_runs %>%
  filter(over < 15)

sadeera_0_15 <- summarize_data(sadeera_0_15_data)

sadeera_15_37_data <- sadeera_runs %>%
  filter(over >= 15 & over <= 37)
sadeera_15_37 <- summarize_data(sadeera_15_37_data)

sadeera_37_50_data <- sadeera_runs %>%
  filter(over > 37)
sadeera_37 <- summarize_data(sadeera_37_50_data)

by_teams <- sadeera_37_50_data %>%
  group_by(team_bowl) %>%
  summarise(runs = sum(score), balls = n())

sadeera_15_balls <- (sadeera_0_15 %>% filter(metric == "balls"))$value

sadeera_15_37_balls <- (sadeera_15_37 %>% filter(metric == "balls"))$value

sadeera_37_balls <- (sadeera_37 %>% filter(metric == "balls"))$value

sadeera_total_balls <- sadeera_15_balls + sadeera_15_37_balls + sadeera_37_balls

sadeera_15_p <- sadeera_15_balls / sadeera_total_balls * 100
sadeera_15_37_p <- sadeera_15_37_balls / sadeera_total_balls * 100
sadeera_37_p <- sadeera_37_balls / sadeera_total_balls * 100

# teams_entry_point

since2019 <- odi_data %>%
  filter(date > as.Date("2019-07-15"))

t_entry <- since2019 %>%
  filter(inns_wkts > 0) %>%
  arrange(p_match, ball_id) %>%
  group_by(p_match, inns, inns_wkts, team_bat) %>%
  summarise(entry_point = head(over, 1)) %>%
  mutate(role = inns_wkts + 2) %>%
  group_by(team_bat, role) %>%
  summarise(entry_point = mean(entry_point))

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

t_entry_top <- t_entry %>%
  filter(team_bat %in% teams)

avg_entry <- t_entry_top %>%
  group_by(role) %>%
  summarise(entry_point = mean(entry_point))

teams_top <- c(
  "India",
  "England",
  "South Africa",
  "New Zealand",
  "Australia"
)

avg_entry_top <- t_entry %>%
  filter(team_bat %in% teams_top) %>%
  group_by(role) %>%
  summarise(entry_point_avg = mean(entry_point), median = median(entry_point))
