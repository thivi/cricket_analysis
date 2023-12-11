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
  arrange(p_match, ball_id) %>%
  group_by(p_match) %>%
  summarise(
    entry_over = head(over, 1),
    opposition = head(team_bowl, 1),
    date = head(date, 1), position = head(inns_wkts, 1) + 2,
    spin = sum(ifelse(bowl_kind == "spin bowler", 1, 0)),
    pace = sum(ifelse(bowl_kind == "pace bowler", 1, 0)),
    runs = sum(batruns),
    outs = sum(ifelse(out == "True" & dismissal != "run out", 1, 0)),
    balls = sum(ballfaced)
  ) %>%
  mutate(
    spin_p = spin / (spin + pace) * 100,
    pace_p = pace / (spin + pace) * 100,
    sr = runs / balls * 100
  )

entry_point_sum <- entry_point %>%
  group_by(entry_over) %>%
  summarise(
    inns = n(),
    avg = sum(runs) / (sum(ifelse(outs == 1, 1, 0))),
    spin = sum(spin) / sum(balls) * 100,
    pace = sum(pace) / sum(balls) * 100,
    sr = sum(runs) / sum(balls) * 100
  )

entry_point_5 <- entry_point %>%
  filter(entry_over <= 5) %>%
  summarise(
    inns = n(),
    balls = sum(balls),
    avg = sum(runs) / (sum(ifelse(outs == 1, 1, 0))),
    spin = sum(spin) / sum(balls) * 100,
    pace = sum(pace) / sum(balls) * 100,
    sr = sum(runs) / sum(balls) * 100
  ) %>%
  mutate(overs = "0-5")

entry_point_10 <- entry_point %>%
  filter(entry_over <= 10 & entry_over > 5) %>%
  summarise(
    inns = n(),
    balls = sum(balls),
    avg = sum(runs) / (sum(ifelse(outs == 1, 1, 0))),
    spin = sum(spin) / sum(balls) * 100,
    pace = sum(pace) / sum(balls) * 100,
    sr = sum(runs) / sum(balls) * 100
  ) %>%
  mutate(overs = "6-10")

entry_point_15 <- entry_point %>%
  filter(entry_over <= 15 & entry_over > 10) %>%
  summarise(
    inns = n(),
    balls = sum(balls),
    avg = sum(runs) / (sum(ifelse(outs == 1, 1, 0))),
    spin = sum(spin) / sum(balls) * 100,
    pace = sum(pace) / sum(balls) * 100,
    sr = sum(runs) / sum(balls) * 100
  ) %>%
  mutate(overs = "11-15")

entry_point_15_20 <- entry_point %>%
  filter(entry_over <= 20 & entry_over > 15) %>%
  summarise(
    inns = n(),
    balls = sum(balls),
    avg = sum(runs) / (sum(ifelse(outs == 1, 1, 0))),
    spin = sum(spin) / sum(balls) * 100,
    pace = sum(pace) / sum(balls) * 100,
    sr = sum(runs) / sum(balls) * 100
  ) %>%
  mutate(overs = "16-20")

entry_point_20_25 <- entry_point %>%
  filter(entry_over <= 25 & entry_over > 20) %>%
  summarise(
    inns = n(),
    balls = sum(balls),
    avg = sum(runs) / (sum(ifelse(outs == 1, 1, 0))),
    spin = sum(spin) / sum(balls) * 100,
    pace = sum(pace) / sum(balls) * 100,
    sr = sum(runs) / sum(balls) * 100
  ) %>%
  mutate(overs = "21-25")

entry_point_25_30 <- entry_point %>%
  filter(entry_over <= 30 & entry_over > 25) %>%
  summarise(
    inns = n(),
    balls = sum(balls),
    avg = sum(runs) / (sum(ifelse(outs == 1, 1, 0))),
    spin = sum(spin) / sum(balls) * 100,
    pace = sum(pace) / sum(balls) * 100,
    sr = sum(runs) / sum(balls) * 100
  ) %>%
  mutate(overs = "26-30")

entry_point_35_40 <- entry_point %>%
  filter(entry_over <= 40 & entry_over > 35) %>%
  summarise(
    inns = n(),
    balls = sum(balls),
    avg = sum(runs) / (sum(ifelse(outs == 1, 1, 0))),
    spin = sum(spin) / sum(balls) * 100,
    pace = sum(pace) / sum(balls) * 100,
    sr = sum(runs) / sum(balls) * 100
  ) %>%
  mutate(overs = "36-40")

entry_points_aggregate <- rbind(
  entry_point_5,
  entry_point_10,
  entry_point_15,
  entry_point_15_20,
  entry_point_20_25,
  entry_point_25_30,
  entry_point_35_40
)

write.csv(entry_points_aggregate,
  file = "./sadeera/entry_points_aggregate.csv"
)

entry_point_15_50 <- entry_point %>%
  filter(entry_over <= 50 & entry_over > 15) %>%
  summarise(
    inns = n(),
    avg = sum(runs) / (sum(ifelse(outs == 1, 1, 0))),
    spin = sum(spin) / sum(balls) * 100,
    pace = sum(pace) / sum(balls) * 100,
    sr = sum(runs) / sum(balls) * 100
  )

mean_position <- mean(entry_point$entry_over)
median_position <- median(entry_point$entry_over)

# Spin vs pace

summarize_data <- function(p_data) {
  total_runs <- sum(p_data$batruns)
  total_balls <- sum(p_data$ballfaced)
  dismissals <- p_data %>%
    filter(out == "True" & dismissal != "run out") %>%
    nrow()
  dots <- p_data %>%
    filter(batruns == 0) %>%
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
      "Runs",
      "Balls",
      "Dismissals",
      "Average",
      "Strike rate",
      "4s",
      "6s",
      "Dot percentage",
      "NBSR"
    ),
    value = c(
      total_runs,
      total_balls,
      dismissals,
      total_runs / dismissals,
      total_runs / total_balls * 100,
      fours,
      sixes,
      dots / total_balls * 100,
      nbr / nbb * 100
    )
  )

  return(data_summary)
}

spin <- sadeera %>%
  filter(bowl_kind == "spin bowler")

spin_summary <- summarize_data(spin)

pace <- sadeera %>%
  filter(bowl_kind == "pace bowler")

pace_summary <- summarize_data(pace)

spin_wc <- sadeera_wc %>%
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

spin_breakdown <- full_join(lb_wc_sum, sla_wc_sum, by = "metric") %>%
  rename(
    lb = value.x,
    sla = value.y
  ) %>%
  full_join(ob_wc_sum, by = "metric") %>%
  rename(
    ob = value
  )

write.csv(spin_breakdown,
  file = "./sadeera/spin_breakdown.csv"
)

pace_wc <- sadeera_wc %>%
  filter(bowl_kind == "pace bowler")

pace_summary_wc <- summarize_data(pace_wc)

scoring_breakdown <- full_join(pace_summary_wc, pace_summary, by = "metric") %>%
  rename(
    pace_wc = value.x,
    pace = value.y
  ) %>%
  full_join(spin_summary_wc, by = "metric") %>%
  rename(
    spin_wc = value
  ) %>%
  full_join(spin_summary, by = "metric") %>%
  rename(
    spin = value
  )

write.csv(scoring_breakdown,
  file = "./sadeera/scoring_breakdown.csv"
)
sadeera_0_15_data <- sadeera %>%
  filter(over < 15)

sadeera_0_15 <- summarize_data(sadeera_0_15_data)

sadeera_15_37_data <- sadeera %>%
  filter(over >= 15 & over <= 37)
sadeera_15_37 <- summarize_data(sadeera_15_37_data)

sadeera_37_50_data <- sadeera %>%
  filter(over > 37)
sadeera_37 <- summarize_data(sadeera_37_50_data)

sadeera_phase_breakdown <- full_join(
  sadeera_0_15,
  sadeera_15_37,
  by = "metric"
) %>%
  rename(
    sadeera_0_15 = value.x,
    sadeera_15_37 = value.y
  ) %>%
  full_join(sadeera_37, by = "metric") %>%
  rename(
    sadeera_37 = value
  )

write.csv(sadeera_phase_breakdown,
  file = "./sadeera/sadeera_phase_breakdown.csv"
)

by_teams <- sadeera_37_50_data %>%
  group_by(team_bowl) %>%
  summarise(runs = sum(batruns), balls = sum(ballfaced))

pak <- sadeera_37_50_data %>%
  filter(p_match == 1384399 & bowl_kind == "pace bowler") %>%
  group_by(bowl_kind) %>%
  summarise(
    runs = sum(batruns),
    balls = sum(ballfaced),
    sr = sum(batruns) / sum(ballfaced) * 100
  )

ban <- sadeera_37_50_data %>%
  filter(p_match == 1388403 & bowl_kind == "pace bowler") %>%
  group_by(bowl_kind) %>%
  summarise(
    runs = sum(batruns),
    balls = sum(ballfaced),
    sr = sum(batruns) / sum(ballfaced) * 100
  )

ban_bowl <- sadeera_37_50_data %>%
  filter(p_match == 1388403 & bowl_kind == "pace bowler") %>%
  group_by(p_bowl, bowl) %>%
  summarise(
    runs = sum(batruns),
    balls = sum(ballfaced),
    sr = sum(batruns) / sum(ballfaced) * 100
  )

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
  filter(team_bat %in% teams) %>%
  filter(role != 12) %>%
  rename(
    "Batting team" = team_bat, team_bat,
    "Role" = role,
    "Entry over" = entry_point
  )

write.csv(t_entry_top,
  file = "./sadeera/t_entry_top.csv"
)

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

sadeera_spin_pace <- sadeera %>%
  group_by(bowl_kind) %>%
  summarise(
    balls = sum(ballfaced)
  ) %>%
  mutate(
    percent = balls / sum(balls) * 100
  )
