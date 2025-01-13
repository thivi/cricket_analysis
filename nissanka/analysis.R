library(tidyverse)

data <- read.csv("C:/Users/thevi/Downloads/odata_full.csv")

pathum <- data %>%
    filter(batsman == "Pathum Nissanka") %>%
    mutate(shotZone = ifelse(shot_angle >= 0 & shot_angle < 45, 1, ifelse(shot_angle >= 45 & shot_angle < 90, 0, ifelse(shot_angle >= 90 & shot_angle < 135, 7, ifelse(shot_angle >= 135 & shot_angle < 180, 6, ifelse(shot_angle >= 180 & shot_angle < 225, 5, ifelse(shot_angle >= 225 & shot_angle < 270, 4, ifelse(shot_angle >= 270 & shot_angle < 315, 3, ifelse(shot_angle >= 315 & shot_angle < 360, 2, ifelse(shot_angle == 360, 1, -1)))))))))) %>%
    filter(!is.na(shot_angle))

pathumSpin <- pathum %>%
    filter(bowlerType == "Orthodox" | bowlerType == "Off Spin" | bowlerType == "Leg Spin")

pathumPace <- pathum %>%
    filter(bowlerType == "Fast Seam" | bowlerType == "Medium" | bowlerType == "Fast Medium")

# pODI2021 <- pathum %>%
#     filter(year == 2021) %>%
#     filter(format == "ODI")
# pODI2022 <- pathum %>%
#     filter(year == 2022) %>%
#     filter(format == "ODI")
# pODI2023 <- pathum %>%
#     filter(year == 2023) %>%
#     filter(format == "ODI")
pODI2024 <- pathum %>%
    filter(year == 2024) %>%
    filter(format == "ODI")
pODIPre2024 <- pathum %>%
    filter(year < 2024) %>%
    filter(format == "ODI")
pODI2024Spin <- pathumSpin %>%
    filter(year == 2024) %>%
    filter(format == "ODI")
pODIPre2024Spin <- pathumSpin %>%
    filter(year < 2024) %>%
    filter(format == "ODI")
pODI2024Pace <- pathumPace %>%
    filter(year == 2024) %>%
    filter(format == "ODI")
pODIPre2024Pace <- pathumPace %>%
    filter(year < 2024) %>%
    filter(format == "ODI")

# pT20I2021 <- pathum %>%
#     filter(year == 2021) %>%
#     filter(format == "T20 International")
# pT20I2022 <- pathum %>%
#     filter(year == 2022) %>%
#     filter(format == "T20 International")
# pT20I2023 <- pathum %>%
#     filter(year == 2023) %>%
#     filter(format == "T20 International")
pT20I2024 <- pathum %>%
    filter(year == 2024) %>%
    filter(format == "T20 International")
pT20IPre2024 <- pathum %>%
    filter(year < 2024) %>%
    filter(format == "T20 International")
pT20I2024Spin <- pathumSpin %>%
    filter(year == 2024) %>%
    filter(format == "T20 International")
pT20IPre2024Spin <- pathumSpin %>%
    filter(year < 2024) %>%
    filter(format == "T20 International")
pT20I2024Pace <- pathumPace %>%
    filter(year == 2024) %>%
    filter(format == "T20 International")
pT20IPre2024Pace <- pathumPace %>%
    filter(year < 2024) %>%
    filter(format == "T20 International")

genZones  <- function(data) {
    data %>%
        group_by(shotZone) %>%
        summarise(total_runs = sum(runs_scored), total_balls = sum(ifelse(outcome != "Wide" & outcome != "LegBye", 1, 0)), total_outs = sum(ifelse(is_wicket == 1 & dismissalType != "RunOut", 1, 0))) %>%
        mutate(average = total_runs / total_outs, strike_rate = total_runs / total_balls * 100) %>%
        mutate(runs_percent = total_runs / sum(total_runs) * 100, balls_percent = total_balls / sum(total_balls) * 100)
}

genShots  <- function(data) {
    data %>%
        group_by(shot_type) %>%
        summarise(total_runs = sum(runs_scored), total_balls = sum(ifelse(outcome != "Wide" & outcome != "LegBye", 1, 0)), total_outs = sum(ifelse(is_wicket == 1 & dismissalType != "RunOut", 1, 0))) %>%
        mutate(average = total_runs / total_outs, strike_rate = total_runs / total_balls * 100) %>%
        mutate(runs_percent = total_runs / sum(total_runs) * 100, balls_percent = total_balls / sum(total_balls) * 100)
}

# pODI2021_zones <- genZones(pODI2021)
# pODI2022_zones <- genZones(pODI2022)
# pODI2023_zones <- genZones(pODI2023)
pODI2024_zones <- genZones(pODI2024)
pODIPre2024_zones <- genZones(pODIPre2024)
pODI2024Spin_zones <- genZones(pODI2024Spin)
pODIPre2024Spin_zones <- genZones(pODIPre2024Spin)
pODI2024Pace_zones <- genZones(pODI2024Pace)
pODIPre2024Pace_zones <- genZones(pODIPre2024Pace)

# pODI2021_shots <- genShots(pODI2021)
# pODI2022_shots <- genShots(pODI2022)
# pODI2023_shots <- genShots(pODI2023)
pODI2024_shots <- genShots(pODI2024)
pODIPre2024_shots <- genShots(pODIPre2024)
pODI2024Spin_shots <- genShots(pODI2024Spin)
pODIPre2024Spin_shots <- genShots(pODIPre2024Spin)
pODI2024Pace_shots <- genShots(pODI2024Pace)
pODIPre2024Pace_shots <- genShots(pODIPre2024Pace)

# pT20I2021_zones <- genZones(pT20I2021)
# pT20I2022_zones <- genZones(pT20I2022)
# pT20I2023_zones <- genZones(pT20I2023)
pT20I2024_zones <- genZones(pT20I2024)
pT20IPre2024_zones <- genZones(pT20IPre2024)
pT20I2024Spin_zones <- genZones(pT20I2024Spin)
pT20IPre2024Spin_zones <- genZones(pT20IPre2024Spin)
pT20I2024Pace_zones <- genZones(pT20I2024Pace)
pT20IPre2024Pace_zones <- genZones(pT20IPre2024Pace)


# pT20I2021_shots <- genShots(pT20I2021)
# pT20I2022_shots <- genShots(pT20I2022)
# pT20I2023_shots <- genShots(pT20I2023)
pT20I2024_shots <- genShots(pT20I2024)
pT20IPre2024_shots <- genShots(pT20IPre2024)
pT20I2024Spin_shots <- genShots(pT20I2024Spin)
pT20IPre2024Spin_shots <- genShots(pT20IPre2024Spin)
pT20I2024Pace_shots <- genShots(pT20I2024Pace)
pT20IPre2024Pace_shots <- genShots(pT20IPre2024Pace)

attackedODIPre2024  <- pODIPre2024_shots %>%
    filter(shot_type == "cut" | shot_type == "cutting" | shot_type == "driving" | shot_type == "flick" | shot_type == "glancing" | shot_type == "hooking" | shot_type== "pulling" | shot_type == "scoop" | shot_type == "slog" | shot_type == "sweeping")
attackedODI2024 <- pODI2024_shots %>%
    filter(shot_type == "cut" | shot_type == "cutting" | shot_type == "driving" | shot_type == "flick" | shot_type == "glancing" | shot_type == "hooking" | shot_type == "pulling" | shot_type == "scoop" | shot_type == "slog" | shot_type == "sweeping")
attackedODIPre2024Spin <- pODIPre2024Spin_shots %>%
    filter(shot_type == "cut" | shot_type == "cutting" | shot_type == "driving" | shot_type == "flick" | shot_type == "glancing" | shot_type == "hooking" | shot_type == "pulling" | shot_type == "scoop" | shot_type == "slog" | shot_type == "sweeping")
attackedODI2024Spin <- pODI2024Spin_shots %>%
    filter(shot_type == "cut" | shot_type == "cutting" | shot_type == "driving" | shot_type == "flick" | shot_type == "glancing" | shot_type == "hooking" | shot_type == "pulling" | shot_type == "scoop" | shot_type == "slog" | shot_type == "sweeping")
attackedODI2024Pace <- pODI2024Pace_shots %>%
    filter(shot_type == "cut" | shot_type == "cutting" | shot_type == "driving" | shot_type == "flick" | shot_type == "glancing" | shot_type == "hooking" | shot_type == "pulling" | shot_type == "scoop" | shot_type == "slog" | shot_type == "sweeping")
attackedODIPre2024Pace <- pODIPre2024Pace_shots %>%
    filter(shot_type == "cut" | shot_type == "cutting" | shot_type == "driving" | shot_type == "flick" | shot_type == "glancing" | shot_type == "hooking" | shot_type == "pulling" | shot_type == "scoop" | shot_type == "slog" | shot_type == "sweeping")


attackedT20IPre2024 <- pT20IPre2024_shots %>%
    filter(shot_type == "cut" | shot_type == "cutting" | shot_type == "driving" | shot_type == "flick" | shot_type == "glancing" | shot_type == "hooking" | shot_type == "pulling" | shot_type == "scoop" | shot_type == "slog" | shot_type == "sweeping")
attackedT20I2024 <- pT20I2024_shots %>%
    filter(shot_type == "cut" | shot_type == "cutting" | shot_type == "driving" | shot_type == "flick" | shot_type == "glancing" | shot_type == "hooking" | shot_type == "pulling" | shot_type == "scoop" | shot_type == "slog" | shot_type == "sweeping")
attackedT20IPre2024Spin <- pT20IPre2024Spin_shots %>%
    filter(shot_type == "cut" | shot_type == "cutting" | shot_type == "driving" | shot_type == "flick" | shot_type == "glancing" | shot_type == "hooking" | shot_type == "pulling" | shot_type == "scoop" | shot_type == "slog" | shot_type == "sweeping")
attackedT20I2024Spin <- pT20I2024Spin_shots %>%
    filter(shot_type == "cut" | shot_type == "cutting" | shot_type == "driving" | shot_type == "flick" | shot_type == "glancing" | shot_type == "hooking" | shot_type == "pulling" | shot_type == "scoop" | shot_type == "slog" | shot_type == "sweeping")
attackedT20I2024Pace <- pT20I2024Pace_shots %>%
    filter(shot_type == "cut" | shot_type == "cutting" | shot_type == "driving" | shot_type == "flick" | shot_type == "glancing" | shot_type == "hooking" | shot_type == "pulling" | shot_type == "scoop" | shot_type == "slog" | shot_type == "sweeping")
attackedT20IPre2024Pace <- pT20IPre2024Pace_shots %>%
    filter(shot_type == "cut" | shot_type == "cutting" | shot_type == "driving" | shot_type == "flick" | shot_type == "glancing" | shot_type == "hooking" | shot_type == "pulling" | shot_type == "scoop" | shot_type == "slog" | shot_type == "sweeping")


attackedODIPre2024Percent <- sum(attackedODIPre2024$total_balls) / sum(pODIPre2024_shots$total_balls) * 100
attackedODI2024Percent <- sum(attackedODI2024$total_balls) / sum(pODI2024_shots$total_balls) * 100
attackedODIPre2024SpinPercent <- sum(attackedODIPre2024Spin$total_balls) / sum(pODIPre2024Spin_shots$total_balls) * 100
attackedODI2024SpinPercent <- sum(attackedODI2024Spin$total_balls) / sum(pODI2024Spin_shots$total_balls) * 100
attackedODI2024PacePercent <- sum(attackedODI2024Pace$total_balls) / sum(pODI2024Pace_shots$total_balls) * 100
attackedODIPre2024PacePercent <- sum(attackedODIPre2024Pace$total_balls) / sum(pODIPre2024Pace_shots$total_balls) * 100

attackedT20IPre2024Percent <- sum(attackedT20IPre2024$total_balls) / sum(pT20IPre2024_shots$total_balls) * 100
attackedT20I2024Percent <- sum(attackedT20I2024$total_balls) / sum(pT20I2024_shots$total_balls) * 100
attackedT20IPre2024SpinPercent <- sum(attackedT20IPre2024Spin$total_balls) / sum(pT20IPre2024Spin_shots$total_balls) * 100
attackedT20I2024SpinPercent <- sum(attackedT20I2024Spin$total_balls) / sum(pT20I2024Spin_shots$total_balls) * 100
attackedT20I2024PacePercent <- sum(attackedT20I2024Pace$total_balls) / sum(pT20I2024Pace_shots$total_balls) * 100
attackedT20IPre2024PacePercent <- sum(attackedT20IPre2024Pace$total_balls) / sum(pT20IPre2024Pace_shots$total_balls) * 100


getShotZones <- function(data, zone){
  final <- data %>%
    group_by(shotZone, shot_type) %>%
    summarise(total_runs = sum(runs_scored), total_balls = sum(ifelse(outcome != "Wide" & outcome != "LegBye", 1, 0)), total_outs = sum(ifelse(is_wicket == 1 & dismissalType != "RunOut", 1, 0)))

  final <- left_join(final, zone, by = "shotZone") %>%
    mutate(SR = total_runs.x / total_balls.x * 100, shotPercent = total_balls.x / total_balls.y * 100, runsPercent = total_runs.x / total_runs.y * 100) %>%
    select(shotZone, shot_type, total_runs.x, total_balls.x, , SR, shotPercent, runsPercent)

  return(final)
}

pODI2024Pace_zones_shots <- getShotZones(pODI2024Pace, pODI2024Pace_zones)
pODI2024Spin_zones_shots <- getShotZones(pODI2024Spin, pODI2024Spin_zones)
pODIPre2024Pace_zones_shots <- getShotZones(pODIPre2024Pace, pODIPre2024Pace_zones)
pODIPre2024Spin_zones_shots <- getShotZones(pODIPre2024Spin, pODIPre2024Spin_zones)

pT20I2024Pace_zones_shots <- getShotZones(pT20I2024Pace, pT20I2024Pace_zones)
pT20I2024Spin_zones_shots <- getShotZones(pT20I2024Spin, pT20I2024Spin_zones)
pT20IPre2024Pace_zones_shots <- getShotZones(pT20IPre2024Pace, pT20IPre2024Pace_zones)
pT20IPre2024Spin_zones_shots <- getShotZones(pT20IPre2024Spin, pT20IPre2024Spin_zones)

getLLShots <- function(data) {
    ll <- data %>%
        group_by(length) %>%
        summarise(total_ll_runs = sum(runs_scored), total_ll_balls = sum(ifelse(outcome != "Wide" & outcome != "LegBye", 1, 0)), total_ll_outs = sum(ifelse(is_wicket == 1 & dismissalType != "RunOut", 1, 0)))

    final <- data %>%
        group_by(length, shot_type) %>%
        summarise(total_runs = sum(runs_scored), total_balls = sum(ifelse(outcome != "Wide" & outcome != "LegBye", 1, 0)), total_outs = sum(ifelse(is_wicket == 1 & dismissalType != "RunOut", 1, 0)))

    final <- left_join(final, ll, by = c("length")) %>%
        mutate(SR = total_runs / total_balls * 100, avg = total_runs / total_outs, runsPercent = total_runs / total_ll_runs * 100, ballsPercent = total_balls / total_ll_balls * 100)

    return(final)
}

pODI2024Pace_ll_shots <- getLLShots(pODI2024Pace)
pODIPre2024Pace_ll_shots <- getLLShots(pODIPre2024Pace)
pODI2024Spin_ll_shots <- getLLShots(pODI2024Spin)
pODIPre2024Spin_ll_shots <- getLLShots(pODIPre2024Spin)

pT20I2024Pace_ll_shots <- getLLShots(pT20I2024Pace)
pT20IPre2024Pace_ll_shots <- getLLShots(pT20IPre2024Pace)
pT20I2024Spin_ll_shots <- getLLShots(pT20I2024Spin)
pT20IPre2024Spin_ll_shots <- getLLShots(pT20IPre2024Spin)

el2024p <- pODI2024Pace %>%
    filter(outcome != "Wide" & outcome != "LegBye") %>%
    group_by(elevation) %>%
    summarise(num = n())

elPre2024p <- pODIPre2024Pace %>%
    filter(outcome != "Wide" & outcome != "LegBye") %>%
    group_by(elevation) %>%
    summarise(num = n())

el2024s <- pODI2024Spin %>%
    filter(outcome != "Wide" & outcome != "LegBye") %>%
    group_by(elevation) %>%
    summarise(num = n())

elPre2024s <- pODIPre2024Spin %>%
    filter(outcome != "Wide" & outcome != "LegBye") %>%
    group_by(elevation) %>%
    summarise(num = n())

el2024T20p <- pT20I2024Pace %>%
    filter(outcome != "Wide" & outcome != "LegBye") %>%
    group_by(elevation) %>%
    summarise(num = n())

elPre2024T20p <- pT20IPre2024Pace %>%
    filter(outcome != "Wide" & outcome != "LegBye") %>%
    group_by(elevation) %>%
    summarise(num = n())

el2024T20s <- pT20I2024Spin %>%
    filter(outcome != "Wide" & outcome != "LegBye") %>%
    group_by(elevation) %>%
    summarise(num = n())

elPre2024T20s <- pT20IPre2024Spin %>%
    filter(outcome != "Wide" & outcome != "LegBye") %>%
    group_by(elevation) %>%
    summarise(num = n())

pDOI2024Paceg <- pODI2024PaceODI %>%
    filter(runs_scored == 6) %>%
    group_by(shot_type) %>%
    summarise(num = n())

hvODI2024Pace <- pT20I2024Pace %>%
    filter(outcome != "Wide" & outcome != "LegBye") %>%
    filter(length == "length ball") %>%
    group_by(shot_type, shotZone) %>%
    summarise(total_runs = sum(runs_scored), total_balls = sum(ifelse(outcome != "Wide" & outcome != "LegBye", 1, 0)), total_outs = sum(ifelse(is_wicket == 1 & dismissalType != "RunOut", 1, 0)))

hvODI2024Spin <- pODIPre2024Spin %>%
    filter(outcome != "Wide" & outcome != "LegBye") %>%
    filter(length == "half volley") %>%
    group_by(length, shotZone) %>%
    summarise(total_runs = sum(runs_scored), total_balls = sum(ifelse(outcome != "Wide" & outcome != "LegBye", 1, 0)), total_outs = sum(ifelse(is_wicket == 1 & dismissalType != "RunOut", 1, 0)))

hvT202024Pace <- pT20I2024Pace %>%
    filter(outcome != "Wide" & outcome != "LegBye") %>%
    filter(length == "half volley") %>%
    group_by(length, shotZone) %>%
    summarise(total_runs = sum(runs_scored), total_balls = sum(ifelse(outcome != "Wide" & outcome != "LegBye", 1, 0)), total_outs = sum(ifelse(is_wicket == 1 & dismissalType != "RunOut", 1, 0)))

hvT202024Spin <- pT20I2024Spin %>%
    filter(outcome != "Wide" & outcome != "LegBye") %>%
    filter(length == "half volley") %>%
    group_by(length, shotZone) %>%
    summarise(total_runs = sum(runs_scored), total_balls = sum(ifelse(outcome != "Wide" & outcome != "LegBye", 1, 0)), total_outs = sum(ifelse(is_wicket == 1 & dismissalType != "RunOut", 1, 0)))

pathumSlogs <- pathum %>%
    filter(shot_type == "slog")

pODISpin2024SR  <- pT20IPre2024Spin %>%
    filter(outcome != "Wide" & outcome != "LegBye") %>%
    group_by(batsman) %>%
    summarise(total_runs = sum(runs_scored), total_balls = sum(ifelse(outcome != "Wide" & outcome != "LegBye", 1, 0)), total_outs = sum(ifelse(is_wicket == 1 & dismissalType != "RunOut", 1, 0))) %>%
    mutate(SR = total_runs / total_balls * 100)
