library(tidyverse)
library(ggplot2)

data <- read.csv("C:/Users/thevi/Downloads/impact_sub_data.csv") %>%
    mutate(team = ifelse(team == "Royal Challengers Bengaluru", "Royal Challengers Bangalore", team)) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    mutate(year=year(date))

batBall <- read.csv("C:/Users/thevi/Downloads/batter_balls.csv") %>%
    mutate(balls = ifelse(balls == "-", 0, balls)) %>%
    mutate(balls = as.numeric(balls))

bowlBall <- read.csv("C:/Users/thevi/Downloads/bowler_balls.csv") %>%
    mutate(balls = ifelse(balls == "-", 0, balls)) %>%
    mutate(balls = as.numeric(balls))

ballData <- merge(batBall, bowlBall, by = "id", all = TRUE) %>%
    mutate(balls.bowl = ifelse(is.na(balls.y), 0, balls.y)) %>%
    mutate(balls.bat = ifelse(is.na(balls.x), 0, balls.x)) %>%
    select(id, player.x, balls.bat, balls.bowl, matches.x) %>%
    mutate(balls.bat.avg = balls.bat / matches.x) %>%
    mutate(balls.bowl.avg = balls.bowl / matches.x)
quantile(ballData$balls.bat.avg, 0.5)
ar <- ballData %>%
    filter(balls.bat.avg >= quantile(balls.bat.avg, 0.5) & balls.bowl.avg >= quantile(balls.bowl.avg, 0.5)) %>%
    mutate(role = "allrounder")

bat <- ballData %>%
    filter(balls.bat.avg >= quantile(balls.bat.avg, 0.5) & balls.bowl.avg < quantile(balls.bowl.avg, 0.5))

bowl <- ballData %>%
    filter(balls.bat.avg < quantile(balls.bat.avg, 0.5) & balls.bowl.avg >= quantile(balls.bowl.avg, 0.5))

utility <- ballData %>%
    filter(balls.bat.avg < quantile(balls.bat.avg, 0.5) & balls.bowl.avg < quantile(balls.bowl.avg, 0.5)) %>%
    mutate(role = "utility")

batAR <- bat %>%
    filter(balls.bowl.avg >= quantile(balls.bowl.avg, 0.75)) %>%
    mutate(role = "batting allrounder")
bowlAR <- bowl %>%
    filter(balls.bat.avg >= quantile(balls.bat.avg, 0.75)) %>%
    mutate(role = "bowling allrounder")
pureBat <- bat %>%
    filter(balls.bowl.avg < quantile(balls.bowl.avg, 0.75)) %>%
    mutate(role = "batter")
pureBowl <- bowl %>%
    filter(balls.bat.avg < quantile(balls.bat.avg, 0.75)) %>%
    mutate(role = "bowler")


roleData  <-  rbind(ar, utility, batAR, bowlAR, pureBat, pureBowl)

ggplot(roleData, aes(x = balls.bat.avg, y = balls.bowl.avg, color = role)) +
    geom_point() +
    labs(title = "Balls faced vs Balls bowled", x = "Balls faced per match", y = "Balls bowled per match")

csvRoles <- roleData %>%
    select(player.x, role, balls.bat.avg, balls.bowl.avg) %>%
    write.csv("C:/Users/thevi/Desktop/roles.csv")

roles <- roleData %>%
    select(id, role)

subData <- merge(data, roles, by = "id", all.x = TRUE)

rolesComp <- subData %>%
    group_by(id) %>%
    summarise(name = unique(player), roleCric = unique(role.x), role = unique(role.y))

combo <- subData %>%
    group_by(match_id, team) %>%
    summarise(batters = sum(role.y == "batter"), ar = sum(role.y == "allrounder"), bowlers = sum(role.y == "bowler"), utility = sum(role.y == "utility"), batting_allrounder = sum(role.y == "batting allrounder"), bowling_allrounder = sum(role.y == "bowling allrounder"), runs = mean(runs), year = unique(year), date = unique(date), batting_first = unique(batting_first))

comboSum <- subData %>%
    group_by(match_id, team) %>%
    summarise(batters = sum(role.y == "batter" | role.y == "batting allrounder"), ar = sum(role.y == "allrounder"), bowlers = sum(role.y == "bowler" | role.y == "bowling allrounder"), utility = sum(role.y == "utility"), runs = mean(runs), date = unique(date), year = unique(year), batting_first = unique(batting_first))

#2022
bowl3_2022  <- comboSum %>%
    filter(year == 2022) %>%
    filter(bowlers == 3 )

bowl4_2022 <- comboSum %>%
    filter(year == 2022) %>%
    filter(bowlers == 4)

bowl5_2022 <- comboSum %>%
    filter(year == 2022) %>%
    filter(bowlers == 5)

bowl_2022  <- comboSum %>%
    filter(year == 2022)

nrow(bowl3_2022) + nrow(bowl4_2022) + nrow(bowl5_2022) == nrow(bowl_2022)
bowl3_2022_percent <- nrow(bowl3_2022) / nrow(bowl_2022) * 100
bowl4_2022_percent <- nrow(bowl4_2022) / nrow(bowl_2022) * 100
bowl5_2022_percent <- nrow(bowl5_2022) / nrow(bowl_2022) * 100

#Since 2022
bowl3  <-  comboSum %>%
    filter(year > 2022) %>%
    filter(bowlers == 3)

bowl4 <- comboSum %>%
    filter(year > 2022) %>%
    filter(bowlers == 4)

bowl5 <- comboSum %>%
    filter(year > 2022) %>%
    filter(bowlers == 5)

bowl2023  <- comboSum %>%
    filter(year > 2022)

bowl6 <- comboSum %>%
    filter(year > 2022) %>%
    filter(bowlers == 6)

bowl3_percent  <- nrow(bowl3) / nrow(bowl2023) * 100
bowl4_percent  <- nrow(bowl4) / nrow(bowl2023) * 100
bowl5_percent  <- nrow(bowl5) / nrow(bowl2023) * 100
bowl6_percent  <- nrow(bowl6) / nrow(bowl2023) * 100

bowl_percentages <- data.frame(year = c(2022, 2023),
                               bowl5 = c(bowl5_2022_percent, bowl5_percent),
                               bowl4 = c(bowl4_2022_percent, bowl4_percent),
                               bowl3 = c(bowl3_2022_percent, bowl3_percent),
                               bowl6 = c(NA, bowl6_percent))

write.csv(bowl_percentages, "C:/Users/thevi/Desktop/bowl_percentages.csv")


averageComboSum2022 <- comboSum %>%
    group_by(year) %>%
    summarise(batters = mean(batters), ar = mean(ar), bowlers = mean(bowlers), utility = mean(utility),  runs = mean(runs))  %>%
    filter(year == 2022)

averageComboSum2023 <- comboSum %>%
    filter(year > 2022) %>%
    mutate(year = 2023) %>%
    group_by(year) %>%
    summarise(batters = mean(batters), ar = mean(ar), bowlers = mean(bowlers), utility = mean(utility),  runs = mean(runs))

averageComboSum <- rbind(averageComboSum2022, averageComboSum2023)

averageCombo2022 <- combo %>%
    filter(year == 2022) %>%
    group_by(year) %>%
    summarise(batters = mean(batters), battingAR = mean(batting_allrounder), bowlingAR = mean(bowling_allrounder), ar = mean(ar), bowlers = mean(bowlers), utility = mean(utility),  runs = mean(runs))

averageCombo2023 <- combo %>%
    filter(year > 2022) %>%
    mutate(year = 2023) %>%
    group_by(year) %>%
    summarise(batters = mean(batters), battingAR = mean(batting_allrounder), bowlingAR = mean(bowling_allrounder), ar = mean(ar), bowlers = mean(bowlers), utility = mean(utility),  runs = mean(runs))

averageCombo <- rbind(averageCombo2022, averageCombo2023)

averageComboSumPer <- averageComboSum %>%
    mutate(tot = 11) %>%
    mutate(batters = (batters / tot) * 100) %>%
    mutate(ar = (ar / tot) * 100) %>%
    mutate(bowlers = (bowlers / tot) * 100) %>%
    mutate(utility = (utility / tot) * 100)

write.csv(averageComboSumPer, "C:/Users/thevi/Desktop/averageComboSumPer.csv")

averageComboPer <- averageCombo %>%
    mutate(tot = batters + ar + bowlers + utility + battingAR + bowlingAR) %>%
    mutate(batters = (batters / tot) * 100) %>%
    mutate(ar = (ar / tot) * 100) %>%
    mutate(bowlers = (bowlers / tot) * 100) %>%
    mutate(utility = (utility / tot) * 100) %>%
    mutate(battingAR = (battingAR / tot) * 100) %>%
    mutate(bowlingAR = (bowlingAR / tot) * 100)


avgRunsBowl <- data.frame(
    year = c(2022, 2023),
    bowl3 = c(
        (comboSum %>%
            filter(year == 2022) %>%
            filter(batting_first == "True") %>%
            filter(bowlers == 3) %>%
            group_by(year) %>%
            summarise(mean(runs)))$`mean(runs)`,
       ( comboSum %>%
            filter(year > 2022) %>%
            filter(batting_first == "True") %>%
            filter(bowlers == 3) %>%
            mutate(year = 2023) %>%
            group_by(year) %>%
            summarise(mean(runs)))$`mean(runs)`
    ),
    bowl4 = c(
        (comboSum %>%
            filter(year == 2022) %>%
            filter(batting_first == "True") %>%
            filter(bowlers == 4) %>%
            group_by(year) %>%
            summarise(mean(runs)))$`mean(runs)`,
        (comboSum %>%
            filter(year > 2022) %>%
            filter(batting_first == "True") %>%
            filter(bowlers == 4) %>%
            mutate(year = 2023) %>%
            group_by(year) %>%
            summarise(mean(runs)))$`mean(runs)`
    ),
    bowl5 = c(
        (comboSum %>%
            filter(year == 2022) %>%
            filter(batting_first == "True") %>%
            filter(bowlers == 5) %>%
            group_by(year) %>%
            summarise(mean(runs)))$`mean(runs)`,
        (comboSum %>%
            filter(year > 2022) %>%
            filter(batting_first == "True") %>%
            filter(bowlers == 5) %>%
            mutate(year = 2023) %>%
            group_by(year) %>%
            summarise(mean(runs)))$`mean(runs)`
    ),
    bowl6 = c(
        0,
        (comboSum %>%
            filter(year > 2022) %>%
            filter(batting_first == "True") %>%
            filter(bowlers == 6) %>%
            mutate(year = 2023) %>%
            group_by(year) %>%
            summarise(mean(runs)))$`mean(runs)`
    )
)

avgRuns <- data.frame(
    year = c(2022, 2023),
    runs = c(
        (comboSum %>%
            filter(year == 2022) %>%
            filter(batting_first == "True") %>%
            group_by(year) %>%
            summarise(mean(runs)))$`mean(runs)`,
        (comboSum %>%
            filter(year > 2022) %>%
            filter(batting_first == "True") %>%
            mutate(year = 2023) %>%
            group_by(year) %>%
            summarise(mean(runs)))$`mean(runs)`
    )
)

comboByTeam <- comboSum %>%
    filter(year > 2022) %>%
    group_by(team) %>%
    summarise(batters = mean(batters), ar = mean(ar), bowlers = mean(bowlers), utility = mean(utility),  runs = mean(runs))

write.csv(comboByTeam, "C:/Users/thevi/Desktop/comboByTeam.csv")
teamBowl5 <- comboSum %>%
    filter(year > 2022) %>%
    filter(bowlers == 5) %>%
    group_by(team) %>%
    summarise(times = n())

teamBowl4 <- comboSum %>%
    filter(year > 2022) %>%
    filter(bowlers == 4) %>%
    group_by(team) %>%
    summarise(times = n())

teamBowl3 <- comboSum %>%
    filter(year > 2022) %>%
    filter(bowlers == 3) %>%
    group_by(team) %>%
    summarise(times = n())

teamBowl6 <- comboSum %>%
    filter(year > 2022) %>%
    filter(bowlers == 6) %>%
    group_by(team) %>%
    summarise(times = n())

comboByTeamNum <- merge(teamBowl3, teamBowl4, by = "team", all = TRUE) %>%
    mutate(bowl3 = ifelse(is.na(times.x), 0, times.x)) %>%
    mutate(bowl4 = ifelse(is.na(times.y), 0, times.y)) %>%
    select(-times.x, -times.y) %>%
    merge(teamBowl5, by = "team", all = TRUE) %>%
    mutate(bowl5 = ifelse(is.na(times), 0, times)) %>%
    select(-times) %>%
    merge(teamBowl6, by = "team", all = TRUE) %>%
    mutate(bowl6 = ifelse(is.na(times), 0, times)) %>%
    select(-times)

write.csv(comboByTeamNum, "C:/Users/thevi/Desktop/comboByTeamNum.csv")
teamBowl522 <- comboSum %>%
    filter(year == 2022) %>%
    filter(bowlers == 5) %>%
    group_by(team) %>%
    summarise(times = n())

teamBowl422 <- comboSum %>%
    filter(year == 2022) %>%
    filter(bowlers == 4) %>%
    group_by(team) %>%
    summarise(times = n())

teamBowl322 <- comboSum %>%
    filter(year == 2022) %>%
    filter(bowlers == 3) %>%
    group_by(team) %>%
    summarise(times = n())

teamBowl622 <- comboSum %>%
    filter(year == 2022) %>%
    filter(bowlers == 6) %>%
    group_by(team) %>%
    summarise(times = n())

comboByTeamNum22 <- merge(teamBowl322, teamBowl422, by = "team", all = TRUE) %>%
    mutate(bowl3 = ifelse(is.na(times.x), 0, times.x)) %>%
    mutate(bowl4 = ifelse(is.na(times.y), 0, times.y)) %>%
    select(-times.x, -times.y) %>%
    merge(teamBowl522, by = "team", all = TRUE) %>%
    mutate(bowl5 = ifelse(is.na(times), 0, times)) %>%
    select(-times) %>%
    merge(teamBowl622, by = "team", all = TRUE) %>%
    mutate(bowl6 = ifelse(is.na(times), 0, times)) %>%
    select(-times)

imp <- subData %>%
    filter(impact_sub == "True" | replaced == "True")  %>%
    select(id, match_id, batting_first, team, player, role.y, impact_sub, replaced, over, innings, year)

# If you bat first, you replace a batsman with a bowler.
# Instances when this did not happen.
bat_sub  <-  imp %>%
    filter(batting_first == "True") %>%
    filter(role.y == "batter" | role.y == "batting allrounder") %>%
    filter(impact_sub == "True")

bowl_sub  <- imp %>%
    filter(batting_first == "False") %>%
    filter(role.y == "bowler" | role.y == "bowling allrounder") %>%
    filter(impact_sub == "True")

bat_su1b <- imp %>%
    filter(batting_first == "True") %>%
    filter(role.y == "bowler" | role.y == "bowling allrounder") %>%
    filter(replaced == "True")

bowl_su1b <- imp %>%
    filter(batting_first == "False") %>%
    filter(role.y == "batter" | role.y == "batting allrounder") %>%
    filter(replaced == "True")
