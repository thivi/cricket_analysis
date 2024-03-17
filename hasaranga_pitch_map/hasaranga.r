library(tidyverse)

data <- read.csv("C:/Users/thevi/Downloads/t20s_2013-23_mgd.csv")
print(colnames(data))

hasaranga <- data %>%
    filter(bowl == "Wanindu Hasaranga" | bowl == "Wanindu Hasaranga de Silva")

rashid <- data %>%
    filter(bowl == "Rashid Khan")

comp <- hasaranga %>%
    filter(wide == 0) %>%
    group_by(competition) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs)
hasaranga <- data %>%
    filter(bowl == "Wanindu Hasaranga" | bowl == "Wanindu Hasaranga de Silva")

sl <- hasaranga %>%
    filter(wide == 0) %>%
    filter(country == "Sri Lanka") %>%
    filter(competition == "T20I") %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs)

away <- hasaranga %>%
    filter(wide == 0) %>%
    filter(country != "Sri Lanka") %>%
    filter(team_bat != "Ireland" & team_bat != "Namibia" & team_bat != "Netherlands" & team_bat != "United Arab Emirates") %>%
    filter(competition == "T20I") %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs)

countries <- hasaranga %>%
    filter(wide == 0) %>%
    filter(competition == "T20I") %>%
    filter(team_bat != "Ireland" & team_bat != "Namibia" & team_bat != "Netherlands" & team_bat != "United Arab Emirates") %>%
    group_by(country) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs)

spin <- data %>%
    filter(competition == "IPL") %>%
    filter(wide == 0) %>%
    filter(bowl_kind == "spin bowler") %>%
    group_by(bowl) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    mutate(strikeRate = runs / balls, bowlsr = balls / outs) %>%
    filter(balls > 500)

write.csv(spin, "C:/Users/thevi/Desktop/spin.csv")

averageER <- sum(spin$runs) / sum(spin$balls)
averageSR <- sum(spin$balls) / sum(spin$outs)

hl <- hasaranga %>%
    filter(wide == 0) %>%
    group_by(pitchLength) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    filter(pitchLength != "") %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

rl <- rashid %>%
    filter(wide == 0) %>%
    group_by(pitchLength) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    filter(pitchLength != "") %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

hll <- hasaranga %>%
    filter(wide == 0) %>%
    group_by(pitchLength, pitchLine) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    filter(pitchLength != "" & pitchLine != "") %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

rll <- rashid %>%
    filter(wide == 0) %>%
    group_by(pitchLength, pitchLine) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    filter(pitchLength != "" & pitchLine != "") %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

hlsl <- hasaranga %>%
    filter(wide == 0) %>%
    filter(country == "Sri Lanka") %>%
    group_by(pitchLength) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    filter(pitchLength != "") %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

hlslc <- hasaranga %>%
    filter(wide == 0) %>%
    filter(country == "Sri Lanka") %>%
    group_by(pitchLength, competition) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    filter(pitchLength != "") %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)


hlaw <- hasaranga %>%
    filter(wide == 0) %>%
    filter(country != "Sri Lanka") %>%
    group_by(pitchLength) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    filter(pitchLength != "") %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

hlc <- hasaranga %>%
    filter(wide == 0) %>%
    group_by(pitchLength, country) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    filter(pitchLength != "") %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

hiplY <- hasaranga %>%
    filter(wide == 0) %>%
    filter(competition == "IPL") %>%
    group_by(year) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

hipl <- hasaranga %>%
    filter(wide == 0) %>%
    filter(competition == "IPL") %>%
    group_by(pitchLength) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    filter(pitchLength != "") %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

ripl <- rashid %>%
    filter(wide == 0) %>%
    filter(competition == "IPL") %>%
    group_by(pitchLength) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    filter(pitchLength != "") %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

hipl2021 <- hasaranga %>%
    filter(wide == 0) %>%
    filter(competition == "IPL") %>%
    filter(year == 2021) %>%
    group_by(pitchLength) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    filter(pitchLength != "") %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

hipl2022 <- hasaranga %>%
    filter(wide == 0) %>%
    filter(competition == "IPL") %>%
    filter(year == 2022) %>%
    group_by(pitchLength) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    filter(pitchLength != "") %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

hipl2023 <- hasaranga %>%
    filter(wide == 0) %>%
    filter(competition == "IPL") %>%
    filter(year == 2023) %>%
    group_by(pitchLength) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    filter(pitchLength != "") %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

hipl2023l <- hasaranga %>%
    filter(wide == 0) %>%
    filter(competition == "IPL") %>%
    filter(year == 2022) %>%
    group_by(pitchLength, pitchLine) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    filter(pitchLength != "" & pitchLine != "") %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

haus <- hasaranga %>%
    filter(wide == 0) %>%

    filter(country == "Australia") %>%
    group_by(pitchLength) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    filter(pitchLength != "") %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

hipl2023Shot <- hasaranga %>%
    filter(wide == 0) %>%
    filter(competition == "IPL") %>%
    filter(year == 2022) %>%
    filter(pitchLength == "SHORT_OF_A_GOOD_LENGTH") %>%
    group_by(shotType, bat_hand) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

avg <- data %>%
    filter(wide == 0) %>%
    filter(bowl_kind == "spin bowler") %>%
    filter(pitchLength != "" & pitchLine != "") %>%
    group_by(pitchLength) %>%
    summarise(runs = sum(bowlruns), outs = sum(dismissal != "" & dismissal != "run out"), balls = n()) %>%
    mutate(average = runs / outs, strikeRate = runs / balls, bowlsr = balls / outs, percent = balls / sum(balls) * 100)

hllstripped <- hasaranga %>%
    filter(wide == 0) %>%
    filter(pitchLength != "" & pitchLine != "") %>%
    mutate(balls = n()) %>%
    group_by(pitchLength, competition, country) %>%
    summarise(sr = round(n() / sum(dismissal != "" & dismissal != "run out"), 2), er = round(sum(bowlruns) / n(), 2), percent = round(n() / mean(balls) * 100, 2)) %>%
    mutate(color = round(percent / 25.86 * 100, 1))

rllstripped <- rashid %>%
    filter(wide == 0) %>%
    filter(competition == "IPL" & year == "2023")  %>%
    filter(pitchLength != "" & pitchLine != "") %>%
    mutate(balls = n()) %>%
    group_by(pitchLength, pitchLine) %>%
    summarise(sr = round(n() / sum(dismissal != "" & dismissal != "run out"), 2), er = round(sum(bowlruns) / n(), 2), percent = round(n() / mean(balls) * 100, 2)) %>%
    mutate(color = round(percent / 28.78 * 100, 1))

her <- hasaranga %>%
    filter(wide == 0) %>%
    filter(pitchLength != "" & pitchLine != "") %>%
    mutate(balls = n(), away = if_else(country == "Sri Lanka", "No", "Yes")) %>%
    group_by(pitchLength, competition, away) %>%
    summarise(er = round(sum(bowlruns) / n(), 2)) %>%
    filter(pitchLength == "FULL" | pitchLength == "GOOD_LENGTH" | pitchLength == "SHORT_OF_A_GOOD_LENGTH") %>%
    mutate(type = if_else(competition == "T20I", if_else(away == "Yes", "T20I Away", "T20I Home"), competition)) %>%
    spread(value = er, key = type) %>%
    group_by(pitchLength) %>%
    summarize("T20I Away" = mean(`T20I Away`, na.rm = TRUE), "T20I Home" = mean(`T20I Home`, na.rm = TRUE), "IPL" = mean(IPL, na.rm = TRUE), "LPL" = mean(LPL, na.rm = TRUE)) %>%
    write.csv("C:/Users/thevi/Desktop/her.csv")
