library(tidyverse)
library(ggplot2)

removeOutliers <- function(data, col) {
    qnt <- quantile(data[[col]], probs = c(.25, .75), na.rm = TRUE)
    H <- 2.5 * IQR(data[[col]], na.rm = TRUE)
    data <- data %>% filter(data[[col]] > (qnt[1] - H) & data[[col]] < (qnt[2] + H))
    return(data)
}

data <- read.csv("data.csv") %>%
    select(-bounce_above_stumps, -boucne_pos_z, -hit_stumps, -spin_rate) %>%
    mutate(year = as.integer(substr(date, 1, 4))) %>%
    mutate(stump_pos_x = stump_pos_x + 10.06) %>%
    mutate(crease_pos_x = crease_pos_x + 10.06) %>%
    mutate(release_pos_x = release_pos_x + 10.06) %>%
    mutate(bounce_pos_y = ifelse(batsman_is_rhb, -1 * bounce_pos_y, bounce_pos_y)) %>%
    mutate(stump_pos_y = ifelse(batsman_is_rhb, -1 * stump_pos_y, stump_pos_y)) %>%
    mutate(crease_pos_y = -1 * crease_pos_y) %>%
    mutate(release_pos_y = -1 * release_pos_y) %>%
    mutate(impact_pos_y = -1 * impact_pos_y) %>%
    mutate(bounce_pos_y = -1 * bounce_pos_y) %>%
    mutate(phase = ifelse(over < 4, "PP1", ifelse(over < 7, "PP2", ifelse(over < 12, "Early Middle", ifelse(over < 17, "Late Middle", "Death"))))) %>%
    separate(runs, c("runs", "extras"), sep = " ") %>%
    mutate(bowler_runs = ifelse(is_leg_bye == TRUE | is_bye == TRUE, 0, ifelse(is_no_ball == TRUE, as.integer(runs) - 1, as.integer(runs))))  %>%
    mutate(length = ifelse(bounce_pos_x < 0, "Full toss", ifelse(bounce_pos_x < 2, "Yorker", ifelse(bounce_pos_x < 6, "Slot", ifelse(bounce_pos_x < 8, "Good length", ifelse(bounce_pos_x < 10, "Short of a good length", ifelse(bounce_pos_x < 12, "Short", "Bouncer")))))))


data <- removeOutliers(data, "bounce_pos_x")
data <- removeOutliers(data, "bounce_pos_y")
data <- removeOutliers(data, "crease_pos_x")
data <- removeOutliers(data, "crease_pos_y")
data  <- removeOutliers(data, "crease_pos_z")
data <- removeOutliers(data, "release_pos_x")
data <- removeOutliers(data, "release_pos_y")
data <- removeOutliers(data, "release_pos_z")
data <- removeOutliers(data, "stump_pos_x")
data <- removeOutliers(data, "stump_pos_y")
data <- removeOutliers(data, "stump_pos_z")
data <- removeOutliers(data, "release_speed")
data <- removeOutliers(data, "initial_angle")
data <- removeOutliers(data, "deviation")
data <- removeOutliers(data, "drop_angle")
data <- removeOutliers(data, "bounce_angle")
data <- removeOutliers(data, "swing")


mp <- data %>%
    filter(bowler_name == "MATHEESHA PATHIRANA")

mp2022  <- mp %>%
    filter(year == "2022")

mp2023  <- mp %>%
    filter(year == "2023")

mp2024 <- mp %>%
    filter(year == "2024")

nt  <-  data %>%
    filter(bowler_name == "NUWAN THUSHARA")

fast <- data %>%
    filter(delivery_type == "Seam")

rfb <- data %>%
    filter(bowler_is_rhb == TRUE & delivery_type == "Seam")  %>%
    filter(bowler_name != "MATHEESHA PATHIRANA" & bowler_name != "NUWAN THUSHARA")
rfb_over  <-  rfb  %>%
    filter(release_pos_y > 0)
rfb_release_z  <- median(rfb_over$release_pos_z)
rfb_release_y  <- median(rfb_over$release_pos_y)

nt_release_z <- median(nt$release_pos_z)
nt_release_y <- median(nt$release_pos_y)

mp_release_z <- median(mp$release_pos_z)
mp_release_y <- median(mp$release_pos_y)

mp2022_release_z <- median(mp2022$release_pos_z)
mp2022_release_y <- median(mp2022$release_pos_y)

mp2023_release_z <- median(mp2023$release_pos_z)
mp2023_release_y <- median(mp2023$release_pos_y)

mp2024_release_z <- median(mp2024$release_pos_z)
mp2024_release_y <- median(mp2024$release_pos_y)

ggplot() +
geom_point(data = data, aes(y = rfb_release_z, x = rfb_release_y), color="orange") +
    geom_point(data = mp, aes(y = mp_release_z, x = mp_release_y), color = "yellow") +
    geom_point(data = nt, aes(y = nt_release_z, x = nt_release_y), color = "red") +
    geom_point(data = mp2022, aes(y = mp2022_release_z, x = mp2022_release_y), color = "blue") +
    geom_point(data = mp2023, aes(y = mp2023_release_z, x = mp2023_release_y), color = "green") +
    geom_point(data = mp2024, aes(y = mp2024_release_z, x = mp2024_release_y), color = "purple") +
    labs(
        title = "Pitch Map",
        x = "Line",
        y = "Length"
    )

fastSwing  <-  fast  %>%
    group_by(over)  %>%
    summarise(swing = mean(swing))

mpSwing <-  mp  %>%
    group_by(over)  %>%
    summarise(swing = mean(swing), no = n())

mp2022Swing <-  mp2022  %>%
    group_by(over)  %>%
    summarise(swing = mean(swing), no = n())

mp2023Swing <-  mp2023  %>%
    group_by(over)  %>%
    summarise(swing = mean(swing), no = n())

mp2024Swing <-  mp2024  %>%
    group_by(over)  %>%
    summarise(swing = mean(swing), no = n())

ntSwingPhase  <-  nt  %>%
    group_by(phase)  %>%
    summarise(swing = mean(swing), no = n())

fastSwingPhase  <-  fast  %>%
    group_by(phase)  %>%
    summarise(swing = mean(swing))

mpSwingPhase <-  mp  %>%
    group_by(phase)  %>%
    summarise(swing = mean(swing), no = n())

mp2022SwingPhase <-  mp2022  %>%
    group_by(phase)  %>%
    summarise(swing = mean(swing), no = n())

mp2023SwingPhase <-  mp2023  %>%
    group_by(phase)  %>%
    summarise(swing = mean(swing), no = n())

mp2024SwingPhase <-  mp2024  %>%
    group_by(phase)  %>%
    summarise(swing = mean(swing), no = n())

mpNT  <- data  %>%
    filter(bowler_name == "MATHEESHA PATHIRANA" | bowler_name == "NUWAN THUSHARA")

mpNT  <-  mpNT  %>%
    mutate(is_slower = ifelse(ball_type == "OFF CUTTER" | ball_type == "Knuckleball" | ball_type == "SLOWER BALL" | ball_type == "SLOW BOUNCER", 1, 0))

mpPhaseTotal <- mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    group_by(phase) %>%
    summarise(no = n())

mpPhaseStrat <- mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    group_by(phase, length, is_slower) %>%
    summarise(no = n())

mpPhaseStart   <-  merge(mpPhaseTotal, mpPhaseStrat, by = "phase", all.x = TRUE) %>%
    mutate(percent = no.y / no.x * 100) %>%
    select(phase, length, is_slower, percent, no.y)

mpPhaseTotal2022 <- mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    filter(year == "2022") %>%
    group_by(phase) %>%
    summarise(no = n())
mpPhaseStrat2022  <- mpNT  %>%
    filter(year == "2022")  %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    group_by(phase, length, is_slower) %>%
    summarise(percent = n() / nrow(mp2022) * 100, no = n())

mpPhaseStrat2022 <- merge(mpPhaseTotal2022, mpPhaseStrat2022, by = "phase", all.x = TRUE) %>%
    mutate(percent = no.y / no.x * 100) %>%
    select(phase, length, is_slower, percent, no.y)

mpPhaseTotal2023 <- mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    filter(year == "2023") %>%
    group_by(phase) %>%
    summarise(no = n())

mpPhaseStart2023  <- mpNT  %>%
    filter(year == "2023")  %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    group_by(phase, length, is_slower) %>%
    summarise(percent = n() / nrow(mp2023) * 100, no = n())

mpPhaseStrat2023 <- merge(mpPhaseTotal2023, mpPhaseStart2023, by = "phase", all.x = TRUE) %>%
    mutate(percent = no.y / no.x * 100) %>%
    select(phase, length, is_slower, percent, no.y)

mpPhaseTotal2024 <- mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    filter(year == "2024") %>%
    group_by(phase) %>%
    summarise(no = n())
mpPhaseStrat2024  <- mpNT  %>%
    filter(year == "2024")  %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    group_by(phase, length, is_slower) %>%
    summarise(percent = n() / nrow(mp2024) * 100, no = n())
mpPhaseStrat2024 <- merge(mpPhaseTotal2024, mpPhaseStrat2024, by = "phase", all.x = TRUE) %>%
    mutate(percent = no.y / no.x * 100) %>%
    select(phase, length, is_slower, percent, no.y)

ntPhaseTotal <- mpNT %>%
    filter(bowler_name == "NUWAN THUSHARA") %>%
    group_by(phase) %>%
    summarise(no = n())
ntPhaseStrat <- mpNT %>%
    filter(bowler_name == "NUWAN THUSHARA") %>%
    group_by(phase, length, is_slower) %>%
    summarise(no = n())
ntPhaseStrat <- merge(ntPhaseTotal, ntPhaseStrat, by = "phase", all.x = TRUE) %>%
    mutate(percent = no.y / no.x * 100) %>%
    select(phase, length, is_slower, percent, no.y)

mpPace <- mean((mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA"))$release_speed)

mpPace2022 <- mean((mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA" & year == "2022"))$release_speed)
mpPace2023 <- mean((mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA" & year == "2023"))$release_speed)
mpPace2024 <- mean((mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA" & year == "2024"))$release_speed)
ntPace <- mean((mpNT %>%
    filter(bowler_name == "NUWAN THUSHARA"))$release_speed)

ntSwingingBalls <- nt %>%
    filter(swing <= -1)

ntSwingLessBalls <- nt %>%
    filter(swing > -1)

ntSwingRR <- sum(ntSwingingBalls$bowler_runs) / nrow(ntSwingingBalls)
ntSwingLessRR <- sum(ntSwingLessBalls$bowler_runs) / nrow(ntSwingLessBalls)
ntSwingSR  <- nrow(ntSwingingBalls) / nrow(ntSwingingBalls  %>% filter(is_bowler_wicket == TRUE))
ntSwingLessSR  <- nrow(ntSwingLessBalls) / nrow(ntSwingLessBalls  %>% filter(is_bowler_wicket == TRUE))
