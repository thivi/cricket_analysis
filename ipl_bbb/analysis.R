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
    mutate(bowler_runs = ifelse(is_leg_bye == TRUE | is_bye == TRUE, 0, ifelse(is_no_ball == TRUE, as.integer(runs) - 1, as.integer(runs)))) %>%
    mutate(length = ifelse(bounce_pos_x < 0, "Full toss", ifelse(bounce_pos_x <= 3, "Yorker", ifelse(bounce_pos_x <= 5, "Slot", ifelse(bounce_pos_x <= 6, "Full", ifelse(bounce_pos_x <= 8, "Good length", ifelse(bounce_pos_x <= 10, "Short of a good length", ifelse(bounce_pos_x <= 12, "Short", "Bouncer")))))))) %>%
    mutate(release_speed = release_speed * 1.61)

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

release_points  <-  data.frame(
    player = c("Right-arm over fast", "Matheesha Pathirana Overall", "Matheesha Pathirana 2022", "Matheesha Pathirana 2023", "Matheesha Pathirana 2024", "Nuwan Thushara"),
    z = c(rfb_release_z, mp_release_z, mp2022_release_z, mp2023_release_z, mp2024_release_z, nt_release_z),
    y = c(rfb_release_y, mp_release_y, mp2022_release_y, mp2023_release_y, mp2024_release_y, nt_release_y)
)
write.csv(release_points, "release_points.csv", row.names = FALSE)
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
mpNT <- data %>%
    filter(bowler_name == "MATHEESHA PATHIRANA" | bowler_name == "NUWAN THUSHARA")

mpNT <- mpNT %>%
    mutate(is_slower = ifelse(ball_type == "OFF CUTTER" | ball_type == "Knuckleball" | ball_type == "SLOWER BALL" | ball_type == "SLOW BOUNCER", 1, 0))

fastSwing <- fast %>%
    group_by(over) %>%
    summarise(swing = mean(abs(swing)))

mpSwing <-  mp  %>%
    group_by(over)  %>%
    summarise(swing = mean(abs(swing)), no = n())

mp2022Swing <-  mp2022  %>%
    group_by(over)  %>%
    summarise(swing = mean(abs(swing)), no = n())

mp2023Swing <-  mp2023  %>%
    group_by(over)  %>%
    summarise(swing = mean(abs(swing)), no = n())

mp2024Swing <-  mp2024  %>%
    group_by(over)  %>%
    summarise(swing = mean(abs(swing)), no = n())

ntSwing <- mpNT %>%
    filter(bowler_name == "NUWAN THUSHARA") %>%
    filter(is_slower == 0) %>%
    filter(swing <0 ) %>%
    group_by(over) %>%
    summarise(swing = mean(abs(swing)), no = n())

ntOutswingPhase <- mpNT %>%
    filter(bowler_name == "NUWAN THUSHARA" & is_slower == 0) %>%
    filter(swing < 0) %>%
    group_by(phase) %>%
    summarise(swing = mean(abs(swing)), no = n())

ntInswingPhase <- mpNT %>%
    filter(bowler_name == "NUWAN THUSHARA" & is_slower == 0) %>%
    filter(swing > 0) %>%
    group_by(phase) %>%
    summarise(swing = mean(abs(swing)), no = n())

ntSwingPhase  <-  nt  %>%
    group_by(phase)  %>%
    summarise(swing = mean(abs(swing)), no = n())

fastSwingPhase <- fast %>%
    group_by(phase) %>%
    summarise(swing = mean(abs(swing)))

mpSwingPhase <- mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    filter(is_slower == 0) %>%
    group_by(phase)  %>%
    summarise(swing = mean(abs(swing)), no = n())

mp2022SwingPhase <- mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    filter(year == "2022") %>%
    filter(is_slower == 0) %>%
    group_by(phase)  %>%
    summarise(swing = mean(abs(swing)), no = n())

mp2023SwingPhase <- mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    filter(year == "2023") %>%
    filter(is_slower == 0) %>%
    group_by(phase)  %>%
    summarise(swing = mean(abs(swing)), no = n())

mp2024SwingPhase <- mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    filter(year == "2024") %>%
    filter(is_slower == 0) %>%
    group_by(phase)  %>%
    summarise(swing = mean(abs(swing)), no = n())

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

mpPhaseStratPace <- mpPhaseStart %>%
    filter(is_slower == 0)

mpPhaseStratSlow <- mpPhaseStart %>%
    filter(is_slower == 1)

mpPhaseStrat <- merge(mpPhaseStratPace, mpPhaseStratSlow, by = c("phase", "length"), all.x = TRUE, all.y = TRUE) %>%
    select(phase, length, percent.x, percent.y) %>%
    rename(Pace = percent.x, Slow = percent.y)
write.csv(mpPhaseStrat, "mpPhaseStart.csv", row.names = FALSE)

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

mpPhaseStratPace <- mpPhaseStrat2022 %>%
    filter(is_slower == 0)

mpPhaseStratSlow <- mpPhaseStrat2022 %>%
    filter(is_slower == 1)

mpPhaseStrat <- merge(mpPhaseStratPace, mpPhaseStratSlow, by = c("phase", "length"), all.x = TRUE, all.y = TRUE) %>%
    select(phase, length, percent.x, percent.y) %>%
    rename(Pace = percent.x, Slow = percent.y)
write.csv(mpPhaseStrat, "mpPhaseStart2022.csv", row.names = FALSE)

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

mpPhaseStratPace <- mpPhaseStrat2023 %>%
    filter(is_slower == 0)

mpPhaseStratSlow <- mpPhaseStrat2023 %>%
    filter(is_slower == 1)

mpPhaseStrat <- merge(mpPhaseStratPace, mpPhaseStratSlow, by = c("phase", "length"), all.x = TRUE, all.y = TRUE) %>%
    select(phase, length, percent.x, percent.y) %>%
    rename(Pace = percent.x, Slow = percent.y)
write.csv(mpPhaseStrat, "mpPhaseStart2023.csv", row.names = FALSE)

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
mpPhaseStratPace <- mpPhaseStrat2024 %>%
    filter(is_slower == 0)

mpPhaseStratSlow <- mpPhaseStrat2024 %>%
    filter(is_slower == 1)

mpPhaseStrat <- merge(mpPhaseStratPace, mpPhaseStratSlow, by = c("phase", "length"), all.x = TRUE, all.y = TRUE) %>%
    select(phase, length, percent.x, percent.y) %>%
    rename(Pace = percent.x, Slow = percent.y)
write.csv(mpPhaseStrat, "mpPhaseStart2024.csv", row.names = FALSE)

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
ntPhaseStratPace <- ntPhaseStrat %>%
    filter(is_slower == 0)

ntPhaseStratSlow <- ntPhaseStrat %>%
    filter(is_slower == 1)

ntPhaseStrat <- merge(ntPhaseStratPace, ntPhaseStratSlow, by = c("phase", "length"), all.x = TRUE, all.y = TRUE) %>%
    select(phase, length, percent.x, percent.y) %>%
    rename(Pace = percent.x, Slow = percent.y)
write.csv(ntPhaseStrat, "ntPhaseStrat.csv", row.names = FALSE)

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
    filter(swing <= -0.5)

ntSwingLessBalls <- nt %>%
    filter(swing > -0.5)

ntSwingRR <- sum(ntSwingingBalls$bowler_runs) / nrow(ntSwingingBalls)
ntSwingLessRR <- sum(ntSwingLessBalls$bowler_runs) / nrow(ntSwingLessBalls)
ntSwingSR  <- nrow(ntSwingingBalls) / nrow(ntSwingingBalls  %>% filter(is_bowler_wicket == TRUE))
ntSwingLessSR  <- nrow(ntSwingLessBalls) / nrow(ntSwingLessBalls  %>% filter(is_bowler_wicket == TRUE))

mpReleaseByLength  <- mp  %>%
    group_by(length)  %>%
    summarise(release_z = mean(release_pos_z), release_y = mean(release_pos_y))

mpReleaseByLength2022 <- mp2022 %>%
    group_by(length) %>%
    summarise(release_z = mean(release_pos_z), release_y = mean(release_pos_y), no = n())

mpReleaseByLength2023  <- mp2023  %>%
    group_by(length)  %>%
    summarise(release_z = mean(release_pos_z), release_y = mean(release_pos_y))

mpReleaseByLength2024  <-  mp2024  %>%
    group_by(length)  %>%
    summarise(release_z = mean(release_pos_z), release_y = mean(release_pos_y))

ntReleaseByLength  <- nt  %>%
    group_by(length)  %>%
    summarise(release_z = mean(release_pos_z), release_y = mean(release_pos_y))

fastReleaseByLength <- fast  %>%
    group_by(length)  %>%
    summarise(release_z = mean(release_pos_z), release_y = mean(release_pos_y))
mpYorkerSuccess <- nrow(mp %>% filter(length == "Yorker")) / nrow(mp %>% filter(length == "Yorker" | length == "Full toss" | length == "Slot")) * 100

mpYorkerSuccess2022 <- nrow(mp2022 %>% filter(length == "Yorker")) / nrow(mp2022 %>% filter(length == "Yorker" | length == "Full toss" | length == "Slot")) * 100
mpYorkerSuccess2023 <- nrow(mp2023 %>% filter(length == "Yorker")) / nrow(mp2023 %>% filter(length == "Yorker" | length == "Full toss" | length == "Slot")) * 100
mpYorkerSuccess2024 <- nrow(mp2024 %>% filter(length == "Yorker")) / nrow(mp2024 %>% filter(length == "Yorker" | length == "Full toss" | length == "Slot")) * 100

ntYorkerSuccess <- nrow(nt %>% filter(length == "Yorker")) / nrow(nt %>% filter(length == "Yorker" | length == "Full toss" | length == "Slot")) * 100

avgYorkerSuccess <-nrow(fast %>% filter(length == "Yorker")) / nrow(fast %>% filter(length == "Yorker" | length == "Full toss" | length == "Slot")) * 100

mpYorkerRR <- sum((mp %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$bowler_runs) / nrow((mp %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss")))
mpYorkerSR <- nrow((mp %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))) / nrow((mp %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss") %>% filter(is_bowler_wicket == TRUE)))

mpYorkerRR2022 <- sum((mp2022 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$bowler_runs) / nrow((mp2022 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss")))
mpYorkerSR2022 <- nrow((mp2022 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))) / nrow((mp2022 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss") %>% filter(is_bowler_wicket == TRUE)))

mpYorkerRR2023 <- sum((mp2023 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$bowler_runs) / nrow((mp2023 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss")))
mpYorkerSR2023 <- nrow((mp2023 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))) / nrow((mp2023 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss") %>% filter(is_bowler_wicket == TRUE)))

mpYorkerRR2024 <- sum((mp2024 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$bowler_runs) / nrow((mp2024 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss")))
mpYorkerSR2024 <- nrow((mp2024 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))) / nrow((mp2024 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss") %>% filter(is_bowler_wicket == TRUE)))

ntYorkerRR <- sum((nt %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$bowler_runs) / nrow((nt %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss")))
ntYorkerSR <- nrow((nt %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))) / nrow((nt %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss") %>% filter(is_bowler_wicket == TRUE)))

mpWrongYorkerRR  <- sum((mp %>% filter(length == "Slot" | length == "Full toss"))$bowler_runs) / nrow((mp %>% filter(length == "Slot" | length == "Full toss")))
mpWrongYorkerSR  <- nrow((mp %>% filter(length == "Slot" | length == "Full toss"))) / nrow((mp %>% filter(length == "Slot" | length == "Full toss") %>% filter(is_bowler_wicket == TRUE)))

mpWrongYorkerRR2022 <- sum((mp2022 %>% filter(length == "Slot" | length == "Full toss"))$bowler_runs) / nrow((mp2022 %>% filter(length == "Slot" | length == "Full toss")))
mpWrongYorkerSR2022 <- nrow((mp2022 %>% filter(length == "Slot" | length == "Full toss"))) / nrow((mp2022 %>% filter(length == "Slot" | length == "Full toss") %>% filter(is_bowler_wicket == TRUE)))

mpWrongYorkerRR2023 <- sum((mp2023 %>% filter(length == "Slot" | length == "Full toss"))$bowler_runs) / nrow((mp2023 %>% filter(length == "Slot" | length == "Full toss")))
mpWrongYorkerSR2023 <- nrow((mp2023 %>% filter(length == "Slot" | length == "Full toss"))) / nrow((mp2023 %>% filter(length == "Slot" | length == "Full toss") %>% filter(is_bowler_wicket == TRUE)))

mpWrongYorkerRR2024 <- sum((mp2024 %>% filter(length == "Slot" | length == "Full toss"))$bowler_runs) / nrow((mp2024 %>% filter(length == "Slot" | length == "Full toss")))
mpWrongYorkerSR2024 <- nrow((mp2024 %>% filter(length == "Slot" | length == "Full toss"))) / nrow((mp2024 %>% filter(length == "Slot" | length == "Full toss") %>% filter(is_bowler_wicket == TRUE)))

ntWrongYorkerRR <- sum((nt %>% filter(length == "Slot" | length == "Full toss"))$bowler_runs) / nrow((nt %>% filter(length == "Slot" | length == "Full toss")))
ntWrongYorkerSR <- nrow((nt %>% filter(length == "Slot" | length == "Full toss"))) / nrow((nt %>% filter(length == "Slot" | length == "Full toss") %>% filter(is_bowler_wicket == TRUE)))

avgWrongYorkerRR <- sum((fast %>% filter(length == "Slot" | length == "Full toss"))$bowler_runs) / nrow((fast %>% filter(length == "Slot" | length == "Full toss")))
avgWrongYorkerSR <- nrow((fast %>% filter(length == "Slot" | length == "Full toss"))) / nrow((fast %>% filter(length == "Slot" | length == "Full toss") %>% filter(is_bowler_wicket == TRUE)))

avgYorkerRR <- sum((fast %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$bowler_runs) / nrow((fast %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss")))
avgYorkerSR <- nrow((fast %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))) / nrow((fast %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss") %>% filter(is_bowler_wicket == TRUE)))

mpYorkerSpeed  <- mean((mp %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$release_speed)
mpYorkerSpeed2022 <- mean((mp2022 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$release_speed)
mpYorkerSpeed2023 <- mean((mp2023 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$release_speed)
mpYorkerSpeed2024 <- mean((mp2024 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$release_speed)
ntYorkerSpeed  <- mean((nt %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$release_speed)

mpYorkerSwing <- mean((mp %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$swing)
mpYorkerSwing2022 <- mean((mp2022 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$swing)
mpYorkerSwing2023 <- mean((mp2023 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$swing)
mpYorkerSwing2024 <- mean((mp2024 %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$swing)
ntYorkerSwing <- mean(abs((nt %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$swing))
ntYorkerPPSwing <- mean((nt %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss" | phase == "PP1"))$swing)
ntYorkerDeathSwing <- mean(abs((nt %>% filter((length == "Yorker" | length == "Slot" | length == "Full toss") & phase == "Death" ))$swing))
avgYorkerSwing <- mean(abs((fast %>% filter(length == "Yorker" | length == "Slot" | length == "Full toss"))$swing))
avgYorkerDeathSwing <- mean(abs((fast %>% filter((length == "Yorker" | length == "Slot" | length == "Full toss") & phase == "Death"))$swing))

ntYorkerCorrectRRDeath  <- sum((nt %>% filter(length == "Yorker" & phase == "Death"))$bowler_runs) / nrow((nt %>% filter(length == "Yorker" & phase == "Death")))
avgYorkerCorrectRRDeath <- sum((fast %>% filter(length == "Yorker" & phase == "Death"))$bowler_runs) / nrow((fast %>% filter(length == "Yorker" & phase == "Death")))
avgYorkerRRDeath <- sum((fast %>% filter((length == "Yorker" | length == "Slot" | length == "Full toss") & phase == "Death"))$bowler_runs) / nrow((fast %>% filter((length == "Yorker" | length == "Slot" | length == "Full toss") & phase == "Death")))
ntYorkerRRDeath <- sum((nt %>% filter((length == "Yorker" | length == "Slot" | length == "Full toss") & phase == "Death"))$bowler_runs) / nrow((nt %>% filter((length == "Yorker" | length == "Slot" | length == "Full toss") & phase == "Death")))
ntWrongYorkerRRDeath <- sum((nt %>% filter((length == "Slot" | length == "Full toss") & phase == "Death"))$bowler_runs) / nrow((nt %>% filter((length == "Slot" | length == "Full toss") & phase == "Death")))
avgWrongYorkerRRDeath <- sum((fast %>% filter((length == "Slot" | length == "Full toss") & phase == "Death"))$bowler_runs) / nrow((fast %>% filter((length == "Slot" | length == "Full toss") & phase == "Death")))

ntYorkerCorrectRR  <- sum((nt %>% filter(length == "Yorker"))$bowler_runs) / nrow((nt %>% filter(length == "Yorker")))
mpYorkerCorrectRR <- sum((mp %>% filter(length == "Yorker"))$bowler_runs) / nrow((mp %>% filter(length == "Yorker")))
mpYorkerCorrectRR2022 <- sum((mp2022 %>% filter(length == "Yorker"))$bowler_runs) / nrow((mp2022 %>% filter(length == "Yorker")))
mpYorkerCorrectRR2023 <- sum((mp2023 %>% filter(length == "Yorker"))$bowler_runs) / nrow((mp2023 %>% filter(length == "Yorker")))
mpYorkerCorrectRR2024 <- sum((mp2024 %>% filter(length == "Yorker"))$bowler_runs) / nrow((mp2024 %>% filter(length == "Yorker")))
mpSlotBounce <- mean((mp %>% filter(length == "Slot"))$crease_pos_z)
mpSlotBounce2022 <- mean((mp2022 %>% filter(length == "Slot"))$crease_pos_z)
mpSlotBounce2023 <- mean((mp2023 %>% filter(length == "Slot"))$crease_pos_z)
mpSlotBounce2024 <- mean((mp2024 %>% filter(length == "Slot"))$crease_pos_z)
ntSlotBounce <- mean((nt %>% filter(length == "Slot"))$crease_pos_z)
avgSlotBounce <- mean((fast %>% filter(length == "Slot"))$crease_pos_z)
avgYorkerCorrectRR <- sum((fast %>% filter(length == "Yorker"))$bowler_runs) / nrow((fast %>% filter(length == "Yorker")))

ntDeathLengthsRR <- mpNT %>%
    filter(phase == "Death" & bowler_name == "NUWAN THUSHARA") %>%
    group_by(length, is_slower) %>%
    summarise(rr = sum(bowler_runs) / n(), no = n())

ntShorterThanSlot <- mpNT %>%
    filter(bowler_name == "NUWAN THUSHARA")  %>%
    filter(length == "Good length" | length == "Short of a good length") %>%
    filter(phase == "Death") %>%
    filter(is_slower == 0)
ntShorterThanSlotRR  <- sum(ntShorterThanSlot$bowler_runs) / nrow(ntShorterThanSlot)
ntPhaseRR <- nt %>%
    group_by(phase) %>%
    summarise(RR = sum(bowler_runs) / n())

mpPhaseRR <- mp %>%
    group_by(phase) %>%
    summarise(RR = sum(bowler_runs) / n())

mpPhaseRR2022 <- mp2022 %>%
    group_by(phase) %>%
    summarise(RR = sum(bowler_runs) / n())

mpPhaseRR2023 <- mp2023 %>%
    group_by(phase) %>%
    summarise(RR = sum(bowler_runs) / n())

mpPhaseRR2024 <- mp2024 %>%
    group_by(phase) %>%
    summarise(RR = sum(bowler_runs) / n())

mpSlowBallDeviation <- mean((mpNT %>% filter(is_slower == 1 & bowler_name == "MATHEESHA PATHIRANA"))$deviation)
mpSlowBallDeviation2022 <- mean((mpNT %>% filter(is_slower == 1 & bowler_name == "MATHEESHA PATHIRANA" & year == "2022"))$deviation)
mpSlowBallDeviation2023 <- mean((mpNT %>% filter(is_slower == 1 & bowler_name == "MATHEESHA PATHIRANA" & year == "2023"))$deviation)
mpSlowBallDeviation2024 <- mean((mpNT %>% filter(is_slower == 1 & bowler_name == "MATHEESHA PATHIRANA" & year == "2024"))$deviation)
ntSlowBallDeviation <- mean((mpNT %>% filter(is_slower == 1 & bowler_name == "NUWAN THUSHARA"))$deviation)

ggplot() +
    geom_point(data = nt, aes(x = release_speed, y = swing)) +
    labs(
        title = "Release Speed vs Swing",
        x = "Release Speed",
        y = "Swing"
    )

ggplot() +
    geom_point(data = mp, aes(x = release_speed, y = swing)) +
    labs(
        title = "Release Speed vs Swing",
        x = "Release Speed",
        y = "Swing"
    )

ggplot() +
    geom_point(data = mp2024, aes(x = release_speed, y = swing)) +
    labs(
        title = "Release Speed vs Swing",
        x = "Release Speed",
        y = "Swing"
    )
ggplot() +
    geom_point(data = mp2023, aes(x = release_speed, y = swing)) +
    labs(
        title = "Release Speed vs Swing",
        x = "Release Speed",
        y = "Swing"
    )
ggplot() +
    geom_point(data = mp2022, aes(x = release_speed, y = swing)) +
    labs(
        title = "Release Speed vs Swing",
        x = "Release Speed",
        y = "Swing"
    )
cor(nt$release_speed, nt$swing)
cor(mp$release_speed, mp$swing)
cor(mp2022$release_speed, mp2022$swing)
cor(mp2023$release_speed, mp2023$swing)
cor(mp2024$release_speed, mp2024$swing)
mpPaceSwing <- mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    filter(year != "2022") %>%
    filter(is_slower == 0) %>%
    select(release_speed, swing)
write.csv(mpPaceSwing, "mpPaceSwing.csv", row.names = FALSE)
mpBounce <- mp %>%
    group_by(length) %>%
    summarise(bounce = mean(crease_pos_z))

mpBounce2022 <- mp2022 %>%
    group_by(length) %>%
    summarise(bounce = mean(crease_pos_z))

mpBounce2023 <- mp2023 %>%
    group_by(length) %>%
    summarise(bounce = mean(crease_pos_z))

mpBounce2024 <- mp2024 %>%
    group_by(length) %>%
    summarise(bounce = mean(crease_pos_z))

ntBounce <- nt %>%
    group_by(length) %>%
    summarise(bounce = mean(crease_pos_z))


mpIA  <-  mp  %>%
    group_by(length)  %>%
    summarise(ia = mean(initial_angle))

mpIA2022 <- mp2022 %>%
    group_by(length) %>%
    summarise(ia = mean(initial_angle))

mpIA2023 <- mp2023 %>%
    group_by(length) %>%
    summarise(ia = mean(initial_angle))

mpIA2024 <- mp2024 %>%
    group_by(length) %>%
    summarise(ia = mean(initial_angle))

cor(mp2024$release_pos_z, mp2024$initial_angle)
cor(mp2023$release_pos_z, mp2023$initial_angle)
cor(mp2022$release_pos_z, mp2022$initial_angle)
cor(nt$release_pos_z, nt$initial_angle)

cor(mp2024$initial_angle, mp2024$crease_pos_z)
cor(mp2024$initial_angle, mp2024$swing)

mp2024SlowLengths <- mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    filter(is_slower == 1) %>%
    filter(year == "2024") %>%
    group_by(length) %>%
    summarise(ia = mean(initial_angle))

mp2024FastLengths <- mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    filter(is_slower == 0) %>%
    filter(year == "2024") %>%
    group_by(length) %>%
    summarise(ia = mean(initial_angle))

mp2024Slow  <- mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    filter(is_slower == 1) %>%
    filter(year == "2024")

mp2024Fast <- mpNT %>%
    filter(bowler_name == "MATHEESHA PATHIRANA") %>%
    filter(is_slower == 0) %>%
    filter(year == "2024")
cor(mp2024Slow$release_pos_z, mp2024Slow$initial_angle)
cor(mp2024Fast$release_pos_z, mp2024Fast$initial_angle)
cor(fast$release_pos_z, fast$initial_angle)
cor(fast$release_pos_z, fast$crease_pos_z)

avgPhaseRR2024 <- data %>%
    filter(year == "2024") %>%
    group_by(phase) %>%
    summarise(RR = sum(bowler_runs) / n())

ntDeath  <- nt %>%
    filter(phase == "Death")

paceData  <- data.frame(
    pace = c("Minimum", "Maximum", "Median", "25th Percentile", "75th Percentile"),
    mp = c(min(mp$release_speed), max(mp$release_speed), median(mp$release_speed), quantile(mp$release_speed, 0.25), quantile(mp$release_speed, 0.75)),
    mp2022 = c(min(mp2022$release_speed), max(mp2022$release_speed), median(mp2022$release_speed), quantile(mp2022$release_speed, 0.25), quantile(mp2022$release_speed, 0.75)),
    mp2023 = c(min(mp2023$release_speed), max(mp2023$release_speed), median(mp2023$release_speed), quantile(mp2023$release_speed, 0.25), quantile(mp2023$release_speed, 0.75)),
    mp2024 = c(min(mp2024$release_speed), max(mp2024$release_speed), median(mp2024$release_speed), quantile(mp2024$release_speed, 0.25), quantile(mp2024$release_speed, 0.75)),
    nt = c(min(nt$release_speed), max(nt$release_speed), median(nt$release_speed), quantile(nt$release_speed, 0.25), quantile(nt$release_speed, 0.75))
)

write.csv(paceData, "paceData.csv", row.names = FALSE)

cor(mp2024$release_pos_z, mp2024$bounce_pos_x )
cor(mp2023$release_pos_z, mp2023$bounce_pos_x )
cor(mp2022$release_pos_z, mp2022$bounce_pos_x )
cor(nt$release_pos_z, nt$bounce_pos_x )
cor(fast$release_pos_z, fast$bounce_pos_x )
write.csv(mpReleaseByLength2024, "mpReleaseByLength2024.csv", row.names = FALSE)
write.csv(ntReleaseByLength, "ntReleaseByLength.csv", row.names = FALSE)

cor(fast$initial_angle, fast$bounce_pos_x)
cor(mp2024$initial_angle, mp2024$bounce_pos_x)
cor(mp2023$initial_angle, mp2023$bounce_pos_x)
cor(mp2022$initial_angle, mp2022$bounce_pos_x)
cor(nt$initial_angle, nt$bounce_pos_x)
cor(mp2024$initial_angle, mp2024$release_pos_z)
cor(mp2023$initial_angle, mp2023$release_pos_z)
cor(nt$initial_angle, nt$release_pos_z)
cor(fast$initial_angle, fast$release_pos_z)

rabada  <-  data %>%
    filter(bowler_name == "Kagiso Rabada")
bumrah <- data %>%
    filter(bowler_name == "JASPRIT BUMRAH")
cummins <- data %>%
    filter(bowler_name == "PAT CUMMINS")
milne <- data %>%
    filter(bowler_name == "KAMLESH NAGARKOTI")
cor(rabada$release_pos_z, rabada$bounce_pos_x)
cor(bumrah$release_pos_z, bumrah$bounce_pos_x)
cor(cummins$release_pos_z, cummins$bounce_pos_x)
cor(milne$release_pos_z, milne$bounce_pos_x)
ggplot(mp2024, aes(x = release_pos_y, y = release_pos_z, color=length)) +
    geom_point() +
    labs(
        title = "Release Z vs Bounce X",
        x = "Release Z",
        y = "Bounce X"
    )

rp_bounce <- fast %>%
    group_by(bowler_name) %>%
    summarise(cor = abs(cor(release_pos_z, bounce_pos_x)), no = n())

bumrahRPByLength <- fast %>%
    filter(bowler_name == "JASPRIT BUMRAH") %>%
    group_by(length) %>%
    summarise(rp = mean(release_pos_z))

milneRPByLength <- fast %>%
    filter(bowler_name == "KAMLESH NAGARKOTI") %>%
    group_by(length) %>%
    summarise(rp = mean(release_pos_z))

cor(bumrah$release_pos_z, bumrah$crease_pos_z)
cor(milne$release_pos_z, milne$crease_pos_z)
cor(mp2024$release_pos_z, mp2024$crease_pos_z)
cor(mp2023$release_pos_z, mp2023$crease_pos_z)

mpBounce2023  <- mean(mp2023$crease_pos_z)
mpBounce2024  <- mean(mp2024$crease_pos_z)
mpBounce2022  <- mean(mp2022$crease_pos_z)

mp20242023GL <- rbind(mp2024, mp2023) %>%
    filter(length == "Good length")
mp20242023Short <- rbind(mp2024, mp2023) %>%
    filter(length == "Short")
milneGL <- milne %>%
    filter(length == "Short")
bumrahShort <- bumrah %>%
    filter(length == "Short")
cor(bumrahShort$release_pos_z, bumrahShort$crease_pos_z)
cor(mp20242023GL$release_pos_z, mp20242023GL$crease_pos_z)
cor(mp20242023Short$release_pos_z, mp20242023Short$crease_pos_z)
cor(milneGL$release_pos_z, milneGL$crease_pos_z)
mean((mp2024  %>% filter(length=="Good length"))$crease_pos_z)
mean((mp2023  %>% filter(length=="Good length"))$crease_pos_z)
mean((mp2024 %>% filter(length == "Short of a good length"))$crease_pos_z)
mean((mp2023 %>% filter(length == "Short of a good length"))$crease_pos_z)
mean((mp2024 %>% filter(length == "Short"))$crease_pos_z)
mean((mp2023 %>% filter(length == "Short"))$crease_pos_z)

cor(milne$release_pos_z, milne$initial_angle)
cor(bumrah$release_pos_z, bumrah$initial_angle)
cor(mp2024$release_pos_z, mp2024$initial_angle)
cor(fast$crease_pos_z, fast$initial_angle)

mpYorkerBounce2024 <- mean((mp2024 %>% filter(length == "Yorker" | length == "Slot"))$crease_pos_z)
fastYorkerBounce <- mean((fast %>% filter(length == "Yorker" | length == "Slot"))$crease_pos_z)
ntYorkerBounce <- mean((nt %>% filter(length == "Yorker" | length == "Slot"))$crease_pos_z)
