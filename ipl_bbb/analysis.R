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
    mutate(bounce_pos_y = -1 * bounce_pos_y)  %>%
    mutate(phase = ifelse(over < 4, "PP1", ifelse(over < 7, "PP2", ifelse(over < 12, "Early Middle", ifelse(over < 17, "Late Middle", "Death")))))

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


data  <- data %>%
    mutate(length = ifelse(bounce_pos_x < 0, "Full toss", ifelse(bounce_pos_x < 2, "Yorker", ifelse(bounce_pos_x < 6, "Slot", ifelse(bounce_pos_x < 8, "Good length", ifelse(bounce_pos_x < 10, "Short of a good length", ifelse(bounce_pos_x < 12, "Short", "Bouncer")))))))

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


ggplot() +
    geom_point(data=data, aes(y = bounce_pos_x, x = bounce_pos_y), color="blue") +
    geom_point(data = data, aes(y = crease_pos_x, x = crease_pos_y), color="red") +
    geom_point(data = data, aes(y = release_pos_x, x = release_pos_y), color="green") +
    geom_point(data = data, aes(y = stump_pos_x, x = stump_pos_y), color="#fac0ff") +
    labs(title = "Pitch Map",
                x = "Line",
                y = "Length")

fast <- data %>%
    filter(delivery_type == "Seam")

fast_rhb <- fast %>%
    filter(batsman_is_rhb == TRUE)

fast_lhb <- fast %>%
    filter(batsman_is_rhb == FALSE)

ggplot(fast_lhb, aes(y = release_pos_z, x = release_pos_y, color = bowler_is_rhb)) +
    geom_point() +
    labs(
        title = "Pitch Map",
        x = "Line",
        y = "Length"
    )

ggplot(data, aes(y = bounce_pos_x, x = bounce_pos_y, color=length)) +
    geom_point() +
    labs(title = "Pitch Map",
                x = "Line",
                y = "Length")

ggplot(data, aes(y = crease_pos_x, x = crease_pos_y, color=batsman_is_rhb)) +
    geom_point() +
    labs(title = "Pitch Map",
                x = "Line",
                y = "Length")

rfb <- data %>%
    filter(bowler_is_rhb == TRUE & delivery_type == "Seam")  %>%
    filter(bowler_name != "MATHEESHA PATHIRANA" & bowler_name != "NUWAN THUSHARA")
rfb_over  <-  rfb  %>%
    filter(release_pos_y > 0)
ggplot() +
    geom_point(data = rfb_over, aes(y = release_pos_z, x = release_pos_y), color="red") +
    labs(title = "Pitch Map",
                x = "Line",
                y = "Length")
rfb_release_z  <- median(rfb_over$release_pos_z)
rfb_release_y  <- median(rfb_over$release_pos_y)

ggplot() +
    geom_point(data = mp, aes(y = release_pos_z, x = release_pos_y), color="yellow") +
    geom_point(data = data, aes(y = rfb_release_z, x = rfb_release_y), color="red") +
    geom_point(data = nt, aes(y = release_pos_z, x = release_pos_y), color="blue") +
    labs(title = "Pitch Map",
                x = "Line",
                y = "Length")

ggplot() +
    geom_point(data = mp2022, aes(y = release_pos_z, x = release_pos_y), color = "yellow") +
    geom_point(data = mp2023, aes(y = release_pos_z, x = release_pos_y), color = "red") +
    geom_point(data = mp2024, aes(y = release_pos_z, x = release_pos_y), color = "blue") +
    labs(
        title = "Pitch Map",
        x = "Line",
        y = "Length"
    )

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

ntSwing  <-  nt  %>%
    group_by(over)  %>%
    summarise(swing = mean(swing), no = n())

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

ggplot() +
    geom_line(data = ntSwing, aes(x = over, y = swing), color = "red") +
    geom_line(data = fastSwing, aes(x = over, y = swing), color = "blue") +
    geom_line(data = mpSwing, aes(x = over, y = swing), color = "yellow") +
    geom_line(data = mp2022Swing, aes(x = over, y = swing), color = "green") +
    geom_line(data = mp2023Swing, aes(x = over, y = swing), color = "purple") +
    geom_line(data = mp2024Swing, aes(x = over, y = swing), color = "orange") +
    labs(
        title = "Swing",
        x = "Over",
        y = "Swing"
    )

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

ggplot() +
    geom_line(data = ntSwingPhase, aes(x = phase, y = swing), color = "red") +
    geom_line(data = fastSwingPhase, aes(x = phase, y = swing), color = "blue") +
    geom_line(data = mpSwingPhase, aes(x = phase, y = swing), color = "yellow") +
    geom_line(data = mp2022SwingPhase, aes(x = phase, y = swing), color = "green") +
    geom_line(data = mp2023SwingPhase, aes(x = phase, y = swing), color = "purple") +
    geom_line(data = mp2024SwingPhase, aes(x = phase, y = swing), color = "orange") +
    labs(
        title = "Swing",
        x = "Phase",
        y = "Swing"
        )

mpLengths  <- mp  %>%
    group_by(year, length)  %>%
    summarise(no = n())

mpYear <- mp %>%
    group_by(year) %>%
    summarise(no = n())

mpLengthsPer <- merge(mpLengths, mpYear, by = "year")  %>%
    mutate(per = no.x / no.y * 100)

ntLengths  <- nt  %>%
    group_by(length)  %>%
    summarise(no = n(), per = n() / nrow(nt) * 100)

ntLengthsPhase  <- nt  %>%
    group_by(phase, length)  %>%
    summarise(no = n(), per = n() / sum(phase) * 100)


s  <- mp2024  %>%
    filter(phase=="Early Middle" | phase == "Late Middle")  %>%
    group_by(bowler_name)  %>%
    summarise(no = n(), swing = mean(swing))

mpRPLength  <-  mp  %>%
    group_by(length)  %>%
    summarise(rp_z = mean(release_pos_z), rp_y = mean(release_pos_y))

mpRPLength2024 <-  mp2024  %>%
    group_by(length)  %>%
    summarise(rp_z = mean(release_pos_z), rp_y = mean(release_pos_y))

mpRPLength2023 <-  mp2023  %>%
    group_by(length)  %>%
    summarise(rp_z = mean(release_pos_z), rp_y = mean(release_pos_y))

mpRPLength2022 <-  mp2022  %>%
    group_by(length)  %>%
    summarise(rp_z = mean(release_pos_z), rp_y = mean(release_pos_y))

bounceMP  <-  mp  %>%
    group_by(year)  %>%
    summarise(bounce = mean(stump_pos_z))

bounceMPLengths  <-  mp  %>%
    group_by(year, length)  %>%
    summarise(bounce = mean(stump_pos_z))

ggplot()+
    geom_point(data = mp2022, aes(x = bounce_pos_x, y = stump_pos_z), color="red") +
    geom_point(data = mp2023, aes(x = bounce_pos_x, y = stump_pos_z), color="blue") +
    geom_point(data = mp2024, aes(x = bounce_pos_x, y = stump_pos_z), color="green") +
    labs(
        title = "Release Point",
        x = "Line",
        y = "Length"
    )

mpNT  <- data  %>%
    filter(bowler_name == "MATHEESHA PATHIRANA" | bowler_name == "NUWAN THUSHARA")

ggplot() +
    geom_point(data = mpNT, aes(x = bowler_name, y = release_speed, color = bowler_name)) +
    labs(
        title = "Release Point",
        x = "Line",
        y = "Length"
    )

mpSlower  <- mp  %>%
    filter(ball_type == "OFF CUTTER" | ball_type == "Knuckleball" | ball_type=="SLOWER BALL")

maxSlowMP = max(mpSlower$release_speed)
maxSlowNT = 78
mpNT  <-  mpNT  %>%
    mutate(is_slower = ifelse(bowler_name == "MATHEESHA PATHIRANA" & release_speed < maxSlowMP, 1, ifelse(bowler_name == "NUWAN THUSHARA" & release_speed < maxSlowNT, 1, 0)))
fastSlot  <- fast  %>%
    group_by(length)  %>%
    summarise(rp_z = mean(stump_pos_z))

mpFast  <-  mpNT  %>%
    filter(is_slower == 0 & bowler_name == "MATHEESHA PATHIRANA")
ggplot() +
    geom_point(data = mpFast, aes(x=release_pos_z, y=swing, color=over)) +
    labs(
        title = "Release Point",
        x = "Line",
        y = "Length"
    )
cor(mpFast$release_pos_z, mpFast$swing)
