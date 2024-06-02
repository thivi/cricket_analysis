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
    mutate(year = substr(date, 1, 4)) %>%
    mutate(stump_pos_x = stump_pos_x + 10.06) %>%
    mutate(crease_pos_x = crease_pos_x + 10.06) %>%
    mutate(release_pos_x = release_pos_x + 10.06) %>%
    mutate(bounce_pos_y = ifelse(batsman_is_rhb, -1 * bounce_pos_y, bounce_pos_y)) %>%
    mutate(stump_pos_y = ifelse(batsman_is_rhb, -1 * stump_pos_y, stump_pos_y)) %>%
    mutate(crease_pos_y = -1 * crease_pos_y) %>%
    mutate(release_pos_y = -1 * release_pos_y) %>%
    mutate(impact_pos_y = -1 * impact_pos_y) %>%
    mutate(bounce_pos_y = -1 * bounce_pos_y)

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
    mutate(length = ifelse(bounce_pos_x < 0, "Full toss", ifelse(bounce_pos_x < 2, "Yorker", ifelse(bounce_pos_x < 6, "Slot", ifelse(bounce_pos_x < 8, "Good length", ifelse(bounce_pos_x < 10, "Short of a good length", "Short"))))))

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
    filter(bowler_is_rhb == TRUE & delivery_type == "Seam")

rfb_release_z  <- median(rfb$release_pos_z)
rfb_release_y  <- median(rfb$release_pos_y)

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
