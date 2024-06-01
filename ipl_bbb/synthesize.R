library(tidyverse)

sanitizeData <- function(data) {
    names(data)[1:(ncol(data) - 1)] <- names(data)[2:ncol(data)]
    data[, ncol(data)] <- NULL
    sanitized <- data %>%
        na.omit() %>%
        mutate(batting_team = as.character(batting_team))  %>%
        mutate(batsman_id = as.character(batsman_id)) %>%
        mutate(batsman_name = as.character(batsman_name)) %>%
        mutate(batsman_is_rhb = as.logical(batsman_is_rhb))  %>%
        mutate(non_striker_id = as.character(non_striker_id)) %>%
        mutate(non_striker_name = as.character(non_striker_name)) %>%
        mutate(non_striker_is_rhb = as.logical(non_striker_is_rhb))  %>%
        mutate(bowling_team = as.character(bowling_team)) %>%
        mutate(bowler_id = as.character(bowler_id)) %>%
        mutate(bowler_name = as.character(bowler_name)) %>%
        mutate(bowler_is_rhb = as.logical(bowler_is_rhb)) %>%
        mutate(delivery_type = as.character(delivery_type)) %>%
        mutate(innings = as.integer(innings)) %>%
        mutate(ball = as.integer(ball)) %>%
        mutate(over = as.integer(over)) %>%
        mutate(shot_is_attacked = as.logical(shot_is_attacked)) %>%
        mutate(shot_is_played = as.logical(shot_is_played)) %>%
        mutate(shot_type = as.character(shot_type)) %>%
        mutate(bounce_above_stumps = as.character(bounce_above_stumps)) %>%
        mutate(bounce_angle = as.double(bounce_angle)) %>%
        mutate(bounce_pos_x = as.double(bounce_pos_x)) %>%
        mutate(bounce_pos_y = as.double(bounce_pos_y)) %>%
        mutate(boucne_pos_z = as.double(boucne_pos_z)) %>%
        mutate(crease_pos_x = as.double(crease_pos_x)) %>%
        mutate(crease_pos_y = as.double(crease_pos_z)) %>%
        mutate(crease_pos_z = as.double(crease_pos_z)) %>%
        mutate(deviation = as.double(deviation)) %>%
        mutate(drop_angle =  as.double(drop_angle)) %>%
        mutate(hit_stumps = as.logical(hit_stumps)) %>%
        mutate(impact_pos_x = as.double(impact_pos_x)) %>%
        mutate(impact_pos_y = as.double(impact_pos_y)) %>%
        mutate(impact_pos_z = as.double(impact_pos_z)) %>%
        mutate(initial_angle = as.double(initial_angle)) %>%
        mutate(landing_pos_x = as.double(landing_pos_x)) %>%
        mutate(landing_pos_y = as.double(landing_pos_y)) %>%
        mutate(landing_pos_z = as.double(landing_pos_z)) %>%
        mutate(pbr = as.double(pbr)) %>%
        mutate(react_time_to_crease = as.double(ifelse(react_time_to_crease == "None",0, react_time_to_crease ))) %>%
        mutate(react_time_to_intercept = as.double(ifelse(react_time_to_intercept == "None",0, react_time_to_intercept ))) %>%
        mutate(real_distance = as.double(real_distance)) %>%
        mutate(release_pos_x = as.double(release_pos_x)) %>%
        mutate(release_pos_y = as.double(release_pos_y)) %>%
        mutate(release_pos_z = as.double(release_pos_z)) %>%
        mutate(release_speed = as.double(release_speed)) %>%
        mutate(spin_rate = as.double(spin_rate)) %>%
        mutate(stump_pos_x = as.double(stump_pos_x)) %>%
        mutate(stump_pos_y = as.double(stump_pos_y)) %>%
        mutate(stump_pos_z = as.double(stump_pos_z)) %>%
        mutate(swing = as.double(swing)) %>%
        mutate(match_id = as.character(match_id)) %>%
        mutate(ball_id = as.character(ball_id)) %>%
        mutate(is_single = as.logical(is_single)) %>%
        mutate(is_double = as.logical(is_double)) %>%
        mutate(is_three = as.logical(is_three)) %>%
        mutate(is_dot = as.logical(is_dot)) %>%
        mutate(is_wide = as.logical(is_wide)) %>%
        mutate(is_no_ball = as.logical(is_no_ball)) %>%
        mutate(is_bye = as.logical(is_bye)) %>%
        mutate(is_leg_bye = as.logical(is_leg_bye)) %>%
        mutate(is_four = as.logical(is_four)) %>%
        mutate(is_six = as.logical(is_six)) %>%
        mutate(is_wicket = as.logical(is_wicket)) %>%
        mutate(wicket_type = as.character(wicket_type)) %>%
        mutate(is_bowler_wicket = as.logical(is_bowler_wicket)) %>%
        mutate(ball_type = as.character(ball_type)) %>%
        mutate(shot_type_b = as.character(shot_type_b)) %>%
        mutate(pitch_x = as.double(pitch_x)) %>%
        mutate(pitch_y = as.double(pitch_y)) %>%
        mutate(ball_line = as.character(ball_line)) %>%
        mutate(ball_length = as.character(ball_length)) %>%
        mutate(runs = as.character(runs)) %>%
        mutate(is_bouncer = as.logical(is_bouncer)) %>%
        mutate(is_free_hit = as.logical(is_free_hit)) %>%
        mutate(innings_no = as.integer(innings_no)) %>%
        mutate(ground = as.character(ground)) %>%
        mutate(date = as.character(date))

    return(sanitized)
}

data2022 <- read.csv("data2022.csv", row.names=NULL) %>%
    sanitizeData()

data2023 <- read.csv("data2023.csv", row.names=NULL) %>%
    sanitizeData()

data2024 <- read.csv("data2024.csv", row.names=NULL) %>%
    sanitizeData()

data  <- bind_rows(data2022, data2023, data2024)

write.csv(data, "data.csv", row.names=FALSE)
