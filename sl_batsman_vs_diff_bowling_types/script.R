library(tidyverse)
library(ggplot2)
library(ggrepel)

full_data <- read.csv("../Downloads/t20_bbb.csv") 

sl_data <- full_data %>% 
  filter(team_bat == "Sri Lanka")

bowling_types <-  unique(full_data$bowl_style)
# Against express pace
fast_bowl <- sl_data %>% 
  filter(bowl_style == "RF" | bowl_style == "LF")

batsmen <- fast_bowl %>%
  mutate(
    across(c(batruns, ballfaced), as.integer),
    across(out, as.logical)
  ) %>% 
  group_by(bat) %>% 
  summarise(runs = sum(batruns), balls=sum(ballfaced), outs = sum(out)) %>% 
  filter(balls > 50) %>% 
  mutate(sr = runs/balls * 100, avg = ifelse(is.infinite(runs / outs), runs, runs / outs))

dilshan <- fast_bowl %>% 
  filter(bat =='Tillakaratne Dilshan') %>% 
  mutate(
    across(c(batruns, ballfaced), as.integer),
    across(out, as.logical)
  ) %>% 
  group_by(bowl) %>% 
  summarise(runs = sum(batruns), balls=sum(ballfaced), outs = sum(out)) %>% 
  mutate(sr = runs/balls * 100, avg = runs / outs)

tp <- fast_bowl %>% 
  filter(bat =='Thisara Perera') %>% 
  mutate(
    across(c(batruns, ballfaced), as.integer),
    across(out, as.logical)
  ) %>% 
  group_by(bowl) %>% 
  summarise(runs = sum(batruns), balls=sum(ballfaced), outs = sum(out)) %>% 
  mutate(sr = runs/balls * 100, avg = runs / outs)

dicka <- fast_bowl %>% 
  filter(bat =='Niroshan Dickwella') %>% 
  mutate(
    across(c(batruns, ballfaced), as.integer),
    across(out, as.logical)
  ) %>% 
  group_by(bowl) %>% 
  summarise(runs = sum(batruns), balls=sum(ballfaced), outs = sum(out)) %>% 
  mutate(sr = runs/balls * 100, avg = runs / outs)

# Plot using ggplot2
plot <- ggplot(batsmen, aes(x = avg, y = sr, label=bat)) +
  geom_point() +                  # Scatter plot
  geom_text_repel() +
  geom_hline(yintercept = mean(batsmen$sr), linetype = "solid", color = "red") +  # Average strike rate line
  geom_vline(xintercept = mean(batsmen$avg, na.rm = TRUE), linetype = "solid", color = "blue") +   # Average runs line
  labs(x = "Average Runs", y = "Strike Rate", title = "Average Runs vs. Strike Rate") +   # Labels and title
  theme_minimal() 
print(plot)

# Against medium pace

fast_bowl <- sl_data %>% 
  filter(bowl_style == "RFM" 
         | bowl_style == "LFM"  
         | bowl_style == "RMF"  
         | bowl_style == "LMF"  
         | bowl_style == "RM"  
         | bowl_style == "LM"  
         | bowl_style == "RSM"  
         | bowl_style == "LSM")

batsmen <- fast_bowl %>%
  mutate(
    across(c(batruns, ballfaced), as.integer),
    across(out, as.logical)
  ) %>% 
  group_by(bat) %>% 
  summarise(runs = sum(batruns), balls=sum(ballfaced), outs = sum(out)) %>% 
  filter(balls > 50) %>% 
  mutate(sr = runs/balls * 100, avg = ifelse(is.infinite(runs / outs), runs, runs / outs))

plot <- ggplot(batsmen, aes(x = avg, y = sr, label=bat)) +
  geom_point() +                  # Scatter plot
  geom_text_repel() +
  geom_hline(yintercept = mean(batsmen$sr), linetype = "solid", color = "red") +  # Average strike rate line
  geom_vline(xintercept = mean(batsmen$avg, na.rm = TRUE), linetype = "solid", color = "blue") +   # Average runs line
  labs(x = "Average Runs", y = "Strike Rate", title = "Average Runs vs. Strike Rate") +   # Labels and title
  theme_minimal() 
print(plot)

# Against leg spin

fast_bowl <- sl_data %>% 
  filter(bowl_style == "LB" 
         | bowl_style == "LBG")

batsmen <- fast_bowl %>%
  mutate(
    across(c(batruns, ballfaced), as.integer),
    across(out, as.logical)
  ) %>% 
  group_by(bat) %>% 
  summarise(runs = sum(batruns), balls=sum(ballfaced), outs = sum(out)) %>% 
  filter(balls > 50) %>% 
  mutate(sr = runs/balls * 100, avg = ifelse(is.infinite(runs / outs), runs, runs / outs))

plot <- ggplot(batsmen, aes(x = avg, y = sr, label=bat)) +
  geom_point() +                  # Scatter plot
  geom_text_repel() +
  geom_hline(yintercept = mean(batsmen$sr), linetype = "solid", color = "red") +  # Average strike rate line
  geom_vline(xintercept = mean(batsmen$avg, na.rm = TRUE), linetype = "solid", color = "blue") +   # Average runs line
  labs(x = "Average Runs", y = "Strike Rate", title = "Average Runs vs. Strike Rate") +   # Labels and title
  theme_minimal() 
print(plot)

pn <- fast_bowl %>% 
  filter(bat =='Pathum Nissanka') %>% 
  mutate(
    across(c(batruns, ballfaced), as.integer),
    across(out, as.logical)
  ) %>% 
  group_by(bowl) %>% 
  summarise(runs = sum(batruns), balls=sum(ballfaced), outs = sum(out)) %>% 
  mutate(sr = runs/balls * 100, avg = runs / outs)

print(bowling_types)

# Against SLA

fast_bowl <- sl_data %>% 
  filter(bowl_style == "SLA")

batsmen <- fast_bowl %>%
  mutate(
    across(c(batruns, ballfaced), as.integer),
    across(out, as.logical)
  ) %>% 
  group_by(bat) %>% 
  summarise(runs = sum(batruns), balls=sum(ballfaced), outs = sum(out)) %>% 
  filter(balls > 50) %>% 
  mutate(sr = runs/balls * 100, avg = ifelse(is.infinite(runs / outs), runs, runs / outs))

plot <- ggplot(batsmen, aes(x = avg, y = sr, label=bat)) +
  geom_point() +                  # Scatter plot
  geom_text_repel() +
  geom_hline(yintercept = mean(batsmen$sr), linetype = "solid", color = "red") +  # Average strike rate line
  geom_vline(xintercept = mean(batsmen$avg, na.rm = TRUE), linetype = "solid", color = "blue") +   # Average runs line
  labs(x = "Average Runs", y = "Strike Rate", title = "Average Runs vs. Strike Rate") +   # Labels and title
  theme_minimal() 
print(plot)


# Against OB

fast_bowl <- sl_data %>% 
  filter(bowl_style == "OB")

batsmen <- fast_bowl %>%
  mutate(
    across(c(batruns, ballfaced), as.integer),
    across(out, as.logical)
  ) %>% 
  group_by(bat) %>% 
  summarise(runs = sum(batruns), balls=sum(ballfaced), outs = sum(out)) %>% 
  filter(balls > 50) %>% 
  mutate(sr = runs/balls * 100, avg = ifelse(is.infinite(runs / outs), runs, runs / outs))

plot <- ggplot(batsmen, aes(x = avg, y = sr, label=bat)) +
  geom_point() +                  # Scatter plot
  geom_text_repel() +
  geom_hline(yintercept = mean(batsmen$sr), linetype = "solid", color = "red") +  # Average strike rate line
  geom_vline(xintercept = mean(batsmen$avg, na.rm = TRUE), linetype = "solid", color = "blue") +   # Average runs line
  labs(x = "Average Runs", y = "Strike Rate", title = "Average Runs vs. Strike Rate") +   # Labels and title
  theme_minimal() 
print(plot)

# BR

fast_bowl <- full_data %>% 
  filter(bat == "Bhanuka Rajapaksa")

batsmen <- fast_bowl %>%
  mutate(
    across(c(batruns, ballfaced), as.integer),
    across(out, as.logical)
  ) %>% 
  group_by(bowl_style) %>% 
  summarise(runs = sum(batruns), balls=sum(ballfaced), outs = sum(out)) %>% 
  filter(balls > 50) %>% 
  mutate(sr = runs/balls * 100, avg = ifelse(is.infinite(runs / outs), runs, runs / outs))

plot <- ggplot(batsmen, aes(x = avg, y = sr, label=bat)) +
  geom_point() +                  # Scatter plot
  geom_text_repel() +
  geom_hline(yintercept = mean(batsmen$sr), linetype = "solid", color = "red") +  # Average strike rate line
  geom_vline(xintercept = mean(batsmen$avg, na.rm = TRUE), linetype = "solid", color = "blue") +   # Average runs line
  labs(x = "Average Runs", y = "Strike Rate", title = "Average Runs vs. Strike Rate") +   # Labels and title
  theme_minimal() 
print(plot)

# Avishka

fast_bowl <- full_data %>% 
  filter(bat == "Avishka Fernando")

batsmen <- fast_bowl %>%
  mutate(
    across(c(batruns, ballfaced), as.integer),
    across(out, as.logical)
  ) %>% 
  group_by(bowl_style) %>% 
  summarise(runs = sum(batruns), balls=sum(ballfaced), outs = sum(out)) %>% 
  filter(balls > 50) %>% 
  mutate(sr = runs/balls * 100, avg = ifelse(is.infinite(runs / outs), runs, runs / outs))

plot <- ggplot(batsmen, aes(x = avg, y = sr, label=bat)) +
  geom_point() +                  # Scatter plot
  geom_text_repel() +
  geom_hline(yintercept = mean(batsmen$sr), linetype = "solid", color = "red") +  # Average strike rate line
  geom_vline(xintercept = mean(batsmen$avg, na.rm = TRUE), linetype = "solid", color = "blue") +   # Average runs line
  labs(x = "Average Runs", y = "Strike Rate", title = "Average Runs vs. Strike Rate") +   # Labels and title
  theme_minimal() 
print(plot)

# nth sr

sr_prog <- full_data %>%
  filter(team_bat == "Sri Lanka") %>% 
  mutate(
    across(c(batruns, ballfaced, cur_bat_bf, cur_bat_runs), as.integer),
    across(out, as.logical)
  ) %>% 
  mutate(sr = cur_bat_runs/cur_bat_bf * 100) %>%
  group_by(bat, cur_bat_bf) %>% 
  summarise(sr=mean(sr), matches=length(unique(p_match))) %>% 
  filter(max(cur_bat_bf) >= 20, cur_bat_bf>0, max(matches)>20) %>% 
  filter(
    bat != "Chamara Kapugedera" &
      bat != "Angelo Mathews" &
      bat != "Dinesh Chandimal" &
      bat != "Isuru Udana" &
      bat != "Thisara Perera" & 
      bat != "Danushka Gunathilaka"
  )

plot <- ggplot(sr_prog, aes(x = cur_bat_bf, y = sr, color=bat)) +
  geom_line(size = 1) +                  # Scatter plot
  labs(x = "Balls", y = "Strike Rate", title = "S/R Progression") +   # Labels and title
  scale_color_manual(values =  c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
    "#aec7e8", "#ff9896"
  )) +
  theme_minimal() 
print(plot)

# Shanaka

fast_bowl <- full_data %>% 
  filter(bat == "Dasun Shanaka")

batsmen <- fast_bowl %>%
  mutate(
    across(c(batruns, ballfaced), as.integer),
    across(out, as.logical)
  ) %>% 
  group_by(bowl_style) %>% 
  summarise(runs = sum(batruns), balls=sum(ballfaced), outs = sum(out)) %>% 
  filter(balls > 50) %>% 
  mutate(sr = runs/balls * 100, avg = ifelse(is.infinite(runs / outs), runs, runs / outs))
