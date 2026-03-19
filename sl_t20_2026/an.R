library(tidyverse)

data <- read.csv("C:\\Users\\thevi\\Desktop\\stats.csv") %>%
    pivot_longer(cols = c(Six.Rate, Four.Rate, Running.Rate, Strike.Rate), names_to = "Runs", values_to = "value") %>%
    pivot_wider(names_from = "Type", values_from = "value") %>%
    mutate(Runs = case_when(
        Runs == "Six.Rate" ~ "Six Rate",
        Runs == "Four.Rate" ~ "Four Rate",
        Runs == "Running.Rate" ~ "Running Rate",
        Runs == "Strike.Rate" ~ "Strike Rate"
    ))

write.csv(data, "C:\\Users\\thevi\\Desktop\\stats_long.csv", row.names = FALSE)
