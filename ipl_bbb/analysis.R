library(tidyverse)

data <- read_csv("data.csv") %>%
    filter(!is.na(date))
