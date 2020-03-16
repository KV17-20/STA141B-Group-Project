# Load necessary libraries
library(dplyr)
source("connections_scripts/get_data.R")
source("connections_scripts/get_self.R")

# Buggy ones are 132, 173, 368

hero_data <- read.csv("connections_scripts/comparison.csv", stringsAsFactors = FALSE)

hero_names <- hero_data %>% select(id, name)

colnames(hero_names) = c("id", "names")