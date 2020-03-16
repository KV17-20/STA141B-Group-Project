library(ggplot2)
library(dplyr)
library(stringr)
library(lintr)
library(tidyr)

#Function that will build the histogram based on two given states to compare
build_hist <- function(
  data, superhero1, superhero2, x = "Superhero", y = "Stats") {
  data <- data %>% select(
    name, intelligence, power, speed, strength, combat, durability) %>%  filter(
    (name == superhero1) | (name == superhero2))
  dat.g <- gather(data, type, value, -name)
  build <- ggplot(dat.g, aes(type, value)) +
    geom_bar(aes(fill = name), stat = "identity", position = "dodge")
  build
}

