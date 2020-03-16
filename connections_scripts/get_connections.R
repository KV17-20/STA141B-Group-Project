
# Load necessary libraries
library(dplyr)
library(stringr)

# Source API Key
source("connections_scripts/get_data.R")


# Given a character id return a map of of each organization for this character
get_connections <- function(character_id) {

  # Get the data for all of the superheroes and split that data by "groups" or "relatives"
  
  response_data <- get_data(character_id)
  
  connections <- response_data$connections
  
  # Split the groupstring by commas to make a list
  groups <- unlist(strsplit(connections$`group-affiliation`, split=", "))
  
  group_data <- tibble(entity = groups, relation = "Groups")
  
  relatives_raw <- connections$relatives
  
  # Split the relatives string commas and semi-colons
  relatives <- gsub("\\s*\\([^\\)]+\\)","",as.character(relatives_raw))
  relatives <- gsub(";", ",", relatives)
  
  relatives <- unlist(strsplit(relatives, split=", "))
  
  relative_data <- tibble(entity = relatives, relation = "Relatives") 
  
  # Join and return the final data
  connection_data <- full_join(group_data, relative_data, by = c("entity", "relation"))
  
  connection_data 
} 

  
