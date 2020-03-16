# Load necessary libraries
library(dplyr)
library(httr)
library(jsonlite)
library(stringr)

# Source API Key
superhero_key = 186700939422355

timed_GET <- function(uri) {
  req <- GET(uri)
  round(req$times * 1000)
}

# Get all of a character's data based on their id number
get_data <- function(character_id) {
  
  base_uri <- "https://superheroapi.com/api"
  
  uri.full <- paste(base_uri, superhero_key, character_id, sep = "/")
  
  response <- GET(uri.full)

  # Parse with JSON
  response_text <- content(response, "text")
  response_data <- fromJSON(response_text)
  
  response_data
}
