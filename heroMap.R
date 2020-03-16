## Create an interactive map of the hero data
superhero_key = 186700939422355
library("httr")
library("jsonlite")
library("dplyr")
library("knitr")
library("stringr")
library("leaflet")

# Create a data frame that renames the columns to be neater and then filters out
# all NA values.
file <- file.choose() #choose the file named "locations.csv" then run object "file" to get the directory. 
marvel_frame <- read.csv("/Users/kazouavang/Documents/superhero-API/locations.csv", stringsAsFactors = FALSE) %>%
  select(
    name, full.name, place.of.birth,publisher)%>%
  rename(
     "Name" = name,
    "Full Name" = full.name,
    "Place of Birth" = place.of.birth,
    "Publisher" = publisher
  ) %>%
  filter(!grepl("-", `Place of Birth`))

# Uses another data set with all US cities to find latitude and longitude of
# Place of Birth column
cities_frame <- read.csv("/Users/kazouavang/Documents/superhero-API/uscitiesv1.4.csv", stringsAsFactors = FALSE) %>%
  select(city, state_name, lat, lng) %>%
  mutate("Place of Birth" = paste0(city, ", ", state_name))

# Joins marvel frame and cities frame based on place of birth to match latitude
# and longitude and
# create final data frame to be used for 'super map'
super_frame <- merge(marvel_frame, cities_frame,
  by = "Place of Birth",
  type = "inner"
) %>%
  select(`Place of Birth`, Name, `Full Name`, Publisher, lat, lng) %>%
  rename(latitude = lat, longitude = lng)

# Makes super hero map with circles plotted at place of birth locations and
# a popup with interesting information about each of the characters


make_super_map <- function(publisher) {
  
  super_frame <- super_frame %>%
    filter(Publisher == publisher)
  
  leaflet(data = super_frame, width = "100%") %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -101.584521, lat = 40.554970, zoom = 4.25) %>%
  addCircles(
    lat = super_frame$latitude,
    lng = super_frame$longitude,
    color = "steelblue",
    popup = paste(
      # super_frame, "<br>",
      "Name: ", super_frame$Name, "<br>",
      "Full Name: ", super_frame$`Full Name`, "<br>",
      "Place of Birth: ", super_frame$`Place of Birth`, "<br>",
      "Publisher: ", super_frame$Publisher, "<br>"
    ),
    radius = 50000,
    stroke = FALSE,
    fillOpacity = 0.6
  )
}

make_super_map(super_frame$Publisher)

