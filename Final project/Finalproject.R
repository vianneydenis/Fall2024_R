library(dplyr)
library(terra)
library(rgbif)
library(sp)
library(leaflet)


setwd("/Users/riversung/NTU/113-1 Fall 2024/Ocean5098 R/Final project")
raw.data <- read.csv("CSD_Tables.csv")

# select the countries needed 
spawning.country <- raw.data %>% 
  filter(Country %in% c('Indonesia', 'Japan', 'Philippines', 'Taiwan')) %>%
  filter(Genus %in% c('Diploastrea', 'Galaxea', 'Lobophyllia', 'Coelastrea', 'Dipsastraea', 'Favites', 
                        'Goniastrea', 'Leptoria', 'Merulina', 'Pectinia', 'Platygyra', 'Porites'))
  
# delete the unneeded columns
spawning.country[ , c('Subsite', 'O_n', 'Depth_m', 'N', 'No_start', 'Quality_start', 'Quality_end', 
                      'Gamete_release', 'Situation', 'Timezone', 'Reference', 'Comments')] <- list(NULL)



leaflet() %>%
  addTiles() %>%
  setView(lng = 121.0, lat = 20, zoom = 3.5) %>% # Center the map on Taiwan
  addCircleMarkers(
    lng = spawning.country$Longitude,
    lat = spawning.country$Latitude,
    popup = spawning.country$Site,
    radius = 1,
    fillOpacity = 0.6
  )


