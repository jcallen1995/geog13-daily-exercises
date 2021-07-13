#Jason Allen
#07/12/2021
#This script is for finding the distance between UCSB and my hometouwn
#attribute csv to https://simplemaps.com/data/us-cities

library(tidyverse)
library(sf)

#Read in the uscites.csv data (lab 1 material) and make it spatial (CRS = 4326)
#Filter to include only Santa Barbara and your home town. (Should only have 2 points!)
Two_Cities_4326 <- readr::read_csv("data/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  filter(city %in% c("Santa Rosa", "Santa Barbara")) %>%
  filter(state_id == "CA")

#Transform your filtered object locations to:
#An equal area projection (EPSG: 5070)
Two_Cities_5070 <- Two_Cities_4326 %>%
  st_transform(crs = 5070)

#An eqidistance projection
eqds <- "+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
Two_Cities_EQDC <- st_transform(Two_Cities_4326, eqds)

#Calculate the distance between the cites in all three projections using st_distance()
dist_4326 <- st_distance(Two_Cities_4326)
dist_5070 <- st_distance(Two_Cities_5070)
dist_eqdc <- units::set_units(st_distance(Two_Cities_EQDC), "km")

(dist_4326)
(dist_5070)
(dist_eqdc)
