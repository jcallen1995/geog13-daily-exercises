#Jason Allen
#07/09/2021
#This is the daily exercise 09

library(tidyverse)
library(USAboundaries)

conus_only = -c("Puerto Rico", "Alaska", "Hawaii")

States = us_states()# %>%
 # filter(state_name != "Puerto Rico")

typeof(States)
class(states)
