#Jason Allen
#07/13/2021
#Using newly learned spatial sf functions

library(tidyverse)
library(sf)

US_States <- filter(USAboundaries::us_states(), !(name %in% c("Alaska", "Hawaii", "Puerto Rico")))


State_Choice <- filter(US_States, state_name == "Colorado")
States_Except_Choice <- filter(US_States, state_name != "Colorado")

#inputs for st_touches must be multipolygons for this data set
Touch_Colorado <- st_filter(States_Except_Choice, State_Choice,  .predicate = st_touches)


Colorado_Touch_Save <- ggplot()+
  geom_sf(data = US_States)+
  geom_sf(data = Touch_Colorado, fill = "red", alpha = .5)+
  geom_sf(data = State_Choice, fill = "gold")+
  labs(title = "States Touching Colorado",
       x = "Longitude",
       y = "Latitude",
       subtitle = "Data from USA Boundaries Library",
       caption = "States Touching Colorado in Red")


ggsave(Colorado_Touch_Save, filename = "./img/states-touching-colorado.png")
