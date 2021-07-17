#Jason Allen
#07/15/2021
#This daily assignment is to practice using functions


library(tidyverse)
library(sf)

#read in csv with us cities

USA_Cities <- st_as_sf(read.csv("./data/uscities.csv"), coords = c("lng", "lat"), crs = 4326)

State_Poly <- USAboundaries::us_states()

#US_Conus_Select <- State_Poly %>%
#  filter(!state_name %in% c( "Alaska", "American Samoa", "Guam", "Hawaii", "Puerto Rico", "Virgin Islands"))

#Now make the conus selection into a function

#This function will return the usa boundaries states that are in the conus
JCA_US_States_Conus <- function(data, name){
  filter(data, !name %in% c( "Alaska", "American Samoa", "Guam", "Hawaii", "Puerto Rico", "Virgin Islands"))
}

Conus_States <- JCA_US_States_Conus(State_Poly, "name")

#now we join the cities data to the us states

#turn cities into an sf
Cities_Per_State <- Conus_States %>%
  st_join(USA_Cities) %>%
  st_transform(5070) %>%
  count(name)

#Turn this into a function
#This function takes a polygon sf and a point sf and outputs an sf with only the number of points in whichever
#column you were checking for
JCA_PIP <- function(data_polygon, data_points, choice_column, name, crs = 5070){
  count(st_transform(st_join(data_polygon, data_points), crs), name)
}

Cities_Per_State_Func <- JCA_PIP(Conus_States, USA_Cities, "name")

#find center of cities data



#not plot my data
plot_to_save <- ggplot()+
  geom_sf(data = Conus_States, fill = "lightgrey", color = "white")+

  geom_sf(data = Cities_Per_State_Func,
          aes(fill = log(n)),
          alpha = .8,
          size = .2,
          color = "white")+
  scale_fill_gradient(low = "ivory1", high = "darkorchid3")+
  theme(legend.position = "none")+
  labs(title = "Number of Cities Per State")+
  theme(panel.grid.major = element_line(color = "lightgrey"))+
  theme(panel.grid.minor = element_line(color = "black"))+
  #scale_x_continuous(breaks = seq(,,.01))+
  #scale_y_continuous(breaks = seq(,,.01))+

  theme(panel.background = element_rect(color = "white", fill = "white"))




#now make it a function



seq(0,10,.01)




#not plot my data
JCA_Plot_PIP <- function(background_geometry, overlay_geometry, plot_title){
plot_to_save <- ggplot()+
  geom_sf(data = background_geometry, fill = "lightgrey", color = "white")+
  geom_sf(data = overlay_geometry,
          aes(fill = log(n)),
          alpha = .8,
          size = .2,
          color = "white")+
  scale_fill_gradient(low = "ivory1", high = "darkorchid3")+
  theme(legend.position = "none")+
  labs(title = plot_title)+
  theme(panel.grid.major = element_line(color = "lightgrey"))+
  theme(panel.grid.minor = element_line(color = "black"))+
  theme(panel.background = element_rect(color = "white", fill = "white"))
}

my_plot <- JCA_Plot_PIP(Conus_States, Cities_Per_State_Func, "Number of Citites Per State")

ggsave(my_plot, filename = "./img/day-14-cities-per-state.png")






