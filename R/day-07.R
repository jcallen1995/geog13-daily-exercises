#Jason Allen
#07/01/2021
#Practice data joining

library(tidyverse)

url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'

covid = read_csv(url)

#create dataframe to add region column to covid data
region = data.frame(state_abb = state.abb, state = state.name, state_region = state.region)

#create variables to relable the plot with labeller
cd_stats.labs <- c("Cases", "Deaths")
names(cd_stats.labs) <- c("region_cases", "region_deaths")


cd_by_region_save = covid %>% #pipe covid dataframe into a ggplot to save later
  filter(county != "Unknown", cases > 0 & deaths > 0) %>%  #clean data
  left_join(region, by = c("state" = "state")) %>% #mutate covid with region column
  group_by(state_region, date) %>%
  summarize(region_cases = sum(cases, na.rm = TRUE), region_deaths = sum(deaths, na.rm = TRUE)) %>% #summarize based on region
  ungroup() %>%
  filter(state_region != "NA") %>% #filter out non continental US states
  pivot_longer(cols = c('region_cases', 'region_deaths'), names_to = 'cd_stats', values_to = 'values') %>% #prepare for ggplot
  ggplot(aes(x = date, y = values)) +
  geom_line(col = "red") +
  geom_point(aes(col = state_region)) +
  labs(title = "US COVID Cases and Deaths by Region",
       subtitle = 'Data from NY Times Current COVID Numbers',
       x = "Date",
       y = "",
       color = "") +
  facet_grid(cd_stats~state_region, scales = "free_y", labeller = labeller(cd_stats = cd_stats.labs)) + #make a grid with two rows of plots, cases and deaths & relabel the plot
  ggthemes::theme_stata() #use ggthemes theme


ggsave(cd_by_region_save, file = "img/cases-and-deaths-by-region.jpeg")
