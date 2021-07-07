#Jason Allen
#07/06/2021
#COVID 7 day rolling mean

library(tidyverse)
library(zoo)

url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'

covid = read_csv(url)

choose_state = "California"

New_Cases = covid %>%
  filter(state == choose_state) %>%
  group_by(date) %>%
  summarize(total_cases_per_day = sum(cases, na.rm = TRUE)) %>%  #sum up all the counties
  add_row(date = as.Date("2020-01-24"), total_cases_per_day = 0, .before = 1) %>% #add a previous day to keep the first case
  mutate(new_cases = total_cases_per_day - lag(total_cases_per_day)) %>% # subtract last days numbers to find new cases
  filter(new_cases != "NA") %>%
  mutate(roll_7 = rollmean(new_cases, 7, fill = NA, align = "right")) %>% #add a rolling mean column, the first 7 days will be NA
  replace(is.na(.), 0) %>% #replace the first 7 values with zero since that is effectively the same since the first cases were quite sparse
  #select(-c(total_cases_per_day)) %>%  #remove total cases
  ggplot(aes(x = date)) +  #only declare x, let geoms handle y
  geom_col(aes(y = new_cases), alpha = 0.2, fill = "red")+
  geom_line(aes(y = roll_7, col = "darkred")) +
  labs(title = "California New Covid Cases Per Day",
       subtitle = "Data Collected from NY Times Covid Data",
       x = "Date",
       y = "New Cases Each Day")+
  ggthemes::theme_tufte()+
  theme(legend.position = "none")+
  theme(aspect.ratio = .5)

ggsave(New_Cases, file = "img/day-08-covid-cases-per-day.jpeg")

