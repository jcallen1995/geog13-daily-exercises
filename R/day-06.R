#Daily Exercise 06
#Jason Allen
#06/30/2021
#This script is for practice rendering plots using ggplot


library(tidyverse)

url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'

covid = read_csv(url)

most_cases = covid %>%
  filter(date == max(date), cases > 0) %>%  #pick only cumulative current date and non-zero/null cases
  group_by(state) %>%
  summarize(state_cases = sum(cases, na.rm = TRUE)) %>% #boil it down to only the state level
  ungroup() %>%
  arrange(-state_cases) %>%
  slice(1:6)  #pick only the top six states by cases


top_six_states = c(pull(most_cases, 'state')) #pull states from new dataframe

top_six = covid %>%
  filter(cases > 0) %>%
  filter(state %in% top_six_states) %>% #filter out zero and null, filter by state from top six
  group_by(date, state) %>%
  summarize(state_cases = sum(cases, na.rm = TRUE)) %>%
  ungroup()



top_six_save = top_six %>%
  ggplot(aes(x = date, y = state_cases)) +
  geom_point(aes(color = state)) +
  facet_wrap(~state, scales = "free") +
  labs(title = "Recent COVID cases by State",
       x = "Date",
       y = "Cases",
       color = "",
       subtitle = "Data from NY Times recent COVID Cases") +
  theme_minimal()


ggsave(top_six_save, file = "img/top_six_states_by_cases.png")


total_US_cases <- covid %>%
  group_by(date) %>%
  summarize(US_cases = sum(cases, na.rm = TRUE))

total_US_cases_save = total_US_cases %>%
  ggplot(aes(x=date, y=US_cases)) +
  geom_point(aes(col = "red")) +
  labs(title = "Total US Cases",
       x = "Date",
       y = "Cases",
       subtitle = "Data from NY Times Recent COVID Cases") +
  theme_minimal()

ggsave(total_US_cases_save, file = "img/total_US_cases.jpg")
