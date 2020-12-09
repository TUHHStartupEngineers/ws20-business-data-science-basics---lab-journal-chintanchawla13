cat("\014")
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(maps)
library(readr)
library(scales)
## DATA PREPARATION
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")


covid_mortality_rate <- covid_data_tbl %>%
  select(countriesAndTerritories, deaths, popData2019) %>% 
  set_names(c("country", "deaths", "population")) %>%
  
  # Selecting columns to focus on and adding a month column
  mutate(mortality_rate = deaths/population)%>%
  mutate(across(country, str_replace_all, "_", " ")) %>%
  mutate(country = case_when(
    
    country == "United Kingdom" ~ "UK",
    country == "United States of America" ~ "USA",
    country == "Czechia" ~ "Czech Republic",
    TRUE ~ country
  )) %>%
  
  group_by(country) %>%
  summarize(deaths=sum(deaths),population=max(population),mortality_rate = sum(mortality_rate)) %>%
  ungroup() 
 

world <- map_data("world")

covid_world_mortality_rate <- left_join(x = world, y = covid_mortality_rate, by=c("region" = "country")) 

ggplot(covid_world_mortality_rate, aes(x=long, y=lat, group = group, fill = (mortality_rate))) + 
  geom_polygon(colour = "white") +
  scale_fill_continuous(low = "dodgerblue",
                        high = "black",
                        guide="colorbar",
                        labels=percent,
                        limits = c(0, .0015)) +
   
  
  
  theme_bw() +
  labs(fill = "Mortality Rate" ,
       title = "Confirmed COVID-19 deaths relative to the size of population", 
       subtitle = "More than 1.5 Million confirmed COVID-19 deaths worldwide",
       x="long", 
       y="lat") 



  # scale_y_continuous(breaks=c()) +
  # scale_x_continuous(breaks=c()) +
  # theme()

