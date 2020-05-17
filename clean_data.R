library(tidyverse)
library(rlang)

create_dataframe <- function(country) {
  
  country <- str_replace(country, " ", "_")
  
  data <- readr::read_csv(paste0('https://raw.githubusercontent.com/TheEconomist/covid-19-excess-deaths-tracker/master/output-data/excess-deaths/', 
                              country, '_excess_deaths.csv'))
  
  data <- data %>% 
    filter(region == country) %>% 
    select(country, region, start_date, end_date, population,
           total_deaths, covid_deaths, expected_deaths, excess_deaths,
           non_covid_deaths)
  
  data$csum <- ave(data$covid_deaths, data$region, FUN=cumsum)

  data <- data %>%
    filter(csum > 50)
  
  assign(country, rbind(data), envir=.GlobalEnv)
}

country_names <- readr::read_csv('https://raw.githubusercontent.com/TheEconomist/covid-19-excess-deaths-tracker/master/source-data/list_of_sources.csv') %>% 
  select(country) %>% 
  distinct() %>% 
  mutate(country = stringr::str_to_lower(country)) %>% 
  filter(country != 'all') %>% 
  pull()

for (country in country_names) {
  print(country)
  create_dataframe(country)
}

dfs = sapply(.GlobalEnv, is.data.frame) 

data <- do.call(rbind, mget(names(dfs)[dfs]))
