library(tidyverse)
library(fs)

files <- dir_ls('data')

data <- files %>%
  enframe() %>%
  mutate(
    name = str_remove(name, 'data/'),
    name = str_remove(name, '.csv'),
    data = map(value, ~ read_csv(.x))
  ) 


process_raw_csv <- function(data) {
  out <- data %>%
    janitor::clean_names() %>%
    select(., 1, 3) %>%
    slice(1:62) %>%
    pivot_wider(names_from = 1, values_from = 2) %>%
    janitor::clean_names()
  
  return(out)
}

data <- data %>%
  mutate(
    data = map(data, ~ process_raw_csv(.x))
  )

data <- data %>%
  rename('city' = name) %>%
  select(-value) %>%
  unnest(data)

data
