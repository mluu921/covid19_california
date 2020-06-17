library(tidyverse)

retrieve_data <- function() {
  url <- 'https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/latimes-place-totals.csv'
  
  df <- read_csv(url, col_types = cols(
    date = col_date(format = ""),
    county = col_character(),
    fips = col_character(),
    place = col_character(),
    confirmed_cases = col_double(),
    note = col_character(),
    x = col_double(),
    y = col_double()
  ))
  
  df <- df %>%
    filter(.,!is.na(x) | !is.na(y)) %>%
    filter(place != 'Pacifica') %>%
    # filter(date %in% mdy('5/31/2020')) %>%
    mutate(
      label = glue::glue(
        '<h4>{place}</h4>
        <b>County:</b> {county} <br>
        <b>Confirmed Cases:</b> {confirmed_cases}'
      )
    )
}

retrieve_data2 <- function() {
  url <- 'https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/latimes-county-totals.csv'
  data <- read_csv(url)
}

df2 <- retrieve_data2()
df <- retrieve_data()

write_csv(df, 'data.csv')
write_csv(df2, 'data2.csv')


## webscrape the quick facts census data

places <- df %>%
  filter(
    county == 'Los Angeles'
  ) %>%
  filter(
    !str_detect(place, 'Unincorporated')
  ) %>%
  filter(
    !str_detect(place, '/')
  ) %>%
  filter(
    !str_detect(place, '-')
  ) %>%
  filter(
    !str_detect(place, '//.')
  ) %>%
  pull(place) %>%
  unique() %>%
  str_to_lower() %>%
  str_remove(., ' ')

data <- places %>%
  enframe() %>%
  mutate(url = glue::glue(
    'https://www.census.gov/quickfacts/fact/csv/{value}citycalifornia/PST045219?'
  ))

possibly_download <- possibly(download.file, NA)
walk2(data$value, data$url, ~ possibly_download(.y, destfile = paste0('data/', .x, '.csv')))




files <- fs::dir_ls('data')

process_raw_csv <- function(data) {
  data <- data %>%
    janitor::clean_names()
  
  data <- data %>%
    select(
      1, 3
    ) %>%
    slice(1:62)
  
  return(data)
}

data$processed[[1]] %>%
  pivot_wider(names_from = 1, values_from = 2) %>% View()



data <- map(
  files, ~ read_csv(.x)
)

data <- data %>%
  enframe() %>%
  mutate(
    processed = map(value, ~ process_raw_csv(.x))
  )









