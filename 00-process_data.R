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
