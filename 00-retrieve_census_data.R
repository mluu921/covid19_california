library(tidyverse)
library(censusapi)
library(tidycensus)

key <- 'bfcc240cf7a426ee8011dfa086cb84e94eae89c9'

Sys.setenv(CENSUS_KEY=key)

apis <- listCensusApis()
View(apis)


getCensus('acs5', '2017', vars = list('AGECAT'))


getCensus(name = "acs/acs1",
          vars = c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"),
          region = "state:*",
          time = 2017)

vars <- listCensusMetadata(name = 'acs/acs1/profile', vintage = 2005, type = 'variables')

vars
https://api.census.gov/data/2005/acs/acs1/profile


https://www.census.gov/quickfacts/fact/csv/sangabrielcitycalifornia,beverlyhillscitycalifornia,alhambracitycalifornia/PST045219