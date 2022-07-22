library(tidyverse)
library(vroom)
library(lubridate)
#library(geobr)


# Lendo os bancos
# https://covid19.who.int/data (Download at 9/5/2022)

# Data downloaded from opendatasus
who <- vroom("~/Downloads/WHO-COVID-19-global-data.csv")

whoBR <- who %>% filter(Country_code == "BR") 

whoBR$Date_reported[whoBR$Cumulative_deaths == 1]

whoBR$Cumulative_deaths[whoBR$Date_reported == "2022-03-11"]
