library('lubridate')
library('dplyr')

source("R/datadir.R")
source("R/depths.R")
source("R/dates.R")

chla <- read.csv(paste0(datadir, "Longterm_data/temp_chl_secchi_wind/cleaned_data/chla_cleaned.csv"), stringsAsFactors = FALSE)

chla_dates <- merge(chla, winterdates, by.x = "Year", by.y = "iceoff_year",
                    all.x = TRUE, all.y = FALSE)

chla_dates <- chla_dates %>%
  filter(iceon <= as.Date(date) & as.Date(date) <= iceoff 
         | month(date) %in% c(7, 8, 9)) %>%
  mutate(season = ifelse(month(date) %in% c(7, 8, 9), "summer", "winter")) %>%
  arrange(date, depth)