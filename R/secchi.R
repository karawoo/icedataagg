library('lubridate')
library('dplyr')

source("R/datadir.R")
source("R/datesdepths.R")

secchi_dates <- data.frame(date = as.Date(unique(secchi$date))) %>%
  filter(sapply(date, date_subset, winterints) | month(date) %in% c(7, 8, 9))

secchi_agg <- secchi %>%
  do(na.omit(.)) %>%
  semi_join(secchi_dates, by = "date") %>%
  mutate(season = ifelse(month(date) %in% c(7, 8, 9), "iceoff", "iceon")) %>%
  mutate(photic_zone = pz(secchi_depth)) %>%
  group_by(year, season) %>%
  summarize(secchidepth = mean(secchi_depth, na.rm = TRUE), 
            photicdepth = mean(photic_zone, na.rm = TRUE), 
            ndates = length(unique(date)), 
            mindate = min(date), 
            maxdate = max(date)) %>%
  arrange(year, desc(season))