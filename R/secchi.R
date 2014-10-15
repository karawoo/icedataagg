library('lubridate')
library('dplyr')

source("R/datadir.R")
source("R/datesdepths.R")

secchi_dates <- data.frame(date = as.Date(unique(secchi$date))) %>%
  filter(sapply(date, date_subset, winterints) | month(date) %in% c(7, 8, 9))

secchi_agg <- secchi %>%
  do(na.omit(.)) %>%
  semi_join(secchi_dates, by = "date") %>%
  mutate(season = ifelse(month(date) %in% c(7, 8, 9), "summer", "winter")) %>%
  mutate(photic_zone = pz(secchi_depth)) %>%
  group_by(year, season) %>%
  summarize(Secchi.Depth = mean(secchi_depth, na.rm = TRUE), 
            PhoticZone = mean(photic_zone, na.rm = TRUE), 
            ndates = length(unique(date)), 
            mindate = min(date), 
            maxdate = max(date)) %>%
  mutate(avg_ndepths = NA) %>%
  arrange(year, desc(season))