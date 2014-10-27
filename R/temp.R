library('lubridate')
library('dplyr')

source("R/datadir.R")
source("R/datesdepths.R")

temp <- read.csv(paste0(datadir, "Longterm_data/temp_chl_secchi_wind/cleaned_data/temp_cleaned.csv"), stringsAsFactors = FALSE)
temp$date <- as.Date(temp$date, format = "%Y-%m-%d")

# select correct dates
temp_dates <- data.frame(date = as.Date(unique(temp$date))) %>%
  filter(sapply(date, date_subset, winterints) | month(date) %in% c(7, 8, 9))

# aggregate
temp_sml <- temp %>%
  do(na.omit(.)) %>%
  semi_join(temp_dates, by = "date") %>%
  mutate(year = year(date), 
         season = ifelse(month(date) %in% c(7, 8, 9), "summer", "winter")) %>%
  left_join(secchi[, c("date", "secchi_depth")], by = "date") %>%
  ## add missing secchi according to the following rules:
  # 1) if missing values are within a year that has other secchi measurements,
  # use the average secchi from that season
  # 2) if missing values are within the time series but there were no other 
  # secchi measurements in that year, use the average winter or summer secchi
  # from the whole time series
  group_by(year, season) %>%
  mutate(secchi_depth = ifelse(is.na(secchi_depth),
                               mean(secchi_depth, na.rm = TRUE),
                               secchi_depth)) %>%
  mutate(secchi_depth = ifelse(is.nan(secchi_depth) & season == "winter", 
                               winter_secchi, ifelse(is.nan(secchi_depth) 
                                                     & season == "summer", 
                                                     summer_secchi, 
                                                     secchi_depth))) %>%
  mutate(photic_zone = pz(secchi_depth)) %>%
  filter(depth <= photic_zone)

sample_info_temp <- temp_sml %>%
  group_by(year, season, date) %>%
  summarize(ndates = length(unique(date)), 
            mindate = min(date), 
            maxdate = max(date))

temp_agg <- temp_sml %>%
  group_by(year, season) %>%
  summarize(watertemp = mean(temp)) %>%
  left_join(sample_info_temp, by = c("year", "season")) %>%
  arrange(year, desc(season))
