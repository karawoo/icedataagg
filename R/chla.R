library('lubridate')
library('dplyr')

source("datadir.R")
source("datesdepths.R")
source("co_var.R")

chla <- read.csv(paste0(datadir, "Longterm_data/temp_chl_secchi_wind/cleaned_data/chla_cleaned.csv"), stringsAsFactors = FALSE)
chla$date <- as.Date(chla$date)
names(chla) <- tolower(names(chla))

# select correct dates 
chla_dates <- data.frame(date = as.Date(unique(chla$date))) %>%
  filter(sapply(date, date_subset, winterints) | month(date) %in% c(7, 8, 9))

# aggregate
chla_sml <- chla %>%
  do(na.omit(.)) %>%
  semi_join(chla_dates, by = "date") %>%
  mutate(season = ifelse(month(date) %in% c(7, 8, 9), "iceoff", "iceon")) %>%
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
  mutate(secchi_depth = ifelse(is.nan(secchi_depth) & season == "iceon", 
                               winter_secchi, ifelse(is.nan(secchi_depth) 
                                                     & season == "iceoff", 
                                                     summer_secchi, 
                                                     secchi_depth))) %>%
  mutate(photic_zone = pz(secchi_depth)) %>%
  filter(depth <= photic_zone)

sample_info_chla <- chla_sml %>%
  group_by(year, season) %>%
  summarize(ndates = length(unique(date)), 
            mindate = min(date), 
            maxdate = max(date))

chla_agg <- chla_sml %>%
  group_by(year, season) %>%
  summarize(avechla = mean(chla, na.rm = TRUE), 
            maxchla = max(chla, na.rm = TRUE),
            cvchla = co_var(chla, na.rm = TRUE)) %>%
  left_join(sample_info_chla, by = c("year", "season")) %>%
  arrange(year, desc(season))
