library('lubridate')
library('dplyr')

source("R/datadir.R")
source("R/depths.R")
source("R/dates.R")

chla <- read.csv(paste0(datadir, "Longterm_data/temp_chl_secchi_wind/cleaned_data/chla_cleaned.csv"), stringsAsFactors = FALSE)
chla$date <- as.Date(chla$date)

# merge chlorophyll data with winterdates - note there's no iceon/iceoff data
# for 2007
chla_dates <- merge(chla, winterdates, by.x = "Year", by.y = "iceoff_year",
                    all.x = TRUE, all.y = FALSE)

# merge with secchi/photic info
chla_secchi <- merge(chla_dates, 
                     secchi_photic[, c("date", "secchi_depth", "photic_zone")], 
                     by = "date", all.x = TRUE, all.y = FALSE)

# aggregate: remove rows with NAs; remove iceon_year column; choose data 
# during ice-covered period or during july, august, september; choose data
# from depths within the photic zone; add season column; for each season
# summarize start date, end date, min depth, max depth, mean chla, mean secchi,
# and mean photic zone depth

chla_agg <- chla_secchi %>%
  do(na.omit(.)) %>%
  select(-iceon_year) %>%
  filter(iceon <= date & date <= iceoff | month(date) %in% c(7, 8, 9)) %>%
  filter(depth <= photic_zone) %>%
  mutate(season = ifelse(month(date) %in% c(7, 8, 9), "summer", "winter")) %>%
  group_by(Year, season) %>%
  summarize(start = min(date), 
            end = max(date), 
            mindepth = min(depth), 
            maxdepth = max(depth), 
            meanchla = mean(chla), 
            meansecchi = mean(secchi_depth), 
            meanphotic = mean(photic_zone))

