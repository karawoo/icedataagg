library('dplyr')
library('lubridate')
library('reshape2')

source("datadir.R")
source("datesdepths.R")
source("co_var.R")

phyto <- read.csv(paste0(datadir, 
                  "Longterm_data/phyto/Baikal_Phyto_zeroesInc_2countRemoved_KeyUpdate_20120728.csv"), 
                  stringsAsFactors = FALSE)
names(phyto) <- tolower(names(phyto))

# convert date to actual date
phyto$date <- as.Date(phyto$date, format = "%m/%d/%Y")

# start in 1974 due to the switch from formalin to Lugol's
phyto_post <- filter(phyto, date >= "1974-01-01")

# rename groups according to ice workshop template
rename_phyto <- function(x) {
  result <- x %>%
    gsub("Green", "chloro", .) %>% 
    gsub("Diatom", "diat", .) %>%
    tolower(.)
  
  result <- ifelse(!result %in% c("chloro", "crypto", "cyano", "diat", "dino"), 
           "otherphyto", result)
  result
}

# dates to use
dates_phyto <- data.frame(date = as.Date(unique(phyto_post$date))) %>%
  filter(sapply(date, date_subset, winterints) | month(date) %in% c(7, 8, 9))

# - convert counts * 1000 / liter to counts/liter;
# - rename groups to match ice template;
# - remove rows with NA in date;
# - subset based on dates that are either during ice cover or during 
# stratification;
# - add "season" column; 
# - merge with secchi data; 
# - insert values when secchi is missing (see comments within the code); 
# - calculate photic zone;
# - subset observations within photic zone

phyto_sml <- phyto_post %>%
  mutate(count_l = density * 1000) %>%
  mutate(group_new = rename_phyto(group)) %>%
  filter(!is.na(date)) %>%
  semi_join(dates_phyto, by = "date") %>%
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
  ungroup() %>%
  filter(depth <= photic_zone)

# calculate total individuals per liter in each sample, then average
# individuals per liter in each season
totphytocount <- phyto_sml %>%
  group_by(year, date, season, depth) %>%
  summarize(totphyto = sum(count_l)) %>%
  group_by(year, season) %>%
  summarize(avephytocount = mean(totphyto, na.rm = TRUE), 
            maxphytocount = max(totphyto, na.rm = TRUE),
            cvphytocount = co_var(totphyto, na.rm = TRUE))

# calculate percentages of different taxa
phytoperc <- phyto_sml %>%
  group_by(year, season, group_new) %>%
  summarize(count_total = sum(count_l)) %>% 
  dcast(., year + season ~ group_new, fun.aggregate = sum, 
        value.var = "count_total") %>%
  rowwise %>%
  mutate(total = sum(chloro, crypto, cyano, diat, dino, otherphyto)) %>%
  mutate(propchloro = chloro / total, 
         propcrypto = crypto / total,
         propcyano = cyano / total, 
         propdiatom = diat / total, 
         propdino = dino / total,
         propotherphyto = otherphyto / total) %>%
  select(-c(chloro, crypto, cyano, diat, dino, otherphyto, total))

# calculate start and end dates and number of samples and depths
sample_info_phyto <- phyto_sml %>%
  group_by(year, season) %>%
  summarize(ndates = length(unique(date)), 
            mindate = min(date), 
            maxdate = max(date))

# combine all
phyto_agg <- totphytocount %>%
  left_join(phytoperc, by = c("year", "season")) %>%
  left_join(sample_info_phyto, by = c("year", "season")) %>%
  arrange(year, desc(season))

  

