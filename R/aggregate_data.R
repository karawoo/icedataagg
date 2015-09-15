########################################################################
####  Aggregate long term Lake Baikal data for Under Ice synthesis  ####
########################################################################

## Kara Woo
## 2015-07-23

## This script aggregates the Baikal time series into a format that matches the
## data template for the under ice project.

## Project website: https://www.nceas.ucsb.edu/node/1625

## Iceon dates are determined by the freeze/thaw dates from the National Snow
## and Ice Data Center portal. We define the summer stratified period as July,
## August, and September.

## The raw data is not publicly available, and hence is not present in this
## repository. If anyone else is to run this code and has the Subversion
## repository that contains the data, they should set the directory location of
## the repository to datadir below.

datadir <- "~/projects/baikal_svn/"

## Load the required packages:
library('dplyr')
library('lubridate')
library('reshape2')

## Load a few functions I wrote for this project:
source("funs.R")

## Load the data:
#### Load freeze/thaw date data
###### Benson, B. and J. Magnuson. 2000, updated 2012. Global Lake and River Ice
###### Phenology Database. Boulder, Colorado USA: National Snow and Ice Data
###### Center. http://dx.doi.org/10.7265/N5W66HP8.
###### Data portal: http://nsidc.org/data/lake_river_ice/freezethaw.html
###### Request data for Lake Baikal, name code NG1. Place in `data` folder.

winter <- read.csv("../data/iceonoff.csv", stringsAsFactors = FALSE)

#### Load long term secchi data

secchi <- read.csv(paste0(
            datadir,
            "Longterm_data/temp_chl_secchi_wind/cleaned_data/secchi_cleaned.csv"
          ), stringsAsFactors = FALSE)
secchi$date <- as.Date(secchi$date)     # convert date column to proper date

#### Load chlorophyll data

chla <- read.csv(paste0(
          datadir,
          "Longterm_data/temp_chl_secchi_wind/cleaned_data/chla_cleaned.csv"
        ), stringsAsFactors = FALSE)
chla$date <- as.Date(chla$date)         # convert date column to proper date
names(chla) <- tolower(names(chla))     # convert column names to lower case

#### Load temperature data

temp <- read.csv(paste0(
          datadir,
          "Longterm_data/temp_chl_secchi_wind/cleaned_data/temp_cleaned.csv"
        ), stringsAsFactors = FALSE)
temp$date <- as.Date(temp$date, format = "%Y-%m-%d") 

#### Load zooplankton data
###### zooplankton counts

zoo <- read.csv(paste0(
         datadir,
         "Longterm_data/zoo/data/zoopzeroskey_alldepths.csv"
       ), stringsAsFactors = FALSE)
names(zoo) <- tolower(names(zoo))

###### species key

key <- read.csv(paste0(
         datadir,
         "Longterm_data/zoo/data/key.csv"
       ), stringsAsFactors = FALSE)
dblcounts <- key[with(key, DoubleCount == "Y"), "KOD"]

#### Load phytoplankton data

phyto <- read.csv(paste0(
           datadir,
           "Longterm_data/phyto/Baikal_Phyto_zeroesInc_2countRemoved_KeyUpdate_20120728.csv"),
           stringsAsFactors = FALSE)
names(phyto) <- tolower(names(phyto))
phyto$date <- as.Date(phyto$date, format = "%m/%d/%Y")

#### Load data template column names
template <- read.csv("../data/IceEcologyDataTemplate_20141119.csv", 
                     stringsAsFactors = FALSE)
template_names <- template$fieldname

## Okay, data is loaded, here we go...

################################
####  Lake Baikal metadata  ####
################################

lakemeta <- data.frame(
    lakename = "Lake Baikal",
    lakeregloc = "Siberia",
    lakecountry = "Russia",
    lakearea = 31722,
    lakemeandepth = 720,
    lakemaxdepth = 1642,
    lakeelevation = 455.5,
    watershedarea = 540000,
    h2oresidence = (330 * 365.25),
    lakefetch = 636,
    stringsAsFactors = FALSE
)

### Resources

### area, max depth, elevation:
### http://users.ugent.be/~mdbatist/intas/morphometry.htm

### residence time:
### http://lin.irk.ru/grachev/eng/introduction.htm

### fetch:
### http://www.tahoebaikal.org/about/lake-tahoe-and-lake-baikal-watersheds/

### mean depth:
### Kozhov - Lake Baikal and Its Life

### watershed:
### Baikalovedeniye book

##################################
####  Metadata for Station 1  ####
##################################

## This is the sampling station visited in the long term sampling program.

stmeta <- data.frame(
    stationdistance = 2.2,
    stationdepth = 800,
    stationlat = 51.88,
    stationlong = 105.083889, 
    stationname = "Station 1"
    )

#####################################################
####  Determine sampling date ranges and depths  ####
#####################################################

## Winter sample dates:

## paste dates together into one date column
winter$iceon <- as.Date(paste(winter$iceon_year, 
                              winter$iceon_month, 
                              winter$iceon_day, sep = "-"), 
                        format = "%Y-%m-%d")

winter$iceoff <- as.Date(paste(winter$iceoff_year, 
                               winter$iceoff_month,
                               winter$iceoff_day, sep = "-"), 
                         format = "%Y-%m-%d")

## keep columns of interest
winter <- winter[, c("iceon", "iceoff", "iceon_year",
                               "iceoff_year")]

## Add dates for 2007 and 2008 (freeze/thaw data only goes to 2006).

## for ice on: use first sample after the annual gap in sampling that indicates
## the period of ice formation. just find this by looking at the data. this will
## be a conservative estimate of ice-on, but since Magnuson et al. found that 
## ice-on is getting later, we don't want to use average ice-on date.

## for ice off: use average ice-off date (ice-off isn't changing).

avgiceoff <- mean(as.numeric(format(winter$iceoff, "%j")))
extrayears <- data.frame(iceon = c("2007-02-25", "2008-02-11"), 
                         iceoff = c(as.Date(avgiceoff, origin = "2007-01-01"), 
                                    as.Date(avgiceoff, origin = "2008-01-01")), 
                         iceon_year = c(2007, 2008),
                         iceoff_year = c(2007, 2008))

## All winter dates:
winterdates <- rbind(winter, extrayears)

## All winter intervals:
winterints <- interval(winterdates$iceon, winterdates$iceoff, tz = "IRKT")

#########################################################################
####  Calculate photic zone depths and average summer/winter secchi  ####
#########################################################################

## calculate photic zone for each date
secchi_photic <- secchi %>% mutate(photic_zone = pz(secchi_depth))

## average secchi depth in summer months -- to be used later for summer dates
## pre-1964 (before the start of the secchi data)
summer_secchi <- mean(secchi[secchi$month %in% c(7, 8, 9), "secchi_depth"], 
                      na.rm = TRUE)

## average secchi depth in ice-covered period
winter_secchi <- secchi %>%
  filter(sapply(date, date_subset, winterints)) %>%
  summarize(winter = mean(secchi_depth))
winter_secchi <- winter_secchi[1, 1]  

##################################
####  Calculate ice duration  ####
##################################

## calculate ice duration for each season
ice_duration <- winter %>%
  rename(year = iceoff_year) %>%
  mutate(iceduration = iceoff - iceon) %>%
  select(-c(iceon_year, iceoff, iceon)) %>%
  group_by(year) %>%
  do(expand.grid(year = .$year, season = c("iceoff", "iceon"), 
                 iceduration = .$iceduration, stringsAsFactors = FALSE)) %>%
  mutate(iceduration = ifelse(season == "iceoff", NA, iceduration)) %>%
  arrange(year, desc(season))

#################################
####  Aggregate secchi data  ####
#################################

## Keep data from iceon period or July, August, September
secchi_dates <- data.frame(date = as.Date(unique(secchi$date))) %>%
  filter(sapply(date, date_subset, winterints) | month(date) %in% c(7, 8, 9))

## Data frame of average and CV of secchi depth by season. Also average photic
## depth.
secchi_agg <- secchi %>%
  do(na.omit(.)) %>%
  semi_join(secchi_dates, by = "date") %>%
  mutate(season = ifelse(month(date) %in% c(7, 8, 9), "iceoff", "iceon")) %>%
  mutate(photic_zone = pz(secchi_depth)) %>%
  group_by(year, season) %>%
  summarize(avesecchidepth = mean(secchi_depth, na.rm = TRUE), 
            cvsecchidepth = co_var(secchi_depth, na.rm = TRUE),
            photicdepth = mean(photic_zone, na.rm = TRUE), 
            ndates = length(unique(date)), 
            mindate = min(date), 
            maxdate = max(date)) %>%
  arrange(year, desc(season))

######################################
####  Aggregate chlorophyll data  ####
######################################

## Keep data from iceon period or July, August, September 
chla_dates <- data.frame(date = as.Date(unique(chla$date))) %>%
  filter(sapply(date, date_subset, winterints) | month(date) %in% c(7, 8, 9))

## subset data by date and depth
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

## Include sample info - number of dates and start/end date (to be used later
## when calculating periodn and overall start/end date)
sample_info_chla <- chla_sml %>%
  group_by(year, season) %>%
  summarize(ndates = length(unique(date)), 
            mindate = min(date), 
            maxdate = max(date))

## aggregate data by season
chla_agg <- chla_sml %>%
  group_by(year, season) %>%
  summarize(avechla = mean(chla, na.rm = TRUE), 
            maxchla = max(chla, na.rm = TRUE),
            cvchla = co_var(chla, na.rm = TRUE)) %>%
  left_join(sample_info_chla, by = c("year", "season")) %>%
  arrange(year, desc(season))

######################################
####  Aggregate temperature data  ####
######################################

## select correct dates
temp_dates <- data.frame(date = as.Date(unique(temp$date))) %>%
  filter(sapply(date, date_subset, winterints) | month(date) %in% c(7, 8, 9))

## subset data by date and depth
temp_sml <- temp %>%
  do(na.omit(.)) %>%
  semi_join(temp_dates, by = "date") %>%
  mutate(year = year(date), 
         season = ifelse(month(date) %in% c(7, 8, 9), "iceoff", "iceon")) %>%
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

## extract start/end dates and number of dates, to be used later when
## calculating overall start/end and periodn
sample_info_temp <- temp_sml %>%
  group_by(year, season) %>%
  summarize(ndates = length(unique(date)), 
            mindate = min(date), 
            maxdate = max(date))

## aggregate data by season
temp_agg <- temp_sml %>%
  group_by(year, season) %>%
  summarize(watertemp = mean(temp)) %>%
  left_join(sample_info_temp, by = c("year", "season")) %>%
  arrange(year, desc(season))

######################################
####  Aggregate zooplankton data  ####
######################################

## dates to use for ice-covered and stratified periods
dates_zoo <- data.frame(date = as.Date(unique(zoo$date))) %>%
  filter(sapply(date, date_subset, winterints) | month(date) %in% c(7, 8, 9))

## Aggregate ALL the data!
## - select only alive and non-double-count data; 
## - remove taxa that are counted too inconsistently to use (harpacticoid, 
## bdelloid, ciliate, cyst, various protist);
## - rename groups based on the categories that we ultimately want in the data; 
## - create "year" column;
## - remove any rows with NA in date;
## - subset based on dates that are either during ice cover or during 
## stratification;
## - add "season" column;
## - merge with secchi data; 
## - insert values when secchi is missing (see comments within the code); 
## - calculate photic zone;
## - subset to keep rows where lower layer is <= photic zone;
## - convert count units to individuals/liter

## Important note: any grouping by year and season will not behave as expected
## if there are cases where ice-on was early and sampling occurred in 
## December. For example, 1989-12-31 will be grouped with the previous ice-on
## season rather than the one ending in spring of 1990. In this data it doesn't
## appear that there are any such cases, however when I have time to come back
## to this I should do it in a way that's robust to this issue.

zoo_sml <- zoo %>% 
  select(-genus, -species, -endemic_cosmo, -lifestage_gen, 
         -lifestage_num, -lifestage_cop, -gender) %>%
  filter(status != "dead" & !kod %in% dblcounts) %>%
  filter(!group_fine %in% c("Ciliate", "Harpacticoid", "Bdelloid",
                            "Various protist", "Cyst")) %>%
  mutate(group_new = rename_grp(group_fine)) %>%
  select(-group_general, -group_fine, -status) %>%
  mutate(year = as.integer(substring(date, 1, 4)), date = as.Date(date)) %>%
  filter(!is.na(date)) %>%
  semi_join(dates_zoo, by = "date") %>%
  mutate(season = ifelse(month(date) %in% c(7, 8, 9), "iceoff", "iceon")) %>%
  left_join(secchi[, c("date", "secchi_depth")], by = "date") %>%
  ## add missing secchi according to the following rules:
  ## 1) if missing values are before the start of secchi monitoring, use average
  ## winter or summer secchi from the whole time series
  ## 2) if missing values are within a year that has other secchi measurements,
  ## use the average secchi from that season
  ## 3) if missing values are within the time series but there were no other 
  ## secchi measurements in that year, use the average winter or summer secchi
  ## from the whole time series
  mutate(secchi_depth = ifelse(is.na(secchi_depth) & season == "iceon"
                               & date <= as.Date("1964-01-17"), winter_secchi,
                               ifelse(is.na(secchi_depth) & season == "iceoff"
                                      & date <= as.Date("1964-01-17"), 
                                      summer_secchi, secchi_depth))) %>%
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
  filter(nig_gr <= photic_zone) %>%
  mutate(count_liter = m2_to_l(count, interval = nig_gr - ver_gr))

## calculate total individuals per liter in each sample, then average
## individuals per liter in each season
totzoopcount <- zoo_sml %>%
  group_by(year, date, season, ver_gr, nig_gr) %>%
  summarize(totzoop = sum(count_liter)) %>%
  group_by(year, season) %>%
  summarize(avezoopcount = mean(totzoop, na.rm = TRUE),
            maxzoopcount = max(totzoop, na.rm = TRUE),
            cvzoopcount = co_var(totzoop, na.rm = TRUE))
  
## calculate percentages of different taxa
zoopperc <- zoo_sml %>%
  group_by(year, season, group_new) %>%
  summarize(count_total = sum(count_liter)) %>% 
  dcast(., year + season ~ group_new, fun.aggregate = sum, 
        value.var = "count_total") %>%
  rowwise %>%
  mutate(total = sum(calanoid, cladoceran, cyclopoid, daphnia, rotifer)) %>%
  mutate(propdaphnia = daphnia / total, 
         propothercladoc = cladoceran / total,
         propcyclopoid = cyclopoid / total, 
         propcalanoid = calanoid / total, 
         proprotifer = rotifer / total) %>%
  select(-calanoid, -cladoceran, -cyclopoid, -daphnia, -rotifer, -total)

## calculate start and end dates and number of dates of sampling
sample_info_zoo <- zoo_sml %>%
  group_by(year, season) %>%
  summarize(ndates = length(unique(date)), 
            mindate = min(date), 
            maxdate = max(date))

## combine all
zoo_agg <- totzoopcount %>%
  left_join(zoopperc, by = c("year", "season")) %>%
  left_join(sample_info_zoo, by = c("year", "season")) %>%
  arrange(year, desc(season))

########################################
####  Aggregate phytoplankton data  ####
########################################

## start in 1974 due to the switch from formalin to Lugol's
phyto_post <- filter(phyto, date >= "1974-01-01")

## dates to use
dates_phyto <- data.frame(date = as.Date(unique(phyto_post$date))) %>%
  filter(sapply(date, date_subset, winterints) | month(date) %in% c(7, 8, 9))

## - convert counts * 1000 / liter to counts/liter;
## - rename groups to match ice template;
## - remove rows with NA in date;
## - subset based on dates that are either during ice cover or during 
## stratification;
## - add "season" column; 
## - merge with secchi data; 
## - insert values when secchi is missing (see comments within the code); 
## - calculate photic zone;
## - subset observations within photic zone

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

## calculate total individuals per liter in each sample, then average
## individuals per liter in each season
totphytocount <- phyto_sml %>%
  group_by(year, date, season, depth) %>%
  summarize(totphyto = sum(count_l)) %>%
  group_by(year, season) %>%
  summarize(avephytocount = mean(totphyto, na.rm = TRUE), 
            maxphytocount = max(totphyto, na.rm = TRUE),
            cvphytocount = co_var(totphyto, na.rm = TRUE))

## calculate percentages of different taxa
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

## calculate start and end dates and number of dates
sample_info_phyto <- phyto_sml %>%
  group_by(year, season) %>%
  summarize(ndates = length(unique(date)), 
            mindate = min(date), 
            maxdate = max(date))

## combine all
phyto_agg <- totphytocount %>%
  left_join(phytoperc, by = c("year", "season")) %>%
  left_join(sample_info_phyto, by = c("year", "season")) %>%
  arrange(year, desc(season))

  

##############################################
####  Combine all aggregated Baikal data  ####
##############################################

## Create one master set of start/end dates from the individual start/end dates
## present in the various aggregations. For each type of data I calculated the
## start/end date; this take the earliest start and the latest end to get the
## appropriate range for each season. Average the ndates columns to get periodn.
sample_info_cols <- c("year", "season", "ndates", "mindate", "maxdate")

sample_replicates <- rbind_list(
  secchi_agg[, sample_info_cols], 
  chla_agg[, sample_info_cols], 
  temp_agg[, sample_info_cols], 
  zoo_agg[, sample_info_cols], 
  phyto_agg[, sample_info_cols]) %>%
  group_by(year, season) %>%
  summarize(start = min(mindate), 
            end = max(maxdate), 
            periodn = mean(ndates, na.rm = TRUE)) %>%
  mutate(startday = day(start), 
         startmonth = month(start, label = TRUE, abbr = TRUE), 
         startyear = year(start), 
         endday = day(end), 
         endmonth = month(end, label = TRUE, abbr = TRUE), 
         endyear = year(end), 
         samplenarrat = "peiodn is the average of the number of sampling dates for different measurements.") %>%
  select(-c(start, end))

## combine all the data!
alldata <- list(secchi_agg, chla_agg, temp_agg, zoo_agg, phyto_agg) %>%
  lapply(function(x) subset(x, select = -c(ndates, mindate, maxdate))) %>%
  Reduce(function(x, y) merge(x, y, by = c("year", "season"), all = TRUE), .) %>%
  merge(sample_replicates, by = c("year", "season"), all = TRUE) %>%
  left_join(ice_duration[, c("year", "season", "iceduration")],
                         by = c("year", "season")) %>%
  ## add station and lake metadata:
  cbind(stmeta) %>%
  cbind(lakemeta) %>%
  ## add missing columns:
  do(cbind(., data.frame(
    matrix(
      NA, 
      nrow = 1, 
      ncol = length(template_names[which(!template_names %in% names(.))]), 
      dimnames = list(c(), template_names[which(!template_names
                                                %in% names(.))])), 
    stringsAsFactors = FALSE)
  )) %>%
  ## fill in a few fields
  mutate(multiplestations = "no", sampletype = "in situ", 
         sidata = "no", fadata = "no", gutdata = "no", 
         icedepth = ifelse(season == "iceoff", 0, NA), 
         snowdepth = ifelse(season == "iceoff", 0, NA),
         researcher = "Kara.Woo") %>%
  ## reorder columns to match template
  do(.[, template_names]) %>%
  ## arrange by year and season 
  arrange(year, desc(season))
 
## Export data (horizontal and vertical)

exportdate <- Sys.Date()
write.csv(alldata,
          paste0("../data/baikal_long_", exportdate, ".csv"),
          row.names = FALSE)
write.csv(t(alldata),
          paste0("../data/baikal_agg_", exportdate, ".csv"),
          row.names = FALSE)



