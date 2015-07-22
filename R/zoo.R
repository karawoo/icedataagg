######################################
####  Aggregate zooplankton data  ####
######################################

library('dplyr')
library('lubridate')
library('reshape2')

source("datadir.R")
source("datesdepths.R")
source("co_var.R")

## load data
zoo <- read.csv(paste0(datadir, "Longterm_data/zoo/data/zoopzeroskey_alldepths.csv"), 
                stringsAsFactors = FALSE)
names(zoo) <- tolower(names(zoo))

## load species key
key <- read.csv(paste0(datadir, "Longterm_data/zoo/data/key.csv"),
                stringsAsFactors = FALSE)
dblcounts <- key[with(key, DoubleCount == "Y"), "KOD"]

## groups needed for aggregation into the template: Daphnia, other cladoceran,
## cylopoids, calanoid copepods, rotifers, other
rename_grp <- function(group_fine) {
  group_fine %>%
    gsub("Calanoid Copepod", "calanoid", .) %>%
    gsub("Cyclopoid Copepod", "cyclopoid", .) %>%
    gsub("Daphnia", "daphnia", .) %>%
    gsub("Ceriodaphnia", "daphnia", .) %>%
    gsub("Cladoceran", "cladoceran", .) %>%
    gsub("Bosmina", "cladoceran", .) %>%
    gsub("Loricate rotifer", "rotifer", .) %>%
    gsub("Illoricate rotifer", "rotifer", .)
}

## function to convert units of zooplankton to individuals/liter
m2_to_l <- function(x, interval) {
  stopifnot(is.numeric(x))
  # takes units in 1000 individuals/m2 and converts to individuals per liter
  individuals <- x * 1000 # convert to individuals/m2
  count_per_liter <- individuals / (interval * 1000) # convert to indiv./liter
  return(count_per_liter)
}

## dates to use for ice-covered and stratified periods
dates_to_use <- data.frame(date = as.Date(unique(zoo$date))) %>%
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
  semi_join(dates_to_use, by = "date") %>%
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

