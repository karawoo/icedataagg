library('dplyr')
library('lubridate')
library('reshape2')

source("R/datadir.R")
source("R/datesdepths.R")

zoo <- read.csv(paste0(datadir, "Longterm_data/zoo/data/zoopzeroskey_alldepths.csv"), 
                stringsAsFactors = FALSE)
names(zoo) <- tolower(names(zoo))

key <- read.csv(paste0(datadir, "Longterm_data/zoo/data/key.csv"),
                stringsAsFactors = FALSE)
dblcounts <- key[with(key, DoubleCount == "Y"), "KOD"]

# groups needed for aggregation: Daphnia, other cladoceran, cylopoids,
# calanoid copepods, rotifers, other
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

# function to convert units of zooplankton to individuals/liter
m2_to_l <- function(x, interval) {
  stopifnot(is.numeric(x))
  # takes units in 1000 individuals/m2 and converts to individuals per liter
  individuals <- x * 1000 # convert to individuals/m2
  count_per_liter <- individuals / (interval * 1000) # convert to indiv./liter
  return(count_per_liter)
}

# select only alive and non-double-count data; 
# remove taxa that are counted too inconsistently to use (harpacticoid, 
# bdelloid, ciliate, cyst, various protist);
# rename groups based on the categories that we ultimately want in the data; 
# create "year" column;
# merge with dates and depth data (this only keeps rows that are present in the
# other data sets, so any times when they sampled zooplankton but not secchi
# are lost -- need to talk to steph about whether this is what we want or if 
# we want to interpolate photic zones so we can use all the zoop data); 
# subset data by date ranges and depths; 
# convert count units to individuals/liter; 
# add season column;
# discard now-unnecessary columns;
# figure out a way to get sums and percentages - try casting to wide first so
# each group has its own column, then sum, then calculate percentages based
# on that sum?
zoo_sml <- zoo %>% 
  select(-genus, -species, -endemic_cosmo, -lifestage_gen, 
         -lifestage_num, -lifestage_cop, -gender) %>%
  filter(status != "dead" & !kod %in% dblcounts) %>%
  filter(!group_fine %in% c("Ciliate", "Harpacticoid", "Bdelloid",
                            "Various protist", "Cyst")) %>%
  mutate(group_new = rename_grp(group_fine)) %>%
  select(-group_general, -group_fine, -status) %>%
  mutate(year = as.integer(substring(date, 1, 4)), date = as.Date(date)) %>%
  merge(winterdates, by.x = "year", by.y = "iceoff_year") %>%
  merge(secchi_photic, by = "date", all.x = TRUE, all.y = TRUE) %>%
  filter(iceon <= date & date <= iceoff | month(date) %in% c(7, 8, 9))
#   filter(nig_gr <= photic_zone) %>%
#   mutate(count_liter = m2_to_l(count, interval = nig_gr - ver_gr)) %>%
#   mutate(season = ifelse(month(date) %in% c(7, 8, 9), "summer", "winter")) %>%
#   select(-year.y, -iceon_year, -count, -iceon, -iceoff, -secchi_depth) %>%
#   group_by(year.x, season, group_new) %>%
#   summarize(count_total = sum(count_liter) )%>% 
#   dcast(., year.x + season ~ group_new, fun.aggregate = sum, 
#         value.var = "count_total")
