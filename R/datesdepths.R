# Determine sampling date ranges and depths

library('dplyr')

source("R/datadir.R")

winterdates <- read.csv("data/iceonoff.csv", stringsAsFactors = FALSE)
secchi <- read.csv(paste0(datadir, "Longterm_data/temp_chl_secchi_wind/cleaned_data/secchi_cleaned.csv"), stringsAsFactors = FALSE)
secchi$date <- as.Date(secchi$date)

####  Dates  ####

###### Winter sample dates - use data from Magnuson et al
# Benson, B. and J. Magnuson. 2000, updated 2012. Global Lake and River
# Ice Phenology Database. [indicate subset used]. Boulder, Colorado USA:
# National Snow and Ice Data Center. http://dx.doi.org/10.7265/N5W66HP8.

# paste dates together
winterdates$iceon <- as.Date(paste(winterdates$iceon_year,
                                   winterdates$iceon_month,
                                   winterdates$iceon_day, sep = "-"),
                             format = "%Y-%m-%d")

winterdates$iceoff <- as.Date(paste(winterdates$iceoff_year,
                                    winterdates$iceoff_month,
                                    winterdates$iceoff_day, sep = "-"),
                              format = "%Y-%m-%d")

# keep columns of interest
winterdates <- winterdates[, c("iceon", "iceoff", "iceon_year",
                               "iceoff_year")]


###### Summer sample dates - use July, August, September



###### Use secchi to calculate depth of photic zone

# function to calculate photic zone depth
pz <- function(secchi) {
  if (!is.numeric(secchi)) stop("secchi must be numeric")
  kd <- 1.7 / secchi
  photic <- log(1000) / kd
  photic
}


# average secchi depth in summer months -- to be used for summer dates pre-1964
summer_secchi <- mean(secchi[secchi$month %in% c(7, 8, 9), "secchi_depth"], 
                      na.rm = TRUE)

# calculate photic zone for each dates  
secchi_photic <- secchi %>% mutate(photic_zone = pz(secchi_depth))

  
  
  