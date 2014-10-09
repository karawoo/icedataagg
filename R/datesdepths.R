# Determine sampling date ranges and depths

library('dplyr')

source("R/datadir.R")

winter <- read.csv("data/iceonoff.csv", stringsAsFactors = FALSE)
secchi <- read.csv(paste0(datadir, "Longterm_data/temp_chl_secchi_wind/cleaned_data/secchi_cleaned.csv"), stringsAsFactors = FALSE)
secchi$date <- as.Date(secchi$date)

####  Dates  ####

###### Winter sample dates - use data from Magnuson et al
# Benson, B. and J. Magnuson. 2000, updated 2012. Global Lake and River
# Ice Phenology Database. [indicate subset used]. Boulder, Colorado USA:
# National Snow and Ice Data Center. http://dx.doi.org/10.7265/N5W66HP8.

# paste dates together
winter$iceon <- as.Date(paste(winter$iceon_year, 
                              winter$iceon_month, 
                              winter$iceon_day, sep = "-"), 
                        format = "%Y-%m-%d")

winter$iceoff <- as.Date(paste(winter$iceoff_year, 
                               winter$iceoff_month,
                               winter$iceoff_day, sep = "-"), 
                         format = "%Y-%m-%d")

# keep columns of interest
winter <- winter[, c("iceon", "iceoff", "iceon_year",
                               "iceoff_year")]

# add dates for 2007 and 2008
# for ice on: use first sample after the annual gap in sampling that indicates
# the period of ice formation. just find this by looking at the data. this will
# be a conservative estimate of ice-on, but since Magnuson et al. found that 
# ice-on is getting later, we don't want to use average ice-on date.
# for ice off: use average ice-off date (ice-off isn't changing).

avgiceoff <- mean(as.numeric(format(winter$iceoff, "%j")))
extrayears <- data.frame(iceon = c("2007-02-25", "2008-02-11"), 
                         iceoff = c(as.Date(avgiceoff, origin = "2007-01-01"), 
                                    as.Date(avgiceoff, origin = "2008-01-01")), 
                         iceon_year = c(2007, 2008),
                         iceoff_year = c(2007, 2008))

winterdates <- rbind(winter, extrayears)

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

# winter secchi - gotta figure out a better way than this nested loop
mult_subset <- function(x, y) {
  keeps <- c()
  for(j in seq_len(nrow(x))) {
    for (i in seq_len(nrow(y))) {
      if (x$date[j] >= y[i, "iceon"] 
          & x$date[j] <= y[i, "iceoff"]) {
        keeps <- c(keeps, j)
      }
    }
  }
  x[keeps, ]
}

winter_secchi <- mean(mult_subset(secchi, winterdates)$secchi_depth, na.rm = TRUE)

# calculate photic zone for each dates  
secchi_photic <- secchi %>% mutate(photic_zone = pz(secchi_depth))

  
  
  