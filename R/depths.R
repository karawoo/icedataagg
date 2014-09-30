###### Use secchi to calculate depth of photic zone

library('dplyr')

source("datadir.R")

secchi <- read.csv(paste0(datadir, "Longterm_data/temp_chl_secchi_wind/cleaned_data/secchi_cleaned.csv"), stringsAsFactors = FALSE)

pz <- function(secchi) {
    if (!is.numeric(secchi)) stop("secchi must be numeric")
    kd <- 1.7 / secchi
    photic <- log(1000) / kd
    photic
}

secchi_photic <- secchi %>% mutate(photic_zone = pz(secchi_depth))
