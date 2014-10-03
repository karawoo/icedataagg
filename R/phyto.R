library('dplyr')
library('lubridate')
library('reshape2')

source("R/datadir.R")
source("R/datesdepths.R")

names(winterdates)[names(winterdates) == "iceoff_year"] <- "year"

phyto <- read.csv(paste0(datadir, 
                  "Longterm_data/phyto/Baikal_Phyto_zeroesInc_2countRemoved_KeyUpdate_20120728.csv"), 
                  stringsAsFactors = FALSE)
names(phyto) <- tolower(names(phyto))

