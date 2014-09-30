###### Winter sample dates - use data from Magnuson et al
# Benson, B. and J. Magnuson. 2000, updated 2012. Global Lake and River
# Ice Phenology Database. [indicate subset used]. Boulder, Colorado USA:
# National Snow and Ice Data Center. http://dx.doi.org/10.7265/N5W66HP8.

winterdates <- read.csv("data/iceonoff.csv", stringsAsFactors = FALSE)

winterdates$iceon <- as.Date(paste(winterdates$iceon_year,
                                   winterdates$iceon_month,
                                   winterdates$iceon_day, sep = "-"),
                             format = "%Y-%m-%d")

winterdates$iceoff <- as.Date(paste(winterdates$iceoff_year,
                                   winterdates$iceoff_month,
                                   winterdates$iceoff_day, sep = "-"),
                             format = "%Y-%m-%d")

winterdates <- winterdates[, c("iceon", "iceoff", "iceon_year",
                               "iceoff_year")]


###### Summer sample dates - use July, August, September
