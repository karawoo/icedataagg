#####################
####  Functions  ####
#####################

## Functions written to facilitate aggregating long-term Lake Baikal data for
## inclusion in the Ecology Under Lake Ice synthesis.
## 
## Kara Woo
## 
## 2015-07-23

## Calculate photic depth based on secchi
## See Hampton et al. 2014
## http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0088920
## DOI: 10.1371/journal.pone.0088920
pz <- function(secchi) {
  if (!is.numeric(secchi)) stop("secchi must be numeric")
  kd <- 1.7 / secchi
  photic <- log(1000) / kd
  photic
}

## Subset dates
date_subset <- function(x, intervals) {
    any(x %within% intervals)
}

## Calculate coefficient of variation
co_var <- function(x, na.rm = TRUE) {
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

## Rename zooplankton groups so that names in the data match names in the data
## template
rename_grp <- function(group_fine) {
  require("dplyr")
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

## Convert units of zooplankton from individuals/m2 to individuals/liter
m2_to_l <- function(x, interval) {
  stopifnot(is.numeric(x))
  # takes units in 1000 individuals/m2 and converts to individuals per liter
  individuals <- x * 1000 # convert to individuals/m2
  count_per_liter <- individuals / (interval * 1000) # convert to indiv./liter
  return(count_per_liter)
}

## Rename phytoplankton groups so that names in the data match names in the data
## template
rename_phyto <- function(x) {
  require("dplyr")
  result <- x %>%
    gsub("Green", "chloro", .) %>% 
    gsub("Diatom", "diat", .) %>%
    tolower(.)
  
  result <- ifelse(!result %in% c("chloro", "crypto", "cyano", "diat", "dino"), 
           "otherphyto", result)
  result
}

