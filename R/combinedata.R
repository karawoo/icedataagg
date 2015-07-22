# combine all aggregated baikal data

## Individual scripts for various sources/types of data (see readme for more
## detailed description of each)

source("co_var.R")
source("lakemeta.R")
source("stmeta.R")
source("datesdepths.R")
source("secchi.R")
source("chla.R") 
source("temp.R")
source("zoo.R")
source("phyto.R")

## Load template column names
template <- read.csv("../data/IceEcologyDataTemplate_20141119.csv", 
                     stringsAsFactors = FALSE)
template_names <- template$fieldname

# one master set of start/end dates, number of samples, and average number of
# depths per sample. 
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

# combine all the data!
alldata <- list(secchi_agg, chla_agg, temp_agg, zoo_agg, phyto_agg) %>%
  lapply(function(x) subset(x, select = -c(ndates, mindate, maxdate))) %>%
  Reduce(function(x, y) merge(x, y, by = c("year", "season"), all = TRUE), .) %>%
  merge(sample_replicates, by = c("year", "season"), all = TRUE) %>%
  left_join(ice_duration[, c("year", "season", "iceduration")],
                         by = c("year", "season")) %>%
  # add station and lake metadata:
  cbind(stmeta) %>%
  cbind(lakemeta) %>%
    # add missing columns:
  do(cbind(., data.frame(
    matrix(
      NA, 
      nrow = 1, 
      ncol = length(template_names[which(!template_names %in% names(.))]), 
      dimnames = list(c(), template_names[which(!template_names
                                                %in% names(.))])), 
    stringsAsFactors = FALSE)
  )) %>%
  # fill in a few fields
  mutate(multiplestations = "no", sampletype = "in situ", 
         sidata = "no", fadata = "no", gutdata = "no", 
         icedepth = ifelse(season == "iceoff", 0, NA), 
         snowdepth = ifelse(season == "iceoff", 0, NA),
         researcher = "Kara.Woo") %>%
  # reorder columns to match template
  do(.[, template_names]) %>%
  # arrange by year and season 
  arrange(year, desc(season))
 
#  
#  write.csv(alldata, "./data/baikal_long_20141119.csv", row.names = FALSE)
#  write.csv(t(alldata), "./data/baikal_agg_20141119.csv")
# 


