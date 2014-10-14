# combine all aggregated baikal data

source("R/lakemeta.R")
source("R/stmeta.R")
source("R/datesdepths.R")
source("R/secchi.R")
source("R/chla.R") 
source("R/temp.R")
source("R/zoo.R")
source("R/phyto.R")

# colnames
library('openxlsx')
template <- read.xlsx("./data/IceDataTemplate-Beta_draft5_20141014_kw.xlsx", 
                      sheet = 1, startRow = 10, colNames = FALSE)
template_names <- template$X3

# one master set of start/end dates, number of samples, and average number of
# depths per sample. 
sample_info_cols <- c("year", "season", "avg_ndepths", "ndates", "mindate", "maxdate")

sample_replicates <- rbind_list(
  secchi_agg[, sample_info_cols], 
  chla_agg[, sample_info_cols], 
  temp_agg[, sample_info_cols], 
  zoo_agg[, sample_info_cols], 
  phyto_agg[, sample_info_cols]) %>%
  group_by(year, season) %>%
  summarize(start = min(mindate), 
            end = max(maxdate), 
            SamplePeriod.n = mean(ndates, na.rm = TRUE), 
            SamplePeriod.depths = mean(avg_ndepths, na.rm = TRUE)) %>%
  mutate(SampleStart.Day = day(start), 
         SampleStart.Mon = month(start), 
         SampleStart.Year = year(start), 
         SampleEnd.Day = day(end), 
         SampleEnd.Mon = month(end), 
         SampleEnd.Year = year(end), 
         SampleNarrat = "SamplePeriod.n and SamplePeriod.depths are averages of the number of samples and sampling depths for different measurements.") %>%
  select(-c(start, end))

# combine all the data!
alldata <- list(secchi_agg, chla_agg, temp_agg, zoo_agg) %>%
  lapply(function(x) subset(x, select = -c(avg_ndepths, ndates, mindate, maxdate))) %>%
  Reduce(function(x, y) merge(x, y, by = c("year", "season"), all = TRUE), .) %>%
  merge(sample_replicates, by = c("year", "season"), all = TRUE) %>%
  cbind(stmeta) %>%
  cb(lakemeta)






