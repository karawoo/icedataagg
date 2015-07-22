# Lake Baikal long-term data aggregation

This is a place for me to get long term data from Lake Baikal aggregated to
match the data template for the
[Under Ice Ecology](https://www.nceas.ucsb.edu/underice) synthesis project.
Maybe I'll make it into a package eventually. This repo doesn't contain the
actual data because unfortunately I can't share it.

See [icetest](https://github.com/karawoo/icetest) for my data validation
package.

`datadir.R` - directory location of long-term data. This is sort of awkward but
the long term data lives in a SVN repository and placing this git repository
within it just seemed too bizarre. So the value of datadir should be the
location of that SVN repository. If anyone besides me is going to run this
code then I should think of a better approach.

`co_var.R` - function to calculate coefficient of variation.

`lakemeta.R` - contains metadata about Lake Baikal along with sources for all
the info.

`stmeta.R` - contains meetadata about Station 1, the sampling station visited in
the long term sampling program.

`datesdepths.R` - uses data from the National Snow and Ice Data Center to find
freeze and thaw dates and ice duration, and uses long-term Baikal secchi data to
determine photic zone depths.

`secchi.R` - creates a data frame of average secchi depth (and coefficient of
variation) by season and average photic depth by season.

`chla.R` - aggregates chlorophyll a data by season.

`temp.R` - aggregates temperature data by season.

`zoo.R` - aggregates zooplankton data by season.

`phyto.R` - aggregates phytoplankton data by season.

`combinedata.R` - sources all of the above. Creates a master set of start/end
dates for each season and calculates the average number of sampling days.
Combines all the metadata and various types of data (secchi, chlorophyll,
temperature, zooplankton, phytoplankton).
