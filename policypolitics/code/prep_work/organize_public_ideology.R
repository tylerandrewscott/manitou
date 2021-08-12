

# original source: https://americanideologyproject.com/

packages = c('data.table','tidyverse')
not_installed = packages[!packages %in% installed.packages()[,'Package']]
if(length(not_installed)>0){lapply(not_installed,install.packages)}
lapply(packages,require,character.only = T)

# using 2015 release
y2000districts = read.csv("policypolitics/raw_curated_inputs/cd_112_TW_ideology_estimates_v2.csv")
y2010districts = read.csv("policypolitics/raw_curated_inputs/cd_113_TW_ideology_estimates.csv")

# assign start and end years 
y2000districts$startYear = as.numeric(2005)
y2000districts$endYear = as.numeric(2012) 
y2010districts$startYear = as.numeric(2013) 
y2010districts$endYear = as.numeric(2018)

# expand each set so there's an observation per year
y2000districts = data.table(y2000districts)
y2000districtsExp = y2000districts[, list(cd_fips, mrp_mean, mrp_sd, abb, name, state_fips,
                                          year = seq(startYear, endYear)), 1:nrow(y2000districts)]
y2000districtsExp$nrow = NULL

y2010districts = data.table(y2010districts)
y2010districtsExp = y2010districts[, list(cd_fips, mrp_mean, mrp_sd, abb, name, state_fips,
                                          year = seq(startYear, endYear)), 1:nrow(y2010districts)]
y2010districtsExp$nrow = NULL

# rbind
cd_ideology = rbind(y2000districtsExp, y2010districtsExp)

# save
saveRDS(cd_ideology, 'policypolitics/prepped_inputs/cd_ideology.rds')


