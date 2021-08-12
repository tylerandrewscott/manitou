
#  original source: https://scorecard.lcv.org/

packages = c('data.table','tidyverse','readxl')
not_installed = packages[!packages %in% installed.packages()[,'Package']]
if(length(not_installed)>0){lapply(not_installed,install.packages)}
lapply(packages,require,character.only = T)

statesFile = read_xls("policypolitics/raw_curated_inputs/states.xls")
class(statesFile$STATEFP)

# attribute data (yaml data originally sourced from: https://theunitedstates.io/congress-legislators/legislators-historical.yaml)
legs = readRDS("policypolitics/raw_curated_inputs/leg-atts_2004-2018.rds")

# lcv data
dir = list.files("policypolitics/raw_curated_inputs/lcv", pattern="*.csv", full.names=T)
readFiles = lapply(dir, read.csv)
lcv = plyr::ldply(readFiles, rbind)
unique(lcv$year)

# create state and district variable
abb = statesFile$abb
lcv$state = str_match(lcv$district, paste(abb, collapse = "|"))
colnames(lcv)[1] = "stateDistrict"
lcv$stateDistrict = gsub("-AL", "-01", lcv$stateDistrict)
lcv$district = str_match(lcv$stateDistrict, "[0-9][0-9]")
lcv$district = as.numeric(as.character(lcv$district))

# at-large districts are 0 in legs and 1 in lcv; change to 1 in legs
legs$district[legs$district==0] = 1

# reps only
legs$chamber = as.character(legs$chamber)
house = subset(legs, chamber=="rep") 

# check
dplyr::count(house, year)
dplyr::count(lcv, year)
  # slightly different counts, likely due to within-term changes that were counted differently in two sources

lcv$party = NULL
lcv$chamber = NULL
house$district = as.numeric(as.character(house$district))
lcv$state = as.character(lcv$state)
house$state = as.character(house$state)
house$lastName = as.character(house$lastName)

# lcv records data for all legislators, including special elections - create average of members who served together in a single year
lcv$annual_score = as.numeric(lcv$annual_score)
lcv = data.table(lcv)
lcv$stateDistrictYear = paste0(lcv$stateDistrict,"-",lcv$year)
dupsLCV = lcv[duplicated(paste0(year,state, district)),] # 32 state-districts duplicated
dupsList = dupsLCV$stateDistrictYear
dups = lcv[lcv$stateDistrictYear %in% dupsList,]
dupsAve = aggregate(dups$annual_score, by=list(dups$stateDistrictYear), FUN = mean, na.rm=TRUE) 
colnames(dupsAve) = c("stateDistrictYear", "lcv_annual_ave")
dups = merge(dups, dupsAve, by = "stateDistrictYear")
dups$member = as.character("special election - shared seat") # changing name so they don't appear mismatched
dups$annual_score = dups$lcv_annual_ave
dups$lcv_annual_ave = NULL
dups$lifetime_score = NULL
dups = unique(dups)

# replace these values
lcv= lcv[!lcv$stateDistrictYear %in% dupsList,]
lcv$lifetime_score = NULL
lcv = rbind(lcv, dups)
lcv[duplicated(paste0(year,state, district)),] # no duplicates
lcv$stateDistrictYear = NULL

# merge with yaml data
houseAtts  = merge(house, lcv, by=c("year", "state", "district"), all=T)
subset(houseAtts, member=="special election - shared seat") # checked

# fill in basic data for long serving legislators excluded in yaml data extraction
houseAtts$firstName[houseAtts$member=="Dingell, John"] = "John"
houseAtts$lastName[houseAtts$member=="Dingell, John"] = "Dingell"
houseAtts$chamber[houseAtts$member=="Dingell, John"] = "rep"
houseAtts$party[houseAtts$member=="Dingell, John"] = "Democrat"

houseAtts$firstName[houseAtts$member=="Conyers, John"] = "John"
houseAtts$lastName[houseAtts$member=="Conyers, John"] = "Conyers"
houseAtts$chamber[houseAtts$member=="Conyers, John"] = "rep"
houseAtts$party[houseAtts$member=="Conyers, John"] = "Democrat"

houseAtts$firstName[houseAtts$member=="Rangel, Charles"] = "Charles"
houseAtts$lastName[houseAtts$member=="Rangel, Charles"] = "Rangel"
houseAtts$chamber[houseAtts$member=="Rangel, Charles"] = "rep"
houseAtts$party[houseAtts$member=="Rangel, Charles"] = "Democrat"

houseAtts$firstName[houseAtts$member=="Young, Don"] = "Don"
houseAtts$lastName[houseAtts$member=="Young, Don"] = "Young"
houseAtts$chamber[houseAtts$member=="Young, Don"] = "rep"
houseAtts$party[houseAtts$member=="Young, Don"] = "Democrat"

houseAtts$firstName[houseAtts$member=="Obey, David"] = "David"
houseAtts$lastName[houseAtts$member=="Obey, David"] = "Obey"
houseAtts$chamber[houseAtts$member=="Obey, David"] = "rep"
houseAtts$party[houseAtts$member=="Obey, David"] = "Democrat"

houseAtts$firstName[houseAtts$member=="Young, C.W. Bill"] = "Bill"
houseAtts$lastName[houseAtts$member=="Young, C.W. Bill"] = "Young"
houseAtts$chamber[houseAtts$member=="Young, C.W. Bill"] = "rep"
houseAtts$party[houseAtts$member=="Young, C.W. Bill"] = "Republican"

# create congressional ID (state-district)
houseAtts$abb = houseAtts$state
houseAtts$state = NULL
houseAtts = merge(houseAtts, statesFile, by=c("abb"), all=T)
houseAtts$fips = NULL
houseAtts$district = formatC(houseAtts$district,width=2,flag = 0)
houseAtts$Congressional_District_ID = as.character(paste0(houseAtts$STATEFP, houseAtts$district))

names(houseAtts)[9] = "fullName"
names(houseAtts)[10] = "LCV_annual"
names(houseAtts)
houseAtts$LCV_annual = as.numeric(as.character(houseAtts$LCV_annual))

# drop cases irrelevant to analysis
drop = c("AS", "DC", "GU", "MP", "PR", "VI")
houseAtts = subset(houseAtts, !grepl(paste(drop, collapse="|"), houseAtts$abb), drop=T)

# add demPres
unique(houseAtts$year)
houseAtts$demPres = as.integer(NA)
houseAtts$demPres[houseAtts$year<2009] = 0
houseAtts$demPres[houseAtts$year>2008 & houseAtts$year<2017] = 1
houseAtts$demPres[houseAtts$year>2016] = 0
table(houseAtts$year, houseAtts$demPres)

# add demCongress
houseAtts$demCongress = as.integer(NA)
houseAtts$demCongress[houseAtts$year<2007] = 0
houseAtts$demCongress[houseAtts$year>2006 & houseAtts$year<2011] = 1
houseAtts$demCongress[houseAtts$year>2010 & houseAtts$year<2015] = 2
houseAtts$demCongress[houseAtts$year>2014] = 0
table(houseAtts$year, houseAtts$demCongress)

# add congress
houseAtts$congress = as.integer(NA)
houseAtts$congress[houseAtts$year==2004] = 108
houseAtts$congress[houseAtts$year==2005] = 109
houseAtts$congress[houseAtts$year==2006] = 109
houseAtts$congress[houseAtts$year==2007] = 110
houseAtts$congress[houseAtts$year==2008] = 110
houseAtts$congress[houseAtts$year==2009] = 111
houseAtts$congress[houseAtts$year==2010] = 111
houseAtts$congress[houseAtts$year==2011] = 112
houseAtts$congress[houseAtts$year==2012] = 112
houseAtts$congress[houseAtts$year==2013] = 113
houseAtts$congress[houseAtts$year==2014] = 113
houseAtts$congress[houseAtts$year==2015] = 114
houseAtts$congress[houseAtts$year==2016] = 114
houseAtts$congress[houseAtts$year==2017] = 115
houseAtts$congress[houseAtts$year==2018] = 115
table(houseAtts$year, houseAtts$congress)

# check
missingAtts = subset(houseAtts, is.na(lastName)) # 23 obs, filled in below
missingLCV = subset(houseAtts, is.na(LCV_annual)) # 25 obs - the remainder are speakers of the House, which are not assigned an LCV score, N/As stay

# replace missing attributes (except party)
houseAtts = subset(houseAtts, !is.na(lastName))
missingAtts$chamber = "rep"
missingAtts$lastName = str_extract(missingAtts$fullName, "\\s*.*,")
missingAtts$lastName = trimws(gsub(",", "", missingAtts$lastName))
missingAtts$firstName = str_extract(missingAtts$fullName, ",\\s*.*")
missingAtts$firstName = trimws(gsub(", ", "", missingAtts$firstName))
houseAtts = rbind(houseAtts, missingAtts)
subset(houseAtts, is.na(lastName)) # none missing

# save
saveRDS(houseAtts,'policypolitics/prepped_inputs/houseAtts.rds')



