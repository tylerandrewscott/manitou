
packages = c('data.table','stringr','tidyverse','sf','lwgeom','ggthemes','lubridate','pbapply','parallel','zoo','readxl')
not_installed = packages[!packages %in% installed.packages()[,'Package']]
if(length(not_installed)>0){lapply(not_installed,install.packages)}
lapply(packages,require,character.only = T)
td = tempdir()
cores = detectCores()-2
first_year = 2000;last_year = 2020
# projection to use for all spatial data
albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
admin_districts <- readRDS('policypolitics/prepped/admin_units_clean.RDS')
admin_districts <- st_transform(admin_districts,crs = st_crs(albersNA))

# create a temporary folder that stores downloaded shapefile, extract contents and load
admin_districts$FORESTORGC = as.character(admin_districts$FORESTORGC)
admin_districts$FOREST_ID = admin_districts$FORESTORGC
admin_districts$FOREST_ID = formatC(admin_districts$FORESTORGC,width=4,flag = 0)
#admin_districts$FOREST_ID = formatC(admin_districts$FORESTORGC,width=4,flag = 0)
congress_ids = data.table(congress = rep(101:116,each=2),CALENDAR_YEAR = 1989:2020)
# make every forest/year combination
tdt = expand.grid(sort(unique(as.character(admin_districts$FOREST_ID))),first_year:last_year)
tdt = data.table(tdt)
names(tdt)<- c('FOREST_ID',"CALENDAR_YEAR")
temp_dt = tdt
#create forest/year/congress table
temp_dt = data.table(left_join(temp_dt,congress_ids))


### this loads other land units that aren't primary administrative units
# lmu_url = "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.temp_dtSLandUnit.zip"
# tf = tempfile(tmpdir=td, fileext=".zip")
# download.file(lmu_url, tf)
# fname = unzip(tf, list=TRUE)
# unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
# fpath = file.path(td, grep('shp$',fname$Name,value=T))
# lmus <- st_read(fpath)
# lmus <- st_transform(lmus,crs = st_crs(albersNA))
# lmus <- st_make_valid(lmus)

####  forest x county overlap values


#fs_county_url = 'http://enterprisecontentnew-usfs.hub.arcgis.com/datasets/9248aea543dd4ef0833680d136fe8496_1.geojson?outSR=%7B%22latestWkid%22:3857,%22wkid%22:102100%7D'

counties = tigris::counties(class = 'sf',year = '2017')
#fs_county = st_read(fs_county_url)
counties = st_make_valid(counties)
counties <- st_transform(counties,crs = st_crs(albersNA))
states = tigris::states(class = 'sf')
counties$CFIPS = formatC(counties$GEOID,width = 5,flag = 0)
#fs_county$CFIPS = paste0(formatC(states$STATEFP[match(fs_county$STATENAME,states$NAME)],width = 2,flag = 0),formatC(fs_county$CENSUSCODE,width = 3,flag = 0))
fs_county_intersects = st_intersection(admin_districts,counties)

fs_county_intersects$prop_in_county = st_area(fs_county_intersects) / st_area(admin_districts)[match(fs_county_intersects$FOREST_ID,admin_districts$FOREST_ID)]
fs_county_intersects$prop_in_county <- round(as.numeric(fs_county_intersects$prop_in_county),2)
fs_county_intersects = fs_county_intersects[fs_county_intersects$prop_in_county>=0.01,]

fs_county_intersects$FOREST_ID <- formatC(fs_county_intersects$FOREST_ID,width = 4,flag = 0)

county_over = as.data.table(fs_county_intersects)
### reads in pre-made file with natural resource gdp by county


county_nr = fread('input/cpb_data/naturalresource_gdp_by_county_2001-2018.csv')
county_nr$CFIPS = formatC(county_nr$CFIPS,width = 5,flag = 0)
setnames(county_nr,'Year','YEAR')
county_nr[,STATE:=NULL]
county_nr$NaturalResources1M <- county_nr$County_naturalresource_GDP /1e6


####### county business pattern overlay
### reads in county business pattern local area employment data
#seasonally adjusted monthly LAU
county_lau = fread('https://download.bls.gov/pub/time.series/la/la.data.64.County',sep = '\t')
#county_lau = county_lau[year>=2002,]
#keep january
county_lau = county_lau[period%in%c('M01','M10'),]
county_lau$CFIPS = str_extract(county_lau$series_id,'(?<=LAUCN)[0-9]{5}')
#keep unemployment percentage
county_lau = county_lau[grepl('3$',series_id),]
county_lau[,series_id:=NULL]
county_lau[,period:=NULL]
county_lau[,footnote_codes:=NULL]
setnames(county_lau,c('value','year'),c('LAU','YEAR'))
county_lau$MONTH = ifelse(duplicated(county_lau[,.(YEAR,CFIPS)]),'October','January')
county_lau$YEAR = as.numeric(county_lau$YEAR)
county_lau$LAU = as.numeric(county_lau$LAU)

county_lau = dcast(county_lau,YEAR + CFIPS ~ MONTH,value.var = 'LAU')
setnames(county_lau,c('January','October'),c('LAU_January','LAU_October'))
county_econ = data.table(full_join(county_lau,county_nr[,.(CFIPS,YEAR,NaturalResources1M)]))


cbp_list = lapply(list.files('input/cpb_data/','with_ann',full.names = T),function(x) {
  tt=fread(x,skip = 1)
  names(tt) = gsub('[0-9]{4}\\s','',names(tt));names(tt) = gsub("Paid employees for pay period including March 12 (number)","Number of employees",names(tt),fixed=T)
  tt})
cbp_dt = rbindlist(cbp_list,fill = T)
cbp_dt = cbp_dt[,.(Id2,`NAICS code`,Year,`Number of employees`)]
setnames(cbp_dt,c('Id2',"NAICS code","Number of employees"),c('CFIPS','NAICS',"Number_employees"))
cbp_dt$CFIPS = formatC(cbp_dt$CFIPS,width=5,flag=0)
cbp_dt = dcast(cbp_dt,CFIPS + Year ~ NAICS,value.var = 'Number_employees')
cbp_dt = cbp_dt[!grepl('000$',cbp_dt$CFIPS),]
setnames(cbp_dt,c('113',"114","21","71","0"),c('forestry_logging','fishing_hunting',"mining","recreation_entertainment","all_employees"))
simulate_blurred_response = function(x){
  round(ifelse(is.na(x),NA,ifelse(!is.na(as.numeric(x)),as.numeric(x),ifelse(x=='a',runif(length(x),1,19),ifelse(x=='b',runif(length(x),20,99),
                                                                                                                 ifelse(x=='c',runif(length(x),100,249),ifelse(x=='e',runif(length(x),250,499),ifelse(x=='f',runif(length(x),500,999),
                                                                                                                                                                                                      ifelse(x=='g',runif(length(x),1000,2499),ifelse(x=='h',runif(length(x),2500,4999),NA))))))))))}
vnames = c('all_employees','mining','recreation_entertainment',
           'forestry_logging','fishing_hunting')
cbp_dt = cbp_dt[,(vnames):=lapply(.SD,simulate_blurred_response),.SDcols=vnames]

cbp_dt$Prop_Forestry_Employ = cbp_dt$forestry_logging/cbp_dt$all_employees
cbp_dt$Prop_Mining_Employ = cbp_dt$mining/cbp_dt$all_employees
cbp_dt$Prop_Recreation =  cbp_dt$recreation_entertainment/cbp_dt$all_employees
cbp_dt$Prop_HuntingFishing = cbp_dt$fishing_hunting/cbp_dt$all_employees
cbp_dt$Prop_NaturalResourceEmployment <- replace_na(cbp_dt$Prop_HuntingFishing,0) + replace_na(cbp_dt$Prop_Forestry_Employ,0)+replace_na(cbp_dt$Prop_Mining_Employ,0)
setnames(cbp_dt,'Year','YEAR')

county_econ = data.table(left_join(county_econ,cbp_dt[,.(CFIPS,YEAR,Prop_NaturalResourceEmployment)]))
county_econ[order(CFIPS,YEAR),Prop_NaturalResourceEmployment:=zoo::na.locf(Prop_NaturalResourceEmployment,na.rm=F),by = .(CFIPS)]

setkey(county_econ,CFIPS)
setkey(county_over,CFIPS)
county_econ = county_econ[county_over,]
forest_weighted_econ = county_econ[,lapply(.SD,weighted.mean,w=prop_in_county,na.rm = T), by=.(FOREST_ID,YEAR),.SDcols = c("LAU_January","LAU_October","NaturalResources1M",'Prop_NaturalResourceEmployment')]
setnames(forest_weighted_econ,'YEAR','CALENDAR_YEAR')


forest_weighted_econ$CALENDAR_YEAR = as.numeric(forest_weighted_econ$CALENDAR_YEAR)
temp_dt$CALENDAR_YEAR <- as.numeric(temp_dt$CALENDAR_YEAR)
setkey(forest_weighted_econ,FOREST_ID,CALENDAR_YEAR)
setkey(temp_dt,FOREST_ID,CALENDAR_YEAR)
temp_dt = left_join(temp_dt,forest_weighted_econ)
temp_dt = data.table(temp_dt)

countyVS = readRDS('input/politics/countyVoteShare_3-2020_imputed.rds')
setkey(countyVS,'CFIPS')
countyVS = countyVS[county_over,]
forest_weighted_demVS = countyVS[,lapply(.SD,weighted.mean,w=prop_in_county), by=.(FOREST_ID,FISCAL_YEAR),.SDcols = c("percentD_H")]
setnames(forest_weighted_demVS,'FISCAL_YEAR','CALENDAR_YEAR')

temp_dt=left_join(temp_dt,forest_weighted_demVS)



# 
# library(readxl)
# receipts = data.table(read_excel('input/usfs_internal_data/USFSGrossReceipts_cleaned.xlsx',skip = 3))
# receipts$`National Forest Code` = formatC(receipts$`National Forest Code`,width = 4,flag = 0)
# #receipts = receipts[Year>=2002,]
# setnames(receipts,c("National Forest Code","Year"),c('FOREST_ID',"CALENDAR_YEAR"))
# 
# receipts = receipts[,.(CALENDAR_YEAR,FOREST_ID,`Itemp_dtlation Adjusted Class1 - Timber`,`Itemp_dtlation Adjusted Class2 - Grazing East`,`Itemp_dtlation Adjusted Class8 - Grazing West`,
#                        `Itemp_dtlation Adjusted Class3 - Land Use`,`Itemp_dtlation Adjusted Class7 - Recreation User Fees`,`Itemp_dtlation Adjusted Class6 - Minerals`,
#                        `Itemp_dtlation Adjusted Class4 - Recreation Special Uses`, `Itemp_dtlation Adjusted Class5 - Power`,`Itemp_dtlation Adjusted Total temp_dtF`)]
# 
# temp_dt = data.table(left_join(temp_dt,receipts))

###
### load timber sales data from google sheet
timber_sales=fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vSDJ_MS5MyRV5MNAOZtNETa3ga0QRRjlUXZL5Fpb3cleugwDhbcJ8JaT7EKxdRXXg/pub?gid=68111876&single=true&output=csv')
timber_sales$FOREST_ID = formatC(timber_sales$`Forest Number`,width=4,flag = 0)

timber_sales$FOREST_ID = gsub('^0108','0111',timber_sales$FOREST_ID)
timber_sales$FOREST_ID = gsub('^0105','0117',timber_sales$FOREST_ID)
timber_sales$FOREST_ID = gsub('^0112','0115',timber_sales$FOREST_ID)

timber_sales$CALENDAR_YEAR = timber_sales$Year
timber_sales = timber_sales[,.(CALENDAR_YEAR,FOREST_ID,`Sold Volume (MBF)`,`Sold Value`,`Cut Volume (MBF)`,`Cut Value`)]

nam = c('Sold Volume (MBF)','Sold Value','Cut Volume (MBF)','Cut Value')
new_nam = c('Sold_Volume_MBF','Sold_Value','Cut_Volume_MBF','Cut_Value')
timber_sales[,(nam):=lapply(.SD,sum),by = .(CALENDAR_YEAR,FOREST_ID),.SDcols = nam]
timber_cut = timber_sales[CALENDAR_YEAR %in% 1999:2004,mean(`Cut Volume (MBF)`),by=.(FOREST_ID)]
setnames(timber_cut,'V1','Avg_MBF_Cut_1999-2004')
setkey(timber_sales,FOREST_ID)
setkey(temp_dt,FOREST_ID)
temp_dt =  left_join(temp_dt,timber_cut)
temp_dt = data.table(temp_dt)


#### reads in county percentD_H vote ####
county_voting = readRDS('input/politics/countyVoteShare_3-2020_imputed.rds')
setnames(county_voting,'FISCAL_YEAR','CALENDAR_YEAR')
county_forest_voting = data.table(left_join(county_voting,county_over))
# NAs are cases where county didn't actually overlap
county_forest_voting = county_forest_voting[!is.na(FOREST_ID),]
forest_voting_weighted = county_forest_voting[,weighted.mean(percentD_H,prop_in_county),by=.(FOREST_ID,congress,CALENDAR_YEAR)]
setnames(forest_voting_weighted,'V1','percentD_H')

setkey(forest_voting_weighted,FOREST_ID,CALENDAR_YEAR)
setkey(temp_dt,FOREST_ID,CALENDAR_YEAR)
temp_dt = data.table(left_join(temp_dt,forest_voting_weighted))

library(readxl)
wui = read_excel('input/wui/COUNTY_WUI_change_1990_2010_Stats_Report.xlsx',sheet = 'HOUSING',skip = 1)
wui$WUI_Housing = wui$`2000...11` 
wui$NON_WUI_Housing = wui$`2000...14`
wui$FIPS = as.integer(wui$FIPS)
wui$FIPS = as.character(formatC(wui$FIPS,width = 5,flag = 0))
wui = data.table(wui)
setnames(wui,'FIPS','CFIPS')
setkey(county_over,CFIPS)
setkey(wui,CFIPS)

akb = fread('input/wui/DEC_00_SF1_H001_with_ann.csv',skip = 1,stringsAsFactors = F)
akb$CFIPS = formatC(akb$Id2,width = 5,flag = 0)
akb$WUI_Housing = akb$Total

wui = rbind(wui,akb[,.(CFIPS,WUI_Housing)],use.names=T,fill = T)
wui$NON_WUI_Housing[is.na(wui$NON_WUI_Housing)]<- 0

wui$Prop_WUI_Housing = wui$WUI_Housing/(wui$NON_WUI_Housing+wui$WUI_Housing)
county_wui_over = data.table(left_join(wui,county_over))

wui_housing_units = county_wui_over[,lapply(.SD,weighted.mean,w=prop_in_county,na.rm=T), by=.(FOREST_ID),.SDcols = 'Prop_WUI_Housing']
#setnames(wui_housing_units,'V1','WUI_Housing_Units')

temp_dt = data.table(left_join(temp_dt,wui_housing_units))

###### add house representative variables
house = readRDS('input/politics/houseAtts_5-2019.RDS')
house = data.table(house)
house= house[!duplicated(paste0(year,stateDistrict)),]
setnames(house,'year','CALENDAR_YEAR')
house$Congressional_District_ID <- formatC(house$Congressional_District_ID,width=4,flag = 0)
cd_ideology = readRDS('input/politics_data/final/cd_ideology_8-2020.RDS')

cd_ideology$cd_fips[grepl('00$',cd_ideology$cd_fips)] <- cd_ideology$cd_fips[grepl('00$',cd_ideology$cd_fips)] + 1
cd_ideology$cd_fips = formatC(cd_ideology$cd_fips,width = 4,flag = 0)


house$mrp_mean<-cd_ideology$mrp_mean[match(
  paste(house$CALENDAR_YEAR,house$Congressional_District_ID),
  paste(cd_ideology$year,formatC(cd_ideology$cd_fips,width = 4,flag = 0)))]
setkey(house,CALENDAR_YEAR,Congressional_District_ID)

house_overs = fread('input/gis_overlap_props/nationalforest_congressdistrict_overlap_props.csv')
house_overs$FOREST_ID = formatC(house_overs$FOREST_ID,width=4,flag = 0)
house_overs$Congressional_District_ID <- formatC(gsub('00$','01',house_overs$Congressional_District_ID),width=4,flag=0)
setnames(house_overs,'Year','CALENDAR_YEAR')
setkey(house_overs,CALENDAR_YEAR,Congressional_District_ID)
house_dt = data.table(left_join(house,house_overs))
house_dt$Prop_Overlap = round(house_dt$Prop_Overlap,2)
house_dt = house_dt[house_dt$Prop_Overlap>0,]
pol_cols = c("LCV_annual","LCV_lifetime","democrat","nominate_dim1",'nominate_dim2','demPres','demCongress','agChairLCV','agChairDW1','nrChairDW1','nrChairLCV','agComLCV','agComDW1','nrComLCV','nrComDW1','mrp_mean')


forest_house_values = house_dt[,lapply(.SD,weighted.mean,w=Prop_Overlap,na.rm=T), by=.(FOREST_ID,CALENDAR_YEAR,congress),.SDcols = pol_cols]
forest_house_values$CALENDAR_YEAR = as.numeric(forest_house_values$CALENDAR_YEAR)
setkey(forest_house_values,'FOREST_ID','CALENDAR_YEAR')
setkey(temp_dt,'FOREST_ID','CALENDAR_YEAR')

temp_dt = left_join(temp_dt,forest_house_values)
temp_dt = data.table(temp_dt)

temp_dt = temp_dt[CALENDAR_YEAR>=first_year&CALENDAR_YEAR<=last_year]
temp_dt = temp_dt[!is.na(congress),]


use = readRDS('policypolitics/prepped/forestUseAverage_2005-2014.RDS')

use$FOREST_ID = paste0(use$REGION,use$FORESTNUMB)

temp_dt$Average_Yearly_Visits = use$visits[match(temp_dt$FOREST_ID,use$FOREST_ID)]


#########
temp_dt$index_in_forestshp = match(temp_dt$FOREST_ID,admin_districts$FOREST_ID)
temp_dt$GIS_Area = st_area(admin_districts)[temp_dt$index_in_forestshp]

#### wildfire burn overlay
#wildfire_burn_url = 'https://wildfire.cr.usgs.gov/firehistory/data/wf_usfs_1980_2016.zip'
wildfire_burn_url = 'http://enterprisecontentnew-usfs.hub.arcgis.com/datasets/9125a35d10e2410fa9d6dcdb46256702_10.geojson?outSR=%7B%22latestWkid%22:4269,%22wkid%22:4269%7D'
wildfire_burn <- st_read(wildfire_burn_url)

wildfire_burn <- st_transform(wildfire_burn,crs = st_crs(albersNA))
wildfire_burn  <- st_make_valid(wildfire_burn)
setnames(wildfire_burn,'FIREYEAR','CALENDAR_YEAR')
wildfire_burn=wildfire_burn[wildfire_burn$CALENDAR_YEAR>=1999,]

temp_dt$CALENDAR_YEAR = as.numeric(temp_dt$CALENDAR_YEAR)
forest_year_combos = expand.grid(FOREST_ID = unique(temp_dt$FOREST_ID),CALENDAR_YEAR = 2004:2018)
forest_year_combos$Area_Burned_P5 = NA
wildfire_index = sapply(seq_along(temp_dt$CALENDAR_YEAR),function(x) which(wildfire_burn$CALENDAR_YEAR<temp_dt$CALENDAR_YEAR[x]&wildfire_burn$CALENDAR_YEAR>(temp_dt$CALENDAR_YEAR[x]-6)))
wildfire_inters = st_intersects(admin_districts,wildfire_burn)
wildfire_intersections = mapply(function(x,y) intersect(x,y),x = wildfire_inters[temp_dt$index_in_forestshp],wildfire_index)
total_burn_area_past5yrs = pbsapply(1:nrow(temp_dt),function(i){if(length(wildfire_intersections[[i]])==0){0}
  else{sum(st_area(st_intersection(admin_districts[temp_dt$index_in_forestshp[i],],wildfire_burn[wildfire_intersections[[i]],])))}},cl = cores)
temp_dt$Burned_Area_Past5yrs = total_burn_area_past5yrs
temp_dt$Burned_Prop_Past5yrs = as.numeric(temp_dt$Burned_Area_Past5yrs/temp_dt$GIS_Area)

###### wilderness area overlay
#wilderness_area_url = 'http://enterprisecontentnew-usfs.hub.arcgis.com/datasets/70dac7184e804326870fd3fdb9b52047_0.geojson?outSR=%7B%22latestWkid%22:3857,%22wkid%22:102100%7D'
#wilderness_area <- st_read(wilderness_area_url)
#wilderness_area <- st_transform(wilderness_area,crs = st_crs(albersNA))
#wilderness_area  <- st_make_valid(wilderness_area)
#wilderness_area$WID <- wilderness_area$AREAID
desig_years_url = 'https://www.wilderness.net/GIS/Wilderness_Areas.zip'
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(desig_years_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
desig_years <- st_read(fpath)
#desig_years_fs = desig_years[desig_years$Agency=='FS',]
desig_years$WID <- formatC(desig_years$WID,width = 3,flag = 0)
desig_years <- st_transform(desig_years,crs = st_crs(albersNA))
desig_years <- st_make_valid(desig_years)
forest_wilderness_intersects = st_intersection(admin_districts,desig_years)
forest_wilderness_intersects$area = st_area(forest_wilderness_intersects)


forest_wilderness_area = data.table(forest_wilderness_intersects)
wilderness_area_by_year = rbindlist(lapply(first_year:last_year,function(y) forest_wilderness_area[YearDesign<y,sum(area),by=.(FOREST_ID)][,CALENDAR_YEAR:=y]))
wilderness_area_by_year$Wilderness_Prop = wilderness_area_by_year$V1/st_area(admin_districts)[match(wilderness_area_by_year$FOREST_ID,admin_districts$FOREST_ID)]
setnames(wilderness_area_by_year,'V1','Wilderness_Area')
wilderness_area_by_year$Wilderness_Prop <- as.numeric(wilderness_area_by_year$Wilderness_Prop)

wilderness_area_by_year = full_join(wilderness_area_by_year,expand.grid(FOREST_ID = unique(temp_dt$FOREST_ID),CALENDAR_YEAR = first_year:last_year))
wilderness_area_by_year = data.table(wilderness_area_by_year)
wilderness_area_by_year[order(FOREST_ID,CALENDAR_YEAR),Wilderness_Area:=zoo::na.locf(Wilderness_Area,na.rm = F),by=.(FOREST_ID)]
wilderness_area_by_year[order(FOREST_ID,CALENDAR_YEAR),Wilderness_Prop:=zoo::na.locf(Wilderness_Prop,na.rm = F),by=.(FOREST_ID)]
wilderness_area_by_year$Wilderness_Area[is.na(wilderness_area_by_year$Wilderness_Area)]<- 0
wilderness_area_by_year$Wilderness_Prop[is.na(wilderness_area_by_year$Wilderness_Prop)]<- 0

setkey(wilderness_area_by_year,'FOREST_ID','CALENDAR_YEAR')
setkey(temp_dt,'FOREST_ID','CALENDAR_YEAR')

temp_dt = temp_dt[wilderness_area_by_year,]


#### limited use overlay
limit_use_url = 'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.OthNatlDesgAreaStatus.zip'
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(limit_use_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
limit_use <- st_read(fpath)
limit_use <- st_transform(limit_use,crs = st_crs(albersNA))
limit_use  <- st_make_valid(limit_use)
limit_use$CALENDAR_YEAR = year(ymd(limit_use$ACTIONDATE))
limit_use = limit_use[limit_use$AREATYPE %in% c('NATIONAL RECREATION TRAIL','NATIONAL MONUMENT','NATIONAL WILDLIFE REFUGE','NATIONAL SCENIC TRAIL','NATIONAL RECREATION AREA','NATIONAL GAME REFUGE','NATIONAL VOLCANIC MONUMENT'),]

forest_limit_intersects = st_intersection(admin_districts,limit_use)
forest_limit_intersects$area = st_area(forest_limit_intersects)
forest_limit_area = data.table(forest_limit_intersects)
limit_area_by_year = rbindlist(lapply(first_year:last_year,function(y) forest_limit_area[CALENDAR_YEAR<y,sum(area),by=.(FOREST_ID)][,CALENDAR_YEAR:=y]))
limit_area_by_year$limit_prop = limit_area_by_year$V1/st_area(admin_districts)[match(limit_area_by_year$FOREST_ID,admin_districts$FOREST_ID)]
setnames(limit_area_by_year,'V1','limit_Area')
limit_area_by_year$limit_prop <- as.numeric(limit_area_by_year$limit_prop)

limit_area_by_year = full_join(limit_area_by_year,expand.grid(FOREST_ID = unique(temp_dt$FOREST_ID),CALENDAR_YEAR = first_year:last_year))
limit_area_by_year = data.table(limit_area_by_year)
limit_area_by_year[order(FOREST_ID,CALENDAR_YEAR),limit_Area:=zoo::na.locf(limit_Area,na.rm = F),by=.(FOREST_ID)]
limit_area_by_year[order(FOREST_ID,CALENDAR_YEAR),limit_Area:=zoo::na.locf(limit_prop,na.rm = F),by=.(FOREST_ID)]
limit_area_by_year$limit_Area[is.na(limit_area_by_year$limit_Area)]<- 0
limit_area_by_year$limit_prop[is.na(limit_area_by_year$limit_prop)]<- 0

setkey(limit_area_by_year,'FOREST_ID','CALENDAR_YEAR')
setkey(temp_dt,'FOREST_ID','CALENDAR_YEAR')

temp_dt = temp_dt[limit_area_by_year,]


esa = fread('input/usfs_internal_data/nfs_tep_group_species_sort_11jul08.csv')
esa$nf = esa$`Forest and or Grassland with Species`
esa$nf <- gsub('NFS','National Forests',esa$nf)
esa$nf <- gsub('NF','National Forest',esa$nf)
esa$nf <- gsub('Grasslnds','Grasslands',esa$nf)
esa$nf <- gsub('\\/','-',esa$nf)
esa = esa[nf!='']

esa$nf[grepl('Alabama',esa$nf)] <- "National Forests in Alabama"
esa$nf[grepl('North Carolina',esa$nf)] <- "National Forests in North Carolina"
esa$nf[grepl('Mississippi',esa$nf)] <- "National Forests in Mississippi"
esa$nf[grepl('Florida',esa$nf)] <- "National Forests in Florida"
esa$nf[grepl('Texas',esa$nf)] <- "National Forests in Texas"
esa$nf[grepl('Finger Lakes|Green Mountain',esa$nf)] <- "Green Mountain and Finger Lakes National Forests"
esa$nf[grepl('Custer|Gallatin',esa$nf)] <- "Custer Gallatin National Forest"
esa$nf[grepl('Helena|Lewis-Clark',esa$nf)] <- "Helena-Lewis and Clark National Forest"
esa$nf[grepl('Uinta|Wasatch|Cache',esa$nf)] <- "Uinta-Wasatch-Cache National Forest"
esa$nf[grepl('Salmon|Challis',esa$nf)] <-"Salmon-Challis National Forest"
esa$nf[grepl('Nebraska',esa$nf)] <-"Nebraska National Forest"
esa$nf[grepl('Nez|Clearwater',esa$nf)] <-"Nez Perce-Clearwater National Forest"
esa$nf[grepl('Humboldt|Toiyabe',esa$nf)] <-"Humboldt-Toiyabe National Forest"
esa$nf[grepl('Caribou|Targhee',esa$nf)] <-"Caribou-Targhee National Forest"
esa$nf[grepl('Arapaho|Roosev',esa$nf)] <-"Arapaho and Roosevelt National Forests"
esa$nf[grepl('Midewin',esa$nf)] <-"Midewin National Tallgrass Prairie"
esa$nf[grepl('Rogue|Siskiyou',esa$nf)] <-"Rogue River-Siskiyou National Forests"
esa$nf[grepl('Baker|Snoqualmie',esa$nf)] <-"Mt. Baker-Snoqualmie National Forest"
esa$nf[grepl('Huron|Manistee',esa$nf)] <-"Huron-Manistee National Forest"
esa$nf[grepl('Tahoe Basin MU',esa$nf)] <-"Lake Tahoe Basin Management Unit"
esa$nf[grepl('Yunque',esa$nf)] <-"El Yunque National Forest" 
esa$nf[grepl('Land Between',esa$nf)] <-"Land Between the Lakes National Recreation Area" 
esa$nf[grepl('Pike|San Isabel',esa$nf)] <- "Pike and San Isabel National Forests"
esa$nf[grepl('Manti|LaSal|La Sal',esa$nf)] <-"Manti-La Sal National Forest"
esa$nf[grepl('Medicine Bow|Routt',esa$nf)] <-"Medicine Bow-Routt National Forest"
esa$nf[grepl('Uncompahgre|Gunnison',esa$nf)] <- "Grand Mesa, Uncompahgre and Gunnison National Forests"
switch = which(!esa$nf %in% admin_districts$FORESTNAME & paste(esa$nf,'National Forest') %in% admin_districts$FORESTNAME)
esa$nf[switch] <- paste(esa$nf[switch],'National Forest')

switch = which(!esa$nf %in% admin_districts$FORESTNAME & paste(esa$nf,'National Forests') %in% admin_districts$FORESTNAME)
esa$nf[switch] <- paste(esa$nf[switch],'National Forests')
esa_counts = esa[,.N,by = .(nf)]
setnames(esa_counts,'N','Count_EorT_Species')
esa_counts$FOREST_ID = admin_districts$FOREST_ID[match(esa_counts$nf,admin_districts$FORESTNAME)]
esa_counts[,nf:=NULL]

#https://www.fs.usda.gov/Internet/FSE_DOCUMENTS/fsbdev2_037582.pdf
ak_counts = data.table(FOREST_ID=c('1004','1005'),Count_EorT_Species = c(18,20))
esa_counts = rbind(esa_counts,ak_counts,use.names=T)

setkey(temp_dt,'FOREST_ID')
setkey(esa_counts,'FOREST_ID')
temp_dt = merge(temp_dt,esa_counts,all.x = T)


##### count listed species over project area
# habitat_url = 'https://ecos.fws.gov/docs/crithab/crithab_all/crithab_all_layers.zip'
# td = tempdir()
# tf = tempfile(tmpdir=td, fileext=".zip")
# download.file(habitat_url, tf)
# fname = unzip(tf, list=TRUE)
# unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
# fpath = file.path(td, grep('shp$',fname$Name,value=T))
# 
# hab_poly <- st_read(grep('POLY',fpath,value=T))
# hab_poly <- st_transform(hab_poly,crs = st_crs(albersNA))
# hab_poly  <- st_make_valid(hab_poly)
# hab_poly$FR_Date = ymd(as.character(hab_poly$pubdate))
# hab_poly$CALENDAR_YEAR = year(hab_poly$FR_Date) + (month(hab_poly$FR_Date)>=10 + 0)
# hab_poly_intersects = st_intersects(admin_districts,hab_poly)
# hab_poly_count = pbsapply(1:nrow(temp_dt),function(i){
#   if(is.na(temp_dt$FOREST_ID[[i]])){NA}
#   else if(length(hab_poly_intersects[[temp_dt$index_in_forestshp[[i]]]])==0){0}
#   else{hab_poly[hab_poly_intersects[[temp_dt$index_in_forestshp[i]]],]%>%filter(CALENDAR_YEAR<temp_dt$CALENDAR_YEAR[i]) %>%
#       filter(!duplicated(sciname)) %>% nrow(.)}})
# 
# hab_line <- st_read(grep('LINE',fpath,value=T))
# hab_line <- st_transform(hab_line,crs = st_crs(albersNA))
# hab_line  <- st_make_valid(hab_line)
# hab_line$FR_Date = ymd(as.character(hab_line$pubdate))
# hab_line$CALENDAR_YEAR = year(hab_line$FR_Date) + (month(hab_line$FR_Date)>=10 + 0)
# hab_line_intersects = st_intersects(admin_districts,hab_line)
# hab_line_count = pbsapply(1:nrow(temp_dt),function(i){
#   if(is.na(temp_dt$FOREST_ID[[i]])){NA}
#   else if(length(hab_line_intersects[[temp_dt$index_in_forestshp[[i]]]])==0){0}
#   else{hab_line[hab_line_intersects[[temp_dt$index_in_forestshp[i]]],]%>%filter(CALENDAR_YEAR<temp_dt$CALENDAR_YEAR[i]) %>%
#       filter(!duplicated(sciname)) %>% nrow(.)}})
# temp_dt$Count_Species_CriticalHabitat = hab_poly_count + hab_line_count
# 
# 
# ####### overlay subsurface mineral rights on ranger district
# mineral_rights_url = 'http://enterprisecontentnew-usfs.hub.arcgis.com/datasets/43324ab0ead14e4c8ffdecfdc62a22c8_0.geojson?outSR=%7B%22latestWkid%22:4269,%22wkid%22:4269%7D'
# mineral_rights <- st_read(mineral_rights_url)
# 
# mineral_rights <- mineral_rights[!is.na(mineral_rights$ACTIONDATE),]
# mineral_rights <- st_transform(mineral_rights,crs = st_crs(albersNA))
# mineral_rights  <- st_make_valid(mineral_rights)
# mineral_rights$CALENDAR_YEAR = mineral_rights$ACTIOtemp_dtISC
# mineral_rights <- mineral_rights[ymd(mineral_rights$ACTIONDATE)  < ymd('2004-01-01'),]
# mineral_rights = mineral_rights[!is.na(mineral_rights$ACTIONDATE),]
# mineral_rights = mineral_rights[year(ymd(mineral_rights$ACTIONDATE)) %in% 1993:2004,]
# over_minerals = st_intersects(admin_districts,mineral_rights)
# admin_districts$MINING_CLAIM_ACTIONS_1993_2004<- sapply(over_minerals,length)
# temp_dt$MINING_CLAIM_ACTIONS_1993_2004 <- admin_districts$MINING_CLAIM_ACTIONS_1993_2004[match(temp_dt$FOREST_ID,admin_districts$FOREST_ID)]

# mineral_index = sapply(seq_along(temp_dt$CALENDAR_YEAR),function(x) which(mineral_rights$CALENDAR_YEAR<temp_dt$CALENDAR_YEAR[x]))
# mineral_inters = st_intersects(admin_districts,mineral_rights)
# mineral_intersections = mapply(function(x,y) {if(length(x)==0){NA}else{intersect(x,y)}},x = mineral_inters[temp_dt$index_in_forestshp],y = mineral_index)
# total_mineral_rights = pbsapply(1:nrow(temp_dt),function(i){if(length(mineral_intersections[[i]])==0){0}
#   else{sum(st_area(st_intersection(admin_districts[temp_dt$index_in_forestshp[i],],st_union(mineral_rights[mineral_intersections[[i]],]))))}},cl = cores)
# temp_dt$Mineral_Rights_Area = total_mineral_rights
# temp_dt$Mineral_Rights_Prop = temp_dt$Mineral_Rights_Area/temp_dt$GIS_Area
# # 
# # 
# ##### overlay rangeland on ranger districts
# allotments_url = 'http://enterprisecontentnew-usfs.hub.arcgis.com/datasets/6c1d57398aa44c36a6badb541f21e461_0.geojson?outSR=%7B%22latestWkid%22:3857,%22wkid%22:102100%7D'
# allotments <- st_read(allotments_url)
# allotments <- st_transform(allotments,crs = st_crs(albersNA))
# allotments  <- st_make_valid(allotments)
# allotments$CALENDAR_YEAR = allotments$NEPA_DEC_A
# allotments = allotments[allotments$CALENDAR_YEAR %in% 1999:2004,]
# over_allotments = st_intersects(admin_districts,allotments)
# admin_districts$ALLOTMENT_NEPA_1993_2004 <- sapply(over_allotments,length)
# temp_dt$ALLOTMENT_NEPA_1993_2004 <- admin_districts$ALLOTMENT_NEPA_1993_2004[match(temp_dt$FOREST_ID,admin_districts$FOREST_ID)]
# 

# allotments_index = sapply(seq_along(temp_dt$CALENDAR_YEAR),function(x) which(allotments$CALENDAR_YEAR<temp_dt$CALENDAR_YEAR[x]))
# allotments_inters = st_intersects(ranger_districts,allotments)
# allotments_intersections = mapply(function(x,y) {if(length(x)==0){NA}else{intersect(x,y)}},x = allotments_inters[temp_dt$index_in_forestshp],y = allotments_index)
# 
# 
# total_allotments = pbsapply(1:nrow(temp_dt),function(i){
#   if(length(allotments_intersections)==0){0}
#   if(i>1 & identical(allotments_intersections[i],allotments_intersections[i-1])){NA}
#   else{sum(st_area(st_intersection(ranger_districts[temp_dt$index_in_forestshp[i],],
#                                    allotments[allotments_intersections[[i]],]))) / temp_dt$GIS_Area[i]
#   }},cl = cores)
# 
# total_allotments = zoo::na.locf(total_allotments)
# 
# temp_dt$Allotments_Area = total_allotments
# temp_dt$Allotments_Prop = temp_dt$Allotments_Area/temp_dt$GIS_Area
# temp_dt$Allotments_Prop[temp_dt$Allotments_Prop>1]<- 1


eco_sections_url = 'http://enterprisecontentnew-usfs.hub.arcgis.com/datasets/14dc96d4cf4a445f8bb65d4cd4803476_2.geojson?outSR=%7B%22latestWkid%22:3857,%22wkid%22:102100%7D'
eco_sections <- st_read(eco_sections_url)
eco_sections <- st_transform(eco_sections,crs = st_crs(albersNA))
eco_sections  <- st_make_valid(eco_sections)
over_eco_sections = st_intersects(admin_districts,eco_sections)

temp_dt$Num_Eco_Sections = sapply(over_eco_sections,length)[temp_dt$index_in_forestshp]
# 

gross_receipts <- read_excel('input/USFSGrossReceipts.xlsx',skip = 3)
gross_receipts <- data.table(gross_receipts )
gross_receipts = gross_receipts[,c('Year','National Forest Code',grep('Inflation Adjusted (Class|Total Gross)',names(gross_receipts),value = T)),with = F]
setnames(gross_receipts,'National Forest Code','FOREST_ID')
gross_receipts$FOREST_ID <- formatC(gross_receipts$FOREST_ID,width = 4, flag = 0)
setnames(gross_receipts, 'Inflation Adjusted Total Gross Receipts','Total_Gross_Receipts')
require(dplyr)

setnames(gross_receipts,
c("Inflation Adjusted Class1 - Timber","Inflation Adjusted Class2 - Grazing East",      
"Inflation Adjusted Class3 - Land Use","Inflation Adjusted Class4 - Recreation Special Uses",
"Inflation Adjusted Class5 - Power" ,"Inflation Adjusted Class6 - Minerals",  
"Inflation Adjusted Class7 - Recreation User Fees" ,"Inflation Adjusted Class8 - Grazing West",           
"Inflation Adjusted Class9 - Quartz Crystals"),
c('Receipts_Timber','Receipts_Grazing_East','Receipts_Land_Use','Receipts_Recreation_SU',
  'Receipts_Power','Receipts_Minerals','Receipts_Recreation_UF','Receipts_Grazing_West',
  'Receipts_Quartz'))

gross_receipts$Receipts_Timber[gross_receipts$Receipts_Timber<0]<- 0
gross_receipts$Receipts_Grazing <- gross_receipts$Receipts_Grazing_East + gross_receipts$Receipts_Grazing_West
gross_receipts$Receipts_Grazing[gross_receipts$Receipts_Grazing<0]<- 0
gross_receipts$Receipts_Minerals <- gross_receipts$Receipts_Minerals + gross_receipts$Receipts_Quartz 
gross_receipts$Receipts_Minerals[gross_receipts$Receipts_Minerals<0]<- 0
gross_receipts$Receipts_Recreation <- gross_receipts$Receipts_Recreation_SU + gross_receipts$Receipts_Recreation_UF
gross_receipts$Receipts_Recreation[gross_receipts$Receipts_Recreation<0]<- 0
gross_receipts$Receipts_LandUseEnergy <- gross_receipts$Receipts_Land_Use + gross_receipts$Receipts_Power
gross_receipts$Receipts_LandUseEnergy[gross_receipts$Receipts_LandUseEnergy<0]<- 0
gross_receipts[,Receipts_Quartz:=NULL]
gross_receipts[,Receipts_Grazing_East:=NULL]
gross_receipts[,Receipts_Grazing_West:=NULL]
gross_receipts[,Receipts_Recreation_SU:=NULL]
gross_receipts[,Receipts_Recreation_UF:=NULL]

#this rolling sum includes current

gross_receipts[order(FOREST_ID,Year),Total_Receipts_P4:=lag(Total_Gross_Receipts,n = 5) - Total_Gross_Receipts,by=.(FOREST_ID)]

gross_receipts[order(FOREST_ID,Year),Receipts_Timber_P5:=frollsum(Receipts_Timber,n = 5,na.rm = F,fill = NA),by=.(FOREST_ID)]
gross_receipts$Receipts_Timber_P4 = gross_receipts$Receipts_Timber_P5 - gross_receipts$Receipts_Timber
gross_receipts[order(FOREST_ID,Year),Receipts_Grazing_P5:=frollsum(Receipts_Grazing,n = 5,na.rm = F,fill = NA),by=.(FOREST_ID)]
gross_receipts$Receipts_Grazing_P4 = gross_receipts$Receipts_Grazing_P5 - gross_receipts$Receipts_Grazing
gross_receipts[order(FOREST_ID,Year),Receipts_Minerals_P5:=frollsum(Receipts_Minerals,n = 5,na.rm = F,fill = NA),by=.(FOREST_ID)]
gross_receipts$Receipts_Minerals_P4 = gross_receipts$Receipts_Minerals_P5 - gross_receipts$Receipts_Minerals
gross_receipts[order(FOREST_ID,Year),Receipts_Recreation_P5:=frollsum(Receipts_Recreation,n = 5,na.rm = F,fill = NA),by=.(FOREST_ID)]
gross_receipts$Receipts_Recreation_P4 = gross_receipts$Receipts_Recreation_P5 - gross_receipts$Receipts_Recreation
gross_receipts[order(FOREST_ID,Year),Receipts_LandUseEnergy_P5:=frollsum(Receipts_LandUseEnergy,n = 5,na.rm = F,fill = NA),by=.(FOREST_ID)]
gross_receipts$Receipts_LandUseEnergy_P4 = gross_receipts$Receipts_LandUseEnergy_P5 - gross_receipts$Receipts_LandUseEnergy


temp_dt$FOREST_ID <- formatC(temp_dt$FOREST_ID,width = 4,flag = 0)
setnames(gross_receipts,'Year','CALENDAR_YEAR')
temp_dt <- merge(temp_dt,gross_receipts,all.x = T)

timber = data.table(read_excel('input/USFSTimberCutSold.xlsx',skip = 3,sheet = 'forests'))
setnames(timber,c('Year','Forest Number','Cut Volume (MBF)'),c('CALENDAR_YEAR','FOREST_ID','Cut_Volume_MBF'))
timber$FOREST_ID <- formatC(timber$FOREST_ID,width = 4,flag = 0)
timber = timber[,.(FOREST_ID,CALENDAR_YEAR,Cut_Volume_MBF)]
timber = timber[order(FOREST_ID,CALENDAR_YEAR),Cut_Volume_MBF_P5:=frollsum(Cut_Volume_MBF,5)-Cut_Volume_MBF,by=.(FOREST_ID)]

temp_dt = merge(temp_dt,timber,all.x = T)

temp_dt$Receipts_TimberMineralsGrazing_P4 <- temp_dt$Receipts_Minerals_P4 + temp_dt$Receipts_Timber_P4 + temp_dt$Receipts_Grazing_P4
temp_dt[order(FOREST_ID,CALENDAR_YEAR),Total_Receipts_4yr_Change_Perc:= (lag(Total_Gross_Receipts,1) - lag(Total_Gross_Receipts,5))/lag(Total_Gross_Receipts,5),by=.(FOREST_ID)]
temp_dt[order(FOREST_ID,CALENDAR_YEAR),Timber_Cut_MBF_4yr_Change_Perc:= (lag(Cut_Volume_MBF,1) - lag(Cut_Volume_MBF,5))/lag(Cut_Volume_MBF,5),by=.(FOREST_ID)]


temp_dt$Num_Eco_Sections[temp_dt$Num_Eco_Sections==0] <- 1


temp_dt = temp_dt[order(FOREST_ID,CALENDAR_YEAR,congress),]
temp_dt = temp_dt[, zoo::na.locf(.SD, na.rm = FALSE),by = .(FOREST_ID)]
temp_dt$Prop_Extraction_Employ = temp_dt$Prop_Forestry_Employ + temp_dt$Prop_Mining_Employ
temp_dt$Perc_Extraction_Employ = temp_dt$Prop_Extraction_Employ*100
temp_dt$Prop_Outdoor_Employ = temp_dt$Prop_HuntingFishing + temp_dt$Prop_Recreation
temp_dt$Wilderness_Prop = as.numeric(temp_dt$Wilderness_Prop)
temp_dt$Wilderness_Perc = temp_dt$Wilderness_Prop * 100
#temp_dt$Limited_Use_Prop = as.numeric(temp_dt$Limited_Use_Prop)
temp_dt$Prop_WUI_Housing = as.numeric(temp_dt$Prop_WUI_Housing)
temp_dt$Perc_WUI_Housing = 100 * temp_dt$Prop_WUI_Housing
temp_dt$Burned_Prop_Past5yrs = as.numeric(temp_dt$Burned_Prop_Past5yrs)
temp_dt$Burned_Perc_Past5yrs = temp_dt$Burned_Prop_Past5yrs * 100
temp_dt$ACRES = admin_districts$GIS_ACRES[match(temp_dt$FOREST_ID,admin_districts$FOREST_ID)]
temp_dt$demCongress[temp_dt$demCongress==2] <- 0
temp_dt$congress = as.character(temp_dt$congress)
temp_dt$nominate_dim1  = temp_dt$nominate_dim1  * -1
temp_dt$nominate_dim2  = temp_dt$nominate_dim2  * -1
temp_dt$mrp_mean = temp_dt$mrp_mean * -1

temp_dt$ComLCV = (temp_dt$nrComLCV+temp_dt$agComLCV)/2
temp_dt$ChairLCV = (temp_dt$nrChairLCV + temp_dt$agChairLCV)/2

temp_dt$Ln_ACRES = log(temp_dt$ACRES)
temp_dt$Ln_AVERAGE_YEARLY_VISITS = log(temp_dt$Average_Yearly_Visits)

library(zoo)
temp_dt = temp_dt[order(FOREST_ID,CALENDAR_YEAR,congress),]
temp_dt = temp_dt[order(FOREST_ID,CALENDAR_YEAR,congress), zoo::na.locf(.SD, na.rm = FALSE),by = .(FOREST_ID)]
temp_dt = temp_dt[order(FOREST_ID,CALENDAR_YEAR,congress), na.locf(.SD, na.rm = FALSE,fromLast=TRUE),by = .(FOREST_ID)]
temp_dt$congress = as.numeric(temp_dt$congress)
temp_dt$ln_County_naturalresource_GDP_1M = log(temp_dt$NaturalResources1M+1)
temp_dt$Prop_Extraction_Employ = temp_dt$Prop_NaturalResourceEmployment
temp_dt$Perc_Extraction_Employ = temp_dt$Prop_Extraction_Employ * 100


temp_dt$`Avg_MBF_Cut_1999-2004`[is.na(temp_dt$`Avg_MBF_Cut_1999-2004`)]<-0
temp_dt$Ln_Avg_MBF_Cut_1999_2004 = log(temp_dt$`Avg_MBF_Cut_1999-2004`+0.001)
temp_dt$ln_Receipts_Extraction_1M_P4 <- log(temp_dt$Receipts_TimberMineralsGrazing_P4/1e6+1)
temp_dt$ln_Receipts_Recreation_1M_P4 <- log(temp_dt$Receipts_Recreation_P4/1e6+1)
temp_dt = temp_dt[order(FOREST_ID,CALENDAR_YEAR),Unemp_Rate:=lag(LAU_October),by = .(FOREST_ID)]

fwrite(temp_dt,'policypolitics/prepped/national_forest_covariates.csv')

