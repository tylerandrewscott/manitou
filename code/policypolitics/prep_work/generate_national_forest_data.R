
# load packages, install if not installed already
if(!require(data.table)){install.packages('data.table');require(data.table)}
if(!require(stringr)){install.packages('stringr');require(stringr)}
if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
if(!require(sf)){install.packages('sf');require(sf)}
if(!require(lwgeom)){install.packages('lwgeom');require(lwgeom)}
if(!require(ggthemes)){install.packages('ggthemes');require(ggthemes)}
if(!require(lubridate)){install.packages('lubridate');require(lubridate)}
if(!require(pbapply)){install.packages('pbapply');require(pbapply)}


first_year = 2000;last_year = 2020
# projection to use for all spatial data
albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

# create a temporary folder that stores downloaded shapefile, extract contents and load
td = tempdir()
admin_url ="https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip"
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(admin_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
admin_districts <- st_read(fpath)
# convert to common projection
admin_districts <- st_transform(admin_districts,crs = st_crs(albersNA))
# fix bad polygons
bad_polys = !st_is_valid(admin_districts)
admin_districts[bad_polys,] <- st_make_valid(admin_districts[bad_polys,])

admin_districts$FORESTORGC = as.character(admin_districts$FORESTORGC)
admin_districts$FOREST_ID = admin_districts$FORESTORGC
admin_districts$FOREST_ID = formatC(admin_districts$FORESTORGC,width=4,flag = 0)
#admin_districts$FOREST_ID = formatC(admin_districts$FORESTORGC,width=4,flag = 0)

congress_ids = data.table(congress = rep(101:116,each=2),CALENDAR_YEAR = 1989:2020)
# make every forest/year combination
tdt = expand.grid(sort(unique(as.character(admin_districts$FOREST_ID))),first_year:last_year)
tdt = data.table(tdt)
setnames(tdt,c('FOREST_ID',"CALENDAR_YEAR"))
temp_dt = tdt
#create forest/year/congress table
temp_dt = data.table(left_join(temp_dt,congress_ids))



### this loads other land units that aren't primary administrative units
# lmu_url = "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.NFSLandUnit.zip"
# tf = tempfile(tmpdir=td, fileext=".zip")
# download.file(lmu_url, tf)
# fname = unzip(tf, list=TRUE)
# unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
# fpath = file.path(td, grep('shp$',fname$Name,value=T))
# lmus <- st_read(fpath)
# lmus <- st_transform(lmus,crs = st_crs(albersNA))
# lmus <- st_make_valid(lmus)

#### ranger district x county overlap values
#### reads in pre-made file with % overlap between forests and counties
county_over = fread('input/gis_overlap_props/forest_county_overlap_props.csv')
county_over$FOREST_ID = formatC(county_over$FOREST_ID,width=4,flag=0)
county_over$CFIPS = formatC(county_over$CFIPS,width=5,flag=0)
county_over$Prop_Overlap = round(county_over$Prop_Overlap,3)
county_over <- county_over[county_over$Prop_Overlap>0,]


### reads in pre-made file with natural resource gdp by county
county_nr = fread('input/cpb_data/naturalresource_gdp_by_county_2002-2018.csv')
county_nr$CFIPS = formatC(county_nr$CFIPS,width = 5,flag = 0)
county_nr[,STATE:=NULL]
county_nr$NaturalResources1M <- county_nr$NaturalResources1k * 1000 /1e6

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
forest_weighted_econ = county_econ[,lapply(.SD,weighted.mean,w=Prop_Overlap), by=.(FOREST_ID,YEAR),.SDcols = c("LAU_January","LAU_October","NaturalResources1M",'Prop_NaturalResourceEmployment')]
setnames(forest_weighted_econ,'YEAR','CALENDAR_YEAR')
forest_weighted_econ$CALENDAR_YEAR = as.numeric(forest_weighted_econ$CALENDAR_YEAR)
temp_dt$CALENDAR_YEAR <- as.numeric(temp_dt$CALENDAR_YEAR)
setkey(forest_weighted_econ,FOREST_ID,CALENDAR_YEAR)
setkey(temp_dt,FOREST_ID,CALENDAR_YEAR)
temp_dt = left_join(temp_dt,forest_weighted_econ)
temp_dt = data.table(temp_dt)



# 
# library(readxl)
# receipts = data.table(read_excel('input/usfs_internal_data/USFSGrossReceipts_cleaned.xlsx',skip = 3))
# receipts$`National Forest Code` = formatC(receipts$`National Forest Code`,width = 4,flag = 0)
# #receipts = receipts[Year>=2002,]
# setnames(receipts,c("National Forest Code","Year"),c('FOREST_ID',"CALENDAR_YEAR"))
# 
# receipts = receipts[,.(CALENDAR_YEAR,FOREST_ID,`Inflation Adjusted Class1 - Timber`,`Inflation Adjusted Class2 - Grazing East`,`Inflation Adjusted Class8 - Grazing West`,
#                        `Inflation Adjusted Class3 - Land Use`,`Inflation Adjusted Class7 - Recreation User Fees`,`Inflation Adjusted Class6 - Minerals`,
#                        `Inflation Adjusted Class4 - Recreation Special Uses`, `Inflation Adjusted Class5 - Power`,`Inflation Adjusted Total NFF`)]
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


library(zoo)
#### reads in county percentD_H vote ####
county_voting = readRDS('input/politics/countyVoteShare_3-2020_imputed.rds')
setnames(county_voting,'FISCAL_YEAR','CALENDAR_YEAR')
county_forest_voting = data.table(left_join(county_voting,county_over))
# NAs are cases where county didn't actually overlap
county_forest_voting = county_forest_voting[!is.na(FOREST_ID),]
forest_voting_weighted = county_forest_voting[,weighted.mean(percentD_H,Prop_Overlap),by=.(FOREST_ID,congress,CALENDAR_YEAR)]
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

wui_housing_units = county_wui_over[,lapply(.SD,weighted.mean,w=Prop_Overlap,na.rm=T), by=.(FOREST_ID),.SDcols = 'Prop_WUI_Housing']
#setnames(wui_housing_units,'V1','WUI_Housing_Units')

temp_dt = data.table(left_join(temp_dt,wui_housing_units))

###### add house representative variables
house = readRDS('input/politics/houseAtts_5-2019.RDS')
house = data.table(house)
house= house[!duplicated(paste0(year,stateDistrict)),]
setnames(house,'year','CALENDAR_YEAR')
house$Congressional_District_ID <- formatC(house$Congressional_District_ID,width=4,flag = 0)
setkey(house,CALENDAR_YEAR,Congressional_District_ID)
house_overs = fread('input/gis_overlap_props/nationalforest_congressdistrict_overlap_props.csv')
house_overs$FOREST_ID = formatC(house_overs$FOREST_ID,width=4,flag = 0)
house_overs$Congressional_District_ID <- formatC(gsub('00$','01',house_overs$Congressional_District_ID),width=4,flag=0)
setnames(house_overs,'Year','CALENDAR_YEAR')
setkey(house_overs,CALENDAR_YEAR,Congressional_District_ID)
house_dt = data.table(left_join(house,house_overs))
house_dt$Prop_Overlap = round(house_dt$Prop_Overlap,2)
house_dt = house_dt[house_dt$Prop_Overlap>0,]
pol_cols = c("LCV_annual","LCV_lifetime","democrat","nominate_dim1",'nominate_dim2','demPres','demCongress','agChairLCV','agChairDW1','nrChairDW1','nrChairLCV','agComLCV','agComDW1','nrComLCV','nrComDW1')

forest_house_values = house_dt[,lapply(.SD,weighted.mean,w=Prop_Overlap,na.rm=T), by=.(FOREST_ID,CALENDAR_YEAR,congress),.SDcols = pol_cols]
forest_house_values$CALENDAR_YEAR = as.numeric(forest_house_values$CALENDAR_YEAR)
setkey(forest_house_values,'FOREST_ID','CALENDAR_YEAR')
setkey(temp_dt,'FOREST_ID','CALENDAR_YEAR')

temp_dt = left_join(temp_dt,forest_house_values)
temp_dt = data.table(temp_dt)
temp_dt = temp_dt[CALENDAR_YEAR>=first_year&CALENDAR_YEAR<=last_year]
temp_dt = temp_dt[!is.na(congress),]

use = readRDS('input/prepped/forestUseAverage_2005-2014.RDS')
use$FOREST_ID = paste0(use$REGION,use$FORESTNUMB)

temp_dt$Average_Yearly_Visits = use$visits[match(temp_dt$FOREST_ID,use$FOREST_ID)]


#########
temp_dt$index_in_forestshp = match(temp_dt$FOREST_ID,admin_districts$FOREST_ID)
temp_dt$GIS_Area = st_area(admin_districts)[temp_dt$index_in_forestshp]


#### wildfire burn overlay
#wildfire_burn_url = 'https://wildfire.cr.usgs.gov/firehistory/data/wf_usfs_1980_2016.zip'
wildfire_burn_url = 'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.MTBS_BURN_AREA_BOUNDARY.zip'
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(wildfire_burn_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
wildfire_burn <- st_read(fpath)
wildfire_burn <- wildfire_burn[wildfire_burn$FIRE_TYPE!='Prescribed Fire',]
wildfire_burn <- st_transform(wildfire_burn,crs = st_crs(albersNA))
wildfire_burn  <- st_make_valid(wildfire_burn)
setnames(wildfire_burn,'YEAR','CALENDAR_YEAR')
wildfire_burn=wildfire_burn[wildfire_burn$CALENDAR_YEAR>=1999,]
temp_dt$CALENDAR_YEAR = as.numeric(temp_dt$CALENDAR_YEAR)

forest_year_combos = expand.grid(FOREST_ID = unique(temp_dt$FOREST_ID),CALENDAR_YEAR = 2004:2018)
forest_year_combos$Area_Burned_P5 = NA


wildfire_index = sapply(seq_along(temp_dt$CALENDAR_YEAR),function(x) which(wildfire_burn$CALENDAR_YEAR<temp_dt$CALENDAR_YEAR[x]&wildfire_burn$CALENDAR_YEAR>(temp_dt$CALENDAR_YEAR[x]-6)))
wildfire_inters = st_intersects(admin_districts,wildfire_burn)
wildfire_intersections = mapply(function(x,y) intersect(x,y),x = wildfire_inters[temp_dt$index_in_forestshp],wildfire_index)
total_burn_area_past5yrs = pbsapply(1:nrow(temp_dt),function(i){if(length(wildfire_intersections[[i]])==0){0}
  else{sum(st_area(st_intersection(admin_districts[temp_dt$index_in_forestshp[i],],wildfire_burn[wildfire_intersections[[i]],])))}},cl = 10)
temp_dt$Burned_Area_Past5yrs = total_burn_area_past5yrs
temp_dt$Burned_Prop_Past5yrs = as.numeric(temp_dt$Burned_Area_Past5yrs/temp_dt$GIS_Area)


###### wilderness area overlay
wilderness_area_url = 'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.Wilderness.zip'
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(wilderness_area_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
wilderness_area <- st_read(fpath)
wilderness_area <- st_transform(wilderness_area,crs = st_crs(albersNA))
wilderness_area  <- st_make_valid(wilderness_area)

desig_years_url = 'https://www.wilderness.net/GIS/Wilderness_Areas.zip'
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(desig_years_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
desig_years <- st_read(fpath)
desig_years_fs = desig_years[desig_years$Agency=='FS',]

wilderness_area$DESIG_YEAR = desig_years$YearDesign[match(formatC(wilderness_area$WID,width=3,flag=0),formatC(desig_years_fs$WID,width=3,flag=0))]
wilderness_area$DESIG_YEAR[wilderness_area$WILDERNE_1=='Granite Mountain Wilderness (CA)']<-2009

over_wilderness = st_intersects(admin_districts,wilderness_area)
wilderness_index = sapply(seq_along(temp_dt$CALENDAR_YEAR),function(x) which(wilderness_area$DESIG_YEAR<temp_dt$CALENDAR_YEAR[x]))
wilderness_inters = st_intersects(admin_districts,wilderness_area)
names(wilderness_inters) <- admin_districts$FOREST_ID
wilderness_intersections = mapply(function(x,y) {if(length(x)==0){NA}else{intersect(x,y)}},x = wilderness_inters[temp_dt$index_in_forestshp],y = wilderness_index)
total_wilderness_area = pbsapply(1:nrow(temp_dt),function(i){if(length(wilderness_intersections[[i]])==0){0}
  else{sum(st_area(st_intersection(admin_districts[temp_dt$index_in_forestshp[i],],st_union(wilderness_area[wilderness_intersections[[i]],]))))}},cl = 10)

temp_dt$Wilderness_Area = unlist(total_wilderness_area)
temp_dt$Wilderness_Prop = temp_dt$Wilderness_Area/temp_dt$GIS_Area


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
limit_use$CALENDAR_YEAR = year(limit_use$ACTIONDATE) + (month(limit_use$ACTIONDATE)>=10 + 0)

limit_use_index = sapply(seq_along(temp_dt$CALENDAR_YEAR),function(x) which(limit_use$CALENDAR_YEAR<temp_dt$CALENDAR_YEAR[x]))
limit_use_inters = st_intersects(admin_districts,limit_use)
limit_use_intersections = mapply(function(x,y) intersect(x,y),x = limit_use_inters[temp_dt$index_in_forestshp],limit_use_index)
total_limited_use_area = pbsapply(1:nrow(temp_dt),function(i){if(length(limit_use_intersections[[i]])==0){0}
  else{sum(st_area(st_intersection(admin_districts[temp_dt$index_in_forestshp[i],],st_union(limit_use[limit_use_intersections[[i]],]))))}},cl = 10)
temp_dt$Limited_Use_Area = total_limited_use_area
temp_dt$Limited_Use_Prop = temp_dt$Limited_Use_Area/temp_dt$GIS_Area


esa = fread('input/usfs_internal_data/nfs_tep_group_species_sort_11jul08.csv')

esa$NF = esa$`Forest and or Grassland with Species`
esa$NF <- gsub('NFs','National Forests',esa$NF)
esa$NF <- gsub('NF','National Forest',esa$NF)
esa$NF <- gsub('Grasslnds','Grasslands',esa$NF)
esa$NF <- gsub('\\/','-',esa$NF)
esa = esa[NF!='']

esa$NF[grepl('Alabama',esa$NF)] <- "National Forests in Alabama"
esa$NF[grepl('North Carolina',esa$NF)] <- "National Forests in North Carolina"
esa$NF[grepl('Mississippi',esa$NF)] <- "National Forests in Mississippi"
esa$NF[grepl('Florida',esa$NF)] <- "National Forests in Florida"
esa$NF[grepl('Texas',esa$NF)] <- "National Forests in Texas"
esa$NF[grepl('Finger Lakes|Green Mountain',esa$NF)] <- "Green Mountain and Finger Lakes National Forests"
esa$NF[grepl('Custer|Gallatin',esa$NF)] <- "Custer Gallatin National Forest"
esa$NF[grepl('Helena|Lewis-Clark',esa$NF)] <- "Helena-Lewis and Clark National Forest"
esa$NF[grepl('Uinta|Wasatch|Cache',esa$NF)] <- "Uinta-Wasatch-Cache National Forest"
esa$NF[grepl('Salmon|Challis',esa$NF)] <-"Salmon-Challis National Forest"
esa$NF[grepl('Nebraska',esa$NF)] <-"Nebraska National Forest"
esa$NF[grepl('Nez|Clearwater',esa$NF)] <-"Nez Perce-Clearwater National Forest"
esa$NF[grepl('Humboldt|Toiyabe',esa$NF)] <-"Humboldt-Toiyabe National Forest"
esa$NF[grepl('Caribou|Targhee',esa$NF)] <-"Caribou-Targhee National Forest"
esa$NF[grepl('Arapaho|Roosev',esa$NF)] <-"Arapaho and Roosevelt National Forests"
esa$NF[grepl('Midewin',esa$NF)] <-"Midewin National Tallgrass Prairie"
esa$NF[grepl('Rogue|Siskiyou',esa$NF)] <-"Rogue River-Siskiyou National Forests"
esa$NF[grepl('Baker|Snoqualmie',esa$NF)] <-"Mt. Baker-Snoqualmie National Forest"
esa$NF[grepl('Huron|Manistee',esa$NF)] <-"Huron-Manistee National Forest"
esa$NF[grepl('Tahoe Basin MU',esa$NF)] <-"Lake Tahoe Basin Management Unit"
esa$NF[grepl('Yunque',esa$NF)] <-"El Yunque National Forest" 
esa$NF[grepl('Land Between',esa$NF)] <-"Land Between the Lakes National Recreation Area" 
esa$NF[grepl('Pike|San Isabel',esa$NF)] <- "Pike and San Isabel National Forests"
esa$NF[grepl('Manti|LaSal|La Sal',esa$NF)] <-"Manti-La Sal National Forest"
esa$NF[grepl('Medicine Bow|Routt',esa$NF)] <-"Medicine Bow-Routt National Forest"
esa$NF[grepl('Uncompahgre|Gunnison',esa$NF)] <- "Grand Mesa, Uncompahgre and Gunnison National Forests"
switch = which(!esa$NF %in% admin_districts$FORESTNAME & paste(esa$NF,'National Forest') %in% admin_districts$FORESTNAME)
esa$NF[switch] <- paste(esa$NF[switch],'National Forest')

switch = which(!esa$NF %in% admin_districts$FORESTNAME & paste(esa$NF,'National Forests') %in% admin_districts$FORESTNAME)
esa$NF[switch] <- paste(esa$NF[switch],'National Forests')
esa_counts = esa[,.N,by = .(NF)]
setnames(esa_counts,'N','Count_EorT_Species')
esa_counts$FOREST_ID = admin_districts$FOREST_ID[match(esa_counts$NF,admin_districts$FORESTNAME)]
esa_counts[,NF:=NULL]

#https://www.fs.usda.gov/Internet/FSE_DOCUMENTS/fsbdev2_037582.pdf
ak_counts = data.table(FOREST_ID=c('1004','1005'),Count_EorT_Species = c(18,20))
esa_counts = rbind(esa_counts,ak_counts,use.names=T)

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
 mineral_rights_url = 'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.MINERALRIGHT.zip'
 td = tempdir()
 tf = tempfile(tmpdir=td, fileext=".zip")
 download.file(mineral_rights_url, tf)
 fname = unzip(tf, list=TRUE)
 unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
 fpath = file.path(td, grep('shp$',fname$Name,value=T))
 mineral_rights <- st_read(fpath)
 mineral_rights <- mineral_rights[!is.na(mineral_rights$ACTIONDATE),]
 mineral_rights <- st_transform(mineral_rights,crs = st_crs(albersNA))
 mineral_rights  <- st_make_valid(mineral_rights)
 mineral_rights$CALENDAR_YEAR = mineral_rights$ACTIONFISC
 mineral_rights <- mineral_rights[ymd(mineral_rights$ACTIONDATE)  < ymd('2004-01-01'),]
 mineral_rights = mineral_rights[!is.na(mineral_rights$ACTIONDATE),]
 mineral_rights = mineral_rights[year(ymd(mineral_rights$ACTIONDATE)) %in% 1993:2004,]
 over_minerals = st_intersects(admin_districts,mineral_rights)
 admin_districts$MINING_CLAIM_ACTIONS_1993_2004<- sapply(over_minerals,length)
 temp_dt$MINING_CLAIM_ACTIONS_1993_2004 <- admin_districts$MINING_CLAIM_ACTIONS_1993_2004[match(temp_dt$FOREST_ID,admin_districts$FOREST_ID)]
 
# mineral_index = sapply(seq_along(temp_dt$CALENDAR_YEAR),function(x) which(mineral_rights$CALENDAR_YEAR<temp_dt$CALENDAR_YEAR[x]))
# mineral_inters = st_intersects(admin_districts,mineral_rights)
# mineral_intersections = mapply(function(x,y) {if(length(x)==0){NA}else{intersect(x,y)}},x = mineral_inters[temp_dt$index_in_forestshp],y = mineral_index)
# total_mineral_rights = pbsapply(1:nrow(temp_dt),function(i){if(length(mineral_intersections[[i]])==0){0}
#   else{sum(st_area(st_intersection(admin_districts[temp_dt$index_in_forestshp[i],],st_union(mineral_rights[mineral_intersections[[i]],]))))}},cl = 10)
# temp_dt$Mineral_Rights_Area = total_mineral_rights
# temp_dt$Mineral_Rights_Prop = temp_dt$Mineral_Rights_Area/temp_dt$GIS_Area
# # 
# # 
# ##### overlay rangeland on ranger districts
allotments_url = 'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.Allotment.zip'
# # create a temporary directory
td = tempdir()
# # create the placeholder file
tf = tempfile(tmpdir=td, fileext=".zip")
# # download into the placeholder file
download.file(allotments_url, tf)
# # get the name of the first file in the zip archive
fname = unzip(tf, list=TRUE)
# # unzip the file to the temporary directory
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
# # fpath is the full path to the extracted file
fpath = file.path(td, grep('shp$',fname$Name,value=T))
allotments <- st_read(fpath)
allotments <- st_transform(allotments,crs = st_crs(albersNA))
allotments  <- st_make_valid(allotments)
allotments$CALENDAR_YEAR = allotments$NEPA_DEC_A
allotments = allotments[allotments$CALENDAR_YEAR %in% 1999:2004,]
over_allotments = st_intersects(admin_districts,allotments)
admin_districts$ALLOTMENT_NEPA_1993_2004 <- sapply(over_allotments,length)
temp_dt$ALLOTMENT_NEPA_1993_2004 <- admin_districts$ALLOTMENT_NEPA_1993_2004[match(temp_dt$FOREST_ID,admin_districts$FOREST_ID)]

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
#   }},cl = 10)
# 
# total_allotments = zoo::na.locf(total_allotments)
# 
# temp_dt$Allotments_Area = total_allotments
# temp_dt$Allotments_Prop = temp_dt$Allotments_Area/temp_dt$GIS_Area
# temp_dt$Allotments_Prop[temp_dt$Allotments_Prop>1]<- 1


eco_sections_url = 'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.EcoMapProvinces.zip'
td = tempdir()
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(eco_sections_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
eco_sections <- st_read(fpath)
eco_sections <- st_transform(eco_sections,crs = st_crs(albersNA))
eco_sections  <- st_make_valid(eco_sections)
over_eco_sections = st_intersects(admin_districts,eco_sections)

temp_dt$Num_Eco_Sections = sapply(over_eco_sections,length)[temp_dt$index_in_forestshp]
# 

#temp_dt[!is.na(County_naturalresource_GDP),.N,by=.(CALENDAR_YEAR)]
fwrite(temp_dt,'input/prepped/national_forest_covariates.csv')

