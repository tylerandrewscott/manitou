
library(sf)
library(lwgeom)
library(sp)
library(rgdal)
library(stringr)
library(data.table)


library(lwgeom)
library(tidyverse)
library(pbapply)

albersNA = aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
td = tempdir()
counties_url = 'https://www2.census.gov/geo/tiger/TIGER2018/COUNTY/tl_2018_us_county.zip'
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(counties_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
counties <- st_read(fpath)
counties <- st_transform(counties,albersNA)
counties <- st_make_valid(counties)

ranger_url ="https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.RangerDistrict.zip"
td = tempdir()
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(ranger_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
ranger_districts <- st_read(fpath)
ranger_districts <- st_transform(ranger_districts,crs = st_crs(albersNA))
ranger_districts  <- st_make_valid(ranger_districts)
ranger_districts$DISTRICTNA = as.character(ranger_districts$DISTRICTNA)
ranger_districts$RANGERDIST = as.character(ranger_districts$RANGERDIST)
ranger_districts$FORESTNAME = as.character(ranger_districts$FORESTNAME)
ranger_districts$DISTRICTOR = as.character(ranger_districts$DISTRICTOR)
ranger_districts$DISTRICT_ID <- str_extract(ranger_districts$DISTRICTOR,'[0-9]{6}$')

ranger_county_overs = st_intersects(ranger_districts,counties)
ranger_county_props = pblapply(seq_along(ranger_county_overs),function(i){
  props = st_area(st_intersection(ranger_districts[i,],counties[ranger_county_overs[[i]],]))/st_area(ranger_districts[i,])
  data.table(DISTRICT_ID = ranger_districts$DISTRICT_ID[i],CFIPS = as.character(counties$GEOID[ranger_county_overs[[i]]]),Prop_Overlap = as.numeric(props))
},cl = 4)
ranger_county_dt = rbindlist(ranger_county_props)
fwrite(ranger_county_dt,file = 'input/gis_overlap_props/rangerdistrict_county_overlap_props.csv')

admin_url ="https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip"
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(admin_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
admin_districts <- st_read(fpath)
admin_districts <- st_transform(admin_districts,crs = st_crs(albersNA))
admin_districts  <- st_make_valid(admin_districts)
admin_districts$FORESTORGC = as.character(admin_districts$FORESTORGC)
admin_districts$FOREST_ID = admin_districts$FORESTORGC
admin_districts$FOREST_ID = formatC(admin_districts$FORESTORGC,width=4,flag = 0)

forest_county_overs = st_intersects(admin_districts,counties)
forest_county_props = pblapply(seq_along(forest_county_overs),function(i){
  props = st_area(st_intersection(admin_districts[i,],counties[forest_county_overs[[i]],]))/st_area(admin_districts[i,])
  data.table(FOREST_ID = admin_districts$FOREST_ID[i],CFIPS = as.character(counties$GEOID[forest_county_overs[[i]]]),Prop_Overlap = as.numeric(props))
},cl = 4)
forest_county_dt = rbindlist(forest_county_props)

fwrite(forest_county_dt,file = 'input/gis_overlap_props/forest_county_overlap_props.csv')


download.file(counties_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
temp <- tempfile()

district_zips = paste0('http://cdmaps.polisci.ucla.edu/shp/districts',formatC(091:114,width = 3,flag = 0),'.zip')
district_zips2 = c('https://www2.census.gov/geo/tiger/TIGER2016/CD/tl_2016_us_cd115.zip','https://www2.census.gov/geo/tiger/TIGER2018/CD/tl_2018_us_cd116.zip')
district_zips = c(district_zips,district_zips2)
congress_years = data.table(Congress = formatC(rep(091:116,each=2),width = 3,flag=0),Year = 1969:2020)
for(i in district_zips){
  tf = tempfile(tmpdir=td, fileext=".zip")
  download.file(i, tf)
  fname = unzip(tf, list=TRUE)
  unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
  fpath = file.path(td, grep('shp$',fname$Name,value=T))
}


flist = list.files('./districtShapes','shp',full.names = T)
flist2 = list.files(td,pattern = 'us_cd[0-9]{3}.shp$',full.names=T)
flist = as.list(c(flist,flist2))
#cat <- unzip(temp)
names(flist) <- congress_years$Year[match(gsub('[a-z]','',str_extract(flist,'(cd|districts)[0-9]{2,3}')),congress_years$Congress)]
congress_shape_list = pblapply(flist,function(x) {
  st_make_valid(st_transform(st_read(x),crs = st_crs(albersNA)))},cl = 8)

names(congress_shape_list) <- names(flist)

for(x in 1:length(congress_shape_list)){
  if(any(colnames(congress_shape_list[[x]])=='DISTRICT')){
    congress_shape_list[[x]]$state.fips = formatC(tigris::fips_codes$state_code[match(congress_shape_list[[x]]$STATENAME,tigris::fips_codes$state_name)],width=2,flag = 0)
    congress_shape_list[[x]]$DISTRICT_ID = paste0(congress_shape_list[[x]]$state.fips,formatC(as.numeric(as.character(congress_shape_list[[x]]$DISTRICT)),width=2,flag =0))}
 else{congress_shape_list[[x]]$DISTRICT_ID <- congress_shape_list[[x]]$GEOID}}

states = tigris::states(class = 'sf')
states = st_transform(states,st_crs(albersNA))
ranger_states = st_intersects(ranger_districts,states)
forest_states = st_intersects(admin_districts,states)
ranger_districts$STATES = sapply(ranger_states,function(x) states$STUSPS[x])
admin_districts$STATES = sapply(forest_states,function(x) states$STUSPS[x])

years = 2000:2020
fips = tigris::fips_codes
congress_shape_list$`2017`$DISTRICT_ID = as.character(congress_shape_list$`2017`$GEOID)
congress_shape_list$`2019`$DISTRICT_ID = as.character(congress_shape_list$`2019`$GEOID)
congress_shape_list$`2017`$STATENAME=fips$state_name[match(congress_shape_list$`2017`$STATEFP,fips$state_code)]
congress_shape_list$`2019`$STATENAME=fips$state_name[match(congress_shape_list$`2019`$STATEFP,fips$state_code)]
  
yearly_overlap = lapply(years,function(j){
  print(j)
  if(is.null(congress_shape_list[[as.character(j)]])){temp_congress = congress_shape_list[[as.character(j-1)]]}else{temp_congress = congress_shape_list[[as.character(j)]]}
  ranger_congress_overs = st_intersects(ranger_districts,temp_congress)
temp = pblapply(seq_along(ranger_congress_overs),function(i){
  if(length(ranger_congress_overs[[i]])>0){
    if(any(ranger_districts$STATES[[i]]=='AK'))
    {data.table(DISTRICT_ID = ranger_districts$DISTRICT_ID[i],Congressional_District_ID = as.character(temp_congress$DISTRICT_ID[which(temp_congress$STATENAME=='Alaska')]),Prop_Overlap = 1)}
    else{
    props = st_area(st_intersection(ranger_districts[i,],temp_congress[ranger_congress_overs[[i]],]))/st_area(ranger_districts[i,])
    data.table(DISTRICT_ID = ranger_districts$DISTRICT_ID[i],Congressional_District_ID = as.character(temp_congress$DISTRICT_ID[ranger_congress_overs[[i]]]),Prop_Overlap = as.numeric(props))
    }
    }
  },cl = 8)
temp_dt = rbindlist(temp)
temp_dt$Year = j
temp_dt
})
ranger_congress_dt = rbindlist(yearly_overlap)
ranger_congress_dt$Congressional_District_ID = gsub('00$','01',ranger_congress_dt$Congressional_District_ID)
fwrite(ranger_congress_dt,file = 'input/gis_overlap_props/rangerdistrict_congressdistrict_overlap_props.csv')


yearly_overlap2 = lapply(years,function(j){
  print(j)
  if(is.null(congress_shape_list[[as.character(j)]])){temp_congress = congress_shape_list[[as.character(j-1)]]}else{temp_congress = congress_shape_list[[as.character(j)]]}
  admin_congress_overs = st_intersects(admin_districts,temp_congress)
  temp = pblapply(seq_along(admin_congress_overs),function(i){
    if(length(admin_congress_overs[[i]])>0){
      if(any(admin_districts$STATES[[i]]=='AK'))
      {data.table(FOREST_ID = admin_districts$FOREST_ID[i],Congressional_District_ID = as.character(temp_congress$DISTRICT_ID[which(temp_congress$STATENAME=='Alaska')]),Prop_Overlap = 1)}
      else{
        props = st_area(st_intersection(admin_districts[i,],temp_congress[admin_congress_overs[[i]],]))/st_area(admin_districts[i,])
        data.table(FOREST_ID = admin_districts$FOREST_ID[i],Congressional_District_ID = as.character(temp_congress$DISTRICT_ID[admin_congress_overs[[i]]]),Prop_Overlap = as.numeric(props))
      }
    }
  },cl = 8)
  temp_dt = rbindlist(temp)
  temp_dt$Year = j
  temp_dt
})


forest_congress_dt = rbindlist(yearly_overlap2)
forest_congress_dt$Congressional_District_ID = gsub('00$','01',forest_congress_dt$Congressional_District_ID)
fwrite(forest_congress_dt,file = 'input/gis_overlap_props/nationalforest_congressdistrict_overlap_props.csv')
