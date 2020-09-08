
# load packages, install if not installed already
if(!require(data.table)){install.packages('data.table');require(data.table)}
if(!require(stringr)){install.packages('stringr');require(stringr)}
if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
if(!require(sf)){install.packages('sf');require(sf)}
if(!require(lwgeom)){install.packages('lwgeom');require(lwgeom)}
if(!require(ggthemes)){install.packages('ggthemes');require(ggthemes)}
if(!require(lubridate)){install.packages('lubridate');require(lubridate)}
if(!require(pbapply)){install.packages('pbapply');require(pbapply)}
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
admin_districts = admin_districts[admin_districts$FORESTNAME!='El Yunque National Forest',]


#admin_districts$FOREST_ID = formatC(admin_districts$FORESTORGC,width=4,flag = 0)
congress_ids = data.table(congress = rep(108:115,each=2),FISCAL_YEAR = 2003:2018)
county_voting = readRDS('input/politics/countyVoteShare_3-2020.rds')
county_voting = as.data.table(county_voting)
county_voting = county_voting[!is.na(county_voting$GEOID),]
county_voting = county_voting[,.(percentD_H,year,GEOID)]
setnames(county_voting,c('percentD_H','year','GEOID'),
         c('percentD_H','FISCAL_YEAR','CFIPS'))
fp = tigris::fips_codes
fp$CFIPS = paste0(fp$state_code,fp$county_code)
county_voting$STATE = fp$state[match(county_voting$CFIPS,fp$CFIPS)]
county_voting$congress = congress_ids$congress[match(county_voting$FISCAL_YEAR,congress_ids$FISCAL_YEAR)]

counties = tigris::counties(year = 2015,class = 'sf')
counties <- st_transform(counties,crs = st_crs(albersNA))
over_a_forest = st_intersects(counties,admin_districts)
counties = counties[sapply(over_a_forest,length)!=0,]


#only bother with counties that overlap a national forest
county_voting = county_voting[county_voting$CFIPS %in% counties$GEOID,]

election_shapes_urls = paste0('http://cdmaps.polisci.ucla.edu/shp/districts',108:114,'.zip')
names(election_shapes_urls )<-108:114
list_of_districts = lapply(election_shapes_urls,function(x){
  temp_url = x
  tf = tempfile(tmpdir=td, fileext=".zip")
  download.file(temp_url, tf)
  fname = unzip(tf, list=TRUE)
  unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
  fpath = file.path(td, grep('shp$',fname$Name,value=T))
  election_districts <- st_read(fpath)
  election_districts <- st_transform(election_districts,crs = st_crs(albersNA))
  election_districts$DISTRICT = as.numeric(as.character(election_districts$DISTRICT))
  election_districts$STARTCONG = as.numeric(as.character(election_districts$STARTCONG))
  election_districts$ENDCONG = as.numeric(as.character(election_districts$ENDCONG))
  election_districts[,c('geometry','STARTCONG','ENDCONG','DISTRICT','STATENAME')]
})

districts = do.call(rbind,list_of_districts)
districts = districts[!duplicated(districts),]

fips_codes = tigris::fips_codes
congress115 = 'https://www2.census.gov/geo/tiger/TIGER2016/CD/tl_2016_us_cd115.zip'
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(congress115, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
districts115 <- st_read(fpath)
districts115  <- st_transform(districts115 ,crs = st_crs(albersNA))
districts115 = districts115[districts115$CD115FP!='ZZ',]
districts115$DISTRICT = as.numeric(districts115$CD115FP)
districts115$STATENAME = fips_codes$state_name[match(districts115$STATEFP,fips_codes$state_code)]
districts115$SID = paste0(districts115$STATE,districts115$DISTRICT)
districts115_over_forest <- st_intersects(districts115,admin_districts)
districts115 <- districts115[sapply(districts115_over_forest,length)>0,]
districts115$STARTCONG = 115
districts115$ENDCONG = 115

districts = rbind(districts,
      districts115[,c('geometry','STARTCONG','ENDCONG','DISTRICT','STATENAME')])
districts$STATE = state.abb[match(districts$STATENAME,state.name)]

districts$DISTRICT[districts$DISTRICT==0]<-1
districts$SID = paste0(districts$STATE,districts$DISTRICT)

district_over_forest = st_intersects(districts,admin_districts)
districts  = districts[sapply(district_over_forest,length)!=0,]

bad_districts = !st_is_valid(districts)
districts[bad_districts,] <- st_make_valid(districts[bad_districts,])


house_rep_voting = fread('input/politics/1976-2018-house.csv')
house_rep_voting = house_rep_voting[stage=='gen'&!writein,]
house_rep_voting$party[house_rep_voting$party=='democratic-farmer-labor'] <- 'democrat'
house_rep_voting = house_rep_voting[stage=='gen'&!writein,]
house_rep_voting$congress = congress_ids$congress[match(house_rep_voting$year+1,congress_ids$FISCAL_YEAR)]
house_rep_voting = house_rep_voting[!is.na(congress) & congress %in% c(108:115),]
no_dem_opponent = house_rep_voting[,sum(party=='democrat'),by=.(congress,state_po,district,year)][V1==0,]
no_dem_opponent$party='democrat'
no_dem_opponent$percentD_H <- 0
house_rep_voting = house_rep_voting[party=='democrat',]
house_rep_voting$percentD_H = as.numeric(house_rep_voting$candidatevotes)/as.numeric(house_rep_voting$totalvotes)
house_rep_voting = rbind(house_rep_voting,no_dem_opponent,use.names=T,fill=T)

house_rep_voting$district <- ifelse(house_rep_voting$district==0,1,house_rep_voting$district)
house_rep_voting$SID = paste0(house_rep_voting$state_po,house_rep_voting$district)


need_to_fill = which(is.na(county_voting$percentD_H))

replacement_vals = pbsapply(need_to_fill,function(i){
temp_districts = districts[districts$STATE==county_voting$STATE[i]&districts$STARTCONG<=county_voting$congress[i]&districts$ENDCONG>=county_voting$congress[i],]
if(nrow(temp_districts)==1){
  val = house_rep_voting[SID == temp_districts$SID&congress==county_voting$congress[i]&state_po == county_voting$STATE[i],]$percentD_H}
if(nrow(temp_districts)>1){
  over_districts = st_intersects(counties[counties$GEOID == county_voting$CFIPS[i],],temp_districts)
  overlap_props = st_area(st_intersection(counties[counties$GEOID == county_voting$CFIPS[i],],temp_districts))/
    st_area(counties[counties$GEOID == county_voting$CFIPS[i],])
  overlap_temp = data.table(overlap_props,SID = temp_districts$SID[unlist(over_districts)])
  overlap_temp$overlap_props = round( as.numeric(overlap_temp$overlap_props),3)
  overlap_temp = overlap_temp[overlap_props>0,]
  temp_house = house_rep_voting[congress==county_voting$congress[i]&state_po == county_voting$STATE[i],]
  temp_house = temp_house[SID %in% overlap_temp$SID,]
  temp_house$overlap = overlap_temp$overlap_props[match(temp_house$SID,overlap_temp$SID)]
  val = weighted.mean(temp_house$percentD_H,temp_house$overlap,na.rm=T)
}
if(nrow(temp_districts)==0){val = NA} 
val},cl = 10)


county_voting$percentD_H[need_to_fill] <-unlist(replacement_vals)


#ak_voting = data.table(congress = 109:115,percentD_H = rep(c(22.44,40.01,44.98,30.51,28.62,40.94,36.02)/100))
#nf$percentD_H[!is.na(nf$USFS_REGION) & nf$USFS_REGION==10] <- ak_voting$percentD_H[match(nf$congress[!is.na(nf$USFS_REGION) & nf$USFS_REGION==10],ak_voting$congress)]


saveRDS(county_voting,'input/politics/countyVoteShare_3-2020_imputed.rds')

# 
# county_over = fread('input/gis_overlap_props/forest_county_overlap_props.csv')
# county_over$FOREST_ID = formatC(county_over$FOREST_ID,width=4,flag=0)
# county_over$CFIPS = formatC(county_over$CFIPS,width=5,flag=0)
# county_over$Prop_Overlap = round(county_over$Prop_Overlap,3)
# county_over <- county_over[county_over$Prop_Overlap>0,]
# 
# 
# counties_over_congress = st_intersects(counties,districts)
# counties_congress_intersects = pblapply(seq_along(counties_over_congress),function(x) st_intersection(counties[x,],districts[unlist(counties_over_congress[[x]]),]),cl = 12)
# 
# 
# lapply(seq_along(nf_congress_intersects),function(x) nf_congress_intersects[[x]]$Prop_Over <<- st_area(nf_congress_intersects[[x]])/st_area(admin_districts[x,]))
# lapply(seq_along(nf_congress_intersects),function(x) {nf_congress_intersects[[x]]$FOREST_ID <<- admin_districts$FOREST_ID[x]})
# 
# 
# 
# districts
# st_intersection()
# 
# 
# 
# 
# nf_over_congress = st_intersects(admin_districts,districts)
# nf_congress_intersects = pblapply(seq_along(nf_over_congress),function(x) st_intersection(admin_districts[x,],districts[unlist(nf_over_congress[[x]]),]),cl = 12)
# lapply(seq_along(nf_congress_intersects),function(x) nf_congress_intersects[[x]]$Prop_Over <<- st_area(nf_congress_intersects[[x]])/st_area(admin_districts[x,]))
# lapply(seq_along(nf_congress_intersects),function(x) {nf_congress_intersects[[x]]$FOREST_ID <<- admin_districts$FOREST_ID[x]})
# 
# nf_congress_sf = do.call(rbind,nf_congress_intersects)
# nf_congress_dt = data.table(nf_congress_sf[,colnames(nf_congress_sf) %in% c('SID','congress','Prop_Over','FOREST_ID')])
# nf_congress_dt$Prop_Over = as.numeric(nf_congress_dt$Prop_Over)
# nf_congress_dt[,geometry:=NULL]
# 
# 
# nf_over_congress115 = st_intersects(admin_districts,districts115)
# 
# nf_congress115_intersects = pblapply(seq_along(nf_over_congress115),function(x) st_intersection(admin_districts[x,],districts115[unlist(nf_over_congress115[[x]]),]),cl = 8)
# lapply(seq_along(nf_congress115_intersects),function(x) nf_congress115_intersects[[x]]$Prop_Over <<- st_area(nf_congress115_intersects[[x]])/st_area(admin_districts[x,]))
# lapply(seq_along(nf_congress115_intersects),function(x) {nf_congress115_intersects[[x]]$FOREST_ID <<- admin_districts$FOREST_ID[x]})
# 
# fips_codes = tigris::fips_codes
# nf_congress115_sf = do.call(rbind,nf_congress115_intersects)
# nf_congress115_sf$STATE = fips_codes$state[match(nf_congress115_sf$STATEFP,fips_codes$state_code)]
# nf_congress115_sf$SID = paste0(nf_congress115_sf$STATE ,as.numeric(nf_congress115_sf$CD115FP))
# nf_congress115_sf$congress = nf_congress115_sf$CDSESSN
# nf_congress115_dt = data.table(nf_congress115_sf[,colnames(nf_congress115_sf) %in% c('SID','congress','Prop_Over','FOREST_ID')])
# nf_congress115_dt$Prop_Over = as.numeric(nf_congress115_dt$Prop_Over)
# nf_congress115_dt[,geometry:=NULL]
# 
# nf_congress_dt = rbind(nf_congress_dt,nf_congress115_dt)
# forest_congress_grid = data.table(expand.grid(FOREST_ID = admin_districts$FOREST_ID,congress = as.character(108:115)))
# nf_congress_dt = nf_congress_dt[order(FOREST_ID,congress,SID),]
# nf_congress_grid = expand(nf_congress_dt,nesting(FOREST_ID,SID),congress=as.character(108:115))
# nf_congress_full = data.table(left_join(nf_congress_grid ,nf_congress_dt))
# nf_congress_full[order(FOREST_ID,SID,congress),Prop_Over:=zoo::na.locf(Prop_Over,na.rm = F),by = .(FOREST_ID,SID)] 
# nf_congress_full[,Prop_Over:=round(Prop_Over,3)]
# nf_congress_full = nf_congress_full[Prop_Over>0,]

