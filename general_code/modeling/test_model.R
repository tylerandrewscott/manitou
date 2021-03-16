
library(data.table)
library(tidyverse)
library(lubridate)
library(sf)
library(lwgeom)
library(stringr)
library(pbapply)


ranger ="https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.RangerDistrict.zip"
admin ="https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip"
lmu = "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.NFSLandUnit.zip"
temp <- tempfile()
download.file(ranger ,temp)
cat = unzip(temp)
fname <- (grep('shp',cat,value=T))
ranger_districts <- st_read(fname)
albersNA = aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
ranger_districts <- st_transform(ranger_districts,crs = st_crs(albersNA))
ranger_districts  <- st_make_valid(ranger_districts)
ranger_districts$DISTRICTNA = as.character(ranger_districts$DISTRICTNA)
ranger_districts$RANGERDIST = as.character(ranger_districts$RANGERDIST)
ranger_districts$FORESTNAME = as.character(ranger_districts$FORESTNAME)
ranger_districts$DISTRICTOR = as.character(ranger_districts$DISTRICTOR)
ranger_districts$DISTRICT_ID <- str_extract(ranger_districts$DISTRICTOR,'[0-9]{6}$')
temp <- tempfile()
download.file(admin ,temp)
cat = unzip(temp)
fname <- (grep('shp',cat,value=T))
admin_districts <- st_read(fname)
admin_districts <- st_transform(admin_districts,crs = st_crs(albersNA))
admin_districts  <- st_make_valid(admin_districts)

temp <- tempfile()
download.file(admin ,temp)
cat = unzip(temp)
fname <- (grep('shp',cat,value=T))
lmus <- st_read(fname)
lmus <- st_transform(lmus,crs = st_crs(albersNA))
lmus  <- st_make_valid(lmus)

fs = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vTPHD71fQvTZpNljqRNVI6lD4b26wW86ptP_dy3R_qWaxkXyOE2QCuyTzroL_38mw/pub?output=csv')
fs2 = fs
fs = fs[fs$`UNIQUE DECISION?`=='Y',]
fs = fs[fs$`DECISION TYPE`!='PAD',]
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='ROD'] <- 'EIS'
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='DN'] <- 'EA'
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='DM'] <- 'CX'
fs$DISTRICT_ID = str_extract(fs$`LMU – DISTRICT`,'[0-9]{8}')
fs$REGION_ID = str_extract(str_extract(fs$DISTRICT_ID,'^[0-9]{4}'),'[0-9]{2}$')
fs$FOREST_ID = str_extract(str_extract(fs$DISTRICT_ID,'^[0-9]{6}'),'[0-9]{4}$')
fs_sub = fs[str_extract(fs$DISTRICT_ID ,'[0-9]{6}$') %in% ranger_districts$DISTRICTOR,]

proj_count = fs_sub[,.N,by=.(DISTRICT_ID,`DECISION TYPE`,`INITIATION FY`)]
proj_count$DISTRICT_ID = str_extract(proj_count$DISTRICT_ID,'[0-9]{6}$')
setnames(proj_count,c('INITIATION FY','DECISION TYPE'),c('FISCAL_YEAR','DECISION_TYPE'))
library(noncompliance)
tdt = expand.grid.DT(sort(unique(str_extract(ranger_districts$DISTRICTOR,'[0-9]{6}$'))),2004:2018,c('CX','EA','EIS'))
setnames(tdt,c('DISTRICT_ID',"FISCAL_YEAR",'DECISION_TYPE'))
setkey(tdt,       DISTRICT_ID,FISCAL_YEAR,DECISION_TYPE)
setkey(proj_count,DISTRICT_ID,FISCAL_YEAR,DECISION_TYPE)

temp = proj_count[tdt,]
temp$N[is.na(temp$N)]<-0
temp_dt = dcast(temp,DISTRICT_ID+FISCAL_YEAR~DECISION_TYPE,value.var = 'N')
cor(temp_dt[,.(CX,EA,EIS)],method = 'kendall')

#https://wildfire.cr.usgs.gov/firehistory/data.html
#"https://wildfire.cr.usgs.gov/firehistory/data/wf_usfs_1980_2016.zip"

wildfire_burn = 'https://wildfire.cr.usgs.gov/firehistory/data/wf_usfs_1980_2016.zip'
temp <- tempfile()
download.file(wildfire_burn,temp)
cat = unzip(temp)
fname <- (grep('shp',cat,value=T))
wildfire_burn <- st_read(fname)
wildfire_burn <- st_transform(wildfire_burn,crs = st_crs(albersNA))
wildfire_burn  <- st_make_valid(wildfire_burn)
over_burned = st_intersects(ranger_districts,wildfire_burn)
temp_dt$Burned_Area = pbsapply(1:nrow(temp_dt),function(i) {
  if(length(over_burned[[temp_dt$ranger_index[[i]]]])==0){0}
  else if(length(intersect(over_burned[[i]],which(wildfire_burn$FiscalYear<temp_dt$FISCAL_YEAR[i])))==0){0}
  else{sum(st_area(st_intersection(
    st_union(wildfire_burn[intersect(over_burned[[i]],which(wildfire_burn$FiscalYear<temp_dt$FISCAL_YEAR[i])),]),
    ranger_districts[temp_dt$ranger_index[[i]],])))}},cl = 1)
temp_dt$Burned_Prop = temp_dt$Burned_Area/temp_dt$GIS_AREA

limit_use = 'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.OthNatlDesgAreaStatus.zip'
temp <- tempfile()
download.file(limit_use,temp)
cat = unzip(temp)
fname <- (grep('shp',cat,value=T))
limit_use <- st_read(fname)
limit_use <- st_transform(limit_use,crs = st_crs(albersNA))
limit_use  <- st_make_valid(limit_use)
limit_use$FISCAL_YEAR = year(limit_use$ACTIONDATE) + (month(limit_use$ACTIONDATE)>=10 + 0)
temp_dt$Limited_Use_Area = pbsapply(1:nrow(temp_dt),function(i) {sum(st_area(st_intersection(
  ranger_districts[ranger_districts$DISTRICT_ID==temp_dt$DISTRICT_ID[i],],
  limit_use[limit_use$FISCAL_YEAR<temp_dt$FISCAL_YEAR[i],])))},cl = 10)
temp_dt$Limited_Use_Prop = temp_dt$Limited_Use_Area/temp_dt$GIS_AREA


wilderness_area = 'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.Wilderness.zip'
temp <- tempfile()
download.file(wilderness_area,temp)
cat = unzip(temp)
fname <- (grep('shp',cat,value=T))
wilderness_area <- st_read(fname)
wilderness_area <- st_transform(wilderness_area,crs = st_crs(albersNA))
wilderness_area  <- st_make_valid(wilderness_area)
desig_years = 'https://www.wilderness.net/GIS/Wilderness_Areas.zip'
temp <- tempfile()
download.file(desig_years,temp)
cat = unzip(temp)
fname <- (grep('shp',cat,value=T))
desig_years <- st_read(fname)
wilderness_area$DESIG_YEAR = desig_years$YearDesign[match(formatC(wilderness_area$WID,width=3,flag=0),formatC(desig_years$WID,width=3,flag=0))]
wilderness_area$DESIG_YEAR[wilderness_area$WILDERNESS=='10460010343\r\n'] <- 1994
temp_dt$Wilderness_Area = pbsapply(1:nrow(temp_dt),function(i) {sum(st_area(st_intersection(
  wilderness_area[ranger_districts$DISTRICT_ID==temp_dt$DISTRICT_ID[i],],
  wilderness_area[wilderness_area$DESIG_YEAR<temp_dt$FISCAL_YEAR[i],])))},cl = 10)
temp_dt$Wilderness_Prop = temp_dt$Wilderness_Area/temp_dt$GIS_AREA

#saveRDS(temp_dt,'scratch/data_file.RDS')

county_overs = fread('input/gis_overlap_props/rangerdistrict_county_overlap_props.csv')
setnames(county_overs,'DISTRICTOR','DISTRICT_ID')
county_overs$DISTRICT_ID = formatC(county_overs$DISTRICT_ID,width = 6, flag = 0)


county_over = fread('input/gis_overlap_props/rangerdistrict_county_overlap_props.csv')
county_over$DISTRICT_ID = formatC(county_over$DISTRICTOR,width=6,flag=0)
county_over$Prop_Overlap = round(county_over$Prop_Overlap,3)
county_over <- county_over[county_over$Prop_Overlap>0,]
county_voting = readRDS('input/politics/countyVoteShare_5-2019.rds')
county_voting = as.data.table(county_voting)
county_voting = county_voting[,.(percentD_H,year,GEOID)]
setnames(county_voting,c('percentD_H','year','GEOID'),
         c('percentD_H','FISCAL_YEAR','CFIPS'))

county_over$CFIPS <- as.character(county_over$CFIPS )
temp_dt$percentD_H = pbsapply(1:nrow(temp_dt),function(i) {
  co = county_over[county_over$DISTRICT_ID==temp_dt$DISTRICT_ID[i],]
  cv = county_voting[county_voting$CFIPS%in%co$CFIPS  &county_voting$FISCAL_YEAR==temp_dt$FISCAL_YEAR[i],]
  cv = full_join(co,cv)
  weighted.mean(cv$percentD_H,cv$Prop_Overlap,na.rm = T)},cl = 1)

house = readRDS('input/politics/houseAtts_5-2019.RDS')
house = as.data.table(house)

house = house[,.(percentD_H,year,GEOID)]
setnames(county_voting,c('percentD_H','year','GEOID'),
         c('percentD_H','FISCAL_YEAR','CFIPS'))

county_over$CFIPS <- as.character(county_over$CFIPS )
temp_dt$percentD_H = pbsapply(1:nrow(temp_dt),function(i) {
  co = county_over[county_over$DISTRICT_ID==temp_dt$DISTRICT_ID[i],]
  cv = county_voting[county_voting$CFIPS%in%co$CFIPS  &county_voting$FISCAL_YEAR==temp_dt$FISCAL_YEAR[i],]
  cv = full_join(co,cv)
  weighted.mean(cv$percentD_H,cv$Prop_Overlap,na.rm = T)},cl = 1)


habitat = 'https://ecos.fws.gov/docs/crithab/crithab_all/crithab_all_layers.zip'
temp <- tempfile(tmpdir = tempdir())
download.file(habitat,destfile = temp)
cat = unzip(temp)
fname <- (grep('POLY.shp',cat,value=T))
hab_poly <- st_read(fname)
hab_poly <- st_transform(hab_poly,crs = st_crs(albersNA))
hab_poly  <- st_make_valid(hab_poly)
hab_poly$FR_Date = ymd(as.character(hab_poly$pubdate))
hab_poly$FISCAL_YEAR = year(hab_poly$FR_Date) + (month(hab_poly$FR_Date)>=10 + 0)

hab_overs = st_intersects(ranger_districts,hab_poly)
temp_dt$ranger_index = match(temp_dt$DISTRICT_ID,ranger_districts$DISTRICT_ID)
#temp_dt$CriticalHabitat_Area = 
hab_props = pblapply(1:nrow(temp_dt),function(i) {
  if(length(hab_overs[[temp_dt$ranger_index[[i]]]])==0){0}
  else if(length(intersect(temp_dt$ranger_index[[i]],which(hab_poly$FISCAL_YEAR<temp_dt$FISCAL_YEAR[i])))==0){0}
  else{sum(st_area(st_intersection(
      st_union(hab_poly[intersect(hab_overs[[temp_dt$ranger_index[[i]]]],which(hab_poly$FISCAL_YEAR<temp_dt$FISCAL_YEAR[i])),]),
  ranger_districts[temp_dt$ranger_index[[i]],])))}},cl = 1)


####### overlay subsurface mineral rights on ranger district
mineral_rights = 'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.MINERALRIGHT.zip'
temp <- tempfile(tmpdir = tempdir())
download.file(mineral_rights,destfile = temp)
cat = unzip(temp)
fname <- (grep('MINERALRIGHT.shp',cat,value=T))
mineral_rights <- st_read(fname)
mineral_rights <- st_transform(mineral_rights,crs = st_crs(albersNA))
mineral_rights  <- st_make_valid(mineral_rights)
mineral_rights$FISCAL_YEAR = mineral_rights$ACTIONFISC
over_minerals = st_intersects(ranger_districts,mineral_rights)
temp_dt$Mineral_Rights_Area = pbsapply(1:nrow(temp_dt),function(i) {
  if(length(over_minerals[[i]])==0){0}
  else if(length(intersect(over_minerals[[i]],which(mineral_rights$FISCAL_YEAR<temp_dt$FISCAL_YEAR[i])))==0){0}
  else{sum(st_area(st_intersection(
    st_union(mineral_rights[intersect(over_minerals[[i]],which(mineral_rights$FISCAL_YEAR<temp_dt$FISCAL_YEAR[i])),]),
    ranger_districts[temp_dt$ranger_index[[i]],])))}},cl = 1)
temp_dt$Mineral_Rights_Prop = temp_dt$Mineral_Rights_Area/temp_dt$GIS_AREA


allotments = 'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.Allotment.zip'
temp <- tempfile(tmpdir = tempdir())
download.file(allotments,destfile = temp)
cat = unzip(temp)
fname <- (grep('Allotment.shp',cat,value=T))
allotments <- st_read(fname)
allotments <- st_transform(allotments,crs = st_crs(albersNA))
allotments  <- st_make_valid(allotments)
allotments$
allotments$FISCAL_YEAR = allotments$ACTIONFISC
over_minerals = st_intersects(ranger_districts,allotments)

##### overlay rangeland on ranger dsitricts
temp_dt$Rangeland_Allotment_Area = pbsapply(1:nrow(temp_dt),function(i) {
  if(length(over_minerals[[temp_dt$ranger_index[[i]]]])==0){0}
  else if(length(intersect(temp_dt$ranger_index[[i]],which(mineral_rights$FISCAL_YEAR<temp_dt$FISCAL_YEAR[i])))==0){0}
  else{sum(st_area(st_intersection(
    st_union(mineral_rights[intersect(over_minerals[[temp_dt$ranger_index[[i]]]],which(mineral_rights$FISCAL_YEAR<temp_dt$FISCAL_YEAR[i])),]),
    ranger_districts[temp_dt$ranger_index[[i]],])))}},cl = 1)

which(is.null(sapply(test,class)))
i = 6162
test[6162]
length(over_minerals[[temp_dt$ranger_index[[i]]]])
length(intersect(temp_dt$ranger_index[[i]],which(mineral_rights$FISCAL_YEAR<temp_dt$FISCAL_YEAR[i])))==0

t1 = st_union(mineral_rights[intersect(over_minerals[[temp_dt$ranger_index[[i]]]],which(mineral_rights$FISCAL_YEAR<temp_dt$FISCAL_YEAR[i])),])
st_area(st_intersection(t1,ranger_districts[temp_dt$ranger_index[[i]],]))
which(!duplicated(sapply(test,class)))
test
table(sapply(test,class))
temp_dt$Burned_Area

ggplot() + geom_sf(data = mineral_rights)

dim(cercla)



i = 2513
sum(st_area(st_intersection(
  st_union(hab_poly[intersect(hab_overs[[temp_dt$ranger_index[[i]]]],which(hab_poly$FISCAL_YEAR<temp_dt$FISCAL_YEAR[i])),]),
  ranger_districts[temp_dt$ranger_index[[i]],])))
proj = readRDS('input/politics/projects_5-2019.rds')


ranger_districts
temp_dt$ranger_index[[i]]
which(hab_poly$FISCAL_YEAR<temp_dt$FISCAL_YEAR[i])
hab_poly[393,]

intersect(hab_overs[[temp_dt$ranger_index[[i]]]],which(hab_poly$FISCAL_YEAR<temp_dt$FISCAL_YEAR[i]))
hab_poly[intersect(hab_overs[[temp_dt$ranger_index[[i]]]],which(hab_poly$FISCAL_YEAR<temp_dt$FISCAL_YEAR[i])),]

ranger_districts[temp_dt$ranger_index[[i]],]

which.max(unlist(hab_props))  
dim(temp_dt)
length(hab_props )
summary(unlist(hab_props)/temp_dt$GIS_AREA)


summary(round(unlist(hab_props),3))
sum(st_area(st_intersection(
  st_union(hab_poly[intersect(hab_overs[[temp_dt$ranger_index[[586]]]],which(hab_poly$FISCAL_YEAR<temp_dt$FISCAL_YEAR[586])),]))))
  
  
  
hab_props[586]
sapply(hab_props,class)
unlist(hab_props)
eco_nc = "https://disturbance.s3.amazonaws.com/EcoregionalRollup.zip"
temp <- tempfile()
download.file(eco_nc,temp)
cat = unzip(temp)
fname <- (grep('shp',cat,value=T))
eco_nc <- st_read(fname)
eco_nc <- st_transform(eco_nc,crs = st_crs(albersNA))
eco_nc  <- st_make_valid(eco_nc)



ggplot() + geom_sf(data = ranger_districts[86,]) + 
  geom_sf(data = hab_poly[[temp_dt$ranger_index]])

temp_dt[323,]
which.max(unlist(hab_props)/temp_dt$GIS_AREA)
summary(unlist(hab_props))


  ggplot() + 
  geom_sf(data = hab_poly[hab_overs[[temp_dt$ranger_index[[i]]]],],fill = 'green') +
    geom_sf(data = ranger_districts[i,],fill = 'red')
  
  
  hab_overs[[temp_dt$ranger_index[[i]]]]
  hab_poly[hab_overs[[temp_dt$ranger_index[[i]]]],]
  hab_poly[hab_poly$FISCAL_YEAR<temp_dt$FISCAL_YEAR[i],])))},cl = 10)

as.character(hab_poly$pubdate)[687]
dim(limit_areas)
dim(limit_use)
unique(limit_use$AREATYPE)
unique(limit_areas$AREATYPE)
limit_use






library(INLA)




inla(formula = )
inla.list.models()
library(INLA)
inla.doc('zeroinflatedbinomial0')
names(temp_dt)

temp_dt$TOTAL = temp_dt$EIS + temp_dt$EA
temp_dt$NF = ranger_districts$FORESTNUMB[match(temp_dt$DISTRICT_ID,ranger_districts$DISTRICT_ID)]
temp_dt$NF = as.character(temp_dt$NF )
library(zoo)
which(is.na(temp_dt$percentD_H))
temp_dt[2130:2146,.(DISTRICT_ID,percentD_H)]
temp_dt[order(DISTRICT_ID,FISCAL_YEAR),
        filled_percentD_H:=na.locf(percentD_H),by=DISTRICT_ID]

mod = inla(EIS ~ 1 + scale(CX) + scale(log(DISTRICT_ACREAGE)) + 
 scale(Burned_Prop) + scale(Limited_Use_Prop) + scale(Wilderness_Prop)+
 #  scale(percentD_H) + 
             f(DISTRICT_ID,model = 'iid') + #f(NF,model = 'iid')+
   f(FISCAL_YEAR,model = 'rw1'),
     family = "binomial", data = temp_dt, Ntrials = temp_dt$TOTAL,verbose=T,num.threads = 8)

cor(temp_dt$Limited_Use_Prop,temp_dt$Wilderness_Prop)








table(is.na(temp_dt$percentD_H))


glm(EIS~1,family = 'poisson',data = test)










tapply(temp$N,temp$DECISION_TYPE,summary)

fs$FOREST_ID = str_extract(fs$`LMU – FOREST`,'[0-9]{8}')
fs$REGION_ID = str_extract(fs$`LMU – REGION`,'[0-9]{8}')
fs$FOCAL_ID = str_extract(fs$`LMU (ACTUAL)`,'[0-9]{8}')

fs
unique(fs$`LMU – REGION`)
unique(fs$`LMU – FOREST`)
unique(fs$`LMU – DISTRICT`)
table(is.na(fs$FOREST_ID))


test = ranger_districts$DISTRICTOR
matches = pblapply(test,function(x) grep(x,fs$`LMU (ACTUAL)`),cl = 10)

test = str_extract(fs$`LMU – DISTRICT`,'[0-9]{1,}')
which(nchar(test)<8)
str_extract(fs$`LMU – DISTRICT`[6488],'[0-9]{1,}')
fs[6488,]

cbind(ranger_districts$DISTRICTNA[which(sapply(matches,length)==0)],
ranger_districts$DISTRICTOR[which(sapply(matches,length)==0)])
ranger_districts$DISTRICTOR[which(sapply(matches,length)!=0)]




ranger_districts[c(477,500),]
(11090403
grep('Manistee|Cadillac',ranger_districts$DISTRICTNA,value=F)
11090403
  090402
 "061601"
grep('Cadillac',fs$`LMU – DISTRICT`,value=T)



ranger_districts$DISTRICTNA[grepl('Cowlitz',ranger_districts$DISTRICTNA)] <- 'Cowlitz Ranger District'

ranger_districts


fs$`LMU – DISTRICT`[grep('Judith|Musselshell',fs$`LMU – DISTRICT`)] <- "Judith-Musselshell Ranger District (11011506)"
fs$`LMU – DISTRICT`[grep('Belt Creek|White Sulphur Springs',fs$`LMU – DISTRICT`)] <- "Belt Creek-White Sulphur Springs Ranger District (11011507)"
fs$`LMU – DISTRICT`[grep('Belt Creek|White Sulphur Springs',fs$`LMU – DISTRICT`)] <- "Belt Creek-White Sulphur Springs Ranger District (11011507)"


head(fs)
lmus$ADMINFORES <- as.character(lmus$ADMINFORES)



head(ranger_districts)
head(lmus)
dim(lmus)
dim(ranger_districts)
dim(admin_districts)

test = str_extract(ranger_districts$RANGERDIST,'[0-9]{8}$')


test2=str_extract(fs$`LMU – DISTRICT`,'[0-9]{1,}')
test2=str_extract(test2,'[0-9]{6}$')

grep("081201",fs$`LMU – DISTRICT`,value=T)
08         12         04 

ranger_districts[grep('Enoree',ranger_districts$DISTRICTNA),]
ranger_districts$DISTRICTOR[grepl("Enoree",)]
test2[!test2 %in% ranger_districts$DISTRICTOR]

table(str_extract(test2,'^[0-9]{2}'))
table(nchar(test2))
test2 = gsub('\\s\\($','',test2,perl=T)

unique(test2[!test2 %in% ranger_districts$DISTRICTNA])


table(test2 %in% ranger_districts$FORESTNAME)

grep('Cowlitz',ranger_districts$DISTRICTNA,value=T)
grep('Cowlitz',fs$`LMU – DISTRICT`,value=T)

ranger_districts$DISTRICTNA
test %in% str_extract(fs$`LMU – DISTRICT`,'[0-9]{1,}')




grep('11100534',str_extract(fs$`LMU – DISTRICT`,'[0-9]{1,}'))
11010302

11060305
99060305010343 
ranger_districts[grepl('Cowlitz',ranger_districts$DISTRICTNA),!colnames(ranger_districts)%in%c('geometry')]

table(grepl('Darby',fs$`LMU – DISTRICT`))
library(ggthemes)
library(ggplot2)
ggplot(data=fs) + geom_density(aes(colour = `DECISION TYPE`,x = `ELAPSED DAYS`),trim=T) + 
  theme_bw() + theme(legend.position = c(0.8,0.3))  +
  scale_color_tableau()

ggplot(data=fs) + geom_boxplot(aes(x = `DECISION TYPE`,y = `ELAPSED DAYS`),trim=T) + 
  theme_bw() + theme(legend.position = c(0.8,0.3))  +
  scale_color_tableau()

table(fs$`ELAPSED DAYS`[fs$`DECISION TYPE`=='DM']==0)


table(nchar(str_extract(fs$`LMU – DISTRICT`,'[0-9]{1,}')))

head(fs)
nchar(as.character(ranger_districts$RANGERDIST))
table(nchar(ranger_districts$DISTRICTNU))
table(nchar(ranger_districts$DISTRICTOR))

fs[fs$`DECISION TYPE`=='PAD',]

fs = fread('input/forest_service_project_detail_2018-12-31.csv')
doc = fread('input/forest_service_document_2018-12-31.csv')
fs$YEAR = year(ymd(fs$Decision.Signed.Date))
fs$YEAR[is.na(fs$YEAR)] <- mdy(str_extract(str_extract(fs$Project.Milestones[is.na(fs$YEAR)],'(Notice of Initiation|Comment Period).*'),'[0-9]{2}[0-9]{2}.[0-9]{2,4}'))
fs <- fs[!is.na(fs$YEAR),]




fs$District=gsub("s: \n",'',fs$District,fixed = T)
ranger_districts$DISTRICTOR <- as.character(ranger_districts$DISTRICTOR)
fs$DISTRICTOR = sapply(str_split(fs$District,';'),function(x) ranger_districts$DISTRICTOR[match(x,ranger_districts$DISTRICTNA)] %>% .[!is.na(.)])
fs = fs[sapply(fs$DISTRICTOR,length)>0]
fs$DISTRICTOR = unlist(fs$DISTRICTOR)

fs_count = fs[,.N,by=.(DISTRICTOR,YEAR,Expected.Analysis.Type)]

grid = expand.grid(DISTRICTOR = as.character(ranger_districts$DISTRICTOR),YEAR = 2000:2018,Expected.Analysis.Type = c('CX','EA','EIS'))
grid = data.table(grid)
fs_count$YEAR <- as.character(fs_count$YEAR)
fs_count$DISTRICTOR = as.character(fs_count$DISTRICTOR)
fs$Expected.Analysis.Type = as.character(fs$Expected.Analysis.Type)
grid$DISTRICTOR <- as.character(grid$DISTRICTOR)
grid$YEAR <- as.character(grid$YEAR)
setkey(grid,'YEAR','DISTRICTOR','Expected.Analysis.Type')
setkey(fs_count,'YEAR','DISTRICTOR','Expected.Analysis.Type')

counts = fs_count[grid,]
counts$N[is.na(counts$N)] <- 0

library(INLA)




fs[,.N,by = list(YEAR,DISTRICTOR)]

test= which(is.na(match(fs$District,ranger_districts$DISTRICTNA)))
fs[test,.(District)]



fs$District










