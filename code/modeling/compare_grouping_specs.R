
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
#if(!require(INLA)){install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)}
require(INLA)
packages = c('data.table','stringr','tidyverse','sf','lwgeom','ggthemes','tigris','lubridate')
not_installed = packages[!packages %in% installed.packages()[,'Package']]
if(length(not_installed)>0){lapply(not_installed,install.packages)}
lapply(packages,require,character.only = T)

#run = 'joint_shared'
start_year = 2005
end_year = 2018

##### this determines whether models run on per-congress (i.e., two year windows) #####
# for "CALENDAR_YEAR" or "congress"
period_type = 'CALENDAR_YEAR'

#### this determines which projects to include ######
keep_decisions = c('EIS','EA','CE')

#### this determines whether ot keep purpose or activity or both
keep_activities = FALSE
keep_purpose = TRUE


td = tempdir()
albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
# # 
# adm_url = "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip"
# tf = tempfile(tmpdir=td, fileext=".zip")
# download.file(adm_url, tf)
# fname = unzip(tf, list=TRUE)
# unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
# fpath = file.path(td, grep('shp$',fname$Name,value=T))
# admin_districts <- st_read(fpath)
# admin_districts <- st_transform(admin_districts,crs = st_crs(albersNA))
# # fix bad polygons
# bad_polys = !st_is_valid(admin_districts)
# admin_districts[bad_polys,] <- st_make_valid(admin_districts[bad_polys,])
# 
# admin_districts$FORESTORGC = as.character(admin_districts$FORESTORGC)
# admin_districts$FOREST_ID = admin_districts$FORESTORGC
# admin_districts$FOREST_ID = formatC(admin_districts$FORESTORGC,width=4,flag = 0)
# saveRDS(admin_districts,'scratch/admin_units_clean.RDS')

admin_districts <- readRDS('scratch/admin_units_clean.RDS')

drop_sites = c("Savannah River Site" ,'El Yunque National Forest',
               "Land Between the Lakes National Recreation Area" ,  "Columbia River Gorge National Scenic Area" ,"Midewin National Tallgrass Prairie" )
drop_units = admin_districts$FOREST_ID[match(drop_sites,admin_districts$FORESTNAME)]

file.remove(list.files('output/policypolitics/tables/',pattern = 'coefs',full.names = T))
states = tigris::states(class = 'sf')
states <- st_transform(states,crs = st_crs(albersNA))
test = st_within(st_centroid(admin_districts),states)
admin_districts$STATE = states[unlist(test),]$STUSPS

fs = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vRuz53_guoKQejdd97Syvws360-S7g21tkjeH73OJ1K6zWKNlGwaDRLvd7bbwIoyLmYCf9RWsAzETu-/pub?output=csv',stringsAsFactors = F)
fs$ongoing = 0
fs2 = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vQgOk_TxujlSfspmA9-V4sBS95AMVu7MbjyCmbsIc1yu5N983sJqoR8usHvOXpMl99d1yBYejWoZtdk/pub?output=csv',stringsAsFactors = F)
numcols = colnames(fs2)[as.vector(apply(fs2,2,function(x) !any(grepl('[^0-9]',x))))]
fs2[,(numcols):=lapply(.SD,as.numeric),.SDcols = numcols]
fs = merge(fs,fs2,all = T)
fs = fs[!duplicated(paste(`PROJECT NUMBER`,`LMU (ACTUAL)`)),] 
fs = fs[fs$`DECISION TYPE`!='PAD',]


#fs = fs2
#fs = fs[fs$`UNIQUE DECISION?`=='Y',]
fs$`LMU (ACTUAL)`[fs$`LMU (ACTUAL)`=="Buffalo Ranger District (11040306)"] <- "Blackrock Ranger District (11040306)"
fs$`LMU (ACTUAL)`[fs$`LMU (ACTUAL)`=="Powell Ranger District  (11010506)"] <- "Lochsa/Powell Ranger District (11011755)"
fs$`LMU (ACTUAL)`[fs$`LMU (ACTUAL)`=="Salmon River Ranger District (11050554)"] <- "Salmon-Scott Ranger District (11050554)"
fs$`LMU (ACTUAL)`[fs$`LMU (ACTUAL)`=="Paintrock Ranger District (11020204)"] <- "Salmon-Scott Ranger District (11020203)"
fs$`LMU (ACTUAL)`[fs$`LMU (ACTUAL)`=="Belt Creek Ranger District (11011503)"] <- "Belt Creek-White Sulphur Springs Ranger District (11011507)"
fs$`LMU (ACTUAL)`[fs$`LMU (ACTUAL)`=="Ashton/Island Park (11041552)" ]<- "Ashton/Island Park Ranger District (11041552)" 
fs$`LMU (ACTUAL)`[fs$`LMU (ACTUAL)`=="Los Angeles River (11050151)" ]<- "Los Angeles River Ranger District (11050151)" 
fs$`LMU (ACTUAL)`[fs$`LMU (ACTUAL)`=="Santa Clara/Mojave Rivers (11050153)"  ]<- "Santa Clara/Mojave Rivers Ranger District (11050153)" 

fs$UNIT_ID = str_extract(str_extract(fs$`LMU (ACTUAL)`,'[0-9]{3,}'),'[0-9]{6}$')
fs$UNIT_ID = gsub('^0108','0111',fs$UNIT_ID)
fs$UNIT_ID = gsub('^0105','0117',fs$UNIT_ID)
fs$UNIT_ID = gsub('^0112','0115',fs$UNIT_ID)
fs$UNIT_ID[fs$UNIT_ID=='011702'] <- '011752'
fs$UNIT_ID[fs$UNIT_ID=='011703'] <- '011753'
fs$UNIT_ID[fs$UNIT_ID=='011501'] <- '011511'
fs$UNIT_ID[fs$UNIT_ID=='011504'] <- '011514'
fs$UNIT_ID[fs$UNIT_ID=='011104'] <- '011184'
fs$UNIT_ID[fs$UNIT_ID=='011502'] <- '011512'
fs$UNIT_ID[fs$UNIT_ID=='011102'] <- '011182'
fs$UNIT_ID[fs$UNIT_ID=='011706'] <- '011755'
fs$FOREST_ID <- str_extract(fs$UNIT_ID,'^[0-9]{4}')
fs$REGION_ID <- str_extract(fs$UNIT_ID,'^[0-9]{2}')

fs$DECISON_LEVEL = NA
fs$DECISON_LEVEL[grepl('National (Forest|Forests) All Units|(National|Natl) Grassland|All Units|Tallgrass',fs$`LMU (ACTUAL)`)]<- "National Forest/Grassland"
fs$DECISON_LEVEL[grepl('R[0-9]|Region All Units',fs$`LMU (ACTUAL)`)] <- 'Region'
fs$DECISON_LEVEL[grepl('Washington Office|11000000',fs$`LMU (ACTUAL)`)] <- 'National'
fs$DECISON_LEVEL[grepl('Ranger District|RD|Mgt Unit|Catalina Field Office|Northern Great Lakes Visitor Center|Stone Nursery|Interpretive Center|Air Center|Research Station',fs$`LMU (ACTUAL)`)]<- "Ranger District/Mgt Unit"
fs$DECISON_LEVEL[grepl('National Recreation Area|National Scenic Area|National Monument|NRA|Volcanic Monument',fs$`LMU (ACTUAL)`)]<- "NRA/NSA/NM"
require(lubridate)
fs$CALENDAR_YEAR <- year(mdy(fs$`INITIATION DATE`))

#fs = fs[!fs$FOREST_ID%in%drop_units,]
fs = fs[order(fs$`DECISION ID`),]
fs = fs[!DECISON_LEVEL%in%c('National','Region'),]
#fs = fs[!duplicated(`PROJECT NUMBER`),]
fs = fs[!is.na(fs$FOREST_ID),]


fs$Congress_Year = fs$CALENDAR_YEAR
fs$Congress_Year[fs$Congress_Year%in% seq(1994,2018,2)] = as.numeric(fs$Congress_Year[fs$Congress_Year%in% seq(1994,2018,2)]) - 1
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='ROD'] <- 'EIS'
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='DN'] <- 'EA'
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='DM'] <- 'CE'
fs$PROJECT_DECISON_LEVEL<-ifelse(fs$DECISON_LEVEL=='National Forest/Grassland','NATIONAL FOREST','RANGER DISTRICT/FIELD UNIT')
congress_ids = data.table(congress = rep(101:116,each=2),CALENDAR_YEAR = 1989:2020)

fs = left_join(fs,congress_ids)
fs = data.table(fs)


totcount = fs[,.N,by=.(FOREST_ID)]
admin_districts = left_join(admin_districts,totcount)

us_counties = counties(class = 'sf',year = 2017)
us_counties = st_transform(us_counties ,albersNA)
#over_forest = st_intersects(us_counties,admin_districts)
#us_counties = us_counties[sapply(over_forest,length)>0,]



congress2015 = tigris::congressional_districts(year = 2015,class = 'sf')
congress2015  = st_transform(congress2015 ,albersNA)
#congress_over_forest = st_intersects(congress2015,admin_districts)
#congress2015= congress2015[sapply(congress_over_forest,length)>0,]

nf = fread('input/prepped/national_forest_covariates.csv')
nf$FOREST_ID = formatC(nf$FOREST_ID,width = 4,flag = 0)

nf$FOREST_NAME = admin_districts$FORESTNAME[match(nf$FOREST_ID,admin_districts$FOREST_ID)]
nf = nf[congress %in% 108:115,]


fs = fs[congress%in%109:115,]
fs = fs[FOREST_ID %in% admin_districts$FOREST_ID,]


fs = fs[fs$`DECISION TYPE`%in%keep_decisions,]
fs$Type_Activity_Extractive <- 0
fs$Type_Activity_Recreation_Wildlife <- 0
fs$Type_Activity_Extractive <- (fs$`TS Timber salves (green) – activity`==1|fs$`SS Timber sales (salvage) – activity`==1|fs$`MG Minerals and geology – purpose`==1|
                                  fs$`NG Natural gas – activity`==1|fs$`OL Oil – activity`==1|fs$`GR Grazing authorizations – activity`==1)+0
fs$Type_Activity_Extractive[grepl('Plan (of|Of) Operations|Timber Sale|Gas Drill|Oil Drill|Gas Well|Oil Well|Grazing Authorization|Gas Line|Gas Pipeline|Gas Transmission',fs$`PROJECT NAME`)] <- 1
fs$Type_Activity_Recreation_Wildlife <- (fs$`HI Species habitat improvements – activity`==1|fs$`PE Species population enhancements – activity`==1|
                                           fs$`RW Recreation management – purpose`==1|fs$`MT Trail management – activity`==1)+0

fs$Type_Purpose_Recreation_Wildlife <- 0
fs$Type_Purpose_Extractive <- 0
fs$Type_Purpose_Extractive[fs$`MG Minerals and geology – purpose` == 1| fs$`TM Forest products – purpose` == 1 | fs$`RG Grazing management – purpose`==1] <- 1
fs$Type_Purpose_Recreation_Wildlife[fs$`WF Wildlife, fish, rare plants – purpose` == 1 | fs$`RW Recreation management – purpose` == 1] <- 1
fs$Type_Purpose_All <- 1

#install.packages("ggcorrplot")
require(ggcorrplot)
if(keep_activities&keep_purpose){
  fs$Type_Purpose_Extractive = (fs$Type_Purpose_Extractive==1|fs$Type_Activity_Extractive  ==1)+0
  fs$Type_Purpose_Recreation_Wildlife = (fs$Type_Purpose_Recreation_Wildlife==1|fs$Type_Activity_Recreation_Wildlife  ==1)+0
}

if(keep_activities&!keep_purpose){
  fs$Type_Purpose_Extractive = fs$Type_Activity_Extractive
  fs$Type_Purpose_Recreation_Wildlife = fs$Type_Activity_Recreation_Wildlife
}

subvars = c('Type_Purpose_All','Type_Purpose_Extractive','Type_Purpose_Recreation_Wildlife')

if(period_type=='congress')
{obs_grid = data.table(expand.grid(congress = 107:116,FOREST_ID = unique(fs$FOREST_ID),DECISION_TYPE= keep_decisions,Project_Type = subvars))}
if(period_type=='CALENDAR_YEAR')
{obs_grid = data.table(expand.grid(CALENDAR_YEAR = start_year:end_year,FOREST_ID = unique(fs$FOREST_ID),DECISION_TYPE= keep_decisions,Project_Type = subvars))}

subset_projtypes = lapply(subvars, function(x) {
  if(period_type=='congress'){
    temp = fs[get(x)==1,.N,by = .(FOREST_ID,`DECISION TYPE`,congress)]}
  if(period_type!='congress'){
    temp = fs[get(x)==1,.N,by = .(FOREST_ID,`DECISION TYPE`,CALENDAR_YEAR)]}
  setnames(temp,c('DECISION TYPE'),c('DECISION_TYPE'))
  temp$Project_Type <- x
  temp = merge(obs_grid[Project_Type==x,],temp,all = T)
  temp
})
counts_by_type = rbindlist(subset_projtypes,use.names = T)
counts_by_type$N[is.na(counts_by_type$N)]<-0


durations = fs[,list(mean(mdy(`DECISION SIGNED`) - mdy(`INITIATION DATE`),na.rm=T),median(mdy(`DECISION SIGNED`) - mdy(`INITIATION DATE`),na.rm=T)),by = .(`DECISION TYPE`)]
names(durations) <- c('Type','mean','median')



#counts_by_type[FOREST_ID=='1004']

nf$Num_Eco_Sections[nf$Num_Eco_Sections==0] <- 1
nf = nf[order(FOREST_ID,get(period_type)),]
nf = nf[, zoo::na.locf(.SD, na.rm = FALSE),by = .(FOREST_ID)]

nf$Prop_Extraction_Employ = nf$Prop_Forestry_Employ + nf$Prop_Mining_Employ
nf$Perc_Extraction_Employ = nf$Prop_Extraction_Employ*100
nf$Prop_Outdoor_Employ = nf$Prop_HuntingFishing + nf$Prop_Recreation
nf$Wilderness_Prop = as.numeric(nf$Wilderness_Prop)
nf$Wilderness_Perc = nf$Wilderness_Prop * 100
#nf$Limited_Use_Prop = as.numeric(nf$Limited_Use_Prop)
nf$Prop_WUI_Housing = as.numeric(nf$Prop_WUI_Housing)
nf$Perc_WUI_Housing = 100 * nf$Prop_WUI_Housing
nf$Burned_Prop_Past5yrs = as.numeric(nf$Burned_Prop_Past5yrs)
nf$Burned_Perc_Past5yrs = nf$Burned_Prop_Past5yrs * 100
nf$ACRES = admin_districts$GIS_ACRES[match(nf$FOREST_ID,admin_districts$FOREST_ID)]
nf$demCongress[nf$demCongress==2] <- 0
nf$congress = as.character(nf$congress)
nf$nominate_dim1  = nf$nominate_dim1  * -1
nf$nominate_dim2  = nf$nominate_dim2  * -1
nf$mrp_mean = nf$mrp_mean * -1

nf$ComLCV = (nf$nrComLCV+nf$agComLCV)/2
nf$ChairLCV = (nf$nrChairLCV + nf$agChairLCV)/2

nf$Ln_ACRES = log(nf$ACRES)
nf$Ln_AVERAGE_YEARLY_VISITS = log(nf$Average_Yearly_Visits)
nf$USFS_REGION = admin_districts$REGION[match(nf$FOREST_ID,admin_districts$FOREST_ID)]

library(zoo)
nf = nf[order(FOREST_ID,congress),]
nf = nf[order(FOREST_ID,congress), zoo::na.locf(.SD, na.rm = FALSE),by = .(FOREST_ID)]
nf = nf[order(FOREST_ID,congress), na.locf(.SD, na.rm = FALSE,fromLast=TRUE),by = .(FOREST_ID)]
nf$congress = as.numeric(nf$congress)
nf$ln_County_naturalresource_GDP_1M = log(nf$NaturalResources1M+1)
nf$Prop_Extraction_Employ = nf$Prop_NaturalResourceEmployment
nf$Perc_Extraction_Employ = nf$Prop_Extraction_Employ * 100
center_continuous_cov = TRUE




admin_districts = admin_districts[!admin_districts$FOREST_ID%in% drop_units,]
nf= nf[!nf$FOREST_ID%in%drop_units,]



if(period_type == 'congress'){
  all_combos = expand.grid(FOREST_ID = sort(unique(admin_districts$FOREST_ID)),congress = 109:115,DECISION_TYPE = c('CE','EA','EIS'))
}
if(period_type !='congress'){
  all_combos = expand.grid(FOREST_ID = sort(unique(admin_districts$FOREST_ID)),CALENDAR_YEAR = start_year:end_year,DECISION_TYPE = c('CE','EA','EIS'))
}

all_combos = data.table(all_combos)
subtypes = unique(counts_by_type$Project_Type)

nf$`Avg_MBF_Cut_1999-2004`[is.na(nf$`Avg_MBF_Cut_1999-2004`)]<-0
nf$Ln_Avg_MBF_Cut_1999_2004 = log(nf$`Avg_MBF_Cut_1999-2004`+0.001)

nf$ln_Receipts_Extraction_1M_P4 <- log(nf$Receipts_TimberMineralsGrazing_P4/1e6+1)
nf$ln_Receipts_Recreation_1M_P4 <- log(nf$Receipts_Recreation_P4/1e6+1)

#setnames(nf,'LAU','Unemp_Rate')
nf = nf[order(FOREST_ID,CALENDAR_YEAR),Unemp_Rate:=lag(LAU_October),by = .(FOREST_ID)]
nf$STATE = admin_districts$STATE[match(nf$FOREST_ID,admin_districts$FOREST_ID)]

dist_by_year = full_join(admin_districts,nf[,.(FOREST_ID,LCV_annual,CALENDAR_YEAR)])
dist_by_year = dist_by_year[!is.na(dist_by_year$CALENDAR_YEAR),]
# 
# tt = ggplot() + geom_sf(data = dist_by_year,aes(fill = LCV_annual,colour = LCV_annual)) + 
#   facet_wrap(~CALENDAR_YEAR,ncol = 3) + 
#   ggtitle('LCV annual by year and forest') + theme_map() + scale_fill_viridis_c() + 
#   scale_colour_viridis_c()
# #ggsave(tt,dpi = 300,filename = 'output/policypolitics/figures/LCV_by_forest_and_year.png',height = 12,width = 8,units = 'in')


dist_by_year = full_join(admin_districts,nf[,.(FOREST_ID,Unemp_Rate ,CALENDAR_YEAR)])
dist_by_year = dist_by_year[!is.na(dist_by_year$CALENDAR_YEAR),]


nf = nf[nf$congress%in%109:115,]


input_data = dcast(counts_by_type,get(period_type) + 
                     FOREST_ID + Project_Type ~ DECISION_TYPE,value.var = 'N',fill = 0)
input_data$Tot_Proj = input_data$CE + input_data$EA + input_data$EIS
if(period_type=='congress'){
  input_data = input_data[Project_Type=='Type_Purpose_Extractive',][period_type>108&period_type<116,]}
if(period_type!='congress'){
  input_data = input_data[Project_Type=='Type_Purpose_Extractive',][period_type>=start_year&period_type<=end_year,]}

yheight = input_data[,.N,by = .(Tot_Proj)][N==max(N),]$N

forest_index = data.table(forest_id = sort(unique(nf$FOREST_ID)),index = seq_along(unique(nf$FOREST_ID)))
region_index = data.table(region_id = sort(unique(nf$USFS_REGION)),index = seq_along(unique(nf$USFS_REGION)))
state_index = data.table(state_id = sort(unique(nf$STATE)),index = seq_along(unique(nf$STATE)))
congress_index = data.table(congress_id = sort(unique(nf$congress)),index = seq_along(unique(nf$congress)))

subtypes = grep('Extract',subtypes,value = T)


counts_by_type$UNIT = admin_districts$FORESTNAME[match(counts_by_type$FOREST_ID,admin_districts$FOREST_ID)]


mod = subtypes; nf = nf; project_type_counts_for_model = counts_by_type;
temp_count = project_type_counts_for_model[Project_Type==mod,,]
temp_count = dcast(temp_count ,get(period_type) + FOREST_ID ~ DECISION_TYPE,fill = 0,value.var = 'N')
setnames(temp_count,'period_type',period_type)

  if(period_type=='congress'){
    temp_nf = nf[order(FOREST_ID,get(period_type)),][!duplicated(paste(FOREST_ID,congress)),]}
  if(period_type=='CALENDAR_YEAR'){
    temp_nf = nf[order(FOREST_ID,get(period_type)),]}

  temp_dt = merge(temp_count,temp_nf,by = c('FOREST_ID',period_type))#,all=T,by = c('FOREST_ID',congress))
  temp_dt$REGION_ID = temp_dt$USFS_REGION
  temp_dt$CONGRESS_ID =temp_dt$congress
  n = nrow(temp_dt)
  u = rowSums(temp_dt[,c('CE','EA','EIS'),with = F])
  narep = length(u)
  y <- ifelse(u>0,temp_dt$CE,NA)
  idat <- list(Y=matrix(NA,2*n,2))
  idat$Y[1:n,1] <- u
  idat$Y[n+1:n,2] <- y
  idat$mu.u <- rep(c(1,NA), each=n)
  idat$mu.y <- rep(c(NA,1), each=n)
  
  
  u_fe_forest_id = dcast(data.table(index = 1:(length(u) + length(y)),FOREST_ID = c(temp_dt$FOREST_ID,rep(NA,length(y)))), index ~ FOREST_ID,fun.aggregate = length)
  u_fe_forest_id[,`NA`:=NULL]
  u_fe_forest_id[,index:=NULL]
  u_fe_forest_id[length(u) + 1:length(y),]<-NA
  y_fe_forest_id = dcast(data.table(index = 1:(length(u) + length(y)),FOREST_ID = c(rep(NA,length(u)),temp_dt$FOREST_ID)), index ~ FOREST_ID,fun.aggregate = length)
  y_fe_forest_id[,`NA`:=NULL]
  y_fe_forest_id[,index:=NULL]
  y_fe_forest_id[1:length(u),]<-NA
  names(y_fe_forest_id) <- paste0('y_forest_id_',names(y_fe_forest_id))
  names(u_fe_forest_id) <- paste0('u_forest_id_',names(u_fe_forest_id))

  u_fe_congress_id = dcast(data.table(index = 1:(length(u) + length(y)),CONGRESS_ID = c(temp_dt$CONGRESS_ID,rep(NA,length(y)))), index ~ CONGRESS_ID,fun.aggregate = length)
  u_fe_congress_id[,`NA`:=NULL]
  u_fe_congress_id[,index:=NULL]
  u_fe_congress_id[length(u) + 1:length(y),]<-NA
  y_fe_congress_id = dcast(data.table(index = 1:(length(u) + length(y)),CONGRESS_ID = c(rep(NA,length(u)),temp_dt$CONGRESS_ID)), index ~ CONGRESS_ID,fun.aggregate = length)
  y_fe_congress_id[,`NA`:=NULL]
  y_fe_congress_id[,index:=NULL]
  y_fe_congress_id[1:length(u),]<-NA
  names(y_fe_congress_id) <- paste0('y_congress_id_',names(y_fe_congress_id))
  names(u_fe_congress_id) <- paste0('u_congress_id_',names(u_fe_congress_id))
  
  
  u_fe_state_id = dcast(data.table(index = 1:(length(u) + length(y)),STATE_ID = c(temp_dt$STATE,rep(NA,length(y)))), index ~ STATE_ID,fun.aggregate = length)
  u_fe_state_id[,`NA`:=NULL]
  u_fe_state_id[,index:=NULL]
  u_fe_state_id[length(u) + 1:length(y),]<-NA
  y_fe_state_id = dcast(data.table(index = 1:(length(u) + length(y)),STATE_ID = c(rep(NA,length(u)),temp_dt$STATE)), index ~ STATE_ID,fun.aggregate = length)
  y_fe_state_id[,`NA`:=NULL]
  y_fe_state_id[,index:=NULL]
  y_fe_state_id[1:length(u),]<-NA
  names(y_fe_state_id) <- paste0('y_state_id_',names(y_fe_state_id))
  names(u_fe_state_id) <- paste0('u_state_id_',names(u_fe_state_id))
  
  
  u_fe_region_id = dcast(data.table(index = 1:(length(u) + length(y)),REGION_ID = c(temp_dt$USFS_REGION,rep(NA,length(y)))), index ~ REGION_ID,fun.aggregate = length)
  u_fe_region_id[,`NA`:=NULL]
  u_fe_region_id[,index:=NULL]
  u_fe_region_id[length(u) + 1:length(y),]<-NA
  y_fe_region_id = dcast(data.table(index = 1:(length(u) + length(y)),REGION_ID = c(rep(NA,length(u)),temp_dt$USFS_REGION)), index ~ REGION_ID,fun.aggregate = length)
  y_fe_region_id[,`NA`:=NULL]
  y_fe_region_id[,index:=NULL]
  y_fe_region_id[1:length(u),]<-NA
  names(y_fe_region_id) <- paste0('y_region_id_',names(y_fe_region_id))
  names(u_fe_region_id) <- paste0('u_region_id_',names(u_fe_region_id))
  
  idat[names(y_fe_forest_id)]<-y_fe_forest_id
  idat[names(u_fe_forest_id)]<-u_fe_forest_id
  
  idat[names(y_fe_congress_id)]<-y_fe_congress_id
  idat[names(u_fe_congress_id)]<-u_fe_congress_id
  
  idat[names(y_fe_region_id)]<-y_fe_region_id
  idat[names(u_fe_region_id)]<-u_fe_region_id
  
  idat[names(y_fe_state_id)]<-y_fe_state_id
  idat[names(u_fe_state_id)]<-u_fe_state_id

  #### binomial EIS / [EA+EIS] model coefficients
  #idat$y_FOREST_LEVEL_DECISIONS = c(rep(NA,narep),scale(temp_dt$FOREST_LEVEL_DECISIONS))
  idat$y_Unemp_Rate = c(rep(NA,narep),scale(temp_dt$Unemp_Rate))
  #idat$y_Unemp_Rate_L1 = c(rep(NA,narep),scale(temp_dt$Unemp_Rate_L1))
  idat$y_ln_Receipts_Extraction_1M_P4 = c(rep(NA,narep),scale(temp_dt$ln_Receipts_Extraction_1M_P4))
  idat$y_ln_Receipts_Recreation_1M_P4 = c(rep(NA,narep),scale(temp_dt$ln_Receipts_Recreation_1M_P4))
  idat$y_Ln_Avg_MBF_Cut_1999_2004 = c(rep(NA,narep),scale(temp_dt$Ln_Avg_MBF_Cut_1999_2004))
  idat$y_ALLOTMENT_NEPA_1993_2004 = c(rep(NA,narep),scale(temp_dt$ALLOTMENT_NEPA_1993_2004))
  idat$y_MINING_CLAIM_ACTIONS_1993_2004 = c(rep(NA,narep),scale(temp_dt$MINING_CLAIM_ACTIONS_1993_2004))
  idat$y_Perc_Extraction_Employ = c(rep(NA,narep),scale(temp_dt$Perc_Extraction_Employ))
    idat$y_Ln_ACRES = c(rep(NA,narep),scale(temp_dt$Ln_ACRES))
  idat$y_Wilderness_Perc = c(rep(NA,narep),scale(temp_dt$Wilderness_Perc)) 
  idat$y_Burned_Perc_Past5yrs = c(rep(NA,narep),scale(temp_dt$Burned_Perc_Past5yrs)) 
  idat$y_Ln_AVERAGE_YEARLY_VISITS = c(rep(NA,narep),scale(temp_dt$Ln_AVERAGE_YEARLY_VISITS)) 
  idat$y_Count_EorT_Species= c(rep(NA,narep),scale(temp_dt$Count_EorT_Species)) 
  idat$y_percentD_H = c(rep(NA,narep),scale(temp_dt$percentD_H)) 
   idat$y_LCV_annual= c(rep(NA,narep),scale(temp_dt$LCV_annual))
  idat$y_mrp_mean= c(rep(NA,narep),scale(temp_dt$mrp_mean))

    idat$y_demPres = c(rep(NA,narep),temp_dt$demPres)
  idat$y_demCongress = c(rep(NA,narep),temp_dt$demCongress)
  idat$y_ComLCV = c(rep(NA,narep),scale(temp_dt$ComLCV))
  idat$y_ChairLCV = c(rep(NA,narep),scale(temp_dt$ChairLCV))
  idat$y_Perc_WUI_Housing = c(rep(NA,narep),scale(temp_dt$Perc_WUI_Housing))
  
  idat$y_ln_County_naturalresource_GDP_1M = c(rep(NA,narep),scale(temp_dt$ln_County_naturalresource_GDP_1M))
   idat$y_Total_Receipts_4yr_Change_Perc = c(rep(NA,narep),scale(temp_dt$Total_Receipts_4yr_Change_Perc))
   idat$u_Unemp_Rate = c(scale(temp_dt$Unemp_Rate),rep(NA,narep))
  idat$u_Ln_Avg_MBF_Cut_1999_2004 = c(scale(temp_dt$Ln_Avg_MBF_Cut_1999_2004),rep(NA,narep))
  idat$u_ALLOTMENT_NEPA_1993_2004 = c(scale(temp_dt$ALLOTMENT_NEPA_1993_2004),rep(NA,narep))
  idat$u_MINING_CLAIM_ACTIONS_1993_2004 = c(scale(temp_dt$MINING_CLAIM_ACTIONS_1993_2004),rep(NA,narep))
  idat$u_ln_Receipts_Extraction_1M_P4 = c(scale(temp_dt$ln_Receipts_Extraction_1M_P4),rep(NA,narep))
  idat$u_ln_Receipts_Recreation_1M_P4 = c(scale(temp_dt$ln_Receipts_Recreation_1M_P4),rep(NA,narep))
  idat$u_Perc_Extraction_Employ = c(scale(temp_dt$Perc_Extraction_Employ),rep(NA,narep))
  idat$u_Total_Receipts_4yr_Change_Perc = c(scale(temp_dt$Total_Receipts_4yr_Change_Perc),rep(NA,narep))
  idat$u_Perc_WUI_Housing = c(scale(temp_dt$Perc_WUI_Housing),rep(NA,narep))
  idat$u_Ln_ACRES = c(scale(temp_dt$Ln_ACRES),rep(NA,narep))
  idat$u_Wilderness_Perc = c(scale(temp_dt$Wilderness_Perc),rep(NA,narep)) 
  idat$u_Burned_Perc_Past5yrs = c(scale(temp_dt$Burned_Perc_Past5yrs),rep(NA,narep)) 
  idat$u_Ln_AVERAGE_YEARLY_VISITS = c(scale(temp_dt$Ln_AVERAGE_YEARLY_VISITS),rep(NA,narep)) 
  idat$u_Count_EorT_Species= c(scale(temp_dt$Count_EorT_Species),rep(NA,narep)) 
  idat$u_percentD_H = c(scale(temp_dt$percentD_H),rep(NA,narep)) 
  #idat$u_democrat = c(scale(temp_dt$democrat),rep(NA,narep))  
  idat$u_LCV_annual= c(scale(temp_dt$LCV_annual),rep(NA,narep)) 
  idat$u_mrp_mean = c(scale(temp_dt$mrp_mean),rep(NA,narep)) 
  idat$u_democrat = c(temp_dt$democrat,rep(NA,narep))
  idat$y_democrat = c(rep(NA,narep),temp_dt$democrat)
  #idat$u_nominate_dim1 = c(scale(temp_dt$nominate_dim1),rep(NA,narep))
  #idat$u_nominate_dim1_x_y_democrat =  c(scale(temp_dt$nominate_dim1) * scale(temp_dt$democrat),rep(NA,narep))
  idat$u_demPres = c(temp_dt$demPres,rep(NA,narep))
  idat$u_demCongress = c(temp_dt$demCongress,rep(NA,narep))
  idat$u_ComLCV = c(scale(temp_dt$ComLCV),rep(NA,narep))
  idat$u_ChairLCV = c(scale(temp_dt$ChairLCV),rep(NA,narep))
  idat$u_ln_County_naturalresource_GDP_1M = c(scale(temp_dt$ln_County_naturalresource_GDP_1M),rep(NA,narep))
  # idat$u_ln_County_naturalresource_GDP_1M_L1 = c(scale(temp_dt$ln_County_naturalresource_GDP_1M_L1),rep(NA,narep))
  idat$n = n;idat$y = y;idat$u = u;

  u.sdres <- sd(idat$u,na.rm = T)#sd(y_like[is.finite(y_lik)])
  y.sdres <- sd(idat$y/idat$u,na.rm=T)
  pc.prec.u = list(prec = list(prior = "pc.prec", param = c(3*u.sdres, 0.01)))
  pc.prec.y = list(prec = list(prior = "pc.prec", param = c(3*y.sdres, 0.01)))
  bprior <- list(prior = 'gaussian', param = c(0,1))
  famcontrol = list(list(prior = "pcprec", param = c(3*u.sdres,0.01)),
                    list(prior = "pcprec", param = c(3*y.sdres,0.01)))
  cinla <- list(strategy = 'adaptive', int.strategy = 'eb') 
  cres <- list(return.marginals.predictor = FALSE, 
               return.marginals.random = FALSE)
  inla.list.models()
  inla.models()$latent$'model name'
  
library(INLA)

#project_type_counts_for_model[,sum(N),by=.(Project_Type)]

#proj_count_input = dcast(counts_by_type,congress + FOREST_ID + Project_Type ~ DECISION_TYPE,value.var = 'N')
pcount = fs[fs$congress>=109&fs$congress<=115,]
table(pcount$`DECISION TYPE`)
table(pcount$`DECISION TYPE`,pcount$Type_Purpose_Extractive)

library(ggrepel)


base_linear_form = Y ~ -1 + mu.u + mu.y + 
  u_Unemp_Rate +  u_Perc_Extraction_Employ + u_ln_County_naturalresource_GDP_1M + 
  u_ln_Receipts_Extraction_1M_P4 + 
  u_Ln_ACRES + u_Wilderness_Perc + 
  u_Burned_Perc_Past5yrs  + 
  u_Ln_AVERAGE_YEARLY_VISITS + u_Count_EorT_Species + 
  u_Perc_WUI_Housing +
  u_demPres + u_demCongress + 
  u_mrp_mean +
  u_LCV_annual + 
  y_Unemp_Rate +  y_Perc_Extraction_Employ + y_ln_County_naturalresource_GDP_1M + 
  y_ln_Receipts_Extraction_1M_P4 + 
  y_Ln_ACRES + y_Wilderness_Perc + 
  y_Burned_Perc_Past5yrs  + 
  y_Ln_AVERAGE_YEARLY_VISITS + y_Count_EorT_Species + 
  y_Perc_WUI_Housing + 
  y_demPres + y_demCongress +
  y_mrp_mean +
  y_LCV_annual 


fe_form = update.formula(base_linear_form, as.formula(paste0("~ . - u_Ln_ACRES - y_Ln_Acres - u_demPres - y_demPres - u_demCongress - y_demCongress - u_CountEorT_Species - y_CountEorT_Species - u_Perc_WUI_Housing - y_Perc_WUI_Housing + ",
                                                             paste(grep('forest_id_0102$|congress_id_109|state_id_AK|region_id_01',grep('_id_',names(idat),value = T),value = T,invert = T),collapse = '+'))))
fe_form0x = update.formula(fe_form, ~ . + u_Unemp_Rate:u_LCV_annual + y_Unemp_Rate:y_LCV_annual)
fe_form1 = update.formula(fe_form, ~ . - u_LCV_annual - y_LCV_annual + u_percentD_H + y_percentD_H)
fe_form1x = update.formula(fe_form1, ~ . + u_Unemp_Rate:u_percentD_H + y_Unemp_Rate:y_percentD_H)
fe_form2 = update.formula(fe_form, ~ . - u_LCV_annual - y_LCV_annual + u_democrat + y_democrat)
fe_form2x =  update.formula(fe_form2, ~ . + u_Unemp_Rate:u_democrat + y_Unemp_Rate:y_democrat)

form_list= grep('fe_form',ls(),value = T)
require(pbapply)
mod_list = pblapply(form_list,function(f) {print(f);gc();inla(get(f),
                                                                family = c('nbinomial', 'betabinomial'),Ntrials = idat$u,
                                                                control.fixed = list(expand.factor.strategy = "inla"),
                                                                control.family = famcontrol,
                                                                control.inla = cinla,
                                                                control.results = cres,
                                                                data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE,config = TRUE),
                                                                control.predictor=list(compute=TRUE),verbose=F)},cl = 4)
names(mod_list) <- form_list

#fixef_results = lapply(seq_along(mod_list),function(x) mod_list[[x]]$summary.fixed[,c(1,3,5)] %>% mutate(coef = rownames(.),form = x,mod = mod))
saveRDS(mod_list,paste0('output/policypolitics/model_objects/fe_models_',subtypes,'.RDS'))




