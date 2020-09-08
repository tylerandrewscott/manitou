
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
# 
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
#saveRDS(admin_districts,'scratch/admin_units_clean.RDS')
admin_districts <- readRDS('scratch/admin_units_clean.RDS')

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

fs$DECISION_LEVEL = NA
fs$DECISION_LEVEL[grepl('R[0-9]|Region All Units',fs$`LMU (ACTUAL)`)] <- 'Region'
fs$DECISION_LEVEL[grepl('National (Forest|Forests) All Units|(National|Natl) Grassland|[^Region] All Units',fs$`LMU (ACTUAL)`)]<- "National Forest/Grassland"
fs$DECISION_LEVEL[grepl('Ranger District|RD|Mgt Unit',fs$`LMU (ACTUAL)`)]<- "Ranger District/Mgt Unit"
fs$DECISION_LEVEL[grepl('National Recreation Area|National Scenic Area|National Monument|NRA|Volcanic Monument',fs$`LMU (ACTUAL)`)]<- "NRA/NSA/NM"
require(lubridate)

fs$CALENDAR_YEAR <- year(mdy(fs$`INITIATION DATE`))

fs = fs[!fs$FOREST_ID%in%c('0000','2403','2408','2400','1300','0816','0836','0622','0860'),]
fs = fs[order(fs$`DECISION ID`),]
fs = fs[DECISION_LEVEL!='Region',]
#fs = fs[!duplicated(`PROJECT NUMBER`),]
fs = fs[!is.na(fs$FOREST_ID),]

fs$Congress_Year = fs$CALENDAR_YEAR
fs$Congress_Year[fs$Congress_Year%in% seq(1994,2018,2)] = as.numeric(fs$Congress_Year[fs$Congress_Year%in% seq(1994,2018,2)]) - 1
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='ROD'] <- 'EIS'
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='DN'] <- 'EA'
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='DM'] <- 'CE'
fs$PROJECT_DECISION_LEVEL<-ifelse(fs$DECISION_LEVEL=='National Forest/Grassland','NATIONAL FOREST','RANGER DISTRICT/FIELD UNIT')
congress_ids = data.table(congress = rep(101:116,each=2),CALENDAR_YEAR = 1989:2020)

fs = left_join(fs,congress_ids)
fs = data.table(fs)


totcount = fs[,.N,by=.(FOREST_ID)]
admin_districts = left_join(admin_districts,totcount)
admin_districts = admin_districts[admin_districts$FORESTNAME!='El Yunque National Forest',]

nf = fread('input/prepped/national_forest_covariates.csv')
nf$FOREST_ID = formatC(nf$FOREST_ID,width = 4,flag = 0)

nf$FOREST_NAME = admin_districts$FORESTNAME[match(nf$FOREST_ID,admin_districts$FOREST_ID)]
nf = nf[congress %in% 107:116,]
fs = fs[congress%in%107:116,]
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

nf= nf[!nf$FOREST_ID%in%c('0000','2403','2408','2400','1300','0816','0836','0622','0860'),]
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

nf = nf[nf$congress%in%109:115,]

dist_by_year = full_join(admin_districts,nf[,.(FOREST_ID,LCV_annual,CALENDAR_YEAR)])
dist_by_year = dist_by_year[!is.na(dist_by_year$CALENDAR_YEAR),]

dist_by_year = full_join(admin_districts,nf[,.(FOREST_ID,Unemp_Rate ,CALENDAR_YEAR)])
dist_by_year = dist_by_year[!is.na(dist_by_year$CALENDAR_YEAR),]



makeIDAT = function(mod,nf,project_type_counts_for_model,period = period_type){
  temp_count = project_type_counts_for_model[Project_Type==mod,,]
  temp_count = dcast(temp_count ,get(period_type) + FOREST_ID ~ DECISION_TYPE,fill = 0,value.var = 'N')
  setnames(temp_count,'period_type',period_type)
  if(period=='congress'){
    temp_nf = nf[order(FOREST_ID,get(period_type)),][!duplicated(paste(FOREST_ID,congress)),]}
  if(period=='CALENDAR_YEAR'){
    temp_nf = nf[order(FOREST_ID,get(period_type)),]}
  temp_dt = merge(temp_count,temp_nf,by = c('FOREST_ID',period_type))#,all=T,by = c('FOREST_ID',congress))
  n = nrow(temp_dt)
  u = rowSums(temp_dt[,c('CE','EA','EIS'),with = F])
  narep = length(u)
  y <- ifelse(u>0,temp_dt$CE,NA)
  idat <- list(Y=matrix(NA,2*n,2))
  idat$Y[1:n,1] <- u
  idat$Y[n+1:n,2] <- y
  idat$mu.u <- rep(1:0, each=n)
  idat$mu.y <- rep(0:1, each=n)
  
  idat$u_forest_id = c(temp_dt$FOREST_ID,rep(NA,length(y)))#temp_dt$FOREST_ID)
  idat$uc_forest_id = c(rep(NA,length(y)),idat$u_forest_id[1:length(u)])
  idat$y_forest_id = c(rep(NA,length(y)),idat$u_forest_id[1:length(u)])
  
  idat$u_congress_id = c(temp_dt$congress,rep(NA,length(y)))
  idat$uc_congress_id =  c(rep(NA,length(y)),idat$u_congress_id[1:length(u)])
  idat$y_congress_id = c(rep(NA,length(y)),idat$u_congress_id[1:length(u)])
  
  idat$u_year_id = c(temp_dt$CALENDAR_YEAR,rep(NA,length(y)))
  idat$uc_year_id =  c(rep(NA,length(y)),idat$u_year_id[1:length(u)])
  idat$y_year_id =   c(rep(NA,length(y)),idat$u_year_id[1:length(u)])
  
  idat$u_state_id = c(temp_dt$STATE,rep(NA,length(y)))
  idat$uc_state_id = c(rep(NA,length(y)),idat$u_state_id[1:length(u)])
  idat$y_state_id = c(rep(NA,length(y)),idat$u_state_id[1:length(u)])
  
  idat$u_region_id = c(temp_dt$USFS_REGION,rep(NA,length(y)))
  idat$uc_region_id = c(rep(NA,length(y)),idat$u_region_id[1:length(u)])
  idat$y_region_id = c(rep(NA,length(y)),idat$u_region_id[1:length(u)])
  
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
  #idat$y_Perc_Extraction_Employ_L1 = c(rep(NA,narep),scale(temp_dt$Perc_Extraction_Employ_L1))
  idat$y_Ln_ACRES = c(rep(NA,narep),scale(temp_dt$Ln_ACRES))
  idat$y_Wilderness_Perc = c(rep(NA,narep),scale(temp_dt$Wilderness_Perc)) 
  idat$y_Burned_Perc_Past5yrs = c(rep(NA,narep),scale(temp_dt$Burned_Perc_Past5yrs)) 
  idat$y_Ln_AVERAGE_YEARLY_VISITS = c(rep(NA,narep),scale(temp_dt$Ln_AVERAGE_YEARLY_VISITS)) 
  idat$y_Count_EorT_Species= c(rep(NA,narep),scale(temp_dt$Count_EorT_Species)) 
  idat$y_percentD_H = c(rep(NA,narep),scale(temp_dt$percentD_H)) 
  #idat$y_democrat = c(rep(NA,narep),scale(temp_dt$democrat))  
  idat$y_LCV_annual= c(rep(NA,narep),scale(temp_dt$LCV_annual))
  idat$y_mrp_mean= c(rep(NA,narep),scale(temp_dt$mrp_mean))
  
  #idat$y_nominate_dim1 = c(rep(NA,narep),scale(temp_dt$nominate_dim1))
  #idat$y_nominate_dim1_x_y_democrat =  c(rep(NA,narep),scale(temp_dt$nominate_dim1) * scale(temp_dt$democrat))
  idat$y_demPres = c(rep(NA,narep),temp_dt$demPres)
  idat$y_demCongress = c(rep(NA,narep),temp_dt$demCongress)
  idat$y_ComLCV = c(rep(NA,narep),scale(temp_dt$ComLCV))
  idat$y_ChairLCV = c(rep(NA,narep),scale(temp_dt$ChairLCV))
  idat$y_Perc_WUI_Housing = c(rep(NA,narep),scale(temp_dt$Perc_WUI_Housing))
  
  idat$y_ln_County_naturalresource_GDP_1M = c(rep(NA,narep),scale(temp_dt$ln_County_naturalresource_GDP_1M))
  #  idat$y_ln_County_naturalresource_GDP_1M_L1 = c(rep(NA,narep),scale(temp_dt$ln_County_naturalresource_GDP_1M_L1))
  idat$y_Total_Receipts_4yr_Change_Perc = c(rep(NA,narep),scale(temp_dt$Total_Receipts_4yr_Change_Perc))
  #idat$u_FOREST_LEVEL_DECISIONS = c(scale(temp_dt$FOREST_LEVEL_DECISIONS),rep(NA,narep))
  idat$u_Unemp_Rate = c(scale(temp_dt$Unemp_Rate),rep(NA,narep))
  # idat$u_Unemp_Rate_L1 = c(scale(temp_dt$Unemp_Rate_L1),rep(NA,narep))
  idat$u_Ln_Avg_MBF_Cut_1999_2004 = c(scale(temp_dt$Ln_Avg_MBF_Cut_1999_2004),rep(NA,narep))
  idat$u_ALLOTMENT_NEPA_1993_2004 = c(scale(temp_dt$ALLOTMENT_NEPA_1993_2004),rep(NA,narep))
  idat$u_MINING_CLAIM_ACTIONS_1993_2004 = c(scale(temp_dt$MINING_CLAIM_ACTIONS_1993_2004),rep(NA,narep))
  idat$u_ln_Receipts_Extraction_1M_P4 = c(scale(temp_dt$ln_Receipts_Extraction_1M_P4),rep(NA,narep))
  idat$u_ln_Receipts_Recreation_1M_P4 = c(scale(temp_dt$ln_Receipts_Recreation_1M_P4),rep(NA,narep))
  idat$u_Perc_Extraction_Employ = c(scale(temp_dt$Perc_Extraction_Employ),rep(NA,narep))
  #  idat$u_Perc_Extraction_Employ_L1 = c(scale(temp_dt$Perc_Extraction_Employ_L1),rep(NA,narep))
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
  idat$n = n;idat$y = y;idat$u = u
  return(idat)}


form_separate_u = Y[,1] ~ 0 + mu.u + 
  f(u_forest_id,model = 'iid',hyper = pc.prec.u) + 
  f(u_congress_id,model = 'iid',hyper = pc.prec.u) + 
  f(u_region_id, model = 'iid',hyper = pc.prec.u) + 
  f(u_state_id, model = 'iid',hyper = pc.prec.u) + 
  u_Unemp_Rate +  u_Perc_Extraction_Employ + u_ln_County_naturalresource_GDP_1M + 
  u_ln_Receipts_Extraction_1M_P4 + u_ln_Receipts_Recreation_1M_P4 +
  u_Total_Receipts_4yr_Change_Perc +
  u_Ln_ACRES + u_Wilderness_Perc + 
  u_Burned_Perc_Past5yrs  + 
  u_Ln_AVERAGE_YEARLY_VISITS + u_Count_EorT_Species + 
  u_Perc_WUI_Housing +
  u_demPres + u_demCongress + 
  u_mrp_mean +
  u_LCV_annual

form_separate_y = Y[,2] ~ 0 + mu.y +
  f(y_forest_id,model = 'iid',hyper = pc.prec.y) +
  f(y_congress_id,model = 'iid',hyper = pc.prec.y) +
  f(y_region_id,model = 'iid',hyper = pc.prec.y) +
  f(y_state_id,model = 'iid',hyper = pc.prec.y) +
  y_Unemp_Rate +  y_Perc_Extraction_Employ + y_ln_County_naturalresource_GDP_1M + 
  y_ln_Receipts_Extraction_1M_P4 + y_ln_Receipts_Recreation_1M_P4 +
  y_Total_Receipts_4yr_Change_Perc +
  y_Ln_ACRES + y_Wilderness_Perc + 
  y_Burned_Perc_Past5yrs  + 
  y_Ln_AVERAGE_YEARLY_VISITS + y_Count_EorT_Species + 
  y_Perc_WUI_Housing + 
    y_demPres + y_demCongress +
    y_mrp_mean +
  y_LCV_annual


  form_joint = Y ~ 0 + mu.u + 
    f(u_forest_id,model = 'iid',hyper = pc.prec.u) + 
    f(u_congress_id,model = 'iid',hyper = pc.prec.u) + 
    f(u_region_id, model = 'iid',hyper = pc.prec.u) + 
    f(u_state_id, model = 'iid',hyper = pc.prec.u) + 
    f(y_forest_id,model = 'iid',hyper = pc.prec.y) +
    f(y_congress_id,model = 'iid',hyper = pc.prec.y) +
    f(y_region_id,model = 'iid',hyper = pc.prec.y) +
    f(y_state_id,model = 'iid',hyper = pc.prec.y) +
    u_Unemp_Rate +  u_Perc_Extraction_Employ + u_ln_County_naturalresource_GDP_1M + 
    u_ln_Receipts_Extraction_1M_P4 + u_ln_Receipts_Recreation_1M_P4 +
    u_Total_Receipts_4yr_Change_Perc +
    u_Ln_ACRES + u_Wilderness_Perc + 
    u_Burned_Perc_Past5yrs  + 
    u_Ln_AVERAGE_YEARLY_VISITS + u_Count_EorT_Species + 
    u_Perc_WUI_Housing +
    u_demPres + u_demCongress + 
    u_mrp_mean +
    u_LCV_annual + 
    mu.y +
    y_Unemp_Rate +  y_Perc_Extraction_Employ + y_ln_County_naturalresource_GDP_1M + 
    y_ln_Receipts_Extraction_1M_P4 + y_ln_Receipts_Recreation_1M_P4 +
    y_Total_Receipts_4yr_Change_Perc +
    y_Ln_ACRES + y_Wilderness_Perc + 
    y_Burned_Perc_Past5yrs  + 
    y_Ln_AVERAGE_YEARLY_VISITS + y_Count_EorT_Species + 
    y_Perc_WUI_Housing + 
    y_demPres + y_demCongress +
    y_mrp_mean +
    y_LCV_annual 


form_joint_shared = update.formula(form_joint, . ~ . +
    f(uc_forest_id,copy = 'u_forest_id',fixed = F,hyper = list(theta=bprior)) +
    f(uc_congress_id,copy = 'u_congress_id',fixed = F,hyper = list(theta=bprior))+
    f(uc_region_id,copy = 'u_region_id',fixed = F,hyper = list(theta=bprior)) +
    f(uc_state_id,copy = 'u_state_id',fixed = F,hyper = list(theta=bprior)))
   

form_shared_only = update.formula(form_joint_shared, . ~ . - 
    f(y_forest_id,model = 'iid',hyper = pc.prec.y)
    -f(y_congress_id,model = 'iid',hyper = pc.prec.y)
    -f(y_region_id,model = 'iid',hyper = pc.prec.y)
    -f(y_state_id,model = 'iid',hyper = pc.prec.y))
                                    
                                
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


library(INLA)

#project_type_counts_for_model[,sum(N),by=.(Project_Type)]

#proj_count_input = dcast(counts_by_type,congress + FOREST_ID + Project_Type ~ DECISION_TYPE,value.var = 'N')
pcount = fs[fs$congress>=109&fs$congress<=115,]
library(ggrepel)

flist = list(form_joint,form_joint_shared,form_shared_only,list(form_separate_u,form_separate_y))
names(flist) <- c('joint','joint_shared','shared','separate')
names(flist$separate) <- c('nbinomial', 'betabinomial')


subtypes = subtypes[grepl('EXTRACTIVE|WILDLIFE',toupper(subtypes))]


#### code from Krainski et al. 2018
getfit <- function(r) {
  fam <- r$dic$family
  data.frame(dic = tapply(r$dic$local.dic, fam, sum), 
             waic = tapply(r$waic$local.waic, fam, sum), 
             cpo = tapply(r$cpo$cpo, fam, 
                          function(x) - sum(log(x), na.rm = TRUE)))
}

for(mod in subtypes){

idat = makeIDAT(mod = mod,nf = nf,project_type_counts_for_model = counts_by_type)


u.sdres <- sd(idat$u,na.rm = T)
y.sdres <- sd(idat$y/idat$u,na.rm=T)
pc.prec.u = list(prec = list(prior = "pc.prec", param = c(3*u.sdres, 0.01)))
pc.prec.y = list(prec = list(prior = "pc.prec", param = c(3*y.sdres, 0.01)))
bprior <- list(prior = 'gaussian', param = c(0,1))
famcontrol = list(list(prior = "pcprec", param = c(3*u.sdres,0.01)),
                  list(prior = "pcprec", param = c(3*y.sdres,0.01)))
cinla <- list(strategy = 'adaptive', int.strategy = 'eb') 
cres <- list(return.marginals.predictor = FALSE, 
             return.marginals.random = FALSE)


idat$u_forest_id <- forest_index$index[match(idat$u_forest_id,forest_index$forest_id)]
idat$y_forest_id <- forest_index$index[match(idat$y_forest_id,forest_index$forest_id)] + nrow(forest_index)
idat$uc_forest_id <-  idat$u_forest_id

idat$u_region_id <- region_index$index[match(idat$u_region_id,region_index$region_id)]
idat$y_region_id <- region_index$index[match(idat$y_region_id,region_index$region_id)] + nrow(region_index)
idat$uc_region_id <-  idat$u_region_id

idat$u_state_id <- state_index$index[match(idat$u_state_id,state_index$state_id)]
idat$y_state_id <- state_index$index[match(idat$y_state_id,state_index$state_id)] + nrow(state_index)
idat$uc_state_id <-  idat$u_state_id

idat$u_congress_id <- congress_index$index[match(idat$u_congress_id,congress_index$congress_id)]
idat$y_congress_id <- congress_index$index[match(idat$y_congress_id,congress_index$congress_id)] + nrow(congress_index)
idat$uc_congress_id <-  idat$u_congress_id

mod.joint = inla(formula = form_joint,
            family = c('nbinomial', 'betabinomial'),Ntrials = idat$u,
            control.fixed = list(expand.factor.strategy = "inla"),
            control.family = famcontrol,
            control.inla = cinla,
            control.results = cres,
            data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE,config = TRUE),
            control.predictor=list(compute=TRUE),verbose=F)

mod.joint.shared = inla(formula = form_joint_shared,
                 family = c('nbinomial', 'betabinomial'),Ntrials = idat$u,
                 control.fixed = list(expand.factor.strategy = "inla"),
                 control.family = famcontrol,
                 control.inla = cinla,
                 control.results = cres,
                 data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE,config = TRUE),
                 control.predictor=list(compute=TRUE),verbose=F)

mod.shared.only = inla(formula = form_shared_only,
                 family = c('nbinomial', 'betabinomial'),Ntrials = idat$u,
                 control.fixed = list(expand.factor.strategy = "inla"),
                 control.family = famcontrol,
                 control.inla = cinla,
                 control.results = cres,
                 data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE,config = TRUE),
                 control.predictor=list(compute=TRUE),verbose=F)

mod.separate.u = inla(formula = form_separate_u,
                      family = c('nbinomial'),
                      control.fixed = list(expand.factor.strategy = "inla"),
                      control.family = famcontrol[[1]],
                      control.inla = cinla,
                      control.results = cres,
                      data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE,config = TRUE),
                      control.predictor=list(compute=TRUE),verbose=F)
mod.separate.y = inla(formula = form_separate_y,
                      family = c('betabinomial'),Ntrials = idat$u,
                      control.fixed = list(expand.factor.strategy = "inla"),
                      control.family = famcontrol[[2]],
                      control.inla = cinla,
                      control.results = cres,
                      data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE,config = TRUE),
                      control.predictor=list(compute=TRUE),verbose=F)

fwrite(data.table(rbind(joint = getfit(mod.joint), 
      joint.shared = getfit(mod.joint.shared), 
      shared.only = getfit(mod.shared.only),
      separate = rbind(getfit(mod.separate.u),
            getfit(mod.separate.y)))[c(1, 3, 5,7, 2, 4, 6.,8),],keep.rownames = T),
      file = paste0('output/policypolitics/tables/variance_gof_',mod,'.csv'))
}


wildlife_gof = fread('output/policypolitics/tables/variance_gof_Type_Purpose_Recreation_Wildlife.csv')
extraction_gof = fread('output/policypolitics/tables/variance_gof_Type_Purpose_Extractive.csv')

sdn = c('dic','waic','cpo')
extraction_gof[,(sdn):=lapply(.SD,round,2),.SDcols = sdn]
htmlTable::htmlTable(extraction_gof[order(dic),])
