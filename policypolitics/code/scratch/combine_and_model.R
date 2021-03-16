library(lme4)
library(sjPlot)
library(glmnet)
library(lavaan)
library(cluster) 
library(INLA)
library(data.table)
library(stringr)
library(tidyverse)
library(sf)
library(lwgeom)
library(ggthemes)
library(glmmTMB)

albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
td = tempdir()
admin_url ="https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip"
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(admin_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
admin_districts <- st_read(fpath)
admin_districts <- st_transform(admin_districts,crs = st_crs(albersNA))
#admin_districts  <- st_make_valid(admin_districts)
admin_districts$FORESTORGC = as.character(admin_districts$FORESTORGC)
admin_districts$FOREST_ID = admin_districts$FORESTORGC
admin_districts$FOREST_ID = formatC(admin_districts$FORESTORGC,width=4,flag = 0)
admin_districts$FORESTNAME <- as.character(admin_districts$FORESTNAME)
admin_districts = admin_districts[!admin_districts$FOREST_ID%in%c('0000','2403','2408','2400','1300','0816','0836'),]

ranger_url ="https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.RangerDistrict.zip"
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(ranger_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
ranger_districts <- st_read(fpath)
ranger_districts <- st_transform(ranger_districts,crs = st_crs(albersNA))
#ranger_districts  <- st_make_valid(ranger_districts)

ranger_districts$DISTRICTNA = as.character(ranger_districts$DISTRICTNA)
ranger_districts$RANGERDIST = as.character(ranger_districts$RANGERDIST)
ranger_districts$FORESTNAME = as.character(ranger_districts$FORESTNAME)
ranger_districts$DISTRICTOR = as.character(ranger_districts$DISTRICTOR)
ranger_districts$DISTRICT_ID  = ranger_districts$DISTRICTOR
#ranger_districts$DISTRICT_ID <- str_extract(ranger_districts$DISTRICTOR,'[0-9]{6}$')
ranger_districts$FOREST_ID = str_extract(ranger_districts$DISTRICT_ID,'^[0-9]{4}')



#fs = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vTPHD71fQvTZpNljqRNVI6lD4b26wW86ptP_dy3R_qWaxkXyOE2QCuyTzroL_38mw/pub?output=csv')
fs = fread('input/usfs_internal_data/Copy of MYTR_National Level(1100).xlsx - Decision Data Set.csv')
fs$ongoing = 0
fs2 = data.table(readRDS('input/usfs_internal_data/FS_ongoing_projects_11-2019.rds'),stringsAsFactors = F)
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
fs$DECISON_LEVEL[grepl('R[0-9]|Region All Units',fs$`LMU (ACTUAL)`)] <- 'Region'
fs$DECISON_LEVEL[grepl('National (Forest|Forests) All Units|(National|Natl) Grassland|[^Region] All Units',fs$`LMU (ACTUAL)`)]<- "National Forest/Grassland"
fs$DECISON_LEVEL[grepl('Ranger District|RD|Mgt Unit',fs$`LMU (ACTUAL)`)]<- "Ranger District/Mgt Unit"
fs$DECISON_LEVEL[grepl('National Recreation Area|National Scenic Area|National Monument|NRA|Volcanic Monument',fs$`LMU (ACTUAL)`)]<- "NRA/NSA/NM"
fs$FISCAL_YEAR <- fs$`INITIATION FY`
#fs <- fs[fs$FISCAL_YEAR>=2004 & fs$FISCAL_YEAR<2019,]
fs = fs[!fs$FOREST_ID%in%c('0000','2403','2408','2400','1300','0816','0836'),]
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='ROD'] <- 'EIS'
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='DN'] <- 'EA'
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='DM'] <- 'CX'

fs = fs[order(fs$`DECISION ID`),]
fs = fs[DECISON_LEVEL!='Region',]
#fs = fs[!duplicated(`PROJECT NUMBER`),]
fs = fs[!is.na(fs$FOREST_ID),]

fs$Congress_Year = fs$FISCAL_YEAR
fs$Congress_Year[fs$Congress_Year%in% seq(1994,2018,2)] = as.numeric(fs$Congress_Year[fs$Congress_Year%in% seq(1994,2018,2)]) - 1

fs$PROJECT_DECISON_LEVEL<-ifelse(fs$DECISON_LEVEL=='National Forest/Grassland','NATIONAL FOREST','RANGER DISTRICT/FIELD UNIT')
congress_ids = data.table(congress = rep(101:116,each=2),FISCAL_YEAR = 1989:2020)

fs = left_join(fs,congress_ids)
fs = data.table(fs)

fs_energy = fs[rowSums(fs[,grep("NG Natural gas|OL Oil|WI Wind – activity|GT Geothermal – activity|HP Hydropower – activity|ET Electric transmission – activity|SL Solar – activity",names(fs),value=T),with=F])>0,]

totcount = fs[fs$`DECISION TYPE`!='CX',.N,by=.(FOREST_ID)]
admin_districts = left_join(admin_districts,totcount)
admin_districts = admin_districts[admin_districts$FORESTNAME!='El Yunque National Forest',]
library(tigris)

us_counties = counties(class = 'sf',year = 2016)
us_counties = st_transform(us_counties ,albersNA)
#over_forest = st_intersects(us_counties,admin_districts)
#us_counties = us_counties[sapply(over_forest,length)>0,]

congress2016 = tigris::congressional_districts(year = 2016,class = 'sf')
congress2016  = st_transform(congress2016 ,albersNA)
#congress_over_forest = st_intersects(congress2016,admin_districts)
#congress2016= congress2016[sapply(congress_over_forest,length)>0,]

states = tigris::states(class = 'sf',year = 2016)
states = st_transform(states,albersNA)
states <- st_crop(states,st_bbox(admin_districts))
us_counties <- st_crop(us_counties,st_bbox(admin_districts))

gg_forests = ggplot() +  #geom_sf(data = states,col = 'grey60') +
  ggtitle('EAs + EISs by National Forest, 2004 to 2018') +
  geom_sf(data = us_counties,col = 'grey70',fill = NA) +
  geom_sf(data = st_crop(congress2016,st_bbox(admin_districts)),col = 'black',fill = NA) + 
  geom_sf(data = admin_districts,aes(fill = N,col = N)) +
  # scale_fill_gradientn(colours = rev(terrain.colors(10)[-10]),name = 'Total projects',)+
  #  scale_color_gradientn(colours = rev(terrain.colors(10)[-10]),name = 'Total projects')+
  scale_fill_viridis_c(name = 'Total projects')+
  scale_color_viridis_c(name = 'Total projects')+
  #geom_sf(data = congress2016,fill = 'blue',alpha=0.5) +
  theme_map() + theme(panel.grid = element_line(colour = "transparent"),title = element_text(size = 14),
                      legend.text = element_text(size = 12),legend.title = element_text(size= 12)) 

#ggsave(plot = gg_forests,filename = 'output/policypolitics/figures/national_forests.png',width=7,height=6,units ='in',dpi = 150)

oregon_districts = admin_districts[admin_districts$REGION=='06',]
oregon_districts = oregon_districts[sapply(st_intersects(oregon_districts,states[states$STUSPS=='OR',]),length)>0,]

oregon115count = fs[fs$`DECISION TYPE`!='CX'&fs$FOREST_ID%in%oregon_districts$FOREST_ID&fs$congress==115,.N,by=.(FOREST_ID)]
oregon_house = congress2016[congress2016$STATEFP=='41',]
oregon_house$ID = paste0('OR-',oregon_house$CD116FP)
oregon_districts = left_join(oregon_districts,oregon115count)
oregon_house$Dem_Rep = c('D','R','D','D','D')

temp_dt = readRDS('input/prepped/national_forest_covariates.RDS')
temp115 = temp_dt[congress==115,]

oregon_districts$democrat = temp115$democrat[match(oregon_districts$FOREST_ID,temp115$FOREST_ID)]
oregon_districts$lcv_annual = temp115$LCV_annual[match(oregon_districts$FOREST_ID,temp115$FOREST_ID)]
gg_oregon = ggplot() + ggtitle('Congressional Districts (115th) and\n National Forests in Oregon')+
  geom_sf(data = oregon_house,aes(fill = ID))+
  geom_sf(data = oregon_districts,aes(alpha = N),fill = 'grey20',col = NA) + 
  # scale_colour_manual(values = 'grey50',label = 'National Forest',name = NULL) + 
  scale_alpha(name = '# EA + EIS\n projects') + 
  scale_fill_tableau(name = 'Representative',labels=c('Bonamici (D)','Walden (R)','Blumenauer (D)','DeFazio (D)','Schrader (D)')) + 
  theme_map() + theme(panel.grid = element_line(colour='transparent'),legend.position = c(0.5,0.05),
                      legend.box = 'horizontal',
                      legend.background = element_rect(fill = alpha('white',0.5)),title = element_text(size = 15),
                      legend.text = element_text(size = 12),
                      legend.title = element_text(size =12))

#ggsave(plot = gg_oregon,filename = 'output/policypolitics/figures/oregon_overlap.png',width=7,height=6,units ='in')

oregon_districts$short_name = gsub(' National (Forest|Forests)$','',oregon_districts$FORESTNAME)
oregon_districts$short_name = gsub(' National Scenic Area',' NSA',oregon_districts$short_name)
oregon_house = lwgeom::st_make_valid(oregon_house)
oregon_districts = lwgeom::st_make_valid(oregon_districts)
gg_rep = ggplot() + ggtitle('House Representation (115th Congress)\n for National Forests in Oregon')+
  geom_sf(data = oregon_house,fill = c('grey50'))+
  geom_sf(data = oregon_districts,aes(fill = democrat,col = democrat)) +
  scale_fill_gradient2_tableau(palette = "Red-Blue Diverging",name = 'Democratic rep.') +
  scale_colour_gradient2_tableau(palette = "Red-Blue Diverging",name = 'Democratic rep.') +
  # scale_colour_manual(values = 'grey50',label = 'National Forest',name = NULL) + 
  geom_sf_text(data = oregon_districts,aes(label=short_name)) +
  geom_sf_label(data = oregon_house,aes(label=gsub('0','',ID)),label.padding = unit(0.1, "lines")) + 
  #scale_fill_tableau(name = 'Representative',labels=c('Bonamici (D)','Walden (R)','Blumenauer (D)','DeFazio (D)','Schrader (D)')) + 
  theme_map() + theme(panel.grid = element_line(colour='transparent'),legend.position = c(0.6,0.05),
                      legend.box = 'horizontal',
                      legend.background = element_rect(fill = alpha('white',0.5)),title = element_text(size = 15),
                      legend.text = element_text(size = 12),
                      legend.title = element_text(size =12))
#ggsave(plot = gg_rep,filename = 'output/policypolitics/figures/oregon_democrat.png',width=7,height=6,units ='in')


proj_count = fs[,.N,by=.(FOREST_ID,`DECISION TYPE`,congress)]
fs$Type_Extractive_Activity = 0 + (0<rowSums(fs[,grep('SI Grazing structural improvements – activity|GR Grazing authorizations – activity|RV Rangeland vegetation improvements – activity|RG Grazing management – purpose|TS Timber salves|SS Timber sales (salvage) – activity|OL Oil|MO Minerals or geology|NC Special products sales|GR Grazing authorizations|NG Natural gas|TM Forest products – purpose',names(fs),value=T),with=F]))
fs$Type_Fuels_Treatment = 0 + (0<rowSums(fs[,grep('FN Fuel treatments – activity|HF Fuels management – purpose',names(fs),value=T),with=F]))
fs$Type_Others_Building_Stuff = 0 + (0<rowSums(fs[,grep("WI Wind – activity|GT Geothermal – activity|HP Hydropower – activity|ET Electric transmission – activity|SL Solar – activity",names(fs),value=T),with=F]))
fs$Type_FS_Building_Stuff = 0 + (0<rowSums(fs[,grep("MF Facility maintenance – activity|FC Facility management – purpose|RD Road maintenance – activity|DS Developed site management – activity|MT Trail management – activity|RI Road improvements/construction – activity|FI Facility improvements/construction – activity",names(fs),value=T),with=F]))
fs$Type_Special_Use = 0 + (0<rowSums(fs[,grep('LA Special use authorizations – activity|SU Special use management – purpose',names(fs),value=T),with=F]))
fs$Type_RecreationWildlife = 0 + (0<rowSums(fs[,grep('PE Species population enhancements – activity|WD Wilderness management – activity|MT Trail management – activity|RA Roadless area management – activity|RW Recreation management – purpose|WF Wildlife, fish, rare plants – purpose|GA Dispersed recreation management – activity',names(fs),value=T),with=F]))

subvars = c('Type_Extractive_Activity','Type_RecreationWildlife','Type_Special_Use','Type_Fuels_Treatment')


fs$FOREST_LEVEL_DECISION = (fs$DECISON_LEVEL!='Ranger District/Mgt Unit')+0
fs = fs[congress%in%108:115,]
subset_projtypes = lapply(subvars, function(x) {
  temp = fs[,.N,by=.(FOREST_ID,`DECISION TYPE`,congress,get(x))]
  setnames(temp,c('DECISION TYPE'),c('DECISION_TYPE'))
  temp = temp[get==1,]
  temp$Project_Type <- x
  temp[,get:=NULL]
  temp
})
counts_by_type = rbindlist(subset_projtypes,use.names = T)

temp = fs[,.N,by=.(FOREST_ID,`DECISION TYPE`,congress)]

temp$Project_Type = 'All'
setnames(temp,c('DECISION TYPE'),c('DECISION_TYPE'))
counts_by_type = rbind(temp, counts_by_type)


nf = readRDS('input/prepped/national_forest_covariates.RDS')
nf= nf[!nf$FOREST_ID%in%c('0000','2403','2408','2400','1300','0816','0836'),]
nf$Num_Eco_Sections[nf$Num_Eco_Sections==0] <- 1
nf = nf[order(FOREST_ID,congress),]
nf = nf[, zoo::na.locf(.SD, na.rm = FALSE),by = .(FOREST_ID)]
nf$Prop_Extraction_Employ = nf$Prop_Forestry_Employ + nf$Prop_Mining_Employ
nf$Prop_Outdoor_Employ = nf$Prop_HuntingFishing + nf$Prop_Recreation
nf$Wilderness_Prop = as.numeric(nf$Wilderness_Prop)
nf$Burned_Prop_Past5yrs = as.numeric(nf$Burned_Prop_Past5yrs)

nf$ACRES = admin_districts$GIS_ACRES[match(nf$FOREST_ID,admin_districts$FOREST_ID)]
nf$demCongress[nf$demCongress==2] <- 0
nf$congress = as.character(nf$congress)
nf$nominate_dim1  = nf$nominate_dim1  * -1
nf$nominate_dim2  = nf$nominate_dim2  * -1
nf = nf[congress%in%c(108:115),]
nf$ComLCV = (nf$nrComLCV+nf$agComLCV)/2
nf$ChairLCV = (nf$nrChairLCV + nf$agChairLCV)/2

nf$Ln_ACRES = log(nf$ACRES)
nf$Ln_AVERAGE_YEARLY_VISITS = log(nf$Average_Yearly_Visits)
nf$Ln_Population = log(nf$Population)
nf$USFS_REGION = admin_districts$REGION[match(nf$FOREST_ID,admin_districts$FOREST_ID)]

library(zoo)
nf = nf[order(FOREST_ID,congress),]
nf = nf[order(FOREST_ID,congress), zoo::na.locf(.SD, na.rm = FALSE),by = .(FOREST_ID)]
nf = nf[order(FOREST_ID,congress), na.locf(.SD, na.rm = FALSE,fromLast=TRUE),by = .(FOREST_ID)]
nf$congress = as.numeric(nf$congress)
nf = nf[nf$congress%in%108:115,]

center_continuous_cov = TRUE

#BASE RATIO: remove nominate_dim1 & all macro variables. remove L1_TOTAL_EA_EIS based on Manny's comments? your call
#Model 2: add macro variables (there are 4 of them -- demPres + demCongress  + ComLCV + ChairLCV
#Model 3: no macro variables, interact LCV_annual X percentD_H (edited) 
#Model 4: no macros, no interaction, interact democrat X Count_Species_CriticalHabitat, democrat X Wilderness_Prop (edited) 
#Model 5: same as above, but instead of democrat use LCV_annual
#Model 6: same as above, but instead use percentD_H


all_combos = expand.grid(FOREST_ID = sort(unique(admin_districts$FOREST_ID)),congress = 108:115,DECISION_TYPE = c('CX','EA','EIS'))
all_combos = data.table(all_combos)
subtypes = unique(counts_by_type$Project_Type)

ak_voting = data.table(congress = unique(temp_dt$congress),
                       percentD_H = c(17.3,22.4,40.01,44.98,30.51,28.62,40.94,36.02)/100)

nf$percentD_H[!is.na(nf$USFS_REGION) & nf$USFS_REGION==10] <- ak_voting$percentD_H[match(nf$congress[!is.na(nf$USFS_REGION) & nf$USFS_REGION==10],ak_voting$congress)]

#imputes = nf[,mean(percentD_H,na.rm=T),by = .(congress)]
#nf$percentD_H[is.na(nf$percentD_H)] <- imputes$V1[match(nf$congress[is.na(nf$percentD_H)] ,imputes$congress)]

form0 = Y ~ 0 + mu.u + mu.y +
  f(u_forestid,model = 'iid') + f(y_forestid,model = 'iid') +
  f(u_congressid,model = 'iid') + f(y_congressid,model = 'iid') +
  f(u_regionid, model = 'iid') + f(y_regionid,model = 'iid') +
  y_Unemp_Rate +  y_Prop_Extraction_Employ + 
  y_Ln_ACRES + y_Wilderness_Prop + y_Burned_Prop_Past5yrs  + y_Ln_AVERAGE_YEARLY_VISITS + y_Count_Species_CriticalHabitat + 
  y_percentD_H + y_democrat + y_LCV_lifetime + y_nominate_dim1 + 
  u_Unemp_Rate +  u_Prop_Extraction_Employ + 
  u_Ln_ACRES + u_Wilderness_Prop + u_Burned_Prop_Past5yrs  + u_Ln_AVERAGE_YEARLY_VISITS + u_Count_Species_CriticalHabitat + 
  u_percentD_H + u_democrat + u_LCV_lifetime + u_nominate_dim1 

form1 = update.formula(form0,~. + u_demPres + u_ChairLCV + u_ComLCV + u_demCongress + y_demPres + y_ChairLCV + y_ComLCV + y_demCongress)
#Model 3: no macro variables, interact LCV_annual X percentD_H (edited) 
form2 = update.formula(form0, ~ . + u_LCV_lifetime*u_percentD_H + y_LCV_lifetime*y_percentD_H)
#Model 4: no macros, no interaction, interact democrat X Count_Species_CriticalHabitat, democrat X Wilderness_Prop (edited) 
form3 = update.formula(form0, ~ . + u_Count_Species_CriticalHabitat * u_democrat + y_Count_Species_CriticalHabitat * y_democrat + 
                         u_Wilderness_Prop * u_democrat + y_Wilderness_Prop * y_democrat)
#Model 5: same as above, but instead of democrat use LCV_annual
form4 = update.formula(form0, ~ . + u_Count_Species_CriticalHabitat * u_LCV_lifetime + y_Count_Species_CriticalHabitat * y_LCV_lifetime + 
                         u_Wilderness_Prop * u_LCV_lifetime + y_Wilderness_Prop * y_LCV_lifetime)
#Model 6: same as above, but instead use percentD_H
form5 = update.formula(form0, ~ . + u_Count_Species_CriticalHabitat * u_percentD_H + y_Count_Species_CriticalHabitat * y_percentD_H + 
                         u_Wilderness_Prop * u_percentD_H + y_Wilderness_Prop * y_percentD_H)



list_of_results = lapply(subtypes,function(mod) {
temp_eaeis_count = counts_by_type[DECISION_TYPE!='CX'&Project_Type==mod,list(EAEIS = sum(N)),by = .(FOREST_ID,congress)]
temp_eis_count = counts_by_type[DECISION_TYPE=='EIS'&Project_Type==mod,list(EIS = sum(N)),by = .(FOREST_ID,congress)]
temp_count = merge(temp_eis_count,temp_eaeis_count,all = T)
temp_dt = merge(temp_count,nf,all=T)
temp_dt$EIS[is.na(temp_dt$EIS)]<-0
temp_dt$EAEIS[is.na(temp_dt$EAEIS)]<-0
#  temp = merge(temp,all_combos,all=T)
#  temp$N[is.na(temp$N)] <- 0
n = nrow(temp_dt)
idat <- list(Y=matrix(NA,2*n,2))
idat$mu.u <- rep(1:0, each=n)
idat$mu.y <- rep(0:1, each=n)
u = temp_dt$EAEIS
y <- ifelse(temp_dt$EAEIS>0,temp_dt$EIS,NA)
idat$Y[1:n,1] <- u
idat$Y[n+1:n,2] <- y
narep = length(u)
idat$u_forestid = c(temp_dt$FOREST_ID,rep(NA,length(y)))
idat$y_forestid = c(rep(NA,narep),temp_dt$FOREST_ID)
idat$u_congressid = c(temp_dt$congress,rep(NA,length(y)))
idat$y_congressid = c(rep(NA,narep),temp_dt$congress)
idat$u_regionid = c(temp_dt$USFS_REGION,rep(NA,length(y)))
idat$y_regionid = c(rep(NA,narep),temp_dt$USFS_REGION)
#### binomial EIS / [EA+EIS] model coefficients
#idat$y_FOREST_LEVEL_DECISIONS = c(rep(NA,narep),scale(temp_dt$FOREST_LEVEL_DECISIONS))
idat$y_Unemp_Rate = c(rep(NA,narep),scale(temp_dt$Unemp_Rate))
idat$y_Prop_Extraction_Employ = c(rep(NA,narep),scale(temp_dt$Prop_Extraction_Employ))
idat$y_Ln_ACRES = c(rep(NA,narep),scale(temp_dt$Ln_ACRES))
idat$y_Wilderness_Prop = c(rep(NA,narep),scale(temp_dt$Wilderness_Prop)) 
idat$y_Burned_Prop_Past5yrs = c(rep(NA,narep),scale(temp_dt$Burned_Prop_Past5yrs)) 
idat$y_Ln_AVERAGE_YEARLY_VISITS = c(rep(NA,narep),scale(temp_dt$Ln_AVERAGE_YEARLY_VISITS)) 
idat$y_Count_Species_CriticalHabitat= c(rep(NA,narep),scale(temp_dt$Count_Species_CriticalHabitat)) 
idat$y_percentD_H = c(rep(NA,narep),scale(temp_dt$percentD_H)) 
idat$y_democrat = c(rep(NA,narep),scale(temp_dt$democrat))  
idat$y_LCV_lifetime= c(rep(NA,narep),scale(temp_dt$LCV_lifetime)) 
idat$y_nominate_dim1 = c(rep(NA,narep),scale(temp_dt$nominate_dim1))
idat$y_nominate_dim1_x_y_democrat =  c(rep(NA,narep),scale(temp_dt$nominate_dim1) * scale(temp_dt$democrat))
idat$y_demPres = c(rep(NA,narep),temp_dt$demPres)
idat$y_demCongress = c(rep(NA,narep),temp_dt$demCongress)
idat$y_ComLCV = c(rep(NA,narep),scale(temp_dt$ComLCV))
idat$y_ChairLCV = c(rep(NA,narep),scale(temp_dt$ChairLCV))


#idat$u_FOREST_LEVEL_DECISIONS = c(scale(temp_dt$FOREST_LEVEL_DECISIONS),rep(NA,narep))
idat$u_Unemp_Rate = c(scale(temp_dt$Unemp_Rate),rep(NA,narep))
idat$u_Prop_Extraction_Employ = c(scale(temp_dt$Prop_Extraction_Employ),rep(NA,narep))
idat$u_Ln_ACRES = c(scale(temp_dt$Ln_ACRES),rep(NA,narep))
idat$u_Wilderness_Prop = c(scale(temp_dt$Wilderness_Prop),rep(NA,narep)) 
idat$u_Burned_Prop_Past5yrs = c(scale(temp_dt$Burned_Prop_Past5yrs),rep(NA,narep)) 
idat$u_Ln_AVERAGE_YEARLY_VISITS = c(scale(temp_dt$Ln_AVERAGE_YEARLY_VISITS),rep(NA,narep)) 
idat$u_Count_Species_CriticalHabitat= c(scale(temp_dt$Count_Species_CriticalHabitat),rep(NA,narep)) 
idat$u_percentD_H = c(scale(temp_dt$percentD_H),rep(NA,narep)) 
idat$u_democrat = c(scale(temp_dt$democrat),rep(NA,narep))  
idat$u_LCV_lifetime= c(scale(temp_dt$LCV_lifetime),rep(NA,narep)) 
idat$u_nominate_dim1 = c(scale(temp_dt$nominate_dim1),rep(NA,narep))
idat$u_nominate_dim1_x_y_democrat =  c(scale(temp_dt$nominate_dim1) * scale(temp_dt$democrat),rep(NA,narep))
idat$u_demPres = c(temp_dt$demPres,rep(NA,narep))
idat$u_demCongress = c(temp_dt$demCongress,rep(NA,narep))
idat$u_ComLCV = c(scale(temp_dt$ComLCV),rep(NA,narep))
idat$u_ChairLCV = c(scale(temp_dt$ChairLCV),rep(NA,narep))

y_like <- log(y/u)
sdres <- sd(y_like[is.finite(y_like)])
pcprior <- list(prec = list(prior="pc.prec", param = c(sdres, 0.01)))

mod_list = lapply(grep('form[0-9]',ls(),value=T),function(f) {print(f);inla(get(f),
                                                       c('poisson', 'binomial'),Ntrials = u,
                                                       control.fixed = list(expand.factor.strategy = "inla"),
                                                       data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                                                       control.predictor=list(compute=TRUE),verbose=F)})

fixef_results = lapply(seq_along(mod_list),function(x) mod_list[[x]]$summary.fixed[,c(1,3,5)] %>% mutate(coef = rownames(.),form = x,mod = mod))
saveRDS(mod_list,paste0('output/policypolitics/model_objects/models_',mod,'.RDS'))
#rbindlist(fixef_results)
})


saveRDS(temp_dt,file = 'output/model_data_flatfile.RDS')


# base_jmod =  inla(Y ~ 0 + mu.u + mu.y +
#                     f(u_forestid,model = 'iid') + f(y_forestid,model = 'iid') +
#                     f(u_congressid,model = 'iid') + f(y_congressid,model = 'iid') +
#                     f(u_regionid, model = 'iid') + f(y_regionid,model = 'iid'),
#                   c('poisson', 'binomial'),Ntrials = u,
#                   control.fixed = list(expand.factor.strategy = "inla"),
#                   data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
#                   control.predictor=list(compute=TRUE),verbose=T)
# 
# cov_form = update.formula(base_jmod$.args$formula, ~ . + 
#                             y_FOREST_LEVEL_DECISIONS +  y_noCX_Type_Extractive_Activity + y_noCX_Type_Special_Use +
#                             y_Unemp_Rate +  y_Prop_Extraction_Employ + 
#                             y_Ln_ACRES + y_Wilderness_Prop + y_Burned_Prop_Past5yrs  + y_Ln_AVERAGE_YEARLY_VISITS + y_Count_Species_CriticalHabitat + 
#                             y_percentD_H + y_democrat + y_LCV_annual + y_nominate_dim1 + 
#                             y_nominate_dim1_x_y_democrat)

# cov_jmod =  inla(cov_form,
#                  c('poisson', 'binomial'),Ntrials = u,
#                  control.fixed = list(expand.factor.strategy = "inla"),
#                  data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
#                  control.predictor=list(compute=TRUE),verbose=T)
# 






