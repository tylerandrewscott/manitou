
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
#if(!require(INLA)){install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)}
library(INLA)
inla.setOption(pardiso.license="../pardiso.lic")
readLines(inla.getOption("pardiso.license"))
inla.pardiso.check()

start_year = 2005
end_year = 2018
if(!require(data.table)){install.packages('data.table');require(data.table)}
if(!require(stringr)){install.packages('stringr');require(stringr)}
if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
if(!require(sf)){install.packages('sf');require(sf)}
if(!require(lwgeom)){install.packages('lwgeom');require(lwgeom)}
if(!require(ggthemes)){install.packages('ggthemes');require(ggthemes)}

##### this determines whether models run on per-congress (i.e., two year windows) #####
# for "CALENDAR_YEAR" or "congress"
period_type = 'CALENDAR_YEAR'

#### this determines which projects to include ######
keep_decisions = c('EIS','EA','CE')

#### this determines whether ot keep purpose or activity or both
keep_activities = FALSE
keep_purpose = TRUE


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

file.remove(list.files('output/policypolitics/tables/',pattern = 'coefs',full.names = T))
states = tigris::states(class = 'sf')
states <- st_transform(states,crs = st_crs(albersNA))
test = st_within(st_centroid(admin_districts),states)
admin_districts$STATE = states[unlist(test),]$STUSPS

# ranger_url ="https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.RangerDistrict.zip"
# tf = tempfile(tmpdir=td, fileext=".zip")
# download.file(ranger_url, tf)
# fname = unzip(tf, list=TRUE)
# unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
# fpath = file.path(td, grep('shp$',fname$Name,value=T))
# ranger_districts <- st_read(fpath)
# ranger_districts <- st_transform(ranger_districts,crs = st_crs(albersNA))
# #ranger_districts  <- st_make_valid(ranger_districts)
# 
# ranger_districts$DISTRICTNA = as.character(ranger_districts$DISTRICTNA)
# ranger_districts$RANGERDIST = as.character(ranger_districts$RANGERDIST)
# ranger_districts$FORESTNAME = as.character(ranger_districts$FORESTNAME)
# ranger_districts$DISTRICTOR = as.character(ranger_districts$DISTRICTOR)
# ranger_districts$DISTRICT_ID  = ranger_districts$DISTRICTOR
# #ranger_districts$DISTRICT_ID <- str_extract(ranger_districts$DISTRICTOR,'[0-9]{6}$')
# ranger_districts$FOREST_ID = str_extract(ranger_districts$DISTRICT_ID,'^[0-9]{4}')

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
fs$DECISON_LEVEL[grepl('R[0-9]|Region All Units',fs$`LMU (ACTUAL)`)] <- 'Region'
fs$DECISON_LEVEL[grepl('National (Forest|Forests) All Units|(National|Natl) Grassland|[^Region] All Units',fs$`LMU (ACTUAL)`)]<- "National Forest/Grassland"
fs$DECISON_LEVEL[grepl('Ranger District|RD|Mgt Unit',fs$`LMU (ACTUAL)`)]<- "Ranger District/Mgt Unit"
fs$DECISON_LEVEL[grepl('National Recreation Area|National Scenic Area|National Monument|NRA|Volcanic Monument',fs$`LMU (ACTUAL)`)]<- "NRA/NSA/NM"
require(lubridate)
fs$CALENDAR_YEAR <- year(mdy(fs$`INITIATION DATE`))

fs = fs[!fs$FOREST_ID%in%c('0000','2403','2408','2400','1300','0816','0836'),]
fs = fs[order(fs$`DECISION ID`),]
fs = fs[DECISON_LEVEL!='Region',]
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
admin_districts = admin_districts[admin_districts$FORESTNAME!='El Yunque National Forest',]
library(tigris)
us_counties = counties(class = 'sf',year = 2017)
us_counties = st_transform(us_counties ,albersNA)
#over_forest = st_intersects(us_counties,admin_districts)
#us_counties = us_counties[sapply(over_forest,length)>0,]

congress2015 = tigris::congressional_districts(year = 2015,class = 'sf')
congress2015  = st_transform(congress2015 ,albersNA)
#congress_over_forest = st_intersects(congress2015,admin_districts)
#congress2015= congress2015[sapply(congress_over_forest,length)>0,]

states = tigris::states(class = 'sf',year = 2015)
states = st_transform(states,albersNA)
states <- st_crop(states,st_bbox(admin_districts))
us_counties <- st_crop(us_counties,st_bbox(admin_districts))

gg_forests = ggplot() +  #geom_sf(data = states,col = 'grey60') +
  ggtitle('EAs + EISs by National Forest, 2005 to 2018') +
  #geom_sf(data = us_counties,col = 'grey70',fill = NA) +
  geom_sf(data = st_crop(congress2015,st_bbox(admin_districts)),
          fill = NA,lwd = 0.25,col = 'grey50',aes(linetype = as.factor(1))) + 
  geom_sf(data = admin_districts,aes(fill = N,col = N)) +
  # scale_fill_gradientn(colours = rev(terrain.colors(10)[-10]),name = 'Total projects',)+
  #  scale_color_gradientn(colours = rev(terrain.colors(10)[-10]),name = 'Total projects')+
  scale_fill_viridis_c(name = '# projects')+
  scale_color_viridis_c(name = '# projects')+
  scale_linetype(name = '',labels = 'House district') + 
  #geom_sf(data = congress2015,fill = 'blue',alpha=0.5) +
  guides(linetype = guide_legend(override.aes = list(fill = NA,lty = 1,col = 'grey50',lwd = 1),title = NULL)) + 
  theme_map() + theme(legend.position = c(0,.1),panel.grid = element_line(colour = "transparent"),title = element_text(size = 14),
                      legend.text = element_text(size = 12),legend.title = element_text(size= 12))

ggsave(plot = gg_forests,filename = 'output/policypolitics/figures/national_forests.png',width=7,height=6,units ='in',dpi = 300)

oregon_districts = admin_districts[admin_districts$REGION=='06',]
oregon_districts = oregon_districts[sapply(st_intersects(oregon_districts,states[states$STUSPS=='OR',]),length)>0,]

oregon115count = fs[fs$`DECISION TYPE`!='CE'&fs$FOREST_ID%in%oregon_districts$FOREST_ID&fs$congress==115,.N,by=.(FOREST_ID)]
oregon_house = congress2015[congress2015$STATEFP=='41',]
oregon_house$ID = paste0('OR-',oregon_house$CD114FP)
oregon_districts = left_join(oregon_districts,oregon115count)
oregon_house$Dem_Rep = c('D','R','D','D','D')

nf = fread('input/prepped/national_forest_covariates.csv')
setnames(nf,'FISCAL_YEAR','CALENDAR_YEAR')
nf$FOREST_ID = formatC(nf$FOREST_ID,width = 4,flag = 0)
nf$FOREST_NAME = admin_districts$FORESTNAME[match(nf$FOREST_ID,admin_districts$FOREST_ID)]
nf = nf[congress %in% 108:115,]

nf115 = nf[congress==115,]
oregon_districts$LCV_annual = nf115$LCV_annual[match(oregon_districts$FOREST_ID,nf115$FOREST_ID)]

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

ggsave(plot = gg_oregon,filename = 'output/policypolitics/figures/oregon_overlap.png',width=7,height=6,units ='in')
library(ggthemes)
oregon_districts$short_name = gsub(' National (Forest|Forests)$','',oregon_districts$FORESTNAME)
oregon_districts$short_name = gsub(' National Scenic Area',' NSA',oregon_districts$short_name)
oregon_house = lwgeom::st_make_valid(oregon_house)
oregon_districts = lwgeom::st_make_valid(oregon_districts)
(gg_rep = ggplot() + ggtitle('House Representation (115th Congress)\n for National Forests in Oregon')+
    geom_sf(data = oregon_house,col = NA,fill='grey90')+
    geom_sf(data = oregon_districts,aes(fill = LCV_annual,col = LCV_annual)) +
    geom_sf(data = oregon_house,lty = 1,fill = NA)+
    scale_fill_gradient_tableau(palette = 'Classic Green',name = 'LCV annual',limits = c(0,100)) +
    scale_colour_gradient_tableau(palette = 'Classic Green',name = 'LCV annual',limits = c(0,100)) +
    geom_sf_text(data = oregon_districts,aes(label=short_name)) +
    geom_sf_label(data = oregon_house,aes(label=gsub('0','',ID)),label.padding = unit(0.1, "lines")) + 
    #scale_fill_tableau(name = 'Representative',labels=c('Bonamici (D)','Walden (R)','Blumenauer (D)','DeFazio (D)','Schrader (D)')) + 
    theme_map() + theme(panel.grid = element_line(colour='transparent'),legend.position = c(0.6,0.05),
                        legend.box = 'horizontal',
                        legend.background = element_rect(fill = alpha('white',0.5)),title = element_text(size = 15),
                        legend.text = element_text(size = 12),
                        legend.title = element_text(size =12)))

ggsave(plot = gg_rep,filename = 'output/policypolitics/figures/oregon_lcv_annual.png',width=7,height=6,units ='in')

fs = fs[congress%in%109:115,]

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

library(lubridate)
durations = fs[,list(mean(mdy(`DECISION SIGNED`) - mdy(`INITIATION DATE`),na.rm=T),median(mdy(`DECISION SIGNED`) - mdy(`INITIATION DATE`),na.rm=T)),by = .(`DECISION TYPE`)]
names(durations) <- c('Type','mean','median')

table(fs$`DECISION TYPE`[fs$Type_Purpose_Extractive==1],
      fs$`FN Fuel treatments – activity`[fs$Type_Purpose_Extractive==1])


table(ifelse(fs$`TM Forest products – purpose`==1,'Purpose:products',0),
      ifelse(fs$`HF Fuels management – purpose`==1,'Purpose:fuels',0))

table(fs$`VM Vegetation management (non-forest products) – purpose`,
      fs$`TM Forest products – purpose`,
      fs$`HF Fuels management – purpose`)


#counts_by_type[FOREST_ID=='1004']
nf= nf[!nf$FOREST_ID%in%c('0000','2403','2408','2400','1300','0816','0836'),]
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

#BASE RATIO: remove nominate_dim1 & all macro variables. remove L1_TOTAL_EA_EIS based on Manny's comments? your call
#Model 2: add macro variables (there are 4 of them -- demPres + demCongress  + ComLCV + ChairLCV
#Model 3: no macro variables, interact LCV_annual X percentD_H (edited) 
#Model 4: no macros, no interaction, interact democrat X Count_EorT_Species, democrat X Wilderness_Prop (edited) 
#Model 5: same as above, but instead of democrat use LCV_annual
#Model 6: same as above, but instead use percentD_H

if(period_type == 'congress'){
  all_combos = expand.grid(FOREST_ID = sort(unique(admin_districts$FOREST_ID)),congress = 109:115,DECISION_TYPE = c('CE','EA','EIS'))
}
if(period_type !='congress'){
  all_combos = expand.grid(FOREST_ID = sort(unique(admin_districts$FOREST_ID)),CALENDAR_YEAR = start_year:end_year,DECISION_TYPE = c('CE','EA','EIS'))
}

all_combos = data.table(all_combos)
subtypes = unique(counts_by_type$Project_Type)

ak_voting = data.table(congress = 109:115,
                       percentD_H = rep(c(22.44,40.01,44.98,30.51,28.62,40.94,36.02)/100))

nf$percentD_H[!is.na(nf$USFS_REGION) & nf$USFS_REGION==10] <- ak_voting$percentD_H[match(nf$congress[!is.na(nf$USFS_REGION) & nf$USFS_REGION==10],ak_voting$congress)]

#nf$Restricted_Prop = nf$Limited_Use_Prop + nf$Wilderness_Prop
#nf$Restricted_Prop[nf$Restricted_Prop>1]<-1
#nf$Restricted_Perc = nf$Restricted_Prop * 100



#nf[order(FOREST_ID,congress,CALENDAR_YEAR),ln_County_naturalresource_GDP_1M_L1:=lag(ln_County_naturalresource_GDP_1M),by=.(FOREST_ID)]
#nf[order(FOREST_ID,congress,CALENDAR_YEAR),Unemp_Rate_L1:=lag(Unemp_Rate),by=.(FOREST_ID)]
#nf[order(FOREST_ID,congress,CALENDAR_YEAR),Perc_Extraction_Employ_L1:=lag(Perc_Extraction_Employ),by=.(FOREST_ID)]
nf$`Avg_MBF_Cut_1999-2004`[is.na(nf$`Avg_MBF_Cut_1999-2004`)]<-0
nf$Ln_Avg_MBF_Cut_1999_2004 = log(nf$`Avg_MBF_Cut_1999-2004`+0.001)
#setnames(nf,'LAU','Unemp_Rate')
nf = nf[order(FOREST_ID,CALENDAR_YEAR),Unemp_Rate:=lag(LAU_October),by = .(FOREST_ID)]

nf$STATE = admin_districts$STATE[match(nf$FOREST_ID,admin_districts$FOREST_ID)]

dist_by_year = full_join(admin_districts,nf[,.(FOREST_ID,LCV_annual,CALENDAR_YEAR)])
dist_by_year = dist_by_year[!is.na(dist_by_year$CALENDAR_YEAR),]
tt = ggplot() + geom_sf(data = dist_by_year,aes(fill = LCV_annual,colour = LCV_annual)) + 
  facet_wrap(~CALENDAR_YEAR,ncol = 3) + 
  ggtitle('LCV annual by year and forest') + theme_map() + scale_fill_viridis_c() + 
  scale_colour_viridis_c()
#ggsave(tt,dpi = 300,filename = 'output/policypolitics/figures/LCV_by_forest_and_year.png',height = 12,width = 8,units = 'in')



dist_by_year = full_join(admin_districts,nf[,.(FOREST_ID,Unemp_Rate ,CALENDAR_YEAR)])
dist_by_year = dist_by_year[!is.na(dist_by_year$CALENDAR_YEAR),]
tt = ggplot() + geom_sf(data = dist_by_year,aes(fill = Unemp_Rate,colour = Unemp_Rate)) + 
  facet_wrap(~CALENDAR_YEAR,ncol = 3) + 
  ggtitle('Unemp. % by year and forest') + theme_map() + scale_fill_viridis_c() + 
  scale_colour_viridis_c()
#ggsave(tt,dpi = 300,filename = 'output/policypolitics/figures/Unemp_by_forest_and_year.png',height = 12,width = 8,units = 'in')



form0 = Y ~ 0 + mu.u + mu.y +
  f(u_forest_id,model = 'iid') + #,hyper = pc.prec.u) + 
  f(y_forest_id,model = 'iid') + #,hyper = pc.prec.y) +
  f(u_congress_id,model = 'iid') + #,hyper = pc.prec.u) + 
   f(y_congress_id,model = 'iid')+ #,hyper = pc.prec.y) +
  f(u_region_id, model = 'iid')+ #,hyper = pc.prec.u) + 
  f(y_region_id,model = 'iid') + #,hyper = pc.prec.y) +
  f(u_state_id, model = 'iid') + #,hyper = pc.prec.u) + 
  f(y_state_id,model = 'iid') + #,hyper = pc.prec.y) +
  y_Unemp_Rate +  y_Perc_Extraction_Employ + y_ln_County_naturalresource_GDP_1M + 
  y_Ln_ACRES + y_Wilderness_Perc + #y_Limited_Use_Perc + 
  y_Burned_Perc_Past5yrs  + 
  y_Ln_AVERAGE_YEARLY_VISITS + y_Count_EorT_Species + 
  y_Perc_WUI_Housing + 
  u_Unemp_Rate +  u_Perc_Extraction_Employ + u_ln_County_naturalresource_GDP_1M + 
  u_Ln_ACRES + u_Wilderness_Perc + #u_Limited_Use_Perc + 
  u_Burned_Perc_Past5yrs  + 
  u_Ln_AVERAGE_YEARLY_VISITS + u_Count_EorT_Species + 
  u_Perc_WUI_Housing +
  u_demPres + u_demCongress + y_demPres + y_demCongress +
  u_LCV_annual + y_LCV_annual 
#   u_Ln_Avg_MBF_Cut_1999_2004 + y_Ln_Avg_MBF_Cut_1999_2004 +
#  u_ALLOTMENT_NEPA_1993_2004 + y_ALLOTMENT_NEPA_1993_2004 +
#  u_MINING_CLAIM_ACTIONS_1993_2004 + y_MINING_CLAIM_ACTIONS_1993_2004
form1 = update.formula(form0, ~ . + u_LCV_annual:u_Unemp_Rate + y_LCV_annual:y_Unemp_Rate)
form2 = update.formula(form0, ~ . - u_LCV_annual - y_LCV_annual + u_percentD_H + y_percentD_H)
form3 = update.formula(form2, ~ . + u_percentD_H:u_Unemp_Rate + y_percentD_H:y_Unemp_Rate)
form4 = update.formula(form0, ~ . - u_LCV_annual - y_LCV_annual + u_democrat + y_democrat)
form5 = update.formula(form4, ~ . + u_democrat:u_Unemp_Rate + y_democrat:y_Unemp_Rate)

#form2C = update.formula(form0, ~ . + u_LCV_annual*u_Prop_Extraction_Employ + y_LCV_annual*y_Prop_Extraction_Employ)
#form2D = update.formula(form0, ~ . + u_LCV_annual*u_ln_County_naturalresource_GDP_1M + y_LCV_annual*y_ln_County_naturalresource_GDP_1M)

#form2E = update.formula(form0, ~ . + u_percentD_H*u_Unemp_Rate + y_percentD_H*y_Unemp_Rate)
#form2F = update.formula(form0, ~ . + u_percentD_H*u_Prop_Extraction_Employ + y_percentD_H*y_Prop_Extraction_Employ)
#form2G = update.formula(form0, ~ . + u_percentD_H*u_ln_County_naturalresource_GDP_1M + y_percentD_H*y_ln_County_naturalresource_GDP_1M)

#model 4: no macros, itneract lcv annual w/ wilderness and species
#form3A = update.formula(form0, ~ . + u_Wilderness_Prop : u_LCV_annual + y_Wilderness_Prop : y_LCV_annual)
#form3B = update.formula(form0, ~ . + u_Count_EorT_Species : u_LCV_annual + y_Count_EorT_Species : y_LCV_annual)

#form4A = update.formula(form0, ~ . + u_Wilderness_Prop : u_percentD_H + y_Wilderness_Prop : y_percentD_H)
#form4B = update.formula(form0, ~ . + u_Count_EorT_Species : u_percentD_H  + y_Count_EorT_Species : y_percentD_H)

#form3C = update.formula(form0, ~ . + u_Ln_AVERAGE_YEARLY_VISITS  : u_LCV_annual + y_Ln_AVERAGE_YEARLY_VISITS  : y_LCV_annual)
#Model 6: same as above, but instead use percentD_H
#form5A = update.formula(form0, ~ . + u_Wilderness_Prop : u_percentD_H + y_Wilderness_Prop : y_percentD_H)
#form5B = update.formula(form0, ~ . + u_Count_EorT_Species : u_percentD_H + y_Count_EorT_Species : y_percentD_H)

list_of_forms = grep('form[0-9]',ls(),value=T)
raw_vars = unlist(lapply(list_of_forms,function(x)  grep('^u_',str_split(as.character(get(x)[[3]])[2],pattern = '\\s\\+\\s')[[1]],value=T) ))
library(matrixStats)
uvars = grep('^u_',raw_vars,value = T)
library(stargazer)
uvars_no_int = grep(':',uvars,value = T,invert = T)
uvars_int = grep(':',uvars,value = T,invert = F)

uvars_interaction_products = as.data.table(sapply(str_split(uvars_int ,':'),function(x) rowProds(as.matrix(nf[FOREST_ID %in% fs$FOREST_ID &CALENDAR_YEAR %in% start_year:end_year,gsub('u_','',(unlist(x))),with = F]))))
names(uvars_interaction_products) <- gsub('u_','',uvars_int)

coef_vals = cbind(nf[FOREST_ID %in% fs$FOREST_ID & CALENDAR_YEAR %in% start_year:end_year,unique(gsub('u_','',uvars_no_int)),with = F],uvars_interaction_products)
swap_names = names(coef_vals)
swap_names = as.factor(swap_names)


fs[FOREST_ID=='0915',]
admin_districts[admin_districts$FOREST_ID=='0915',]
length(unique(nf$FOREST_ID))
unique(nf$FOREST_ID)[!unique(nf$FOREST_ID) %in% unique(model_list_of_lists[[1]][[1]]$.args$data$u_forest_id)]

swap_names = fct_recode(swap_names,
                        '(intercept)' = 'mu.u',
                        '% wilderness area' = 'Wilderness_Perc',
                        '% dem. vote share' = 'percentD_H',
                        '% housing in WUI' = 'Perc_WUI_Housing',
                        '# listed species' = 'Count_EorT_Species','ln(forest acreage)'='Ln_ACRES',
                        '% burned last 5 years'='Burned_Perc_Past5yrs','LCV annual score'='LCV_annual',
                        'Unemployment %' = 'Unemp_Rate','% extraction employ.' = 'Perc_Extraction_Employ',
                        'ln(yearly visitation)' = 'Ln_AVERAGE_YEARLY_VISITS','ln($1M county NR GDP)' = 'ln_County_naturalresource_GDP_1M',
                        'ln(avg. board feet, 1999 to 2004)' = "Ln_Avg_MBF_Cut_1999_2004" ,
                        'NEPA grazing actions, 1993 to 2004' = "ALLOTMENT_NEPA_1993_2004",
                        'Mining claim actions, 1993 to 2004' = "MINING_CLAIM_ACTIONS_1993_2004" ,
                        'Democratic president' = 'demPres','Democratic congress' = 'demCongress',
                        "% dem. vote x unemp. %" = "Unemp_RatexpercentD_H"   ,
                        'Dem. rep.' = 'democrat','Dem. rep. x unemp. %' = "Unemp_Ratexdemocrat" ,
                        'LCV annual x unemp. %' = 'Unemp_RatexLCV_annual',
                        'House committee LCV' = 'ComLCV','House chair LCV' = 'ChairLCV')
colnames(coef_vals) <- as.character(swap_names)

coef_html_table = stargazer(coef_vals,summary = T,out = 'output/policypolitics/tables/variable_summaries.html')

corr <- round(cor(coef_vals,use = 'pairwise.complete.obs'), 2)

#install.packages("ggcorrplot")
require(ggcorrplot)
ggc = ggcorrplot(corr,method = 'circle',show.diag = F,type = 'upper',lab = TRUE)
ggsave(plot = ggc,filename = 'output/policypolitics/figures/correlation_plot.png',width=  8,height = 8, units = 'in',dpi = 300)


# 
# subnf = nf[congress==115,.(FOREST_ID,percentD_H,LCV_annual,Count_EorT_Species,Wilderness_Prop,Prop_Extraction_Employ,ln_County_naturalresource_GDP_1M)]
# admin_districts$percentD_H115 = subnf[match(admin_districts$FOREST_ID,FOREST_ID),]$percentD_H
# admin_districts$LCV_annual115 = subnf[match(admin_districts$FOREST_ID,FOREST_ID),]$LCV_annual
# admin_districts$Prop_Extraction_Employ = subnf[match(admin_districts$FOREST_ID,FOREST_ID),]$Prop_Extraction_Employ
# admin_districts$ln_County_naturalresource_GDP_1M = subnf[match(admin_districts$FOREST_ID,FOREST_ID),]$ln_County_naturalresource_GDP_1M
# admin_districts$Wilderness_Prop = subnf[match(admin_districts$FOREST_ID,FOREST_ID),]$Wilderness_Prop
# admin_districts$Count_EorT_Species = subnf[match(admin_districts$FOREST_ID,FOREST_ID),]$Count_EorT_Species
# g1 = ggplot(admin_districts) + geom_sf(aes(fill = percentD_H115,colour = percentD_H115)) + theme_map() + 
#   ggtitle('percentD_H, 115th congress by  unit') + 
#   scale_color_viridis_c() + scale_fill_viridis_c()+ 
#   theme(legend.title  = element_blank(),legend.position = c(0.1,0.2),text = element_text(size = 12))
# g2 = ggplot(admin_districts) + geom_sf(aes(fill = LCV_annual115,colour = LCV_annual115)) + theme_map()+ 
#   ggtitle('LCV Annual, 115th congress by unit')+ 
#   scale_color_viridis_c(option = 'C') + scale_fill_viridis_c(option = 'C') + 
#   theme(legend.title  = element_blank(),legend.position = c(0.1,0.2),text = element_text(size = 12))
# #install.packages('gridExtra')
# library(gridExtra)
# grb = grid.arrange(g1,g2,ncol = 1)
# #ggsave(grb,filename = 'output/policypolitics/figures/choropleth_politics_by_forest.png',width = 9,height = 11,units = 'in',dpi = 300)
# 
# g3 = ggplot(admin_districts) + geom_sf(aes(fill = Wilderness_Prop,colour = Wilderness_Prop)) + theme_map() + 
#   ggtitle('Wilderness %, 115th congress by unit') + 
#   scale_color_viridis_c(option = 'A') + scale_fill_viridis_c(option = 'A')+ 
#   theme(legend.title  = element_blank(),legend.position = c(0.1,0.2),text = element_text(size = 12))
# g4 = ggplot(admin_districts) + geom_sf(aes(fill = ,colour = Count_EorT_Species)) + theme_map()+ 
#   ggtitle('# ESA lists, 115th congress by unit')+ 
#   scale_color_viridis_c(option = 'B') + scale_fill_viridis_c(option = 'B') + 
#   theme(legend.title  = element_blank(),legend.position = c(0.1,0.2),text = element_text(size = 12))
# #install.packages('gridExtra')
# 
# grb2 = grid.arrange(g3,g4,ncol = 1)
# #ggsave(grb2,filename = 'output/policypolitics/figures/choropleth_enviro_by_forest.png',width = 9,height = 11,units = 'in',dpi = 300)
# 
# 
# g5 = ggplot(admin_districts) + geom_sf(aes(fill = Prop_Extraction_Employ,colour = Prop_Extraction_Employ)) + theme_map() + 
#   ggtitle('Extraction employ., 115th congress by unit') + 
#   scale_color_viridis_c(option = 'A') + scale_fill_viridis_c(option = 'A')+ 
#   theme(legend.title  = element_blank(),legend.position = c(0.1,0.2),text = element_text(size = 12))
# g6 = ggplot(admin_districts) + geom_sf(aes(fill = ln_County_naturalresource_GDP_1M,colour = ln_County_naturalresource_GDP_1M)) + theme_map()+ 
#   ggtitle('County resource GDP, 115th congress by unit')+ 
#   scale_color_viridis_c(option = 'B') + scale_fill_viridis_c(option = 'B') + 
#   theme(legend.title  = element_blank(),legend.position = c(0.1,0.2),text = element_text(size = 12))
# #install.packages('gridExtra')
# 
# grb3 = grid.arrange(g5,g6,ncol = 1)
# #ggsave(grb3,filename = 'output/policypolitics/figures/choropleth_extractecon_by_forest.png',width = 9,height = 11,units = 'in',dpi = 300)
# 

input_data = dcast(counts_by_type,get(period_type) + 
                     FOREST_ID + Project_Type ~ DECISION_TYPE,value.var = 'N',fill = 0)
input_data$Tot_Proj = input_data$CE + input_data$EA + input_data$EIS
if(period_type=='congress'){
  input_data = input_data[Project_Type=='Type_Purpose_Extractive',][period_type>108&period_type<116,]}
if(period_type!='congress'){
  input_data = input_data[Project_Type=='Type_Purpose_Extractive',][period_type>=start_year&period_type<=end_year,]}

yheight = input_data[,.N,by = .(Tot_Proj)][N==max(N),]$N


g1 = ggplot(data = input_data,aes(x = Tot_Proj)) + geom_histogram(bins = 100)+
  scale_y_continuous(name = '# forest-congress observations') +
  scale_x_continuous(name = '# projects by period') + theme_bw() + 
  ggtitle('# Extractive projects')

g2 = ggplot(data = input_data,aes(x = CE/Tot_Proj)) + 
  scale_x_continuous(name = '# CE / total NEPA analyses')+
  geom_density() + ggtitle('CE / total NEPA analyses')  +
  theme_bw() + scale_y_continuous(name = 'density') + 
  coord_flip()

g3 = ggplot(data = input_data,aes(x = Tot_Proj,y = CE)) + geom_jitter(pch = 21) + theme_bw() + 
  scale_x_continuous(name = '# projects in period') +
  scale_y_continuous(name = '# CEs issued') + 
  ggtitle('CEs vs. total extractive projects')


library(gridExtra)
ggsave(grid.arrange(g1,g2,g3,ncol = 2),filename = 'output/policypolitics/figures/DV_plot.png',width = 8,units = 'in',height= 8,dpi = 300)


nf = nf[nf$congress%in%109:115,]

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
  idat$u_forest_id = c(temp_dt$FOREST_ID,rep(NA,length(y)))
  idat$y_forest_id = c(rep(NA,narep),temp_dt$FOREST_ID)
  idat$u_congress_id = c(temp_dt$congress,rep(NA,length(y)))
  idat$y_congress_id = c(rep(NA,narep),temp_dt$congress)
  idat$u_yearid = c(temp_dt$CALENDAR_YEAR,rep(NA,length(y)))
  idat$y_yearid = c(rep(NA,narep),temp_dt$CALENDAR_YEAR)
  idat$u_state_id = c(temp_dt$STATE,rep(NA,narep))
  idat$y_state_id = c(rep(NA,narep),temp_dt$STATE)
  idat$u_region_id = c(temp_dt$USFS_REGION,rep(NA,length(y)))
  idat$y_region_id = c(rep(NA,narep),temp_dt$USFS_REGION)
  #### binomial EIS / [EA+EIS] model coefficients
  #idat$y_FOREST_LEVEL_DECISIONS = c(rep(NA,narep),scale(temp_dt$FOREST_LEVEL_DECISIONS))
  idat$y_Unemp_Rate = c(rep(NA,narep),scale(temp_dt$Unemp_Rate))
  #idat$y_Unemp_Rate_L1 = c(rep(NA,narep),scale(temp_dt$Unemp_Rate_L1))
  
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
  #idat$y_nominate_dim1 = c(rep(NA,narep),scale(temp_dt$nominate_dim1))
  #idat$y_nominate_dim1_x_y_democrat =  c(rep(NA,narep),scale(temp_dt$nominate_dim1) * scale(temp_dt$democrat))
  idat$y_demPres = c(rep(NA,narep),temp_dt$demPres)
  idat$y_demCongress = c(rep(NA,narep),temp_dt$demCongress)
  idat$y_ComLCV = c(rep(NA,narep),scale(temp_dt$ComLCV))
  idat$y_ChairLCV = c(rep(NA,narep),scale(temp_dt$ChairLCV))
  idat$y_Perc_WUI_Housing = c(rep(NA,narep),scale(temp_dt$Perc_WUI_Housing))
  idat$y_ln_County_naturalresource_GDP_1M = c(rep(NA,narep),scale(temp_dt$ln_County_naturalresource_GDP_1M))
#  idat$y_ln_County_naturalresource_GDP_1M_L1 = c(rep(NA,narep),scale(temp_dt$ln_County_naturalresource_GDP_1M_L1))
  
  #idat$u_FOREST_LEVEL_DECISIONS = c(scale(temp_dt$FOREST_LEVEL_DECISIONS),rep(NA,narep))
  idat$u_Unemp_Rate = c(scale(temp_dt$Unemp_Rate),rep(NA,narep))
 # idat$u_Unemp_Rate_L1 = c(scale(temp_dt$Unemp_Rate_L1),rep(NA,narep))
  
  idat$u_Ln_Avg_MBF_Cut_1999_2004 = c(scale(temp_dt$Ln_Avg_MBF_Cut_1999_2004),rep(NA,narep))
  idat$u_ALLOTMENT_NEPA_1993_2004 = c(scale(temp_dt$ALLOTMENT_NEPA_1993_2004),rep(NA,narep))
  idat$u_MINING_CLAIM_ACTIONS_1993_2004 = c(scale(temp_dt$MINING_CLAIM_ACTIONS_1993_2004),rep(NA,narep))
  
  idat$u_Perc_Extraction_Employ = c(scale(temp_dt$Perc_Extraction_Employ),rep(NA,narep))
#  idat$u_Perc_Extraction_Employ_L1 = c(scale(temp_dt$Perc_Extraction_Employ_L1),rep(NA,narep))
  
  idat$u_Perc_WUI_Housing = c(scale(temp_dt$Perc_WUI_Housing),rep(NA,narep))
  idat$u_Ln_ACRES = c(scale(temp_dt$Ln_ACRES),rep(NA,narep))
  idat$u_Wilderness_Perc = c(scale(temp_dt$Wilderness_Perc),rep(NA,narep)) 
  idat$u_Burned_Perc_Past5yrs = c(scale(temp_dt$Burned_Perc_Past5yrs),rep(NA,narep)) 
  idat$u_Ln_AVERAGE_YEARLY_VISITS = c(scale(temp_dt$Ln_AVERAGE_YEARLY_VISITS),rep(NA,narep)) 
  idat$u_Count_EorT_Species= c(scale(temp_dt$Count_EorT_Species),rep(NA,narep)) 
  idat$u_percentD_H = c(scale(temp_dt$percentD_H),rep(NA,narep)) 
  #idat$u_democrat = c(scale(temp_dt$democrat),rep(NA,narep))  
  idat$u_LCV_annual= c(scale(sqrt(temp_dt$LCV_annual)),rep(NA,narep)) 
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
  return(idat)}

subtypes = subtypes[grepl('EXTRACTIVE|WILDLIFE',toupper(subtypes))]
library(INLA)

#project_type_counts_for_model[,sum(N),by=.(Project_Type)]

#proj_count_input = dcast(counts_by_type,congress + FOREST_ID + Project_Type ~ DECISION_TYPE,value.var = 'N')
pcount = fs[fs$congress>=109&fs$congress<=115,]
table(pcount$`DECISION TYPE`)
table(pcount$`DECISION TYPE`,pcount$Type_Purpose_Extractive)
library(ggrepel)


ggplot() + geom_point(data = nf,aes(x = percentD_H,y = LCV_annual)) +
  geom_label_repel(data = nf[percentD_H<0.3&LCV_annual>50,],aes(x = percentD_H,y = LCV_annual,label = FOREST_NAME))
ggplot() + geom_point(data = nf,aes(x = percentD_H,y = LCV_annual)) +
  geom_label_repel(data = nf[percentD_H>0.55&LCV_annual<15,],aes(x = percentD_H,y = LCV_annual,label = FOREST_NAME))

counts_by_type[Project_Type=='Type_Purpose_Extractive',][order(-N),][CALENDAR_YEAR==start_year&FOREST_ID=='0302',]
counts_by_type[Project_Type=='Type_Purpose_Extractive',sum(N),by=.(CALENDAR_YEAR,FOREST_ID)][order(-V1),][CALENDAR_YEAR==start_year&FOREST_ID=='0302',]


list_of_results = lapply(subtypes,function(mod) {
  mod = subtypes[2]
  idat = makeIDAT(mod = mod,nf = nf,project_type_counts_for_model = counts_by_type)
  #  temp = merge(temp,all_combos,all=T)
  #  temp$N[is.na(temp$N)] <- 0
  #y_lik <- log(idat$y/idat$u)
  u.sdres <- sd(idat$u,na.rm = T)#sd(y_like[is.finite(y_lik)])
  y.sdres <- sd(idat$y/idat$u,na.rm=T)
  pc.prec.u = list(prec = list(prior = "pc.prec", param = c(3*u.sdres, 0.01)))
  pc.prec.y = list(prec = list(prior = "pc.prec", param = c(3*y.sdres, 0.01)))
  famcontrol = list(list(prior = "pcprec", param = c(3*u.sdres,0.01)),
                        list(prior = "pcprec", param = c(3*y.sdres,0.01)))
  mod_list = lapply(list_of_forms,function(f) {print(f);gc();inla(get(f),
                                                             family = c('nbinomial', 'betabinomial'),Ntrials = idat$u,
                                                             control.fixed = list(expand.factor.strategy = "inla"),
                                                            # control.family = famcontrol,
                                                             data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                                                             control.predictor=list(compute=TRUE),verbose=F)})
  #fixef_results = lapply(seq_along(mod_list),function(x) mod_list[[x]]$summary.fixed[,c(1,3,5)] %>% mutate(coef = rownames(.),form = x,mod = mod))
  saveRDS(mod_list,paste0('output/policypolitics/model_objects/models_',mod,'.RDS'))
  #rbindlist(fixef_results)
})

# 
# sapply(list_of_results,function(x) sapply(x,function(y) y$waic$waic))
# waic_scores = data.table(sapply(list_of_results,function(x) sapply(x,function(y) y$waic$waic)))
# waic_scores$formula <- list_of_forms
# colnames(waic_scores)[1:length(subtypes)]<-subtypes
# library(htmlTable)
# waic_scores[,1:length(subtypes)] <- round(waic_scores[,1:length(subtypes)],3)
# htmlTable(waic_scores)
# 
# waic_scores$spec = c("baseline","national_politics","lcv_x_demvoteshare","lcv_x_unemployment",
#   "lcv_x_prop_extraction_employ","lcv_x_naturalresourceGDP","lcv_x_wilderness",
# "lcv_x_species","demvote_x_wilderness","demvote_x_species")
# if('CE' %in% keep_decisions & !keep_activities ){fwrite(waic_scores,'output/policypolitics/tables/waic_table.csv')}
# if(!'CE' %in% keep_decisions & !keep_activities){fwrite(waic_scores,'output/policypolitics/tables/waic_table_noCE.csv')}
# if('CE' %in% keep_decisions & keep_activities ){fwrite(waic_scores,'output/policypolitics/tables/waic_table_activities.csv')}
# if(!'CE' %in% keep_decisions & keep_activities){fwrite(waic_scores,'output/policypolitics/tables/waic_table_noCE_activities.csv')}
# 



lcv_vs_unemp = ggplot(nf,aes(x = LCV_annual,y = Unemp_Rate)) + geom_point(pch = 21,alpha = 0.5) + theme_bw() + 
  scale_x_continuous(name = 'annual LCV score') + scale_y_continuous(name = 'Unemployment %') + 
  ggtitle('Unemployment vs. LCV score', subtitle = 'administrative unit-year observations')

ggsave(lcv_vs_unemp,filename = 'output/policypolitics/figures/IV_plot.png',dpi=300,width = 6, height = 6, units = 'in')


cvals = coef_vals[,!grepl(':',names(coef_vals)),with = F]
cv = melt(cvals)
cv
ggplot(cv) + geom_histogram(aes(x = as.numeric(value))) + facet_wrap(~variable, ncol = 3,scales = 'free') + 
  theme_bw()



