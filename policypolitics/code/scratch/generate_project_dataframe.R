
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

keep_decisions = c('EIS','EA','CE')

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

saveRDS(fs,'input/prepped/fs_PALS_cleaned_project_datatable.RDS')



