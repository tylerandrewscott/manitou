require('sf')

rangers = st_read('https://opendata.arcgis.com/datasets/668753ac078342b286f79fd8529d9e80_1.geojson')

require(data.table)
require(stringr)
timber = fread('~/Downloads/Timber_Harvests__Feature_Layer_.csv')
timber = timber
#timber = timber[FY_AWARDED>=2004,]
#timber_nepa = timber[timber$NEPA_PROJECT_ID!='NOT REQD',]
timber$FOREST_ID = paste0(formatC(timber$ADMIN_REGION_CODE,width = 2, flag = 0),formatC(timber$ADMIN_FOREST_CODE,width = 2,flag = 0))
timber$NEPA_PROJECT_ID <- as.integer(timber$NEPA_PROJECT_ID)
timber$NEPA_PROJECT_ID[!is.na(timber$NEPA_PROJECT_ID)]<- formatC(timber$NEPA_PROJECT_ID[!is.na(timber$NEPA_PROJECT_ID)],width = 8,flag = 0)

#test=timber[timber$FY_AWARDED=='2014'&!timber$NEPA_PROJECT_ID %in% pals$`PROJECT NUMBER`,]


fs = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vRuz53_guoKQejdd97Syvws360-S7g21tkjeH73OJ1K6zWKNlGwaDRLvd7bbwIoyLmYCf9RWsAzETu-/pub?output=csv',stringsAsFactors = F)
fs$ongoing = 0
fs2 = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vQgOk_TxujlSfspmA9-V4sBS95AMVu7MbjyCmbsIc1yu5N983sJqoR8usHvOXpMl99d1yBYejWoZtdk/pub?output=csv',stringsAsFactors = F)
numcols = colnames(fs2)[as.vector(apply(fs2,2,function(x) !any(grepl('[^0-9]',x))))]
fs2[,(numcols):=lapply(.SD,as.numeric),.SDcols = numcols]
fs = rbind(fs,fs2,use.names = T,fill = T)
fs = fs[!duplicated(paste(`PROJECT NUMBER`,`LMU (ACTUAL)`)),] 

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
fs$`PROJECT NUMBER`<- formatC(fs$`PROJECT NUMBER`,width = 8,flag = 0)
#fs = fs[fs$`DECISION TYPE`!='PAD',]
pals = fs
#pals = pals[pals$`INITIATION FY`>=2004,]

#pals = pals[{`TS Timber salves (green) – activity`==1|`SS Timber sales (salvage) – activity`==1|`FN Fuel treatments – activity`==1|`FV Forest vegetation improvements – activity`==1},]



timber_nopals = timber[!timber$NEPA_PROJECT_ID %in% pals$`PROJECT NUMBER`,]

temp = timber_nopals[!is.na(NEPA_PROJECT_ID)][,.(FOREST_ID,ADMIN_FOREST_NAME,ADMIN_DISTRICT_NAME,FACTS_ID,SUID,SALE_NAME,ACTIVITY_CN,FY_PLANNED,FY_COMPLETED,NEPA_PROJECT_ID,NEPA_DOC_NAME,NEPA_PROJECT_CN)]
saveRDS(temp,'fact_projects_withNEPAID_not_in_PALS.rds')


table(timber$NEPA_PROJECT_ID[!duplicated(timber$NEPA_PROJECT_ID)] %in% pals$`PROJECT NUMBER`,timber$FY_AWARDED[!duplicated(timber$NEPA_PROJECT_ID)])

pals[`PROJECT NUMBER`=='00051107',]
pals[grep('HULL',toupper(pals$`PROJECT NAME`)),][FOREST_ID=='0617',]
temp[NEPA_DOC_NAME=="(PALS)MT. HULL RESTORATION PROJECT" ]
temp$NEPA_DOC_NAME[temp$FY_PLANNED==2018]
temp[NEPA_DOC_NAME=="CANYON CREEK RESEARCH PROJECT EA",]

