
require(data.table)
require(stringr)
require(lubridate)


#fs = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vTPHD71fQvTZpNljqRNVI6lD4b26wW86ptP_dy3R_qWaxkXyOE2QCuyTzroL_38mw/pub?output=csv')
fs = fread('input/usfs_internal_data/Copy of MYTR_National Level(1100).xlsx - Decision Data Set.csv')
fs$ongoing = 0
fs2 = data.table(readRDS('input/usfs_internal_data/FS_ongoing_projects_11-2019.rds'),stringsAsFactors = F)
numcols = colnames(fs2)[as.vector(apply(fs2,2,function(x) !any(grepl('[^0-9]',x))))]
fs2[,(numcols):=lapply(.SD,as.numeric),.SDcols = numcols]
fs = merge(fs,fs2,all = T)
fs = fs[!duplicated(paste(`PROJECT NUMBER`,`LMU (ACTUAL)`)),] 
fs = fs[fs$`DECISION TYPE`!='PAD',]
fs = fs[fs$`DECISION TYPE`!='',]
fs$`DECISION SIGNED`<-mdy(fs$`DECISION SIGNED`)
fs = fs[`SIGNED FY`>=2005,]
fs = fs[`SIGNED FY`<2019,]
fs <- fs[`DECISION TYPE` %in% c('DN','ROD'),]

grep('584864',fs$`PROJECT NUMBER`)

projs = fread('../eis_documents/agency_nepa_libraries/usfs/metadata/forest_service_project_detail.csv')

head(projs$Project_Page)
"https://www.fs.usda.gov/project/?project=584864"
 
fs[!`PROJECT NUMBER` %in% projs$Proj_Num & fs$`DECISION TYPE`=='ROD',]

projs = projs[Proj_Num %in% fs$`PROJECT NUMBER`,]


table(is.na(fs$FOREST))
require(lubridate)
purposes <- melt(fs[,grepl('purpose$',names(fs)),with = F])
purposes$variable <- str_remove(str_remove(purposes$variable,' – purpose$'),'^[A-Z]{2,}\\s')
require(htmlTable)

htmlTable(purposes[,sum(value),by=.(variable)][order(-V1)])
htmlTable(fs[,.N,by=.(`SIGNED FY`)][order(`SIGNED FY`)])

doc_record = fread('../eis_documents/agency_nepa_libraries/usfs/metadata/forest_service_document_record.csv')
doc_record[Project_Num=='50823']


doc_record = doc_record[Project_Num %in% fs$`PROJECT NUMBER`,]
doc_record = doc_record[Stage %in% c('Analysis','Assessment','Scoping','Decision'),]

ref_dt = rbindlist(lapply(list.files('scratch',pattern = 'Environmental_Assessment|Environmental_Impact_Statement',full.names = T),readRDS),fill = T,use.names = T)
ref_dt <- ref_dt[PROJECT_ID %in% fs$`PROJECT NUMBER`,]

ref_dt[File=='45203_99694_FSPLT3_3902120.json'][title == 'Learning to coexist with wildfire']
ref_dt[File=='45203_99694_FSPLT3_3902120.json'][title == 'A fuel treatment reduces fire severity and increases suppression efficiency in a mixed conifer forest']
ref_dt[title == 'A fuel treatment reduces fire severity and increases suppression efficiency in a mixed conifer forest,Learning to coexist with wildfire']

ref_dt[order(File),][grepl('WF06066|13946',doi)][File=='']

scimag = fread('input/scimago/scimagojr 2019.csv')

require(rcrossref)


test = ref_dt[!sapply(ref_dt$doi,is.null),]
test = test[sapply(test$doi,length)==1,]
doi_bibtext = lapply(1:nrow(test),function(i){
cr_cn(dois = test$doi[[i]], format = "bibtex")
})




test = fread('../eis_documents/agency_nepa_libraries/blm/metadata/document_record.csv')
test = 
test[NEPA_ID=='DOI-BLM-NV-B010-2015-0016-EA']




test

pdf_info('../eis_documents/agency_nepa_libraries/blm/nepa_documents/2016/DOI-BLM-AK-F010-2016-0019-EA--Final_ROW_EA19_DR.pdf')
doc_record[basename(doc_record$File_Name) %in% gsub('json','pdf',test$File),]



list.files('../eis_documents/enepa_repository/documents/2015',pattern = '20150086')




require(rcrossref)

crossref_email= "name@example.com"



head(scimag)


scimag$`Total Refs.`



unlist(ref_dt$doi)
scimag
dim(ref_dt)










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
fs = fs[order(fs$`DECISION ID`),]
fs = fs[DECISON_LEVEL!='Region',]
#fs = fs[!duplicated(`PROJECT NUMBER`),]
fs = fs[!is.na(fs$FOREST_ID),]
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='ROD'] <- 'EIS'
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='DN'] <- 'EA'
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='DM'] <- 'CX'
fs$PROJECT_DECISON_LEVEL<-ifelse(fs$DECISON_LEVEL=='National Forest/Grassland','NATIONAL FOREST','RANGER DISTRICT/FIELD UNIT')
fs = data.table(fs)


fs

