
library(data.table)
#fs = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vTPHD71fQvTZpNljqRNVI6lD4b26wW86ptP_dy3R_qWaxkXyOE2QCuyTzroL_38mw/pub?output=csv')
fs = fread('input/Copy of MYTR_National Level(1100).xlsx - Decision Data Set.csv')
fs = fs[fs$`DECISION TYPE`=='ROD',]
names(fs) <- gsub('–|–','',names(fs))
names(fs) <- gsub('\\s{2,}',' ',names(fs))
fs = fs[!duplicated(fs[,.(`PROJECT NUMBER`,`PROJECT NAME`,`PROJECT CREATED`,`LMU FOREST`)]),]
fs = fs[fs$`PROJECT STATUS`=='Complete',]
fs$`PROJECT NUMBER` <- formatC(fs$`PROJECT NUMBER`,width = 5,flag = 0)
fs$`DECISION ID` <- formatC(fs$`DECISION ID`,width = 5,flag = 0)

fs_pbase = fread('input/forest_service_project_overview_2019-08-12.csv')
setnames(fs_pbase,'Page','Project_Page')
fs_pbase <- fs_pbase[!duplicated(fs_pbase),]
fs_proj = fread('input/forest_service_project_detail_2019-08-12.csv')
fs_proj$Proj_Num <- formatC(fs_proj$Proj_Num,width = 5,flag = 0)
fs_proj <- fs_proj[!duplicated(fs_proj),]
fs_proj = fs_proj[,!duplicated(names(fs_proj)),with=F]
fs_proj = merge(fs_pbase,fs_proj)
fs_proj[,Project_Num:=NULL]

fs = fs[fs$`PROJECT NUMBER` %in% fs_proj$Proj_Num,]
fs_proj <- fs_proj[Proj_Num %in% fs$`PROJECT NUMBER`,]

fs_docs = fread('input/forest_service_document_2019-08-12.csv')
fs_docs[File_Name=='2019_2043_FSPLT2_375368.pdf']
fs_docs
fs_docs[,Proj_Num:=NULL]
setnames(fs_docs,"Project_Num",'Proj_Num')
fs_docs$Proj_Num <- formatC(fs_docs$Proj_Num,width = 5,flag = 0)
fs_docs <- fs_docs[Proj_Num %in% fs_proj$Proj_Num,]

flist = list.files(path = '../../../../net/tmp/tscott1/manitou_scratch/scratch/usfs_project_documents/',full.names = T,all.files = F,pattern = 'pdf',recursive = T)
floc = '../../../../net/tmp/tscott1/manitou_scratch/scratch/usfs_project_documents/'
fs_docs$File_Loc = paste0(floc,paste(fs_docs$forest,fs_docs$File_Name,sep = '/'))
fs_docs = fs_docs[Document_Status == 'Have copy',]

string_find = c('References','Citations','Bibliography')
string_search = paste(c(string_find,toupper(string_find)),collapse='|')
dont_find = c('Table of Contents')
dont_search = paste(c(dont_find,toupper(dont_find)),collapse='|')
fs_docs

file.copy(from = c(fs_docs$File_Loc),to='../../../../net/tmp/tscott1/manitou_scratch/scratch/eis_files/')


fs_docs[forest=='whitemountain']




library(pdftools)

for(i in 1:nrow(fs_docs)){
  if(file.exists(fs_docs$File_Loc[i])){
    i = 10
    fs_docs$File_Loc[i]
    ttext = pdf_text(fs_docs$File_Loc[i])
    ref_pages = intersect(grep(string_search,ttext),grep(dont_search,ttext,invert = T))
    ttext[ref_pages]
    ref_pages
    
    
    ttext[169]

    
    ttext[178]
    }


test = fs[!fs$`PROJECT NUMBER` %in% fs_proj$Proj_Num,]

test$`PROJECT NAME`[test$`LMU FOREST`=='UWCF']

as.data.table(table(test$`LMU – FOREST`))[order(N),]

test$FOREST
fs[fs_proj$Proj_Num %in% fs$`PROJECT NUMBER`,]

table(fs_proj$Proj_Num %in% fs$`PROJECT NUMBER`)



fs_proj$Proj_Num <- formatC(fs_proj$Proj_Num,width = 5,flag = 0)


table(fs_proj$Proj_Num %in% fs$`PROJECT NUMBER`)


table(nchar(fs_proj$Proj_Num))
#fs2 = fs

flist = list.files('../../../../net/tmp/tscott1/manitou_scratch/scratch/usfs_project_documents/willamette/',pattern = 'pdf',full.names = T,recursive = T)

head(fs)

fs[,.(`PROJECT NUMBER`,`DECISION ID`)]


table(sapply(fs$`PROJECT NUMBER`,function(x) any(grepl(x,flist))))

fs = fs[fs$`UNIQUE DECISION?`=='Y',]

fs$`LMU – DISTRICT`[fs$`LMU – DISTRICT`=="Buffalo Ranger District (11040306)"] <- "Blackrock Ranger District (11040306)"
fs$`LMU – DISTRICT`[fs$`LMU – DISTRICT`=="Powell Ranger District  (11010506)"] <- "Lochsa/Powell Ranger District (11011755)"
fs$`LMU – DISTRICT`[fs$`LMU – DISTRICT`=="Salmon River Ranger District (11050554)"] <- "Salmon-Scott Ranger District (11050554)"
fs$`LMU – DISTRICT`[fs$`LMU – DISTRICT`=="Paintrock Ranger District (11020204)"] <- "Salmon-Scott Ranger District (11020203)"
fs$`LMU – DISTRICT`[fs$`LMU – DISTRICT`=="Belt Creek Ranger District (11011503)"] <- "Belt Creek-White Sulphur Springs Ranger District (11011507)"
fs$`LMU (ACTUAL)`[fs$`LMU (ACTUAL)`=="Ashton/Island Park (11041552)" ]<- "Ashton/Island Park Ranger District (11041552)" 
fs$`LMU (ACTUAL)`[fs$`LMU (ACTUAL)`=="Los Angeles River (11050151)" ]<- "Los Angeles River Ranger District (11050151)" 
fs$`LMU (ACTUAL)`[fs$`LMU (ACTUAL)`=="Santa Clara/Mojave Rivers (11050153)"  ]<- "Santa Clara/Mojave Rivers Ranger District (11050153)" 
fs$DISTRICT_ID = str_extract(str_extract(fs$`LMU – DISTRICT`,'[0-9]{1,}'),'[0-9]{6}$')
fs$DISTRICT_ID = gsub('^0108','0111',fs$DISTRICT_ID)
fs$DISTRICT_ID = gsub('^0105','0117',fs$DISTRICT_ID)
fs$DISTRICT_ID = gsub('^0112','0115',fs$DISTRICT_ID)
fs$DISTRICT_ID[fs$DISTRICT_ID=='011702'] <- '011752'
fs$DISTRICT_ID[fs$DISTRICT_ID=='011703'] <- '011753'
fs$DISTRICT_ID[fs$DISTRICT_ID=='011501'] <- '011511'
fs$DISTRICT_ID[fs$DISTRICT_ID=='011504'] <- '011514'
fs$DISTRICT_ID[fs$DISTRICT_ID=='011104'] <- '011184'
fs$DISTRICT_ID[fs$DISTRICT_ID=='011502'] <- '011512'
fs$DISTRICT_ID[fs$DISTRICT_ID=='011102'] <- '011182'
fs$DISTRICT_ID[fs$DISTRICT_ID=='011706'] <- '011755'
fs$FOREST_ID <- str_extract(fs$DISTRICT_ID,'^[0-9]{4}')

fs$REGION_ID <- str_extract(fs$DISTRICT_ID,'^[0-9]{2}')
fs$DECISON_LEVEL = NA
fs$DECISON_LEVEL[grepl('R[0-9]',fs$`LMU (ACTUAL)`)] <- 'Region'
fs$DECISON_LEVEL[grepl('National (Forest|Forests) All Units|(National|Natl) Grassland|All Units',fs$`LMU (ACTUAL)`)]<- "National Forest/Grassland"
fs$DECISON_LEVEL[grepl('Ranger District|RD|Mgt Unit',fs$`LMU (ACTUAL)`)]<- "Ranger District/Mgt Unit"
fs$DECISON_LEVEL[grepl('National Recreation Area|National Scenic Area|National Monument|NRA|Volcanic Monument',fs$`LMU (ACTUAL)`)]<- "NRA/NSA/NM"
fs$FISCAL_YEAR <- fs$`INITIATION FY`