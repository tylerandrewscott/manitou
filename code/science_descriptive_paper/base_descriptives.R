require(data.table)
require(stringr)
require(lubridate)


eis = fread('../eis_documents/enepa_repository/meta_data/eis_record_detail.csv')

projs = fread('../eis_documents/agency_nepa_libraries/usfs/metadata/forest_service_project_detail.csv')

projs$`Current Status` <- str_remove_all(projs$`Current Status`,'\n|\t|\r')
projs = projs[`Current Status`=='Analysis Completed',]
projs$Year <- year(ymd(projs$`Decision Signed Date`))
projs$`Expected Analysis Type` = str_remove_all(projs$`Expected Analysis Type`,'\n|\r|\t')
projs = projs[`Expected Analysis Type`=='Environmental Impact Statement',]

table(projs$Year)

projs$`Expected Analysis Type` <- 


docs = fread('../eis_documents/agency_nepa_libraries/usfs/metadata/forest_service_document_record.csv')
projs = projs[YEAR %in% c(2010:2019),]
projs$ID = str_extract(projs$Project_Page,'[0-9]{1,}$')
projs$`Expected Analysis Type` = str_remove_all(projs$`Expected Analysis Type`,'\n|\r|\t')
projs = projs[`Expected Analysis Type`=='Environmental Impact Statement',]




docs = docs[docs$Project_Num %in% projs$Proj_Num,]

projs$PURPOSES = sapply(str_split(str_remove(projs$`Project Purpose`,'\n'),';'),str_squish)

purposes = as.data.table(table(unlist(projs$PURPOSES)))[order(-N)]

require(forcats)
purposes$V1 = fct_other(f = purposes$V1,keep = purposes$V1[1:10])
require(htmlTable)
htmlTable(purposes[,sum(N),by=.(V1)])

projs[,.N,by=.(YEAR)][order(YEAR)]

test = fread('../eis_documents/enepa_repository/meta_data/eis_record_detail.csv') 
test$YEAR = str_extract(test$EIS.Number,'^[0-9]{4}')
test = test[Agency =='Forest Service']

table(test$Agency,test$YEAR)


table(projs$`Expected Analysis Type`)
dim(projs)


projs[is.na(Proj_Num),]

projs$Project_Num = docs$Project_Num[match(projs$Project_Page,docs$Project_Page)]

projs[is.na(Project_Num),]

639/nrow(projs)

projs$
table(year(mdy(projs$`Last Updated:`)))




