
library(data.table)
library(jsonlite)
library(pbapply)
library(magrittr)
library(stringr)
library(citationClassify)
library(lubridate)
should_have <- c('title','authors','publisher','year','journal_title','doi','miscid')
fl <- list.files('usfs_bibliometrics/scratch/js/',full.names = T,pattern = 'json')
fl_list = pblapply(fl,function(x) fromJSON(x) %>% data.table() %>% .[,File:=basename(x)],cl = 4)
retry = which(sapply(fl_list,class)=='try-error')
replace_entries = pblapply(fl[retry],function(x) fromJSON(x) %>% data.table() %>% .[,File:=basename(x)])

fl_list[retry] <- replace_entries
fl_dt = rbindlist(fl_list,fill = T,use.names=T)
fl_dt = fl_dt[nchar(title)<400,]

dets = fread('../eis_documents/agency_nepa_libraries/usfs/metadata/forest_service_project_detail.csv')
dets$PType = str_remove(dets$`Expected Analysis Type`,'\\r\\n\\t\\t')
dets <- dets[year(ymd(dets$`Decision Signed Date`))%in%2010:2019,]

fl_dt$PROJECT_ID = str_remove(fl_dt$File,'_.*')
fl_dt <- fl_dt[PROJECT_ID %in% dets$Proj_Num,]
fl_dt$PType = dets$PType[match(fl_dt$PROJECT_ID,dets$Proj_Num)]
fl_dt$PType[fl_dt$PType=='']<-'Unclassified'
fl_dt$container_title <- unlist(sapply(fl_dt$`container-title`,function(x) if(length(x)>0){x[1]}else{NA}))
fl_dt <- journal_disambig(df = fl_dt,column = "container_title")
fl_dt$author <- sapply(fl_dt$author,function(x) if(length(x)==0){NA}else{paste(paste(x$given,x$family),collapse = ';')})
setnames(fl_dt,'author','authors')
fl_dt$title <- sapply(fl_dt$title,function(x) if(length(x)==0){NA}else{paste(x,collapse = ';')})
fl_dt$publisher <- sapply(fl_dt$publisher,function(x) if(length(x)==0){NA}else{paste(x,collapse = ';')})
setnames(fl_dt,'journal.disam','journal_title')
fl_dt$doi <- sapply(fl_dt$doi,function(x) if(length(x)==0){NA}else{paste(x,collapse = ';')})


temp_year <- lapply(fl_dt$date,str_extract_all,pattern = '[0-9]{4}')
real_years <- 1940:2022
temp_year <- lapply(temp_year,function(x) x[x %in% real_years])
temp_year <- unlist(sapply(temp_year,function(x) if(length(x)==0){NA}else{x[1]}))
fl_dt$year <- temp_year

fl_dt$miscid <- 1:nrow(fl_dt)

fl_dt2 <- fl_dt[,c(should_have,'File'),with = F]
fl_dt2[,miscid:=NULL]
### remove if only three non-file items are present
### these tend to be preparer lists miscast
fl_dt2 <- fl_dt2[rowSums(is.na(fl_dt2))<(ncol(fl_dt2)-4),]

### "titles" with lots of characters are bad
# also this tosses half the results
fl_dt2 <- fl_dt2[nchar(journal_title)<400,]
fl_dt2 <- fl_dt2[nchar(title)<300,]
fl_dt2 <- fl_dt2[!(grepl('Fish and Wildlife',fl_dt2$journal_title)&!grepl('Journal',fl_dt2$journal_title)),]
fl_dt2 <- fl_dt2[!grepl('Federal Register',fl_dt2$journal_title),]
fl_dt2 <- fl_dt2[!grepl('USDI',tm::removePunctuation(fl_dt2$authors)),]
fl_dt2 <- fl_dt2[!journal_title%in%c(month.name,'In','Region','Amendment','Exhibit','Chapter','Ch','Version'),]
fl_dt2 <- fl_dt2[!grepl('Department',journal_title),]

fl_dt2 <- fl_dt2[!grepl("X{3,}",tm::removePunctuation(authors)),]
fl_dt2 <- fl_dt2[!grepl("Wildlife Service",authors),]
fl_dt2  <- fl_dt2 [!(grepl('response to comments',tolower(title))|
                                           grepl('response to comments',tolower(journal_title))),]
fl_dt2 <- fl_dt2[str_count(fl_dt2$title,';')<3,]
fl_dt2$uq_id <- 1:nrow(fl_dt2)
saveRDS(fl_dt2,file = 'usfs_bibliometrics/scratch/extracted_results.rds')




