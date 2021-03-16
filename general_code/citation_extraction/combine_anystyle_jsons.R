library(data.table)
library(jsonlite)
library(pbapply)
library(magrittr)

fl = list.files('extracts/',full.names = T,pattern = 'json')
fl_list = pblapply(fl,function(x) fromJSON(x) %>% data.table() %>% .[,File:=basename(x)],cl = 4)

retry = which(sapply(fl_list,class)=='try-error')
replace_entries = pblapply(fl[retry],function(x) fromJSON(x) %>% data.table() %>% .[,File:=basename(x)])

fl_list[retry] <- replace_entries
fl_dt = rbindlist(fl_list,fill = T,use.names=T)


fl_dt = fl_dt[nchar(title)<400,]

dets = fread('../eis_documents/agency_nepa_libraries/usfs/metadata/forest_service_project_detail.csv')
fl_dt$PROJECT_ID = str_remove(fl_dt$File,'_.*')
dets$PType = str_remove(dets$`Expected Analysis Type`,'\\r\\n\\t\\t')

fl_dt$PType = dets$PType[match(fl_dt$PROJECT_ID,dets$Proj_Num)]
fl_dt$PType[fl_dt$PType=='']<-'Unclassified'
split_by_type = split(fl_dt,fl_dt$PType)

sapply(names(split_by_type),function(x){
  fname = paste0('scratch/anycite_results_',gsub('\\s','_',x),'V2.rds')
  saveRDS(split_by_type[[x]],fname)
  if(file.info(fname)$size>25e6){
    div = floor(file.info(fname)$size/25e6) + 1
    file.remove(fname)
    sapply(1:div, function(s) {
     subsplit = dplyr::ntile(1:nrow(split_by_type[[x]]),n = div)
     saveRDS(split_by_type[[x]][subsplit==s,],gsub('.rds',paste0(s,'V2.rds'),fname))
    }
    )
  }
}
)
