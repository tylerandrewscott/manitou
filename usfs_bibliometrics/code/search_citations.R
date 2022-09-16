library(citationSearch)
library(stringr)
library(data.table)
library(parallel)

extracts_filtered <- readRDS('usfs_bibliometrics/scratch/extracted_results.rds')
extracts_filtered$q <- citationSearch::create_queries(extracts_filtered[,c('title','authors','publisher','year','journal_title','doi')])
ex_index <- extracts_filtered[,.(uq_id,q,File)]
unique_queries <- unique(extracts_filtered$q)

system('solr start -c')
ref_dt <- readRDS('usfs_bibliometrics/scratch/index_ref.rds')
should_have <- c('title','authors','publisher','year','journal_title','doi','miscid','source')
ref_dt$source <- 'openalex'
ref_dt$miscid <- ref_dt$oa_id
ref_dt[ref_dt=='']<-NA
ref_dt <- ref_dt[!is.na(title),]
split_i <- nrow(ref_dt)/2
ref_dt1 <- ref_dt[1:split_i,]
ref_dt2 <- ref_dt[(split_i+1):nrow(ref_dt),]
index_records(ref_dt1, collection_name="enviro1",overwrite = T)
index_records(ref_dt2, collection_name="enviro2",overwrite = T)

res_list <- mclapply(seq_along(unique_queries),function(i){
  #print(i)
  q1 <- search_collection(unique_queries[[i]], collection_name="enviro1",topn = 1)
  q2 <- search_collection(unique_queries[[i]], collection_name="enviro2",topn = 1)
  if(q1$score>q2$score){data.table(q1,q = unique_queries[[i]])}else{data.table(q2,q = unique_queries[[i]])}
},mc.cores = 6,mc.preschedule = T,mc.cleanup = T,mc.set.seed = 24)

res_dt <- rbindlist(res_list,fill = T,use.names= T)
res_dt$oa_id <- ref_dt$miscid[match(paste(res_dt$title,res_dt$doi),paste(ref_dt$title,ref_dt$doi))]
names(res_dt)[!names(res_dt)%in%c('q','oa_id')] <- paste0('match.',names(res_dt)[!names(res_dt)%in%c('q','oa_id')])
saveRDS(object = res_dt,file = 'usfs_bibliometrics/scratch/query_match_scores.RDS')
system('solr start -c')