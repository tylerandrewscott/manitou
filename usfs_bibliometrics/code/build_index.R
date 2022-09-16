library(citationSearch)
library(referenceBuild)
library(data.table)
library(jsonlite)
library(rjson)
library(stringr)

should_have <- c('title','authors','publisher','year','journal_title','doi','miscid','source')

#### note that in anystyle jsons, "source" is things like the book a chapter came out of.
#### for citationSearch, source = what reference database

ref_files <- list.files('usfs_bibliometrics/scratch/openAlex_data/',full.names = T)
#temp <- fromJSON('usfs_bibliometrics/scratch/openAlex_data/wildfire_suppression_2022-08-27.json.gz')
index_file <- 'usfs_bibliometrics/scratch/index_ref.rds'
author_file <- 'usfs_bibliometrics/scratch/index_author.rds'
host_file <- 'usfs_bibliometrics/scratch/index_host.rds'
meta_file <- 'usfs_bibliometrics/scratch/index_meta.rds'
index_record <- 'usfs_bibliometrics/scratch/already_in_index.RDS'
if(file.exists(index_file)){ref_dt <- readRDS(index_file)}else{ref_dt <- data.table()}
if(file.exists(host_file)){host_dt <- readRDS(host_file)}else{host_dt <- data.table()}
if(file.exists(meta_file)){meta_dt <- readRDS(meta_file)}else{meta_dt <- data.table()}
if(file.exists(author_file)){author_dt <- readRDS(author_file)}else{author_dt <- data.table()}
if(file.exists(index_record)){already_in_index <- readRDS(index_record)}else{already_in_index <- NULL}

sz <- file.size(ref_files)
rf <- data.frame(ref_files,sz)[order(sz),]
ref_files <- rf$ref_files

ref_files <- ref_files[!basename(ref_files) %in% already_in_index]

for(r in ref_files){
  print(r)
  tfile <- works2dt(r)
  tfile$oa_id <- basename(tfile$id)
  tfile <- tfile[!oa_id %in% ref_dt$oa_id,]
  tfile$authors <- tfile$author.display_name
  tfile$publisher <- tfile$host.publisher
  tfile$year <- tfile$publication_year
  tfile$journal_title <- tfile$host.display_name
  host_i <- tfile[,.(host.id,host.issn_l,oa_id)]
  author_i <- tfile[,.(authors,author.id,oa_id)]
  meta_i <- tfile[,.(oa_id,is_oa,oa_status,type,cited_by_count)]

  tfile[,host.publisher:=NULL]
  tfile[,host.display_name:=NULL]
  tfile[,host.id:=NULL]
  tfile[,host.issn_l:=NULL]
  tfile[,author.display_name:=NULL]
  tfile[,publication_year:=NULL]
  tfile[,author.id:=NULL]
  tfile[,cited_by_count:=NULL]
  tfile[,is_oa:=NULL]
  tfile[,oa_status:=NULL]
  tfile[,type:=NULL]
  tfile[,id:=NULL]
  author_dt <- rbind(author_dt,author_i,use.names = T,fill = T)
  meta_dt <- rbind(meta_dt,meta_i,use.names = T,fill = T)
  ref_dt <- rbind(ref_dt,tfile,use.names = T,fill = T)
  host_dt <- rbind(host_dt,host_i,use.names = T,fill = T)
  rm(tfile);rm(meta_i);rm(host_i);rm(author_i)
  already_in_index <- append(already_in_index,r)
}


saveRDS(object = author_dt,file = author_file)
saveRDS(object = host_dt,file = host_file)
saveRDS(object = meta_dt,file = meta_file)
saveRDS(object = ref_dt,file = index_file)
saveRDS(object = already_in_index, file =index_record )

