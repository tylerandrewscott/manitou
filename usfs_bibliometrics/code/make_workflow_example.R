library(citationSearch)
library(stringr)
library(data.table)
library(parallel)

extracts_filtered <- readRDS('usfs_bibliometrics/scratch/extracted_results.rds')
extracts_filtered <-extracts_filtered[grepl("Effects of exurban development",extracts_filtered$title),][2,]
extracts_filtered$q <- citationSearch::create_queries(extracts_filtered[,c('title','authors','publisher','year','journal_title','doi')])
system('solr start -c')

result <- search_collection(extracts_filtered$q, collection_name="enviro1",topn = 5)

htmlTable::htmlTable(result)
