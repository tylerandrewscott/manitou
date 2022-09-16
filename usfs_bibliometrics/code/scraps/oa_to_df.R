library(jsonlite)
library(data.table)
library(citationSearch)
library(referenceBuild)
ref_files <- list.files('usfs_bibliometrics/scratch/openAlex_data/',full.names = T)
#temp <- fromJSON('usfs_bibliometrics/scratch/openAlex_data/wildfire_suppression_2022-08-27.json.gz')
empty_dt <- data.table()
test <- json2dt(jfile = ref_files[9],file_location = 'disk')
for(i in ref_files){
  tfile <- json2dt(i,file_location = 'disk')
  empty_dt <- rbind(empty_dt,tfile)
  rm(tfile)
}


test <- queryVenues(venue_string = 'Journal of Public Administration Research and Theory')
jres <- extractVenues(mailto = 'tascott@ucdavis.edu',venue_page = test$results[[1]]$id,per_page = 200,to_date = '2018',from_date = '2016')
jres2 <- fromJSON(jres)
wdt <- works2dt(jres2)



test <- queryVenues(venue_string = "Journal of Wildlife Management" )
test$results[[1]]$x_concepts
test$results[[1]]
sapply(test$results[[1]]$x_concepts,'[[','display_name')


lapply(authors,'[[','author')
authors[[1]]
dres <- works2dt(jres)
dres

library(tidyjson)
library(tidyjson)
tidyjson::as_data_frame(jres[[1]])

fromJSON(authors_json)
authors_json = lapply(jres,'[[','authorships')

lapply(lapply(authors_json[[1]],'[[','institutions'),'[[','display_name')

lapply(lapply(authors_json,'[[','author')[[1]],'[[','institutions')

str(authors_json)
authors_jsonp[[1]]
institutional_var = 'ror'
sapply(sapply(lapply(lapply(authors_json,'[[','author')[[1]],'[[','institutions'),'[[',institutional_var),paste,collapse=';')


head(lapply(authors_json,'[[','author'))

authors_json[[1]][[1]]$
head(dtres)
flattenAuthorInsitutions(authors_json = lapply(jres_json,'[[','authorships'),institutional_var = 'ror')


works2dt
dtres <- works2dt(jres)
dtres$author_institutions_ror


dtres$publication_date

test <- read_json(ref_files[9])

l
test[[11]]$authorships[[1]]$institutions
dim(empty_dt)
empty_dt <- empty_dt[!duplicated(empty_dt),]


library(stringr)
extracts <- fread('usfs_bibliometrics/scratch/reference_set_df_10-13-19.csv',header = T)
extracts$uq_id <- paste0(extracts$File,'__',1:nrow(extracts))
cites_should_have <- c('title','authors','year','journal_title','doi','publisher')
# title	String	Title of the paper
# authors	String	Concatenated list of authors (e.g "Hileman, Jacob; Bastos, Marco T. A; Lubell, Mark")
# year	Int	4 digit year (between 1800 and 2025)
# publisher	String	Name of publisher
# journal_title	String	Name of journal
# doi	String	DOI
extracts$title <- extracts$Title
extracts$authors <- extracts$Authors
extracts$authors <- sapply(extracts$authors,list_authors)
extracts$year <- as.numeric((str_extract(extracts$Date,'[0-9]{4}')))
extracts$publisher <- extracts$Publisher
extracts$journal_title <- extracts$Publication
extracts$doi <- extracts$DOI
extracts <- extracts[!(year <1800|year>2025),]
extracts_filtered <- extracts[,colnames(extracts) %in% cites_should_have,with = F]
extracts_filtered[extracts_filtered=='']<-NA
extracts_filtered$publisher <- NA


library(citationSearch,attach.required = T)
records <- empty_dt
records$miscid <- 1:nrow(records)
names(records) <- str_replace_all(tolower(names(records)),'\\s','_')
should_have <- c('title','authors','publisher','year','journal_title','doi','source','miscid')
names(records)[names(records) %in% should_have]
should_have[!should_have %in% names(records)]

index_records(records, collection_name="oa_set")

ext_for_query <- extracts_filtered[journal_title=='Forest Ecology and Management',]
queries <- citationSearch::create_queries(ext_for_query)
results = list()
for(i in seq_along(queries)){
  results[[i]] <- search_collection(queries[[i]], collection_name="oa_set",topn = 1)
  results[[i]]$query <- queries[[i]]
}
res_dt <- rbindlist(results,fill = T)


test <- records[grepl('Forest Ecology.*Management',journal_title),]


grep('https://doi.org/10.1016/j.foreco.2005.08.003',test$doi,fixed = T)

test[grepl('Powers',test$authors),]$title
res_dt[114,]

ext_for_query[114,]$title
https://api.openalex.org/works?search=%22north%20american%22long-term%22soil%22productivity%22experiment
res_dt[journal_title=='Forest Ecology and Management',]
res_dt[,list(.N,mean(score)),by=.(journal_title)][order(-N),][1:5,]


extracts_filtered[,.N,by=.(journal_title)][order(-N),][1:5,]

records[,.N,by=.(journal_title)][grepl('Forest Ecology and Management',journal_title),]

table(res_dt$score>10)


test <- res_dt[order(-score),][1:5,]

test$title[1]
test$query[1]


ind <- match(test$query,queries)





