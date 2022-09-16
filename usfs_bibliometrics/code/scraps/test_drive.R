library(googlesheets4)
library(data.table)
#this url is the concept tree for openAlex
oa_concept_sheet <- 'usfs_bibliometrics/scratch/OpenAlex concepts in use (17 August 2022) - concepts.csv'
oa_concepts <- fread(oa_concept_sheet)

findOAid <- function(string,maxlevel = 5){
  oa_concepts[grepl(string,`normalized_name`)&level<=maxlevel,]$openalex_id
}

ids <- lapply(c('environmental planning','forestry','^ecology'),function(x) findOAid(x,maxlevel = 1))
cepts <- str_extract(unlist(ids),'[A-Za-z0-9]+$')
#devtools::install_github('massimoaria/openalexR')
library(openalexR)
cepts <- cepts[1]
works_api_url <-  "https://api.openalex.org/works?mailto=tascott@ucdavis.edu&filter=publication_year:>1959,publication_year:<1961,cited_by_count:>0,concept.id:"

query_urls <- paste0(works_api_url,paste(cepts,collapse = '|'))
check_counts <- paste0(query_urls,'&group_by=publication_year')

library(jsonlite)
query_result_count <- fromJSON(check_counts)
total_pages <- ceiling(sum(query_result_count$group_by$count)/query_result_count$meta$per_page)

p = 1
first <- paste0(query_urls,'&per-page=100&cursor=*')#,"&sort=cited_by_count:desc")#&page=1")

p1 <- read_json(first)
count <- p1$meta$count
result_list <- list()
next_cursor <- p1$meta$next_cursor

parse_oa_json <- function(r,vars = c('id','title','doi','display_name','publication_year','ids','open_access','authorships','cited_by_count','concepts','is_paratext','ngrams_url')){
  r[vars]}

result_list <- append(x = result_list,lapply(p1$results,parse_oa_json))

while(!is.null(next_cursor)){
  p = p + 1
  print(paste('p=',p,'c=',next_cursor))
  new_q <- str_replace(first,'\\*$',next_cursor)
  p_new <- read_json(new_q)
  next_cursor <- p_new$meta$next_cursor
  if(length(p_new$results)>0){
    result_list <- append(x = result_list,lapply(p_new$results,parse_oa_json))
  }
  Sys.sleep(0.1)
}






vars = c('id','title','doi','display_name','publication_year','publisher','ids','open_access','authorships','cited_by_count','is_paratext')
test$results[[1]][vars]
parse_oa_json(test$results[[1]])

hames(p_new$results[[1]])
result_list[[10]][,22]
lapply(result_list,dim)
dim(result_list[[10]])
length(result_list)
rbindlist(result_list,fill = T)

while(!is.null(p1$meta$next_cursor) * !is.null(continue$meta$next_cursor)){
  
}
p1$meta
test

table(extracts_filtered$year)

type:journal_article,

C2778407487"
?oa_query
openalexR::oa_query(entity = 'works',)

findOAid(string = 'environmental planning',maxlevel = 2)

oa_concepts[grepl('environmental planning',`normalized_name`),]
oa_concepts[grepl('forestry',`normalized_name`)&level<2,]
oa_concepts[grepl('^ecology',`normalized_name`)&level<2,]


head(oa_concepts)
file.exists(oa_concept_sheet)

head(oa_concepts)

googlesheets4::
#https://docs.google.com/spreadsheets/d/1LBFHjPt4rj_9r0t0TTAlT68NwOtNH8Z21lBMsJDMoZg/edit#gid=575855905
environmental planning
oa_fetch(
  identifier = NULL,
  entity = "works",
  title.search = c("bibliometric analysis", "science mapping"),
  cited_by_count = ">50", 
  from_publication_date = "2020-01-01",
  to_publication_date = "2021-12-31",
  search = NULL,
  sort = "cited_by_count:desc",
  endpoint = "https://api.openalex.org/",
  count_only = TRUE,
  verbose = TRUE
)


library(citationSearch,attach.required = T)
library(data.table)
library(stringr)
lib rary()
system('solr start -c')

records = citationSearch::wos_forestry # sample data for demo purposes

names(records) <- tolower(names(records))
should_have <- c('title','authors','publisher','year','journal_title','doi','source','miscid')
names(records)[names(records) %in% should_have]
should_have[!should_have %in% names(records)]

records$title <- ifelse(!is.na(records$`article title`),records$`article title`,records$title)
setnames(records,c('publication year','source title'),c('year','journal_title'))

records$publisher <- NA
records$source <- 'wos_forestry'
records$miscid <- 1:nrow(records)

records <- records[,names(records) %in% should_have]
###
# title	String	Title of the paper
# authors	String	Concatenated list of authors (e.g "Hileman, Jacob; Bastos, Marco T. A; Lubell, Mark")
# year	Int	4 digit year (between 1800 and 2025)
# publisher	String	Name of publisher
# journal_title	String	Name of journal
# doi	String	DOI
# source	String	Helpful name you have given for the collection
# miscid	String	A unique id field
###
index_records(records, collection_name="WOS_forestry")
queries <- citationSearch::create_queries(extracts_filtered)

results = list()
for(i in seq_along(queries)){
  results[[i]] <- search_collection(queries[[i]], collection_name="WOS_forestry",topn = 1)
  results[[i]]$query <- queries[[i]]
}


res_dt <- rbindlist(results,fill = T)
test <- res_dt[order(-score),][score>14.96&score<15,]
ind <- match(test$query,queries)


table(res_dt$score>15)
extracts_filtered[ind,]$title[1:3]
test$journal_title[1:3]

test$title
results_df[797,]$q
q = queries[797]
test <- search_collection(q, collection_name="WOS_forestry")
test
q

results = list()
count = 1
for (q in queries) {
  res = search_collection(q, collection_name="WOS_forestry")
  res$id = count
  res$q = q
  results[[count]] = res
  count = count + 1
}
results_df = do.call(dplyr::bind_rows, results)


results_df[results_df$score>15,][5,]$journal_title
results_df[results_df$score>15,]
(results_df[results_df$score>15,]$q[5])
(results_df[results_df$score>15,]$title[5])
create_queries
test <- extracts_filtered[toupper(journal_title)=='JOURNAL OF FORESTRY',]
test_qset <- citationSearch::create_queries()

queries[1:2]
results = search_collection(q = queries[3],
                            collection_name = "WOS_forestry", 
                            topn=5)

test[grepl("MOCKRI",toupper(test$authors)),]
results_df[results_df$score>10,]

results
test_qset[22]
table(wos_forestry$`Publication Year`)
grep('firewise',tolower(wos_forestry$Title),value= T)
results
test_qset[1]
table(wos_forestry$`Source Title`)
qset[2]
results
table(records$journal_title)

   
   