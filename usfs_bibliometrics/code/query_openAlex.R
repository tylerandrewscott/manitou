library(referenceBuild)
library(data.table)
library(jsonlite)
library(rjson)
library(stringr)

### note this doesn't automatically do what I did for enviro. science, which is HUGE


cepts_of_interest <- c('environmental impact assessment','ecosystem','greenhouse gas','climate change',
                       'ecology','restoration ecology',
                       'environmental science','grazing','fishery','agroforestry','outdoor recreation','population biology','wilderness','forestry','environmental economics',
                       'community forestry','urban forestry','wildlife management','restoration',
                       'wildlife','habitat','wildfire suppression','environmental assessment','environmental resource management','natural resource management','air quality')

#look for environmental concepts
ids <- queryConcepts(concept_string =  paste(cepts_of_interest,collapse = '|'),mailto = 'tascott@ucdavis.edu',maxlevel = 3,max_return = 50,per_page = 100)
ids$content_queries <- str_replace_all(tolower(ids$display_name),'\\s','_')
ids <- ids[level<4,]
ids <- ids[!grepl("\\,",display_name),]
ids <- ids[order(-works_count),]
ids$display_name <- tolower(ids$display_name)
ids <- ids[display_name %in% cepts_of_interest,]

early_year <- 1970
current_year <- year(Sys.Date())
year_range <- c(1970,2021)
year_breaks <- seq(1970,2020,10)
#concept_finds <- lapply(content_queries,function(x) find_concepts(query_string = x,mailto = 'tascott@ucdavis.edu',maxlevel = 3))
existing_files <- list.files('usfs_bibliometrics/scratch/openAlex_data/')
content_queries <- ids$display_name
min_to_split <- 1e6
redo <- F
for(content in content_queries){
  print(content)
  #ids <- lapply(c('environmental planning','forestry','^ecology'),function(x) findOAid(x,maxlevel = 1))
  cid <- ids$id[ids$display_name==content]
  est <- ids$works_count[ids$display_name==content]
  if(est>min_to_split){first_year <- year_breaks;last_year <- year_breaks+9}else{first_year <- early_year;last_year <- current_year}
  for (y in seq_along(first_year)){
    y1 <- first_year[y];y2 <- min(last_year[y],current_year)
    temp_file <-(paste0('usfs_bibliometrics/scratch/openAlex_data/',str_replace_all(content,'\\s','_'),'_',y1,'_',y2,'.json.gz'))
    if({file.exists(temp_file)|content %in% c('environmental science')} & !redo){print(paste('already have',temp_file,'skipping'));next}else{
      #this is how I manually split enviro science
      #print(paste('querying',content,'from',y1,'to',y2))
      #y1<-2020;y2<-2020
      #yr_fill <- if(y1!=y2){paste(y1,y2,sep = '_')}else{y1}
      #temp_file <- str_replace(temp_file,'[0-9]{4}_[0-9]{4}',as.character(yr_fill))
      res <- extractWorks(override = T,dest_file = temp_file,mailto = 'tascott@ucdavis.edu',return = F,
                         concept_id = cid,cursor = T,from_date = y1,to_date = y2,
                         per_page = 200,keep_paratext = F,debug = F,sleep_time = 0.01)
        }
  }
  }
   