library(data.table)
library(stm)
library(tidytext)
library(tm)
library(textclean)
library(textstem)
set.seed(24)
library(spacyr)
library(udpipe)
#start the English language dictionary of spacy
spacy_initialize(model = "en_core_web_sm")

# library(reticulate)
# py_config()
# py_install("textblob")
library(spacyr)
library(quanteda)
library(stringr)
library(stringi)
library(textclean)
library(hunspell)
library(tokenizers)
library(textstem)
library(tm)
library(devtools)
library(textreuse)
starting_file <- 'comment_topography/scratch/case_comments_from_cara_w_zips.RDS'
dt <- readRDS(starting_file)
dt <- data.table(dt)
dt$Letter.Text <- enc2utf8(dt$Letter.Text)
dt <- dt[Letter.Text!='',]
dt$uq <- udpipe::unique_identifier(dt,c('Project.Number','Letter.Text'))

dt$Author.ZipCode <- str_remove(dt$Author.ZipCode,'^\\s+')
dt$Author.ZipCode <- str_remove(dt$Author.ZipCode,'\\s+$')
dt$Author.ZipCode <- str_remove(dt$Author.ZipCode,'\\)+')
dt$Author.ZipCode <- str_replace(dt$Author.ZipCode,'([0-9]{5})\\s([0-9]{4})','\\1-\\2')
dt$Author.ZipCode[grepl('^[0-9]{9}$',dt$Author.ZipCode)] <- str_replace(dt$Author.ZipCode[grepl('^[0-9]{9}$',dt$Author.ZipCode)],'([0-9]{5})([0-9]{4})','\\1-\\2')
dt$Author.ZipCode[grepl('^[0-9]{5}-[0-9]{0,3}$',dt$Author.ZipCode)] <- str_remove(dt$Author.ZipCode[grepl('^[0-9]{5}-[0-9]{0,3}$',dt$Author.ZipCode)],'-.*')
dt$Author.ZipCode[!is.na(dt$Author.ZipCode) & (!nchar(dt$Author.ZipCode) %in% c(5,10))] <- NA

dt$Capture.ZipCode <- NA
dt$last100chars <- str_sub(dt$Letter.Text,start = -100)
dt$temp_ext <- str_extract(dt$last100chars,'[^\\,]+$')
dt$temp_ext <- str_remove(dt$temp_ext,'^\\s{1,}')
dt$is_state <- str_extract(dt$temp_ext,'^[A-Z]{2}(?=\\s)') %in% c(state.abb,'DC')
dt$Capture.ZipCode <- str_extract(dt$temp_ext,'[0-9]{5}(|(\\s|-)[0-9]{4})(?=\\\n)')
dt$Capture.ZipCode <- gsub('\\s','-',dt$Capture.ZipCode)
dt$Use.Zipcode <- ifelse(!is.na(dt$Author.ZipCode),dt$Author.ZipCode,dt$Capture.ZipCode)
dt$Author.City[dt$Author.City==''] <- NA
dt$Author.State[dt$Author.State==''] <- NA

dt <- dt[,!grepl('activity$|purpose$|FACTS',names(dt)),with = F]
dt <- dt[,grepl('[a-z]',names(dt)),with = F]
dt[,grab_zip:=NULL]
dt[,new_zip:=NULL]
dt[,Capture.ZipCode:=NULL]

uq_count <- dt[,.N,by=names(dt)]
uq_count$has_email <- grepl('[A-Za-z0-9]{1,}@[A-Za-z0-9]{1,}\\.(com|net|org|edu)',uq_count$Letter.Text)
#uq_count$unique_but_bad_identifiers <- (uq_count$N==1 & !uq_count$has_email & !is.na(uq_count$Use.Zipcode))

uq_count[,.N,by=.(Project.Number)]

library(pbapply)
line_list <- tokenize_lines(uq_count$Letter.Text)
alphanumOnly <- function(x){
  x[grepl('[A-Za-z0-9]',x)]
}

line_list <- lapply(line_list,alphanumOnly)

removeToFrom <- function(x){
  x[!grepl('^To\\:|^From\\:',x)]
}
line_list <- lapply(line_list,removeToFrom)

garbageUnicode <- function(x,code){
  str_remove_all(x,'\\\x94')
}
line_list <- lapply(line_list,garbageUnicode)

minCharacter <- function(x,minchar){
  x[nchar(x)>=minchar]
}
line_list <- lapply(line_list,minCharacter,minchar = 35)
combine_back <- lapply(line_list,paste,collapse = ' ')
uq_count$Letter.Text <- unlist(combine_back)

#remove numbers
uq_count$Letter.Text <- tm::removeNumbers(uq_count$Letter.Text)

#remove garbage symbols
uq_count$Letter.Text <- str_remove_all(uq_count$Letter.Text,'\\*|\\#')

#drop emails ### this takes awhile for some reason
#uq_count$Letter.Text.Clean <- textclean::replace_email(uq_count$Letter.Text.Clean)
#this regex seems quicker, I suspect just because stringr is quick
uq_count$Letter.Text <- str_remove_all(uq_count$Letter.Text,'[^\\s]+@[^\\s]+')

###### NOTE TO DO ######
# GET RID OF GEOGRAPHIC NAMES FROM TEXT
# (RUN ENTITY EXTRACT, FILTER OUT PROPER NAMES)
# code example in tuolumne repo for eis boilerplate

#make lowercase
#dt$Letter.Text.Clean <- tolower(dt$Letter.Text.Clean)


###### NOTE TO DO ######
# use hunspell to spell check and replace
# here's one example: https://rstudio-pubs-static.s3.amazonaws.com/555339_14eeb3de14d04b6197261520947bf360.html

###OPEN QUESTION: LEMMATIZE? 
# lemmatize common in topic model kind of stuff, but in this context man words with same lemma mean different things


# REMOVE STOPWORDS? YES FOR NOW
library(tm)
#dt$Letter.Text.Clean <- removeWords(dt$Letter.Text.Clean,stopwords(language = 'en'))
uq_count = uq_count[uq_count$Letter.Text!='',]
uq_count = uq_count[nchar(uq_count$Letter.Text)>50,]

#uq_count$id_hash <- as.character(uq_count$id_hash)

dt_meta <- data.table(uq_count) 
dt_meta$Project.Number<-formatC(dt_meta$Project.Number,width = max(nchar(dt_meta$Project.Number)),flag = '0')
#uq_count$Letter.Text <- enc2utf8(uq_count$Letter.Text)

#dt_meta[,letter_sents:=NULL]
dt_meta[,Letter.Text:=NULL]
dt_meta[,grab_address:=NULL]
dt_meta[,last100chars:=NULL]
dt_meta[,temp_ext:=NULL]



dt_meta <- dt_meta %>% mutate_if(is.character,enc2utf8) %>%
  select(-Analysis.Type,-Project.Name,-forest,-calendarYearSigned,-RegionName,-Comment.Period.Start.Year) %>%
  data.table()

library(utf8)

dt_meta$usfsErrors[!utf8::utf8_valid(dt_meta$usfsErrors)] <- NA

table(duplicated(data.table(uq_count)[,.(Letter.Text,uq)]))
## question: custom stop words? probably. things like USFS.
saveRDS(data.table(uq_count)[,.(Letter.Text,uq)],file = 'comment_topography/input/cleaned_comment_text.RData')
saveRDS(dt_meta ,file = 'comment_topography/input/cleaned_comment_meta.RData')

