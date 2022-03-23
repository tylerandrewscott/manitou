library(data.table)
library(stm)
library(tidytext)
library(tm)
library(textclean)
library(textstem)

library(spacyr)
#start the English language dictionary of spacy
spacy_initialize(model = "en_core_web_sm")

library(reticulate)
py_config()
py_install("textblob")
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


dt <- readRDS('comment_topography/scratch/case_comments_from_cara_w_zips.RDS')
dt <- data.table(dt)

last_20 = str_sub(dt$Letter.Text,start = -20)
zip_extracts <- str_extract_all(last_20,'[^0-9][0-9]{5}[^0-9]')

zip_extracts <- lapply(zip_extracts,function(x) {str_remove_all(x,'[^0-9]')})
zip_extracts <- lapply(zip_extracts,function(x) unique(x))
zip_extracts <- lapply(zip_extracts,function(x) x[1])

zips = ifelse(sapply(zip_extracts,length)==0,NA,zip_extracts)

dt$extracted_zip <- unlist(zips)
dt$ZIPCODE <- ifelse(!is.na(dt$Author.ZipCode),dt$Author.ZipCode,dt$extracted_zip)

dt$Letter.Text.Clean <- dt$Letter.Text
dt$UQID <- 1:nrow(dt)

library(pbapply)
line_list <- tokenize_lines(dt$Letter.Text.Clean)
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
dt$Letter.Text.Clean <- unlist(combine_back)

#remove numbers
dt$Letter.Text.Clean <- tm::removeNumbers(dt$Letter.Text.Clean)

#remove garbage symbols
dt$Letter.Text.Clean <- str_remove_all(dt$Letter.Text.Clean,'\\*|\\#')

#drop emails ### this takes awhile for some reason
#dt$Letter.Text.Clean <- textclean::replace_email(dt$Letter.Text.Clean)
#this regex seems quicker, I suspect just because stringr is quick
dt$Letter.Text.Clean <- str_remove_all(dt$Letter.Text.Clean,'[^\\s]+@[^\\s]+')



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
dt = dt[dt$Letter.Text.Clean!='',]
dt = dt[nchar(dt$Letter.Text.Clean)>50,]
## question: custom stop words? probably. things like USFS.
fwrite(data.table(dt)[,.(Letter.Text.Clean,UQID)],file = 'comment_topography/scratch/cleaned_comment_text.txt',sep = '\t')
dt_meta <- data.table(dt) 
dt_meta[,Letter.Text:=NULL]
#dt_meta[,letter_sents:=NULL]
dt_meta[,Letter.Text.Clean:=NULL]

dt_meta <- dt_meta[,.(Project.Number,FOREST_ID,Letter.Number,Author.ZipCode,UQID,new_zip)]
dt_meta$Author.ZipCode[dt_meta$Author.ZipCode=='']<-NA

dt_meta$Letter.Number<-formatC(dt_meta$Letter.Number,width = max(nchar(dt_meta$Letter.Number)),flag = '0')
dt_meta$Project.Number<-formatC(dt_meta$Project.Number,width = max(nchar(dt_meta$Project.Number)),flag = '0')

dt_meta <- apply(dt_meta,2,enc2utf8)
fwrite(data.table(dt_meta),file = 'comment_topography/scratch/cleaned_comment_meta.txt',sep = '\t')

