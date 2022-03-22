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
devtools::install_github("quanteda/quanteda.sentiment")
library('quanteda.sentiment')

dt <- readRDS('comment_topography/scratch/usfs_cara/generated_from_raw/comments_with_pals_data.RDS')
dt <- data.table(dt)
dt <- dt[Project.Number %in% dt[,.N,by=.(Project.Number)][N>50,]$Project.Number,]
dt$Letter.Text.Clean <- dt$Letter.Text
library(pbapply)
line_list <- lapply(dt$Letter.Text.Clean,tokenize_lines)
line_list <- pblapply(line_list,function(x) x[[1]][nchar(x[[1]])>35])
line_list <- pblapply(line_list,function(x) x[!grepl('^To\\:|^From\\:',x)],cl = 5)
combine_back <- pblapply(line_list,paste,collapse = ' ')
dt$Letter.Text.Clean <- unlist(combine_back)

#remove numbers
dt$Letter.Text.Clean <- tm::removeNumbers(dt$Letter.Text.Clean)
#drop emails ### this takes awhile for some reason, skip for now
#dt$Letter.Text.Clean <- textclean::replace_email(dt$Letter.Text.Clean)

dt$Letter.Text.Clean <- str_replace_all(dt$Letter.Text.Clean, '[^[:alnum:]|^\\s|\\,|\\.]', '')
#remove symbols
#dt$Letter.Text.Clean <- stri_trans_general(str = dt$Letter.Text.Clean, id = "Latin-ASCII")

  

###### NOTE TO DO ######
# GET RID OF GEOGRAPHIC NAMES FROM TEXT
# (RUN ENTITY EXTRACT, FILTER OUT PROPER NAMES)
# code example in tuolumne repo for eis boilerplate

#make lowercase
dt$Letter.Text.Clean <- tolower(dt$Letter.Text.Clean)


###### NOTE TO DO ######
# use hunspell to spell check and replace
# here's one example: https://rstudio-pubs-static.s3.amazonaws.com/555339_14eeb3de14d04b6197261520947bf360.html

###OPEN QUESTION: LEMMATIZE? 
# lemmatize common in topic model kind of stuff, but in this context man words with same lemma mean different things

# REMOVE STOPWORDS? YES FOR NOW
library(tm)
dt$Letter.Text.Clean <- removeWords(dt$Letter.Text.Clean,stopwords(language = 'en'))

## question: custom stop words? probably. things like USFS.
fwrite(data.table(dt)[,.(Letter.Text.Clean)],file = 'comment_topography/scratch/cleaned_comment_text.txt')
dt_meta <- data.table(dt) 
dt_meta[,Letter.Text:=NULL]
#dt_meta[,letter_sents:=NULL]
dt_meta[,Letter.Text.Clean:=NULL]

dt_meta <- dt_meta[,.(Project.Number,FOREST_ID,Letter.Number,Author.ZipCode)]
dt_meta$Author.ZipCode[dt_meta$Author.ZipCode=='']<-NA

dt_meta$Letter.Number<-formatC(dt_meta$Letter.Number,width = max(nchar(dt_meta$Letter.Number)),flag = '0')
dt_meta$Project.Number<-formatC(dt_meta$Project.Number,width = max(nchar(dt_meta$Project.Number)),flag = '0')

dt_meta <- apply(dt_meta,2,enc2utf8)
fwrite(dt_meta,file = 'comment_topography/scratch/cleaned_comment_meta.txt')

