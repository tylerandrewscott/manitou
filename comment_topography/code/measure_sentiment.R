
packages = c('data.table','stringr','tidyverse','sf','lwgeom','tigris','sentimentr')
not_installed = packages[!packages %in% installed.packages()[,'Package']]
if(length(not_installed)>0){lapply(not_installed,install.packages)}
lapply(packages,require,character.only = T)

set.seed(24)
library(tidyverse)
library(vader)
library(tm)
library(data.table)
library(tokenizers)
library(quanteda)
library(quanteda.textstats)
library(vader)
library(pbapply)
dt = fread('comment_topography/scratch/cleaned_comment_text.txt',sep = '\t')
meta = fread('comment_topography/scratch/cleaned_comment_meta.txt',sep = '\t')
#dt = dt[meta$FORM==0,]
#meta = meta[meta$FORM==0,]
#meta$Author.ZipCode <- str_extract(meta$Author.ZipCode,'^[0-9]{5}')
meta$BIGCHAR = (nchar(dt$Letter.Text.Clean)>5e4) + 0

library(textreuse)
dt$hashes <- hash_string(dt$Letter.Text.Clean)
meta$hashes <- dt$hashes
sentiment_file = 'comment_topography/scratch/sentiment_scores.txt'

#### for now this skips docs with really large character counts that gum up the works ### 
dt_uq <- dt[,.(hashes,Letter.Text.Clean)]
dt_uq <- dt_uq[!duplicated(hashes),] 
dt_uq$nchar <- nchar(dt_uq$Letter.Text.Clean)
dt_uq$small_enough <- (dt_uq$nchar<3e3)
dt_large <- dt_uq[small_enough==F,]
dt_uq <- dt_uq[small_enough==T,]  
nt = ntile(1:nrow(dt_uq),nrow(dt_uq)/100)
batch_sentiment = pblapply(unique(nt),function(x) {
  temp = sentiment(text.var = get_sentences(dt_uq$Letter.Text.Clean[nt == x]))
  temp$hashes = dt_uq$hashes[nt == x][temp$element_id]
  temp
  },cl = 4)

batch_dt <- rbindlist(lapply(batch_sentiment,as.data.table),use.names = T)
sent_dt <- batch_dt[,list(sum(word_count),sentimentr::average_weighted_mixed_sentiment(sentiment)),by=.(hashes)]

nt_large = ntile(1:nrow(dt_large),nrow(dt_large)/10)
batch_sentiment_large = pblapply(unique(nt_large),function(x) {
  temp = sentiment(text.var = get_sentences(dt_large$Letter.Text.Clean[nt_large == x]))
  temp$hashes = dt_large$hashes[nt_large == x][temp$element_id]
  temp
},cl = 1)
batch_large_dt <- rbindlist(lapply(batch_sentiment_large,as.data.table),use.names = T)
sent_large_dt <- batch_large_dt[,list(sum(word_count),sentimentr::average_weighted_mixed_sentiment(sentiment)),by=.(hashes)]

dt_all <- rbind(dt_uq,dt_large)
sent_all <- rbind(sent_dt,sent_large_dt)
names(sent_all) <- c('hashes','word_count','sentiment')

dt_with_sent = full_join(dt_all,sent_all)

meta_sent = cbind(meta,dt_with_sent[match(meta$hashes,dt_with_sent$hashes),.(nchar,word_count,sentiment)])
saveRDS(meta_sent[,.(UQID,sentiment,nchar,word_count)],'comment_topography/input/sentiment_scores.txt',sep = '\t')


