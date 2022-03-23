
library(tidyverse)
library(vader)
library(tm)
library(data.table)
library(tokenizers)
library(quanteda)
library(quanteda.textstats)
library(textreuse)
library(statnet)
library(intergraph)
library(textmineR)
require(proxyC)
require(Matrix)
## Loading required package: Matrix
require(RcppParallel)
library(igraph)
## Loading required package: magrittr
# Set number of threads
setThreadOptions(7)

dt = fread('comment_topography/scratch/cleaned_comment_text.txt',sep = '\t')
meta = fread('comment_topography/scratch/cleaned_comment_meta.txt',sep = '\t')

meta$FORM = NA
meta$group <- NA
meta$hash = textreuse::hash_string(dt$Letter.Text.Clean)
dt$hash <- meta$hash
dt_uq <- dt[!duplicated(dt$hash),]
meta_uq <- meta[!duplicated(dt$hash),]

dt_uq[,UQID:=NULL]
meta_uq[,UQID:=NULL]
##use quick hash to identify identical text (after clean up, many form letters are identical)
dt_uq$Letter.Text.Clean <- tolower(dt_uq$Letter.Text.Clean)
dt_uq$Letter.Text.Clean <- tm::removeWords(dt_uq$Letter.Text.Clean,words = tm::stopwords())
dt_uq$Letter.Text.Clean <- tm::removePunctuation(dt_uq$Letter.Text.Clean)

corp <- quanteda::corpus(
  dt_uq$Letter.Text.Clean,
  docnames = dt_uq$hash)
#toks = quanteda::tokenize_word(corp)
# create a document term matrix 
dfm = dfm(x = corp)
pnums = unique(meta_uq$Project.Number)
for(p in pnums){
  print(p)
  hash_set = unique(meta_uq$hash[meta_uq$Project.Number==p])
  dfm_sub <- dfm[rownames(dfm) %in% hash_set,]  
  dfm_sub <- dfm_sub[,colSums(dfm_sub)>0 & colSums(dfm_sub)<nrow(dfm_sub)]
  sparse_dfm <- as(dfm_sub, "CsparseMatrix")
  cosign_sim <- proxyC::simil(sparse_dfm,margin = 1,method = 'cosine',
                              digits = 3,min_simil = 0.95,use_nan=T)
  cosign_sums <- rowSums(cosign_sim>=0.95,na.rm = T)
  is_form <-  (cosign_sums > 1) + 0
  temp_is_form_dt = data.table(hash = rownames(dfm_sub),FORM = is_form)
  meta_uq$FORM[meta_uq$Project.Number==p] <- temp_is_form_dt$FORM[match(meta_uq$hash[meta_uq$Project.Number==p],temp_is_form_dt$hash)]
  meta$FORM[meta$Project.Number==p] <- meta_uq$FORM[match(meta$hash[meta$Project.Number==p],meta_uq$hash)]

  if(p!=pnums[4]){ 
  cosign_sim_binary <- (cosign_sim>=0.95) + 0
   adjacency_trip <- as(cosign_sim_binary,"TsparseMatrix")
   edge_dt <- data.table(i = adjacency_trip@i+1,j = adjacency_trip@j+1)
   edge_dt <- edge_dt[i!=j,]
   #endes = ntile(1:nrow(edge_dt),100)
   #uq_endes <- unique(endes)
   #edge_list <- split(edge_dt,endes)
   ig <- make_empty_graph(n = nrow(cosign_sim),directed = F)
   V(ig)$hash = rownames(cosign_sim)
   ig <- add_edges(ig,edges = as.matrix(edge_dt[,.(i,j)]))
 # # length(uq_endes)
 #  for(i in uq_endes){
 #    print(i)
 #    ig <- add_edges(ig,edges = as.matrix(edge_list[[i]][,.(i,j)]))
 #  }
   comps <- igraph::components(ig)
   hash_group_temp <- data.table(hash = V(ig)$hash,letter_group = comps$membership)
  meta_uq$group[meta_uq$hash %in% hash_group_temp$hash] <- paste0(p,'_',hash_group_temp$letter_group[match(meta_uq$hash[meta_uq$hash %in% hash_group_temp$hash],hash_group_temp$hash)])
  meta$GROUP[meta$Project.Number==p] <- meta_uq$group[match(meta$hash[meta$Project.Number==p],meta_uq$hash)]
  }
}


# form_summary <- meta[,list(round(mean(FORM),2),.N),by=.(Project.Number)]
# names(form_summary) <- c('project #','% form letters','total comments')
# library(htmlTable)
# htmlTable(form_summary)

  
fwrite(meta[,.(UQID,FORM,GROUP)],'comment_topography/scratch/form_letter_designation.txt',sep = '\t')




