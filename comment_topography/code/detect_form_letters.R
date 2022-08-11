
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

dt = readRDS('comment_topography/input/cleaned_comment_text.RDS')
meta = readRDS('comment_topography/input/cleaned_comment_meta.RDS')

meta$FORM = NA
meta$GROUP <- NA
meta$FORM[meta$N>1]<-1

dt$Letter.Text <- tolower(dt$Letter.Text)
dt$Letter.Text <- tm::removeWords(dt$Letter.Text,words = tm::stopwords())
dt$Letter.Text <- tm::removePunctuation(dt$Letter.Text)
#dt$temp_hash <- hash_string(dt$Letter.Text)

#meta$group <- NA
count_dups <- dt[,.N,by=.(Letter.Text)][N>1,]
meta$FORM[is.na(meta$FORM) & meta$uq %in% dt[Letter.Text %in% count_dups$Letter.Text,]$uq] <- 2

dt_uq <- dt[!duplicated(Letter.Text),]
meta_uq <- meta[uq %in% dt_uq$uq,]

corp <- quanteda::corpus(
  dt_uq$Letter.Text,
  docnames = dt_uq$uq)
#toks = quanteda::tokenize_word(corp)
# create a document term matrix 
dfm = dfm(x = corp)
pnums = unique(meta_uq$Project.Number)

for(p in pnums){
  print(p)
  hash_set = unique(meta_uq$uq[meta_uq$Project.Number==p])
  dfm_sub <- dfm[rownames(dfm) %in% hash_set,]  
  if(nrow(dfm_sub)<50e3){m = 0}else{m = 2}
  if(nrow(dfm_sub)<50e3){pr = 1}else{pr = 0.2}
  dfm_sub <- dfm_sub[,colSums(dfm_sub)>m & colMeans(dfm_sub>0)<pr]
  sparse_dfm <- as(dfm_sub, "CsparseMatrix")
  cosign_sim <- proxyC::simil(sparse_dfm,margin = 1,method = 'cosine',
                              digits = 3,min_simil = 0.95,use_nan=T)
  cosign_sums <- rowSums(cosign_sim,na.rm = T)
  is_form <-  (cosign_sums > 1) + 0
  temp_is_form_dt = data.table(temp_hash =  rownames(sparse_dfm),FORM = is_form)
  temp_is_form_dt <- temp_is_form_dt[FORM==1,]
  meta_uq$FORM[meta_uq$Project.Number==p&is.na(meta_uq$FORM)&meta_uq$uq %in% temp_is_form_dt$temp_hash] <- 3

  # form_hashes <- temp_is_form_dt$hash[temp_is_form_dt$FORM==1] 

  #sub_ig <- induced_subgraph(ig,which(V(ig)$hash %in% form_hashes))
  
  #decomp <- decompose.graph(sub_ig)
  adjacency_trip <- as(cosign_sim,"TsparseMatrix")
  edge_dt <- data.table(i = adjacency_trip@i+1,j = adjacency_trip@j+1,x =adjacency_trip@x )
  edge_dt <- edge_dt[!is.na(x),]  
  edge_dt <- edge_dt[i!=j,]   

  #edge_dt <- edge_dt[x==1,]
  net <- network.initialize(nrow(cosign_sim),directed = F,loops = F)
  network.vertex.names(net) <- rownames(cosign_sim)
  network::add.edges(net,tail = edge_dt$i,head = edge_dt$j)
  inet <- intergraph::asIgraph(net)
  V(inet)$hash <- network.vertex.names(net)
  idecomp <- decompose.graph(inet)
  comp <- components(inet)
  #### can use to verify
#i = which(sapply(idecomp,vcount)==3)[1]
#hh <-  V(idecomp[[i]])$hash
#cosign_sim[rownames(cosign_sim) %in% hh, colnames(cosign_sim) %in% hh]
#dt_uq$Letter.Text[dt_uq$temp_hash %in% hh]

  hash_group_temp <- data.table(temp_hash = V(inet)$hash,group = comp$membership)
  
  hash_group_temp$group_size <- comp$csize[comp$membership]
  hash_group_temp$group_id <- paste0(p,'_',hash_group_temp$group)
  meta_uq$GROUP[meta_uq$Project.Number==p] <- hash_group_temp$group_id[match(meta_uq$uq[meta_uq$Project.Number==p],hash_group_temp$temp_hash)]
  meta_uq$group_size[meta_uq$Project.Number==p] <- hash_group_temp$group_size[match(meta_uq$uq[meta_uq$Project.Number==p],hash_group_temp$temp_hash)]
  }



# form_summary <- meta[,list(round(mean(FORM),2),.N),by=.(Project.Number)]
# names(form_summary) <- c('project #','% form letters','total comments')
# library(htmlTable)
# htmlTable(form_summary)
meta_uq$FORM[is.na(meta_uq$FORM)] <- 0

dt_uq$FORM<-meta_uq$FORM[match(dt_uq$uq,meta_uq$uq)]
dt_uq$GROUP <- meta_uq$GROUP[match(dt_uq$uq,meta_uq$uq)]
dt_uq$group_size <- meta_uq$group_size[match(dt_uq$uq,meta_uq$uq)]

dt <- cbind(dt,dt_uq[match(dt$Letter.Text,dt_uq$Letter.Text),.(FORM,GROUP,group_size)])
fwrite(dt[,.(uq,FORM,GROUP,group_size)],'comment_topography/input/form_letter_designation.txt',sep = '\t')



