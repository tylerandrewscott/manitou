
packages = c('data.table','stringr','tidyverse','sf','lwgeom','tigris','textreuse')
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
library(pals)
library(udpipe)
#dt = fread('comment_topography/scratch/cleaned_comment_text_form_desig.txt',sep = '\t')
dt = fread('comment_topography/scratch/cleaned_comment_text.txt',sep = '\t')
meta = fread('comment_topography/scratch/cleaned_comment_meta.txt',sep = '\t')
form_letter = fread('comment_topography/scratch/form_letter_designation.txt',sep = '\t')
sent_values = fread('comment_topography/scratch/sentiment_scores.txt',sep = '\t')

meta = left_join(meta,sent_values)
meta = left_join(meta,form_letter)

library(textreuse)

project.name.key = data.table(Project.Number = unique(meta$Project.Number),Project =
                                c("Westside Fire Recovery",
                                  "Stibnite Gold Mine",
                                  "Sage-grouse Amendments",
                                  "Alaska Roadless Rule"))

meta = left_join(meta,project.name.key)

library(ggthemes)




gg_density = ggplot() + 
  geom_density(data = meta[FORM==0,],aes(x = sentiment,col = Project,linetype = 'form'),trim = T) +
  geom_density(data = meta[FORM==1,],aes(x = sentiment,col = Project,linetype = 'unique'),trim = T) +
  scale_linetype_manual(values = c(2,1),labels = c('form letter','unique comment')) +
  theme_bw() + theme(legend.title = element_blank(),axis.text.y = element_blank(),legend.position = c(0.2,0.5)) +
  ggtitle('Distribution of sentiment by project and comment type')
ggsave(plot = gg_density,filename = 'comment_topography/output/sentiment_density_project_type.png',dpi = 300,units = 'in',width = 10,height = 10)

sentiment_boxplot = ggplot(meta) + 
  geom_boxplot(aes(y = Project,x = sentiment,col = as.factor(FORM))) +
  scale_color_colorblind(name = 'comment',labels=c('individual','form'))+
  theme_bw() + ggtitle('Sentiment by project and comment type') +
  scale_x_continuous(name = 'comment sentiment') + 
  theme(
    axis.text.y = element_text(size = 12,angle = 45),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_blank())

ggsave(plot = sentiment_boxplot,filename = 'comment_topography/output/sentiment_boxplot.png',width = 10,height = 7,units = 'in',dpi = 300)

sentiment_vs_length <- ggplot(meta) + 
  geom_point(aes(x = log(nchar),y = sentiment),pch = 21,alpha = 0.25) +
  theme_bw() + ggtitle('sentiment vs. comment length') +
  scale_x_continuous('ln(# characters)') + 
  scale_y_continuous('estimated sentiment')

ggsave(plot = sentiment_vs_length,filename = 'comment_topography/output/sentiment_vs_length.png',dpi= 300,units = 'in',height = 7,width = 7)

tab = meta[,list(.N,mean(FORM)),by=.(Project.Number)]
tab$Project.Name <- project.name.key$Project[match(tab$Project.Number,project.name.key$Project.Number)]

setnames(tab,c('N','V2'),c('# letters','% form letter'))
tab$`% form letter` <- round(tab$`% form letter`,2)
htmlTable::htmlTable(tab[,.(Project.Name,`# letters`,`% form letter`)],file = 'comment_topography/output/count_summary.html')

library(ggmaps)
library(ggthemes)
library(tigris)
states <- states(year = 2020,class = 'sf',cb = T)
zcta <- zctas(class = 'sf',year = 2010)
zcta <- st_transform(zcta,st_crs(albersNA))
meta[meta$new_zip==''] <- NA
meta$new_zip <- str_extract(meta$new_zip,'^[0-9]{5}')
projs <- unique(meta$Project.Number)

td = tempdir()
albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
# # 
adm_url = "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip"
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(adm_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
admin_districts <- st_read(fpath)
admin_districts <- st_transform(admin_districts,crs = st_crs(albersNA))
# fix bad polygons
bad_polys = !st_is_valid(admin_districts)
admin_districts[bad_polys,] <- st_make_valid(admin_districts[bad_polys,])
admin_districts$FOREST_ID <- paste0(admin_districts$REGION,admin_districts$FORESTNUMB)
project.name.key$FORESTS <- list('0505','0412',c(paste0('04',admin_districts$FORESTNUMB[admin_districts$REGION=='04'])),c(paste0('10',admin_districts$FORESTNUMB[admin_districts$REGION=='10'])))

##### map sentiment by zcta and project
plyr::l_ply(seq(nrow(project.name.key)),function(p){
proj <- meta[Project.Number == project.name.key$Project.Number[p],]
pzip <- zcta[zcta$ZCTA5CE10 %in% proj$new_zip,]
sent_by_zip <- proj[,list(.N,mean(sentiment,na.rm=T)),by=.(new_zip)]
setnames(sent_by_zip,'new_zip','ZCTA5CE10')
zcta_temp <- zcta[zcta$ZCTA5CE10 %in% proj$new_zip,]
zcta_temp <- left_join(zcta_temp,sent_by_zip)
admin_temp <- admin_districts[admin_districts$FOREST_ID %in% project.name.key$FORESTS[[which(project.name.key$Project.Number[p]==project.name.key$Project.Number)]],]

gg_p <- ggplot() + theme_map() + 
  scale_x_continuous(limits=c(NA,0))+
  scale_y_continuous(limits=c(15,NA))+
  geom_sf(data = states,fill = NA,col = 'grey50')+
  geom_sf(data = admin_temp,fill = NA,col = 'dark green') +
  geom_sf(data = zcta_temp,aes(fill = V2,col = V2)) + 
  theme(legend.position = c(0.8,0.3))+
  ggtitle(paste0('Avg. sentiment by zipcode,',project.name.key$Project[p]))+
  scale_fill_gradientn(limits= c(-0.5,0.5),breaks = c(-0.5,0,0.5),labels = c('more negative','neutral', 'more positive'),name = 'avg. sentiment',colours=coolwarm(100), guide = "colourbar")+
scale_colour_gradientn(limits= c(-0.5,0.5),breaks = c(-0.5,0,0.5),labels = c('more negative','neutral', 'more positive'),name = 'avg. sentiment',colours=coolwarm(100), guide = "colourbar")

ggsave(plot = gg_p,filename = paste0('comment_topography/output/all_sentiment_map_',project.name.key$Project.Number[p],'.png'),dpi = 300,units = 'in',height = 7,width = 10)
})

## map unique letter sentiment by project
plyr::l_ply(seq(nrow(project.name.key)),function(p){
  proj <- meta[Project.Number == project.name.key$Project.Number[p] & FORM==0,]
  pzip <- zcta[zcta$ZCTA5CE10 %in% proj$new_zip,]
  sent_by_zip <- proj[,list(.N,mean(sentiment,na.rm=T)),by=.(new_zip)]
  setnames(sent_by_zip,'new_zip','ZCTA5CE10')
  zcta_temp <- zcta[zcta$ZCTA5CE10 %in% proj$new_zip,]
  zcta_temp <- left_join(zcta_temp,sent_by_zip)
  admin_temp <- admin_districts[admin_districts$FOREST_ID %in% project.name.key$FORESTS[[which(project.name.key$Project.Number[p]==project.name.key$Project.Number)]],]
  
  gg_p <- ggplot() + theme_map() + 
    scale_x_continuous(limits=c(NA,0))+
    scale_y_continuous(limits=c(15,NA))+
    geom_sf(data = states,fill = NA,col = 'grey50')+
    geom_sf(data = admin_temp,fill = NA,col = 'dark green') +
    geom_sf(data = zcta_temp,aes(fill = V2,col = V2)) + 
    theme(legend.position = c(0.8,0.3))+
    ggtitle(paste0('Avg. sentiment by zipcode,',project.name.key$Project[p]),'unique letters only')+
    scale_fill_gradientn(limits= c(-.5,0.5),breaks = c(-0.5,0,0.5),labels = c('more negative','neutral', 'more positive'),name = 'avg. sentiment',colours=coolwarm(100), guide = "colourbar")+
    scale_colour_gradientn(limits= c(-.5,0.5),breaks = c(-0.5,0,0.5),labels = c('more negative','neutral', 'more positive'),name = 'avg. sentiment',colours=coolwarm(100), guide = "colourbar")
  ggsave(plot = gg_p,filename = paste0('comment_topography/output/unique_sentiment_map_',project.name.key$Project.Number[p],'.png'),dpi = 300,units = 'in',height = 7,width = 10)
})


#### map form letter sentiment by project
plyr::l_ply(seq(nrow(project.name.key)),function(p){
  p  = 1
  proj <- meta[Project.Number == project.name.key$Project.Number[p] & FORM==1,]
  pzip <- zcta[zcta$ZCTA5CE10 %in% proj$new_zip,]
  sent_by_zip <- proj[,list(.N,mean(sentiment,na.rm=T)),by=.(new_zip)]
  setnames(sent_by_zip,'new_zip','ZCTA5CE10')
  zcta_temp <- zcta[zcta$ZCTA5CE10 %in% proj$new_zip,]
  zcta_temp <- left_join(zcta_temp,sent_by_zip)
  admin_temp <- admin_districts[admin_districts$FOREST_ID %in% project.name.key$FORESTS[[which(project.name.key$Project.Number[p]==project.name.key$Project.Number)]],]
  
  gg_p <- ggplot() + theme_map() + 
    scale_x_continuous(limits=c(NA,0))+
    scale_y_continuous(limits=c(15,NA))+
    geom_sf(data = states,fill = NA,col = 'grey50')+
    geom_sf(data = admin_temp,fill = NA,col = 'dark green') +
    geom_sf(data = zcta_temp,aes(fill = V2,col = V2)) + 
    theme(legend.position = c(0.8,0.3))+
    ggtitle(paste0('Avg. sentiment by zipcode,',project.name.key$Project[p]),'form letters only')+
    scale_fill_gradientn(limits= c(-.5,0.5),breaks = c(-0.5,0,0.5),labels = c('more negative','neutral', 'more positive'),name = 'avg. sentiment',colours=coolwarm(100), guide = "colourbar")+
    scale_colour_gradientn(limits= c(-.5,0.5),breaks = c(-0.5,0,0.5),labels = c('more negative','neutral', 'more positive'),name = 'avg. sentiment',colours=coolwarm(100), guide = "colourbar")
  ggsave(plot = gg_p,filename = paste0('comment_topography/output/form_sentiment_map_',project.name.key$Project.Number[p],'.png'),dpi = 300,units = 'in',height = 7,width = 10)
})



#####
proj_list <- lapply(seq(nrow(project.name.key)),function(p){
  proj <- meta[Project.Number == project.name.key$Project.Number[p],]
  setnames(proj,'new_zip','ZCTA5CE10')
  pzip <- zcta[zcta$ZCTA5CE10 %in% proj$new_zip,]
  zcta_temp <- zcta[zcta$ZCTA5CE10 %in% proj$ZCTA5CE10,]
  admin_temp <- admin_districts[admin_districts$FOREST_ID %in% project.name.key$FORESTS[[which(project.name.key$Project.Number[p]==project.name.key$Project.Number)]],]
  dist_mat <- st_distance(zcta_temp,admin_temp)
  zcta_temp$km_from_project <- apply(dist_mat,1,min)/1000
  proj$km_from_project <- zcta_temp$km_from_project[match(proj$ZCTA5CE10,zcta_temp$ZCTA5CE10)]
  proj})
proj_dt <- rbindlist(proj_list)

sent_by_distance <- ggplot(proj_dt,aes(x = sqrt(km_from_project),
                                       y = sentiment,group = Project,col = Project)) + 
  scale_color_colorblind(name = 'Project') + theme_bw()+
  geom_point(pch = 19,alpha = 0.5) + 
  stat_smooth() + 
  ggtitle('comment sentiment vs. distance from project') + 
  scale_y_continuous('comment sentiment') + 
  scale_x_continuous('sqrt(km) from project area') + 
  theme(legend.position = c(0.8,0.1),legend.title = element_blank())
ggsave(plot = sent_by_distance,filename = 'comment_topography/output/all_sentiment_vs_distance.png',dpi = 300,units = 'in',width = 10,height = 10)
  
sent_by_distance_and_letter <- ggplot(proj_dt,
    aes(x = sqrt(km_from_project),y = sentiment,group = as.factor(FORM),
        col = as.factor(FORM))) + 
  scale_color_colorblind(name = 'letter type',labels = c('form letter','unique comment')) + theme_bw()+
  geom_point(pch = 21,alpha = 0.5) + 
  stat_smooth() + 
  ggtitle('comment sentiment vs. distance from project','by letter type') + 
  #scale_linetype_manual(values = c(3,1),labels = c('form letter','unique comment')) +
  scale_y_continuous('comment sentiment') + 
  scale_x_continuous('sqrt(km) from project area') + 
  theme(legend.position = c(0.8,0.1),legend.title = element_blank())
ggsave(plot = sent_by_distance_and_letter,filename = 'comment_topography/output/sentiment_vs_distance_by_type.png',dpi = 300,units = 'in',width = 10,height = 10)

meta$GROUP[meta$GROUP==''] <- NA

meta[Project=='Stibnite Gold Mine',.N,by=.(GROUP)][order(-N),]
cumSum <- meta[,.N,by=.(GROUP,Project)][order(Project,N),][!is.na(GROUP),]
cumSum$pindex <- 1
cumSum[,cumN := cumsum(N),by=.(Project)]
cumSum[,pindex := cumsum(pindex),by=.(Project)]
cumSum[Project=='Stibnite Gold Mine']

cumulative_dist <- ggplot(data = cumSum) + 
  facet_wrap(~Project,scales = 'free',ncol = 2) + 
  #geom_point(aes(y = cumN,x=pindex))
  geom_path(data = cumSum,aes(y = cumN,x=pindex)) +
  geom_point(data = cumSum[N>1,],aes(y = cumN,x = pindex),size = 0.5)+
  scale_x_continuous(name = 'unique submission') + theme_bw() +
  scale_y_continuous(name = 'total comments')+
  labs(caption = 'points show form letters (N>1)') +
  ggtitle('cumulative distribution of letter grouped by uniqueness')

ggsave(plot = cumulative_dist,filename = 'comment_topography/output/cumulative_distribution_by_unique_product.png',units = 'in',width = 10,height = 10)


ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
dt_nf = dt[meta$FORM==0,]
meta_nf = meta[FORM==0,]

p = project.name.key$Project.Number[1]
dt_nf <- dt_nf[UQID %in%  meta_nf$UQID[meta_nf$Project.Number==p]]
meta_nf <- meta_nf[Project.Number == p,]
meta_nf$sent_group <- ifelse(meta_nf$sentiment<(-0.2),'negative',ifelse(meta_nf$sentiment>(0.2),'positive','neutral'))

x <- udpipe_annotate(ud_model, x = tolower(dt_nf$Letter.Text.Clean),doc_id = meta_nf$UQID)
x <- as.data.frame(x)
x$sentiment_group <- meta_nf$sent_group[match(x$doc_id,meta_nf$UQID)]

i = 'negative'
nmin = 10; nr = 0
rake <- keywords_rake(x = x[x$sentiment_group==i,], 
                      term = "token", group = c('sentiment_group'),
                      relevant = x$upos[x$sentiment_group==i] %in% c("NOUN", "ADJ",'PROPN'),
                      ngram_max = 3,n_min = nmin)  %>%
  arrange(-rake) %>%  mutate(sentiment_group = i) %>% head(.,3) 
rake

#x <- x[x$upos !='PUNCT',]
#x <- x[!x$token %in% stopwords::stopwords(),]
cluster_keys <- lapply(sort(unique(M_dt2$cluster)),function(i) {
  print(i)
  nr <- 0;nmin <- 10
  while(nr == 0){
    rake <- keywords_rake(x = x[x$cluster==i,], 
                          term = "token", group = c('cluster'),
                          relevant = x$upos[x$cluster==i] %in% c("NOUN", "ADJ",'PROPN'),
                          ngram_max = 3,n_min = nmin)  %>%
      arrange(-rake) %>%  mutate(cluster = i) %>% head(.,3) 
    # filter(!keyword %in% c('network governance','governance networks','policy networks','policy network','social networks','network analysis',
    #                   'natural resource','environmental management','environmental policy','environmental decision')) %>%
    #  filter(!keyword %in% c('policy networks','policy network','social network analysis')) %>% 
    #  head(.,3) %>% 
    nr = nrow(rake)
    nmin = nmin -1





dt_uq$val <- dt_uq$pos - dt_uq$neg

dt$net_positivity <- dt_uq$val[match(dt$hashes,dt_uq$hashes)]

ggplot(data = dt[meta$FORM==T,]) + 
  geom_density

table(is.na(dt_uq$neg))

sent_dt$neg[match(dt_uq$hashes,sent_dt$hashes)]
sent_dt$pos[match(dt_uq$hashes,sent_dt$hashes)]
sent_dt$neu[match(dt_uq$hashes,sent_dt$hashes)]

temp = vader::vader_df(dt_uq$Letter.Text.Clean[dt_uq$nchar<2000])


while(any(is.na(dt_uq$val))){
  for(i in which(is.na(dt_uq$val))){
    i = which(is.na(dt_uq$val))[10]
    temp <- get_vader(dt_uq$Letter.Text.Clean[i])[c('neg','pos','neu')]
    temp <- as.numeric(temp)
    temp['val'] <- temp['pos'] - temp['new']
    
    }
}

dt_uq <- left_join(dt_uq,test)

test = readRDS('comment_topography/comment_sentiments.rds')



sent_file <- 'comment_topography/comment_sentiments.rds'
if(!file.exists(sent_file)){
  tiles <- ntile(1:nrow(dt_uq),n = 1000)
  sent_all_dt <- data.table()
  for(t in unique(tiles)){
    print(t)
    vader_list = pblapply(dt_uq$Letter.Text.Clean[tiles==t],get_vader,neu_set = T,cl = 7)
    sent_dt = data.table(hashes = dt_uq$hashes[tiles==t],
                         neg = as.numeric((sapply(vader_list,function(x) x['neg']))),
                         pos = as.numeric((sapply(vader_list,function(x) x['pos']))),
                         neu = as.numeric((sapply(vader_list,function(x) x['neu']))))
    sent_all_dt <- rbind(sent_all_dt,sent_dt,use.names = T,fill = T)
  }
}

 names(vader_list) <- dt$UQID
# 
 sent_dt = data.table(UQID = meta$UQID,
                     neg = as.numeric((sapply(vader_list,function(x) x['neg']))),
                      pos = as.numeric((sapply(vader_list,function(x) x['pos']))),
                      neu = as.numeric((sapply(vader_list,function(x) x['neu']))))
sent_dt$val = sent_dt$pos - sent_dt$neg
 saveRDS(sent_dt,'comment_topography/comment_sentiments.rds')
}

sent_dt = readRDS('comment_topography/comment_sentiments.rds')
setnames(sent_dt,'val','net_positivity')
meta = left_join(meta,sent_dt)
#meta = meta[!is.na(net_positivity),]

meta[FORM==F&is.na(net_positivity),]
sent_dt[UQID==2076,]

zcta = tigris::zctas(year = 2018,class = 'sf')
zcta = zcta[zcta$ZCTA5CE10 %in% meta$Author.ZipCode,]



meta[,.N,by=.(Author.ZipCode,FORM)][!is.na(Author.ZipCode),]





for(p in unique(meta$Project.Number)){
  p = unique(meta$Project.Number)[1]
  meta_sub = left_join(meta[Project.Number==p,],sent_dt)
  count_and_sentiment_dt = meta_sub[,list(.N,mean(val,na.rm = T)),
                                    by=.(Author.ZipCode)]
  setnames(count_and_sentiment_dt,'Author.ZipCode','ZCTA5CE10')
  
  p1_zcta = zcta[zcta$ZCTA5CE10 %in% count_and_sentiment_dt$ZCTA5CE10,]
  p1_zcta$N = count_and_sentiment_dt$N[match(p1_zcta$ZCTA5CE10,count_and_sentiment_dt$ZCTA5CE10)]
  p1_zcta$Avg_Net_Sentiment = count_and_sentiment_dt$V2[match(p1_zcta$ZCTA5CE10,count_and_sentiment_dt$ZCTA5CE10)]
  
  
  



meta$FOREST_ID <- formatC(meta$FOREST_ID,width = 4,flag = '0')
admin_districts <- admin_districts[admin_districts$FORESTORGC %in% meta$FOREST_ID,]

ggplot() + geom_sf(data =admin_districts)

admin_districts <- st_transform(admin_districts,st_crs(zcta))
dist_mat = st_distance(zcta,admin_districts)
dist_mat <- as.matrix(dist_mat)
colnames(dist_mat) <- admin_districts$FORESTORGC
rownames(dist_mat) <- zcta$ZCTA5CE10

which_forest <- match(meta$FOREST_ID,colnames(dist_mat))
meta$km_to_forest <- ifelse(which_forest==1,dist_mat[,1],dist_mat[,2])/1e3

meta$net_positivity <- sent_dt$val[match(meta$UQID,sent_dt$UQID)]


g_sent = ggplot(meta,aes(x = sqrt(km_to_forest),y = net_positivity)) + 
  geom_point() + stat_smooth() + 
  scale_x_continuous('sqrt(km between zip code and forest)')+
  scale_y_continuous(name = 'net sentiment rating') +
  labs(caption = 'net sentiment = proportion positive - proportion negative\nexcludes neutral language')+
  theme_bw() + ggtitle('comment sentiment vs. distance to project','(non-form letters only)')+
ggsave(g_sent,filename = 'comment_topography/output/sentiment_vs_distance_non_form_letters.png',dpi = 300,units = 'in',width = 7,height = 7)

#table(is.na(meta$Author.ZipCode))
#table(meta$Author.ZipCode %in% zcta$ZCTA5CE10)
#table(meta$Author.ZipCode[!is.na(meta$Author.ZipCode)] %in% zcta$ZCTA5CE10)

library(tigris)
library(sf)
library(ggthemes)
states = tigris::states(cb = T,year = 2019,class = 'sf')

table(meta$FOREST_ID)
meta[is.na(FOREST_ID),]
meta$FOREST_ID %>% unique()
ggplot() + geom_sf(data = )






g1 = ggplot() + 
 # geom_sf(data = states,col = 'grey50',fill = NA) + 
  geom_sf(data = p1_zcta,aes(fill = N)) + 
  scale_color_viridis_c() + theme_map() + 
  ggtitle('# of comments')


g2 = ggplot() + 
  geom_sf(data = states,col = 'grey50',fill = NA) + 
  geom_sf(data = p1_zcta,aes(fill = Avg_Net_Sentiment)) + 
  scale_color_viridis_c() + theme_map() + 
  ggtitle('Average sentiment')


library(gridExtra)
ggsave(filename = paste0('comment_topography/output/figures/p',p,'sentiment_by_zipcode.png'),plot = grid.arrange(g1,g2,ncol = 1),height = 10,width = 10,dpi = 300, units = 'in')

temp = readRDS('comment_topography/scratch/case_comments_from_cara_w_zips.RDS')
chars = sapply(temp$Letter.Text,nchar)

nchar(temp[temp$Project.Number==45579 & temp$Letter.Number == 1031,]$Letter.Text)
which(nchar(dt$Letter.Text.Clean)>1e5)
get_vader(dt$Letter.Text.Clean[2930])
vd_list <- pblapply(dt_sub$Letter.Text.Clean,get_vader,cl = 6)



rbindlist(vd_list)


vad = vader_df(dt_sub$Letter.Text.Clean)

vad$word_scores
plot(vad$pos,vad$neg)


library(textmineR)
corp = quanteda::corpus(dt$Letter.Text.Clean)
#toks = quanteda::tokenize_word(corp)
# create a document term matrix 
dfm = dfm(x = corp)
meta$FORM <- NA
### now, we can practice on one project:



require(proxyC)
require(Matrix)
## Loading required package: Matrix
require(microbenchmark)
## Loading required package: microbenchmark
require(RcppParallel)
## Loading required package: magrittr
# Set number of threads
setThreadOptions(4)

for(p in pnums){
print(p)
  p = pnums[2]
dfm_sub = dfm[meta$Project.Number==p,]
trim = 5
dfm_sub = quanteda::dfm_trim(dfm_sub,min_termfreq = trim)
nt = dplyr::ntile(1:nrow(dfm_sub),n = round(nrow(dfm_sub)/10e3))
nt = sample(nt)

dfm_split <- lapply(sort(unique(nt)),function(x) {
  dfm_sub[nt==x,]})
sparse_dfm_list <- lapply(dfm_split,as, "CsparseMatrix")
cosign_sim_list <- lapply(sparse_dfm_list,simil,margin = 1,method = 'cosine',min_simil = 0.95)

row_exceeds_list <- lapply(cosign_sim_list,function(y) apply(y,1,function(x) sum(x>=0.95)>1))
form_names_list <- mapply(function(x,y) rownames(x[y,]), x = sparse_dfm_list,y = row_exceeds_list)
meta$FORM[as.numeric(str_extract(unlist(form_names_list),'[0-9]{1,}'))] <- 1
}

meta$FORM[is.na(meta$FORM)]<-0
form_summary <- meta[,list(round(mean(FORM),2),.N),by=.(Project.Number)]
names(form_summary) <- c('project #','% form letters','total comments')
form_summary
library(htmlTable)
htmlTable(form_summary)
fwrite(meta,'comment_topography/scratch/cleaned_comment_meta_form_desig.txt',sep = ',')


csim <- dfm_tfidf / sqrt(rowSums(dfm_tfidf * dfm_tfidf))
csim <- csim %*% t(csim)
cdist <- as.dist(1 - csim)


hc <- hclust(cdist, "ward.D")
clustering <- cutree(hc, 10)
plot(hc, main = "Hierarchical clustering of 100 NIH grant abstracts",
     ylab = "", xlab = "", yaxt = "n")
rect.hclust(hc, 10, border = "red")


# TF-IDF and cosine similarity
tfidf <- t_dtm_term_sort * tf_mat$idf
tfidf <- t(tfidf)

csim <- tfidf / sqrt(rowSums(tfidf * tfidf))
csim <- csim %*% t(csim)
sparsity(dfm)
cdist <- as.dist(1 - csim)

hc <- hclust(cdist, "ward.D")

clustering <- cutree(hc, 10)

plot(hc, main = "Hierarchical clustering of 100 NIH grant abstracts",
     ylab = "", xlab = "", yaxt = "n")

rect.hclust(hc, 10, border = "red")





