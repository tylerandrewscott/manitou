
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
dt = readRDS('comment_topography/input/cleaned_comment_text.RDS')
metad = readRDS('comment_topography/input/cleaned_comment_meta.RDS')

form_letter = fread('comment_topography/input/form_letter_designation.txt',sep = '\t')
form_letter$UQID <- as.character(form_letter$UQID)
sent_values = fread('comment_topography/input/sentiment_scores.txt',sep = '\t')
sent_values$UQID <- as.character(sent_values$UQID)
dist_values = fread('comment_topography/input/distance_to_project.txt',sep = '\t')
dist_values$UQID <- as.character(dist_values$UQID)

metad = left_join(metad,sent_values)
metad = left_join(metad,form_letter)
metad = left_join(metad,dist_values)
#table(is.na(metad$km_from_project))
#table(is.na(metad$km_from_project),is.na(metad$new_zip))
library(textreuse)

project.name.key = data.table(Project.Number = unique(metad$Project.Number),Project =
                                c("Westside Fire Recovery",
                                  "Stibnite Gold Mine",
                                  "Sage-grouse Amendments",
                                  "Alaska Roadless Rule"))

metad = left_join(metad,project.name.key)

library(ggthemes)

gg_density = ggplot() + 
  geom_density(data = metad[FORM==0,],aes(x = sentiment,col = Project,linetype = 'form'),trim = T) +
  geom_density(data = metad[FORM==1,],aes(x = sentiment,col = Project,linetype = 'unique'),trim = T) +
  scale_linetype_manual(values = c(2,1),labels = c('form letter','unique comment')) +
  theme_bw() + theme(legend.title = element_blank(),axis.text.y = element_blank(),legend.position = c(0.2,0.5)) +
  ggtitle('Distribution of sentiment by project and comment type')
ggsave(plot = gg_density,filename = 'comment_topography/output/sentiment_density_project_type.png',dpi = 300,units = 'in',width = 10,height = 10)

sentiment_boxplot = ggplot(metad) + 
  geom_boxplot(aes(y = Project,x = sentiment,col = as.factor(FORM))) +
  scale_color_colorblind(name = 'comment',labels=c('individual','form'))+
  theme_bw() + ggtitle('Sentiment by project and comment type') +
  scale_x_continuous(name = 'comment sentiment') + 
  theme(
    axis.text.y = element_text(size = 12,angle = 45),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_blank())

ggsave(plot = sentiment_boxplot,filename = 'comment_topography/output/sentiment_boxplot.png',width = 10,height = 7,units = 'in',dpi = 300)

sentiment_vs_length <- ggplot(metad) + 
  geom_point(aes(x = log(nchar),y = sentiment),pch = 21,alpha = 0.25) +
  theme_bw() + ggtitle('sentiment vs. comment length') +
  scale_x_continuous('ln(# characters)') + 
  scale_y_continuous('estimated sentiment')

ggsave(plot = sentiment_vs_length,filename = 'comment_topography/output/sentiment_vs_length.png',dpi= 300,units = 'in',height = 7,width = 7)

tab = metad[,list(.N,mean(FORM)),by=.(Project.Number)]
tab$Project.Name <- project.name.key$Project[match(tab$Project.Number,project.name.key$Project.Number)]

setnames(tab,c('N','V2'),c('# letters','% form letter'))
tab$`% form letter` <- round(tab$`% form letter`,2)
htmlTable::htmlTable(tab[,.(Project.Name,`# letters`,`% form letter`)],file = 'comment_topography/output/count_summary.html')

ggplot(data = metad[!is.na(FORM),]) + 
  geom_boxplot(aes(x = as.factor(FORM),y = km_from_project)) + 
  theme_bw() + ggtitle('Distance from project area') +
  scale_x_discrete(name = 'letter type',labels = c('unique','form')) +
  scale_y_continuous('km from project')

ggplot(data = metad[!is.na(FORM),]) + 
  geom_boxplot(aes(x = Project,y = km_from_project,col = as.factor(FORM))) + 
  theme_bw() + ggtitle('Distance from project area') +
 scale_colour_discrete(name = 'letter type',labels = c('unique','form')) +
  scale_y_continuous('km from project')

library(ggmaps)
library(ggthemes)
library(tigris)
states <- states(year = 2020,class = 'sf',cb = T)
zcta <- zctas(class = 'sf',year = 2010)
zcta <- st_transform(zcta,st_crs(albersNA))
metad[metad$new_zip==''] <- NA
metad$new_zip <- str_extract(metad$new_zip,'^[0-9]{5}')
projs <- unique(metad$Project.Number)

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
proj <- metad[Project.Number == project.name.key$Project.Number[p],]
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
  proj <- metad[Project.Number == project.name.key$Project.Number[p] & FORM==0,]
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
  proj <- metad[Project.Number == project.name.key$Project.Number[p] & FORM==1,]
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
  print(p)
  p = 1
  proj <- metad[Project.Number == project.name.key$Project.Number[p],]
  setnames(proj,'new_zip','ZCTA5CE10')
  pzip <- zcta[zcta$ZCTA5CE10 %in% proj$new_zip,]
  zcta_temp <- zcta[zcta$ZCTA5CE10 %in% proj$ZCTA5CE10,]
  admin_temp <- admin_districts[admin_districts$FOREST_ID %in% project.name.key$FORESTS[[which(project.name.key$Project.Number[p]==project.name.key$Project.Number)]],]
  if(nrow(admin_temp)==1){
    dist_list <- pblapply(1:nrow(zcta_temp),function(x) st_distance(zcta_temp[x,],admin_temp),cl = 7)
  }
  if(nrow(admin_temp)>1){
    which_nearest <- st_nearest_feature(zcta_temp,admin_temp)
  
  
  pbmapply(function(x,y) st_distance(x,y),x = zcta_temp,y = admin_temp[which_nearest])
  dist = st_distance(zcta_temp,which_nearest)
  
  #temp_combo = st_union(admin_temp)
  
  
  dist_list = pblapply(1:nrow(zcta_temp),function(x) st_distance(zcta_temp[x,],temp_combo),cl = 7)
  #dist_mat <- st_distance(zcta_temp,temp_combo)
  zcta_temp$km_from_project <- unlist(dist_list)/1000
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


metad$GROUP[metad$GROUP==''] <- NA
metad[Project=='Stibnite Gold Mine',.N,by=.(GROUP)][order(-N),]
cumSum <- metad[,.N,by=.(GROUP,Project)][order(Project,N),][!is.na(GROUP),]
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
dt_nf = dt[metad$FORM==0,]
metad_nf = metad[FORM==0,]

p = project.name.key$Project.Number[1]
dt_nf <- dt_nf[UQID %in%  metad_nf$UQID[metad_nf$Project.Number==p]]
metad_nf <- metad_nf[Project.Number == p,]
metad_nf$sent_group <- ifelse(metad_nf$sentiment<(-0.2),'negative',ifelse(metad_nf$sentiment>(0.2),'positive','neutral'))

# 
# 
# x <- udpipe_annotate(ud_model, x = tolower(dt_nf$Letter.Text.Clean),doc_id = metad_nf$UQID)
# x <- as.data.frame(x)
# sentence_sentiment <- sentimentr::sentiment(x$sentence)
# x$sentiment_group <- metad_nf$sent_group[match(x$doc_id,metad_nf$UQID)]
# i = 'negative'
# nmin = 10; nr = 0
# rake <- keywords_rake(x = x[x$sentiment_group==i,], 
#                       term = "token", group = c('sentiment_group'),
#                       relevant = x$upos[x$sentiment_group==i] %in% c("NOUN", "ADJ",'PROPN'),
#                       ngram_max = 3,n_min = nmin)  %>%
#   arrange(-rake) %>%  mutate(sentiment_group = i) %>% head(.,3) 
# rake
# 
# #x <- x[x$upos !='PUNCT',]
# #x <- x[!x$token %in% stopwords::stopwords(),]
# cluster_keys <- lapply(sort(unique(M_dt2$cluster)),function(i) {
#   print(i)
#   nr <- 0;nmin <- 10
#   while(nr == 0){
#     rake <- keywords_rake(x = x[x$cluster==i,], 
#                           term = "token", group = c('cluster'),
#                           relevant = x$upos[x$cluster==i] %in% c("NOUN", "ADJ",'PROPN'),
#                           ngram_max = 3,n_min = nmin)  %>%
#       arrange(-rake) %>%  mutate(cluster = i) %>% head(.,3) 
#     # filter(!keyword %in% c('network governance','governance networks','policy networks','policy network','social networks','network analysis',
#     #                   'natural resource','environmental management','environmental policy','environmental decision')) %>%
#     #  filter(!keyword %in% c('policy networks','policy network','social network analysis')) %>% 
#     #  head(.,3) %>% 
#     nr = nrow(rake)
#     nmin = nmin -1
# 
# 


