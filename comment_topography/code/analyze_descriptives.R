
packages = c('data.table','stringr','tidyverse','sf','lwgeom','tigris','textreuse','ggthemes')
not_installed = packages[!packages %in% installed.packages()[,'Package']]
if(length(not_installed)>0){lapply(not_installed,install.packages)}
lapply(packages,require,character.only = T)

searchK <- F
hardK <- 60
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
sent = fread('comment_topography/input/google_sentiment_scores_by_group.csv')

metad <- cbind(metad,form_letter[match(metad$uq,form_letter$uq),.(FORM,GROUP,group_size)])
metad <- cbind(metad,sent[match(metad$GROUP,sent$GROUP),.(score,magnitude)])

dist = fread('comment_topography/input/distance_zcta_to_forest.txt',sep = '\t')
dist$FOREST_ID <- formatC(dist$FOREST_ID,width = 4,flag = '0')
dist$ZCTA5CE10 <- formatC(dist$ZCTA5CE10,width = 5,flag = '0')

metad$Use.Zipcode <- str_sub(metad$Use.Zipcode,1,5)
zcta_guide <- fread('https://raw.githubusercontent.com/censusreporter/acs-aggregate/master/crosswalks/zip_to_zcta/geonames_us_zips.csv')

zcta_out_of_states <- zcta_guide[zcta_guide$stusab=='',]
zcta_out_of_states$zip <- formatC(zcta_out_of_states$zip ,width = 5,flag = '0')
zip_to_zcta <- fread('https://raw.githubusercontent.com/censusreporter/acs-aggregate/master/crosswalks/zip_to_zcta/zip_zcta_xref.csv')
zip_to_zcta$zip_code <- formatC(x = zip_to_zcta$zip_code,width = 5,flag = '0')
zip_to_zcta <- zip_to_zcta[!zip_code %in%zcta_out_of_states$zip,]
zip_to_zcta$zcta <- formatC(x = zip_to_zcta$zcta,width = 5,flag = '0')


table(metad$group_size[!duplicated(metad$GROUP)])


#metad$ZCTA <- metad$new_zip %in% zcta$ZCTA5CE10

metad$ZCTA<-zip_to_zcta$zcta[match(metad$Use.Zipcode,as.character(zip_to_zcta$zip_code))]

project.name.key = data.table(Project.Number = unique(metad$Project.Number),Project =
                                c("Westside Fire Recovery",
                                  "Stibnite Gold Mine",
                                  "Sage-grouse Amendments",
                                  "Alaska Roadless Rule"))

metad$Project <- project.name.key$Project[match(metad$Project.Number,project.name.key$Project.Number)]
td = tempdir()
#albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
library(httr)
library(sf)
library(tmap)

library(sf)
library(leaflet)
library(geojsonsf)
library(dplyr)
library(urltools)

admin_districts <- fread("comment_topography/input/Forest_Administrative_Boundaries_(Feature_Layer).csv")
admin_districts$FOREST_ID <- paste0(admin_districts$REGION,admin_districts$FORESTNUMB)
project.name.key$FORESTS <- list('0505','0412',c(paste0('04',admin_districts$FORESTNUMB[admin_districts$REGION=='04'])),c(paste0('10',admin_districts$FORESTNUMB[admin_districts$REGION=='10'])))

elig_forests <- project.name.key$FORESTS[match(metad$Project.Number,project.name.key$Project.Number)]

#dist$FOREST_ID <- formatC(dist$FOREST_ID,width = 4,flag = '0')
dist$REGION <- str_sub(dist$FOREST_ID,1,2)

dm <- lapply(list('0505','0412','10','04'),function(x) {
  print(x)
  if(nchar(x)==4){
    print(x)
    dist[FOREST_ID == x,] %>% select(ZCTA5CE10,km_to_forest)}
  else{
    dist[REGION == x,][,min(km_to_forest,na.rm = T),by=.(ZCTA5CE10)] %>%
      rename(km_to_forest = V1) 
  }
}
)

temp <- rbindlist(dm)
temp$Project.Number <- rep(project.name.key$Project.Number,each = nrow(temp)/4)
metad$km_to_forest <- temp$km_to_forest[match(paste(metad$Project.Number,metad$ZCTA),paste(temp$Project.Number,temp$ZCTA5CE10))]

metad$Project.Name[grepl('Amendments',metad$Project.Name)] <- 'Sage-grouse amendments'


md <- metad[,.N,by=.(GROUP,Project,score,magnitude)]
md$FM <- (md$N>1)+0

#md$Project.Name[grepl('Amendments',md$Project.Name)] <- 'Sage-grouse amendments'
nms <- c('# obs.','prop. form','prop. geotagged')

sm <- metad[,list(.N,mean(FORM>0),mean(!is.na(km_to_forest))),by=.(Project.Name)]
setnames(sm,names(sm)[-1],nms)

htmlTable::htmlTable(sm %>% mutate_if(is.numeric,round,2),file = 'comment_topography/output/tables/basic_descriptives.html')


bad_zips <- metad$Use.Zipcode[is.na(metad$km_to_forest)&!is.na(metad$Use.Zipcode)]

zcta_acsDP03 <- fread(skip = 1,file = 'comment_topography/scratch/ACSDP5Y2011.DP03_data_with_overlays_2022-07-18T134956.csv')
zcta_acsDP03 <- zcta_acsDP03[,!duplicated(colnames(zcta_acsDP03)),with = F]
zcta_acsDP05 <- fread(skip = 1,file = 'comment_topography/scratch/ACSDP5Y2011.DP05_data_with_overlays_2022-07-18T134108.csv')
zcta_acsDP05 <- zcta_acsDP05[,!duplicated(names(zcta_acsDP05)),with = F]
zcta_acs <- merge(zcta_acsDP03,zcta_acsDP05)
zcta_acs$ZCTA5CE10 <- str_extract(zcta_acs$`Geographic Area Name`,'[0-9]{5}')

vals <- c(
  "Estimate!!SEX AND AGE!!Total population" ,
  "Estimate!!INCOME AND BENEFITS (IN 2011 INFLATION-ADJUSTED DOLLARS)!!Median household income (dollars)",
  "Percent!!SEX AND AGE!!62 years and over" ,
  "Percent!!RACE!!White")
renm <- c('total_population','median_household_income','perc_over_62','perc_white')
setnames(zcta_acs,vals,renm)

zcta_acs <- zcta_acs[,c('ZCTA5CE10',renm),with = F]
(gdist <- ggplot(metad[!is.na(km_to_forest),],aes(x = Project,colour = FORM>0,y = km_to_forest+1)) + 
    geom_boxplot(varwidth = T,alpha = 0.2) + theme_bw() + coord_flip() + 
    scale_color_colorblind(labels= c('unique','form'),
                           name = 'comment type') + 
    ggtitle('How far away are letters sent from?') +
    labs(caption = '*barwidth reflects relative sample size')+
    scale_y_sqrt(name = 'distance (km) from project',breaks = c(10,100,1000,2500,5000))+
    theme(axis.text.y = element_text(angle = 45),legend.position = c(0.8,0.1)))


ggsave(plot = gdist,filename = 'comment_topography/output/figures/distance_from_project.png',dpi = 300,units = 'in',height = 6,width = 6)

sga_plot <- metad[Project == 'Sage-grouse Amendments'&!is.na(ZCTA),list(.N,mean(FORM>0)),by=.(ZCTA)]
setnames(sga_plot,names(sga_plot),c('ZCTA5CE10','# observed','prop. form'))

zcta_sga <- left_join(zcta,sga_plot)

###   ### NOTE AT SOME POINT MAKE AK AND HI AN INSET 
drop_states <- c('AK','PR','HI','AS','MP','GU','VI')
filter_state <- states[!states$STUSPS%in%drop_states,]
zcta_sga_filter <- st_intersects(zcta_sga,filter_state)

ggplot() + geom_sf(data = filter_state,fill = NA) +
  geom_sf(data = zcta_sga[sapply(zcta_sga_filter,length)>0,],aes(fill = `# observed`))+
  scale_fill_viridis_c()


metad <- metad[,group_median_km_to_forest:=median(km_to_forest,na.rm = T),by=.(GROUP)]
metad2 <- metad[ !is.na(km_to_forest) & {!duplicated(GROUP)} & {nchar(dt$Letter.Text)<100000},]
dt2 <- dt[!is.na(metad$km_to_forest) & {!duplicated(metad$GROUP)} & {nchar(Letter.Text)<100000},]

tp <- stm::textProcessor(documents = dt2$Letter.Text,
                            metadata = as.data.frame(metad2),lowercase = T,
                         removestopwords = T,
                            removepunctuation = T,removenumbers = T)
library(quanteda)
# library(spacyr)
# docs <- spacyr::spacy_parse(dt2$Letter.Text)
# entex <- entity_consolidate(docs)
# entex <- data.table(entex)
# table(entex$entity_type)
# entcount <- entex[entex$entity_type %in% c('LOC','ORG','PERSON'),][,.N,by=.(token)][order(-N),]
# ent200 <- entcount[N>200,]
# test <- str_replace_all(dt2$Letter.Text[1],'\\s','_')


corp = corpus(dt2$Letter.Text,docnames = metad2$GROUP,docvars = metad2,unique_docnames = F)
toks = tokens(corp,remove_symbols = T,split_hyphens = F,padding = F,remove_url = T)
toks = tokens_select(toks,pattern = stopwords("en"),selection = 'remove')
library(rvest)
gloss <- 'https://en.wikipedia.org/wiki/Glossary_of_environmental_science'
gloss_words <- read_html(gloss) %>% html_nodes('ul b a') %>% html_text(trim = T)
ngram2_gloss <- gloss_words[str_count(gloss_words,'\\s')==1]
ngram2_gloss <- tolower(ngram2_gloss[!grepl('[0-9]',ngram2_gloss)])

epa_gloss <- 'https://19january2017snapshot.epa.gov/climatechange/glossary-climate-change-terms_.html'
epa_words <- read_html(epa_gloss) %>% html_nodes('strong') %>% html_text(trim = T)
epa_words <- str_remove(epa_words,'\\s\\(.*')
ngram2_epa <- epa_words[str_count(epa_words,'\\s')==1]
ngram2_epa <- tolower(ngram2_epa)

compound_words = c("greenhouse gas*",'climate change*','global warming','carbon emission*','climate impact*','ocean acidification*',
                   'alternative energy','anthropogenic emissions','carbon dioxide','extreme weather','storm surge',
                   'adaptive capacit*','adaptation cost*','renewable energy','sea level rise',
                   'extreme heat','atmospheric river','paris agreement',
                   'renewable energy','solar energy','wind energy','geothermal energy',
                   'fossil fuel*','natural gas','climate mitigat*','climate adapt*',
                   'severe weather',
                   'sealevel rise','air quality','water quality','invasive species','land use','marine heat wave*',
                   'environmental impact statement*','flood control','heat wave*','atmospheric river*',
                   'Midas Gold*','Roadless_Rule','Goosenest Ranger District','Klamath National Forest','Stibnite Gold Project',
                   'Vicki Christiansen','Salmon River','Tongass National Forest','Forest Service','Environmental Impact Statement',
                   'Southeast Alaska','Chugach National Forest','Department of Agriculture','Federal Government')

compound_words <- unique(do.call(c,list(compound_words,ngram2_gloss,ngram2_epa)))

toks = tokens_select(toks,min_nchar = 3,max_nchar = max(nchar(compound_words)))
toks = tokens_compound(toks,pattern = phrase(compound_words))

qdfm = dfm(toks)
qdfm <- dfm_trim(qdfm,min_docfreq = 5) 

qdfm <- dfm_trim(qdfm, max_docfreq = 0.3,docfreq_type = 'prop')
qdfm <- qdfm[ntoken(qdfm)>0,]
qdfm <- qdfm[,!grepl('[x]{3,}',qdfm@Dimnames$features)]
qdfm_stem = dfm_wordstem(qdfm)
qdfm_stem <- qdfm_stem[ntoken(qdfm_stem)>0,]

dfm2stm <- convert(qdfm_stem, to = "stm")

dfm2stm$meta$IS_FORM <- dfm2stm$meta$FORM>0
if(searchK){
#use k = 0 to automate guess to understand range of k
model.search <- stm(dfm2stm$documents, dfm2stm$vocab, K = 0, data = dfm2stm$meta,
                    prevalence = ~Project + IS_FORM, content = ~Project,
                    init.type = "Spectral",verbose = T,#ngroups = 5,
                    seed = 24,max.em.its = 40,emtol = 0.0001) 
Kval <- model.search$settings$call$K
}else{Kval<-hardK}

prev_form <- ~Project + s(km_to_forest) + IS_FORM  + score + magnitude
model_nocontent <- stm(dfm2stm$documents, dfm2stm$vocab, K = hardK, data = dfm2stm$meta,
            prevalence = prev_form,
            init.type = "Spectral",verbose = T,#ngroups = 5,
            seed = 24,max.em.its = 40,emtol = 0.0001)
if(!model_nocontent$convergence$converged){
  model_nocontent <- stm(dfm2stm$documents, dfm2stm$vocab, K = hardK, data = dfm2stm$meta,model = model_nocontent)
}

model<- stm(dfm2stm$documents, dfm2stm$vocab, K = hardK, data = dfm2stm$meta,
            prevalence =prev_form, content = ~Project,
            init.type = "Spectral",verbose = T,#ngroups = 5,
            seed = 24,max.em.its = 50,emtol = 0.0001,reportevery=10)

#econ
etop <- findTopic(model,list('economi','job','jobs','labor','econom','tourism','industri','work','devel','incom','invest','employ','poverty'),
          n = 20,
          type = c('frex'),
          verbose = TRUE)
tlab <- labelTopics(model,topics = etop,n = 20)

econ_thought <- stm::findThoughts(model = model,
                                  texts = dt2$Letter.Text[dt2$uq %in% dfm2stm$meta$uq],
                                  topics = etop,n = 1)
econkeys <- paste0('Topic #',etop,': ',(paste(c(tlab$topics),collapse = ', ')))
showtops <- data.table('econ'=c(econkeys,paste0('Topic #',etop,': ',econ_thought$docs[1])))

 
envtop <- findTopic(model,list('habitat','biodiv','health','eco-system'),
                  n = 20,
                  type = c('frex'),
                  verbose = TRUE)
env_labels <- labelTopics(model,n = 10,topics = envtop,frexweight = .5)
env_thought <- stm::findThoughts(model = model,
                                  texts = dt2$Letter.Text[dt2$uq %in% dfm2stm$meta$uq],
                                  topics = envtop,n = 1)

env_keys <- paste(paste0('topic #',envtop,': '),
as.character(apply(env_labels$topics,1,paste,collapse = ', ')))

env_thought_blurbs <- paste(paste0('topic #',envtop,': '),env_thought$docs)

showtops_econ <- data.table('econ'=c(paste(econkeys,collapse = '\n'),paste(econ_thought$docs, collapse = '\n')))
showtops_env <- data.table('env'=c(paste(env_keys,collapse = '\n'),paste(env_thought_blurbs,collapse = '\n')))

htmlTable::htmlTable(cbind(showtops_econ,showtops_env),file = 'comment_topography/output/topic_summary.html',
                     rowlabels = T,
                     rnames = c('keywords','modal comment'),
                     header = c('Economic focus','Env. focus'))

dfm2stm$meta$IS_FORM <- dfm2stm$meta$IS_FORM + 0
#eff_est <- estimateEffect(as.formula(paste('etop',paste(as.character(prev_form),collapse = ' '))), model,dfm2stm$meta)
distance_form <- estimateEffect(etop ~ s(km_to_forest) + IS_FORM + IS_FORM:s(km_to_forest), model,dfm2stm$meta)
#distance_form <- estimateEffect(etop ~ s(sqrt(km_to_forest)), model,dfm2stm$meta)
plot(distance_form, covariate = 'km_to_forest',
     moderator.value = 0,moderator = 'IS_FORM',
     method="continuous", topics=etop, model=model, printlegend=FALSE, xaxt="n", 
     xlab="km")

spline_list <- lapply(c(0,1),function(x) {plot(distance_form, covariate = 'km_to_forest',
     moderator.value = x,moderator = 'IS_FORM',
     method="continuous", topics=etop, model=model, printlegend=FALSE, xaxt="n", 
     xlab="km")})

pal <- colorblind_pal()(2)
econ_ci <- rbindlist(list(melt(spline_list[[1]]$ci) %>% mutate(FORM = 0),
                          melt(spline_list[[2]]$ci)%>% mutate(FORM = 1)))

econ_ci <- econ_ci %>% mutate(km = spline_list[[1]]$x[Var2]) %>%
  dplyr::select(-L1,Var2) 

econ_ci <- dcast(econ_ci,FORM + km ~ Var1) %>% mutate(mean = unlist(c(spline_list[[1]]$means,spline_list[[2]]$means)))

(g_econ <- ggplot()+ theme_bw() +
  scale_x_continuous(name = 'km from forest*')+
  scale_y_continuous(name = 'spline for econ. topic proportion\n (mean + 95% CI)')+
  theme(legend.position = c(0.2,0.2))+
  scale_color_manual(name = 'letter type',values = pal,labels = c('unique','form')) + 
  geom_ribbon(lty = 2,fill = NA,data = econ_ci[FORM==0,],aes(x = km,ymin = `2.5%`,ymax = `97.5%`,colour = as.factor(FORM)))+
  geom_path(data = econ_ci[FORM==0,],aes(x = km,y = mean,colour = as.factor(FORM)),lty = 1)) +
  ggtitle('focus on econ. topic given distance from forest')

ggsave(plot = g_econ,filename = 'comment_topography/output/figures/econ_vs_distance.png',dpi = 300,units = 'in',width = 6,height = 4.5)

distance_form_env <- estimateEffect(envtop ~ s(km_to_forest) + IS_FORM + IS_FORM:s(km_to_forest), model,dfm2stm$meta)

spline_env <- plot(distance_form_env, covariate = 'km_to_forest',
                                               moderator.value = 0,moderator = 'IS_FORM',
                                               method="continuous", topics=envtop, model=model, printlegend=FALSE, xaxt="n", 
                                               xlab="km")

env_ci <- rbindlist(lapply(seq_along(spline_env$topics),function(i){
data.table(km = spline_env$x,spline_env$topics[i],spline_env$means[[i]],t(spline_env$ci[[i]]))})
)
setnames(env_ci,c('V2','V3'),c('topic','mean'))

(g_env <- ggplot()+ theme_bw() +
  scale_x_continuous(name = 'km from forest*')+
  scale_y_continuous(name = 'spline for env. topic proportion\n (mean + 95% CI)')+
  theme(legend.position = c(0.2,0.7),legend.background = element_rect(fill = alpha('white',.5)))+
  geom_path(data=env_ci,aes(col = as.factor(topic),x = km,y = mean))+
  geom_path(data=env_ci,lty = 2,aes(col = as.factor(topic),x = km,y = `2.5%`))+
  geom_path(data=env_ci,lty = 2,aes(col = as.factor(topic),x = km,y = `97.5%`))+
  scale_color_colorblind(name = 'enviro. topic',labels = paste0('Topic #',unique(env_ci$topic))) + 
    ggtitle('focus on enviro. topics given distance from forest')+
    NULL)

ggsave(plot = g_env,filename = 'comment_topography/output/figures/envior_vs_distance.png',dpi = 300,units = 'in',width = 6,height = 4)



metad$ZCTA5CE10<-metad$ZCTA
metad2 <- merge(metad,zcta_acs)

metad2$is_form <- ifelse(metad2$FORM>0,'form letter','non-form letter')
distance_scatter <- ggplot(metad2[!is.na(km_to_forest),]) + theme_bw() + 
  facet_wrap(~is_form) + 
  scale_y_continuous(name = 'med. household income')+
  scale_x_continuous(name = 'ln(distance (km)) to project',breaks = log(c(1,10,100,1000)),
                     labels = paste0(c(1,10,100,1000),'km'))+
  scale_color_colorblind(name= 'project') + 
  geom_point(pch = 21,alpha = 0.3,
             aes(x = log(km_to_forest+1),y = as.numeric(median_household_income),col = Project))+
  ggtitle('Commenter address vs. ZCTA med. household income') + 
  theme(legend.title = element_blank(),legend.position = c(0.8,0.8),legend.background = element_rect(fill = alpha('white',.4)))

ggsave(plot = distance_scatter,filename = 'comment_topography/output/figures/distance_vs_income.png',dpi = 300,units = 'in',height =4,width = 6.5)

temp <- metad2[!is.na(km_to_forest)&FORM==0,]
table(temp$median_household_income>150000,temp$category)
temp <- temp %>% mutate(category=cut(km_to_forest, breaks=c(-Inf,25, 100, 500,Inf), 
                             labels=c("0-25km","25-100km","100-500km","500+km")))
temp$median_household_income<-as.numeric(temp$median_household_income)
library(ggridges)
temp$median_household_income_trunc150 <- ifelse(temp$median_household_income>150000,150000,temp$median_household_income)

ggplot(d, aes(`Min Temperature [F]`, Month, group = Month, height = ..density..)) +
  geom_density_ridges(stat = "density", trim = TRUE)

(g_income <- ggplot(temp, aes(x = median_household_income,
                              y = category,height = ..density..)) +
  geom_density_ridges(trim = T,scale = 2.5,panel_scaling = F,alpha = 0.5,stat = 'density',trim = T) +
  ggtitle('median income by distance from project')+
  scale_y_discrete(name = 'ZCTA distance to project',expand = c(0.01, 0)) +
  scale_x_continuous(breaks = 1000*c(10,50,100,150),
                     labels=paste0('$',c(10,50,100,150),'k'),
    limits = c(NA,150000),name = 'median household income (by ZCTA)',expand = c(0.01, 0)) +
  theme_ridges() )

ggsave(plot = g_income,filename = 'comment_topography/output/figures/distribution_income_distance.png',dpi = 300,units = 'in',width = 6,height = 4.5)
ggplot(temp,aes(x = as.numeric(median_household_income,
             y = category,group = category))) 
  
geom_density_ridges()


  geom_density_ridges(aes()))
?geom_density_ridges
distance_scatter2 <- ggplot() + theme_bw() + 
  facet_wrap(~is_form) + 
  scale_y_continuous(name = 'med. household income')+
  scale_x_continuous(name = 'ln(distance (km)) to project') + 
                     #breaks = log(c(1,10,100,1000)),
                     #labels = paste0(c(1,10,100,1000),'km'))+
  scale_color_colorblind(name= 'project') + 
  geom_point(pch = 21,alpha = 0.3,
             aes(x = km_to_forest,y = as.numeric(median_household_income),col = Project))+
  ggtitle('Commenter address vs. ZCTA med. household income') + 
  theme(legend.title = element_blank(),legend.position = c(0.8,0.8),legend.background = element_rect(fill = alpha('white',.4)))

ggsave(plot = distance_scatter2,filename = 'comment_topography/output/figures/fardistance_vs_income.png',dpi = 300,units = 'in',height =4,width = 6.5)



  
  geom_path(lty = 2,data = as.data.table(spline_list2[[1]]$means[[1]])%>%mutate(x = 1:nrow(.)),aes(col = pal[1],y = V1,x = spline_list2[[1]]$means))+
  #geom_path(data=melt(spline_list2[[2]]$ci[[1]]),aes(x = Var2,y = value,group = Var1,col = pal[2]))+
  #geom_path(lty = 2,data = as.data.table(spline_list2[[2]]$means[[1]])%>%mutate(x = 1:nrow(.)),aes(col = pal[2],y = V1,x = x)) +
  labs(caption = '*for form letter,\\nmedian distance for letter group') +
  ggtitle('focus on env. topic (#9)')


g2 <- ggplot()+ theme_bw() +
  scale_x_continuous(name = 'km from forest*')+
  scale_y_continuous(name = 'spline for env. topic (#49) proportion\n (mean + 95% CI)')+
  theme(legend.position = c(0.2,0.2))+
  scale_color_manual(name = 'letter type',values = pal,labels = c('unique','form')) + 
  geom_path(data=melt(spline_list2[[1]]$ci[[2]]),aes(col = pal[1],x = Var2,y = value,group = Var1))+
  #geom_path(lty = 2,data = as.data.table(spline_list2[[1]]$means[[2]])%>%mutate(x = 1:nrow(.)),aes(col = pal[1],y = V1,x = x))+
  #geom_path(data=melt(spline_list2[[2]]$ci[[2]]),aes(x = Var2,y = value,group = Var1,col = pal[2]))+
  geom_path(lty = 2,data = as.data.table(spline_list2[[2]]$means[[2]])%>%mutate(x = 1:nrow(.)),aes(col = pal[2],y = V1,x = x)) +
  #labs(caption = '*for form letter,\\nmedian distance for letter group') +
  ggtitle('focus on env. topic (#49)')


library(gridExtra)
grid.arrange(g1,g2,ncol = 2)

test$ci
monthseq <- seq(from=as.Date("2008-01-01"), to=as.Date("2008-12-01"), by="month")
monthnames <- months(monthseq)
axis(1, at=as.numeric(monthseq)-min(as.numeric(monthseq)), labels=monthnames)



plot(distance_form, "s(sqrt(km_to_forest))1", model = model, method="pointestimate")
summary(distance_form)
plot(distance_form,topics = 53)
summary(distance_form)

plot(prep, "treatment", method="pointestimate",
     cov.value1=1, cov.value2=0, xlim=c(-1,1), moderator="binaryvar", moderator.value=1)
plot(prep, "treatment", method="pointestimate",
     cov.value1=1, cov.value2=0, xlim=c(-1,1), moderator="binaryvar",
     moderator.value=0)
etop


distance_form$varlist



parms <- summary(distance_form)
pdt <- data.table(parms$tables[[1]])
pdt$coef <- rownames(parms$tables[[1]])
interact <- data.table(km = rep(seq(0,3000,10),2))
interact$FORM <- rep(c(0,1),each = nrow(interact)/2)
interact$beta1 <- pdt$Estimate[pdt$coef=='sqrt(km_to_forest)']
interact$beta2 <- pdt$Estimate[pdt$coef=='sqrt(km_to_forest):IS_FORMTRUE']
interact$alpha <- pdt$Estimate[pdt$coef=='(Intercept)']
interact$coef_est <- interact$alpha + sqrt(interact$km) * interact$beta1 + sqrt(interact$km) * interact$FORM * interact$beta2

ggplot(interact,aes(x = km,y = coef_est,group = FORM,
                    col = as.factor(FORM))) + 
  scale_x_continuous(name = 'km*')+theme_bw() + 
  theme(legend.position = c(0.8,0.4),
        legend.background = element_rect(fill = alpha('white',.5)))+
  geom_path() + scale_color_colorblind(name = 'form letter?',labels = c('no','yes')) +
  labs(caption = '*for form letter,\\nmedian distance for letter group')
parms <- distance_form$parameters

str(parms)
distance_formparameters
summary(model)
plot(distance_form)
?estimateEffect
prev_form
summary(distance_uq)
summary(distance_form)
?estimateEffect
(distance_form)

estimateEffect(c(),stmobj = model)









stm(documents)
metad2_form <- metad2[FORM>0,]
dt2_form <- dt2[metad2$FORM>0,]

metad2_uq <- metad2[FORM==0,]
dt2_uq <- dt2[metad2$FORM==0,]

metad2_form[,.N,by=.(GROUP)]

metad[uq %in% test,]
test <- metad[GROUP=='54511_23549']$uq

metad[Organization=='Idaho State Senate']
metad[,.N,by=.(Organization)][order(-N),]
t1 <- dt$Letter.Text[dt$uq %in% test][1]
t2 <- dt$Letter.Text[dt$uq %in% test][2]

library(textreuse)
align_local(t1,t2)
library(stm)
tp_uq <- stm::textProcessor(documents = dt2_uq$Letter.Text,
            metadata = as.data.frame(metad2_uq),lowercase = T,removestopwords = T,
            removepunctuation = T,removenumbers = T)
tp_uq


tp_fm <- stm::textProcessor(documents = dt2_form$Letter.Text,
                            metadata = metad2_form,lowercase = T,removestopwords = T,removepunctuation = T,removenumbers = T)



library(usmap)
library(ggplot2)
install.packages('choroplethrZip')
                 
                 zip_choropleth(df_pop_zip,
                                title      = "2012 ZCTA Population Estimates",
                                num_colors = 7,
                                legend     = "Population")
plot_usmap(data = statepop, values = "pop_2015", color = "red") + 
  scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
  theme(legend.position = "right")
sentiment_scatter<-ggplot(md,aes(y = Project.Name,
              x = score,colour = as.factor(FM))) + 
  #geom_point() + 
  geom_boxplot(aes(weight = N),alpha = .8,notch = F,outlier.alpha = 0)  +
scale_color_brewer(type = 'qual',palette = 2,name = 'letter',labels=c('not form','form')) +
  
  scale_alpha_manual(values = c(0.4,0.75),name='letter',labels = c('not form','form'))+
  geom_point(aes(alpha = as.factor(FM),size=N),
             pch = 21,position = position_jitterdodge(dodge.width = 0.5,jitter.height = 0.01)) + 
   scale_size_area(breaks = c(1,10,1000,10000,50000),max_size = 10,name = '# in group',n.breaks = 4) + theme_bw() +
  ggtitle('sentiment by project and letter group')+
  xlab('negative <- sentiment -> positive') + 
  theme(axis.text.y = element_text(angle = 40)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ggplot2::annotate("text", x = 0.5,y = .55, label = c("*boxplot quantiles weighted\nby group size"))


ggsave(plot =sentiment_scatter,filename = 'comment_topography/output/figures/sentiment_scatter.png',width = 10,height = 10,units = 'in',dpi = 400)



observed_locs <- ggplot(metad) + facet_wrap(~Project.Name,scales = 'free_y') + 
  geom_bar(aes(x = score,fill = !is.na(km_to_forest)),position = 'dodge') + #,colour = !is.na(km_to_forest))) + 
  scale_fill_colorblind(name = 'observed\nlocation?',labels = c('no','yes')) + 
  scale_x_continuous(name = 'sentiment score')  + 
  scale_y_continuous(name = '# comments')+
  theme_bw() + ggtitle('comparing sentiment of observed/unobserved locations')+
  theme(legend.position = c(0.9,0.2))
ggsave(plot = observed_locs,filename = 'comment_topography/output/figures/observed_unobserved_location_and_sentiment.png',units = 'in',dpi = 300,width = 6, height = 6)


library(textreuse)
library(ggthemes)

metad$st[metad$magnitude>3 & metad$score > 0.3] <- 'clearly '
metad$magnitude>3 & metad$score < -0.3
metad$magnitude>3 & metad$score <= 0.3 & metad$score >=-0.3
metad$magnitude<3 & metad$score <= 0.3 & metad$score >=-0.3

summary(metad$magnitude)
ggplot(data = metad) + geom_boxplot(aes(y = score,x = Project.Name,colour = as.factor(FORM>0)))

gg_density = ggplot() + facet_wrap(~FORM>0) + 
  geom_density(data = metad[FORM==0,],aes(x = score,col = Project.Name,linetype = 'unique'),trim = T) +
  geom_density(data = metad[FORM>0,],aes(x = score,col = Project.Name,linetype = 'form'),trim = T) +
  scale_linetype_manual(values = c(2,1),labels = c('form letter','unique comment')) +
  theme_bw() + theme(legend.position = 'bottom')+#theme(legend.title = element_blank(),axis.text.y = element_blank(),legend.position = c(0.2,0.5)) +
  ggtitle('Distribution of sentiment by project and comment type')
ggsave(plot = gg_density,filename = 'comment_topography/output/sentiment_density_project_type.png',dpi = 300,units = 'in',width = 10,height = 10)

metad_g <- metad[,.N,by=.(GROUP,score,magnitude,Project,FORM)]

uqid ='13675'
group = '45579_249'
metad[GROUP=='45579_249']
metad[UQID %in% c('2249','2801')]
dt[dt$UQID %in% metad$UQID[metad$GROUP=='45579_51'],]
metad_g[FORM==1,][N==1,]
(sentiment_boxplot = ggplot(metad_g) + 
  geom_boxplot(aes(y = Project,x = score,col = as.factor(FORM))) +
  geom_point(aes(y = Project,x = score,col = as.factor(FORM),alpha = as.factor(FORM))) +
  #geom_point(aes(y = Project,x = score2,col = as.factor(FORM))) +
  scale_color_colorblind(name = 'comment',labels=c('individual','form'))+
  theme_bw() + ggtitle('Sentiment by project and comment type') +
  scale_x_continuous(name = 'comment sentiment') + 
  scale_alpha_discrete(values = c(0,1))+
  theme(
    axis.text.y = element_text(size = 12,angle = 45),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_blank()))
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


