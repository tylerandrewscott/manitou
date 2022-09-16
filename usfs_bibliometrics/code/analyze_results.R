library(citationSearch)
library(stringr)
library(data.table)
library(parallel)
library(stringdist)
library(referenceBuild)
library(rlist)
library(lubridate)
library(pbapply)
library(tidyverse)
extracts_filtered <- readRDS('usfs_bibliometrics/scratch/extracted_results.rds')
extracts_filtered$q <- citationSearch::create_queries(extracts_filtered[,c('title','authors','publisher','year','journal_title','doi')])
res_dt <- readRDS('usfs_bibliometrics/scratch/query_match_scores.RDS')
mt <- cbind(extracts_filtered,res_dt[match(extracts_filtered$q,res_dt$q),]%>%dplyr::select(-q))
### right now, anythnign with score over 80 is bad
mt <- mt[match.score<80,]
mt$stringsim_osa <- unlist(pbsapply(X = 1:nrow(mt),function(i) {
  stringsim(tolower(mt$title[i]),tolower(mt$match.title[i]),method = 'osa')},cl = 4))
nrow(mt)/nrow(extracts_filtered)
mt <- mt[match.score>20|stringsim_osa>0.75,]


### add project details
dets = fread('../eis_documents/agency_nepa_libraries/usfs/metadata/forest_service_project_detail.csv')
dets$PType = str_remove(dets$`Expected Analysis Type`,'\\r\\n\\t\\t')
dets <- dets[year(ymd(dets$`Decision Signed Date`))%in%2010:2019,]
table(dets$`Expected Analysis Type`)/nrow(dets)

docs <- fread('../eis_documents/agency_nepa_libraries/usfs/metadata/forest_service_document_record.csv')
mt$Project_Num <- docs$Project_Num[match(mt$File,str_replace(docs$File_Name,'pdf$','json'))]
det_match <- dets[match(mt$Project_Num,dets$Proj_Num),]
mt$Forest <- det_match$Forest
mt$Analysis <- det_match$PType
mt$Decision.Signed.Date <- decimal_date(ymd(det_match$`Decision Signed Date`))

mt <- mt[!duplicated(mt[,.(oa_id,Project_Num)]),]

### add journal data
host_meta <- readRDS('usfs_bibliometrics/scratch/index_host.rds')
mt$host.issn_l <- host_meta$host.issn_l[match(mt$oa_id,host_meta$oa_id)]
mt$host.id <- host_meta$host.id[match(mt$oa_id,host_meta$oa_id)]

works_meta <- readRDS('usfs_bibliometrics/scratch/index_meta.rds')
mt <- merge(mt,works_meta,all.x = T)

scimago_issn_dt <- readRDS('usfs_bibliometrics/input/scimago_issn.RDS')
mt$scimago_jid <- scimago_issn_dt$Sourceid[match(str_remove_all(mt$host.issn_l,'-'),scimago_issn_dt$ISSN)]
mt_notin <- mt[is.na(scimago_jid),]
mt2<- mt[!is.na(scimago_jid),]

scimago_dt <- readRDS('usfs_bibliometrics/input/scimago_journal_data.RDS')

scimag_info <- scimago_dt[match(paste(mt2$scimago_jid,floor(mt2$Decision.Signed.Date)),paste(scimago_dt$Sourceid,scimago_dt$Year)),]
names(scimag_info) <- paste0('sm.',names(scimag_info))
mt3 <- cbind(mt2,scimag_info)

######
#author_meta <- readRDS('usfs_bibliometrics/scratch/index_author.rds')
#auth_ids <- str_extract_all(author_meta$author.id,'[A-Za-z0-9]{1,}($|;)')
#auth_ids <- lapply(auth_ids,str_remove_all,pattern = ';')
#auth_work_ids <- mapply(function(x,y) cbind(x,y),x = auth_ids,y = author_meta$oa_id)
#auth_work_ids_dt <- rbindlist(lapply(auth_work_ids,data.table))
######

mt4 <- cbind(mt3,works_meta[match(mt3$oa_id,works_meta$oa_id),])
mt4 <- mt4[,!duplicated(names(mt4)),with = F]
mt4$Decision.Year <- floor(mt4$Decision.Signed.Date)
mt4 <- mt4[mt4$Analysis!='Decision Memo',]
mt4$match.year <- as.numeric(mt4$match.year)

# ######
# unique_works <- unique(mt4$oa_id)
# uqu_50 <- dplyr::ntile(unique_works,n = ceiling(length(unique_works)/50))
# uq_split <- split(unique_works,uqu_50)
# cit_by_year <- data.table()
# concepts_by_work <- data.table()
# pub_date <- data.table()
# count = 0
# for(q in uq_split){
#   count <- count + 1
#   print(count)
#   qr <- openalexR::oa_fetch(identifier = q,per_page = 50,output = 'list',abstract = F,mailto = 'tascott@ucdavis.edu')
#   ids <- str_extract(sapply(qr,'[[','id'),'[A-Z0-9]{1,}$')
#   pd <- sapply(qr,'[[','publication_date')
#   pdate <- data.table(publication_date = pd,id = ids)
#   pub_date <- rbind(pdate,pub_date)
#   count_by_year <- rbindlist(mapply(function(x,y) {
#     cy <- rbindlist(Map(as.data.frame, x))
#     cy$id <- y;return(cy)
#     },
#     x = lapply(qr,'[[','counts_by_year'),y = ids,SIMPLIFY = F),use.names = T,fill = T)
#   cit_by_year <- rbind(cit_by_year,count_by_year)
#   cp <- lapply(qr,'[[','concepts')
#   cept_dt <- rbindlist(lapply(seq_along(cp),function(i){
#     if(length(cp[[i]])>0){
#   temp <- rbindlist(Map(as.data.frame,cp[[i]]))[,.(id,display_name,score)]
#   temp$id <- ids[i]
#   return(temp)
#   }}),use.names = T,fill = T)
#   concepts_by_work <- rbind(concepts_by_work,cept_dt)
# }
# pub_date <- pub_date[!duplicated(pub_date),]
# cit_by_year <- cit_by_year[!duplicated(cit_by_year),]
# concepts_by_work <- concepts_by_work[!duplicated(concepts_by_work),]
# saveRDS(list(pub_date,cit_by_year,concepts_by_work),file = 'usfs_bibliometrics/scratch/matched_oa_metadata.RDS')

oa_other_data <- readRDS('usfs_bibliometrics/scratch/matched_oa_metadata.RDS')
concepts_by_work <- oa_other_data[[3]]
cit_by_year <- oa_other_data[[2]]
pub_date <- oa_other_data[[1]]
#####
mt4$publication_date <- pub_date$publication_date[match(mt4$oa_id,pub_date$id)]
mt4$pub_dec_date <- decimal_date(ymd(mt4$publication_date))

mt5 <- mt4[Project_Num %in% mt4[,.N,by=.(Project_Num)][N>2,]$Project_Num,]


t1 <- ggplot(mt5) + geom_bar(aes(x = match.year)) + theme_bw() + 
  ggtitle('Year of publication, all matched references')
t2 <- ggplot(mt5[!duplicated(oa_id),]) + geom_bar(aes(x = match.year)) + theme_bw() + 
  ggtitle('Year of publication, unique matched references')+
  scale_y_continuous(limits = c(NA,4000))+
  labs(caption="*search was truncated to publications > 1960")

library(gridExtra)
grid.arrange(t1,t2,ncol = 2)

id_count <- mt5[,.N,by=.(oa_id)][order(N),]
id_count$cum <- cumsum(id_count$N)
id_count$index <- 1:nrow(id_count)

ggplot(id_count,aes(x = index,y = cum)) + 
  geom_path(size = 0.02) + 
  geom_point(size= 0.2) + 
  scale_y_continuous('total matched references') + 
  scale_x_continuous(name = 'unique reference') + 
  ggtitle('Cumulative total matched references') + 
  theme_bw()

ggplot(id_count,aes(x = N)) + 
  geom_histogram(bins = 200) + 
  theme_bw() + ggtitle('distribution of reference frequency') + 
  scale_x_continuous('# observations per reference') + 
  scale_y_continuous('# references')


library(Rarity)
concepts_by_work$score <- as.numeric(concepts_by_work$score)
concepts_by_work$score <- 1
concepts_by_work <- concepts_by_work[id %in% mt5$oa_id,]
project_concepts <- concepts_by_work[match(mt5$oa_id,concepts_by_work$id),]
project_concepts$Project_Num <- mt5$Project_Num
project_concepts <- project_concepts[!is.na(score),]

project_concept_matrix_dt <- dcast(project_concepts,Project_Num ~ display_name,fun.aggregate = sum,value.var = 'score')
project_concept_mat <- project_concept_matrix_dt
project_concept_mat[,Project_Num:=NULL]
project_concept_mat <- as.matrix(project_concept_mat)

library(adespatial)
lcbd <- adespatial::beta.div(Y = project_concept_mat)

proj_dt <- dets
proj_dt <- proj_dt[Project_Num %in%project_concept_matrix_dt$Project_Num & !duplicated(Project_Num),]
proj_dt$LCBD <- lcbd$LCBD[match(proj_dt$Project_Num,project_concept_matrix_dt$Project_Num)]


purp_splits <- str_split(str_remove(proj_dt$`Project Purpose`,'^\\n'),';')
library(rlist)
purp_splits <- lapply(purp_splits ,str_remove_all,pattern = '^\\s+')
purp_dt <- rbindlist(lapply(rlist::list.zip(proj_dt$Project_Num,purp_splits),as.data.table))
proj_dt$DECISION_YEAR <- year(ymd(proj_dt$`Decision Signed Date`))
purp_dt$YEAR <- proj_dt$DECISION_YEAR[match(purp_dt$V1,proj_dt$Project_Num)]
purp_dt$purp_splits <- forcats::fct_rev(forcats::fct_infreq(purp_dt$purp_splits))
purp_dt$purp_splits <- forcats::fct_recode(purp_dt$purp_splits,'Vegetation management' = "Vegetation management (other than forest products)")

proj_dt$Analysis.Type <- str_extract(proj_dt$`Expected Analysis Type`,'(\\w+(\\s|$)){1,}')
purp_dt$type <- proj_dt$Analysis.Type[match(purp_dt$V1,proj_dt$Project_Num)]
library(ggthemes)
g1 <- ggplot(purp_dt,aes(x = purp_splits)) + geom_bar() +
  coord_flip() + theme_bw() + scale_y_continuous(name = '# of projects') +
  scale_x_discrete(name = 'Project purpose(s)') +
  ggtitle('Designated project purposes')
g2 <- ggplot(purp_dt[!duplicated(V1),],aes(x = YEAR,fill = type)) + geom_bar(position = 'dodge') +
 theme_bw() + scale_y_continuous(name = '# of projects') +
  scale_fill_colorblind(labels = c('EA','EIS')) + theme(legend.title = element_blank())+
  scale_x_continuous(name = 'Year of decision',breaks = c(2010:2019)) +
  ggtitle('Assessments by year')

grid.arrange(g1,g2,ncol = 2)
med_age <- mt5[,list(median(Decision.Signed.Date-pub_dec_date)),by=.(Project_Num)]
proj_dt$median_ref_age <- med_age$V1[match(proj_dt$Project_Num,med_age$Project_Num)]

citcount_by_year <- mt5[,median(cited_by_count/{Decision.Signed.Date-pub_dec_date}),by=.(Project_Num)]
proj_dt$cites_per_year <- citcount_by_year$V1[match(proj_dt$Project_Num,citcount_by_year$Project_Num)]

new_citations <- mt5[order(Decision.Signed.Date),.(Decision.Signed.Date,Project_Num,oa_id)][!duplicated(oa_id),][,.N,by=.(Project_Num)]
proj_dt$novel_citation_count <- new_citations$N[match(proj_dt$Project_Num,new_citations$Project_Num)]
proj_dt$novel_citation_count[is.na(proj_dt$novel_citation_count)]<-0

forest_covs <- fread('usfs_bibliometrics/input/national_forest_covariates.csv')
proj_dt$Decision.Signed.Date <- mt5$Decision.Signed.Date[match(proj_dt$Project_Num,mt5$Project_Num)]
proj_dt$Analysis.Type <- mt5$Analysis[match(proj_dt$Project_Num,mt5$Project_Num)]

admins <- readRDS('policypolitics/prepped_inputs/admin_units_clean.RDS')
proj_dt$Forest <- str_replace(proj_dt$Forest,'\\s\\/\\s','-')
proj_dt$Forest <- str_remove(proj_dt$Forest,'.*\\n')
proj_dt$Forest <- str_replace(proj_dt$Forest,'Perce Clear','Perce-Clear')
proj_dt$Forest <- str_replace(proj_dt$Forest,'Shasta Trinity','Shasta-Trinity')
proj_dt$Forest <- str_replace(proj_dt$Forest,'Basin Mgt Unit','Basin Management Unit')
proj_dt$Forest <- str_replace(proj_dt$Forest,'Lasal','La Sal')
proj_dt$Forest <- str_replace(proj_dt$Forest,'orests$','orest')
admins$FORESTNAME <- str_replace(admins$FORESTNAME,'orests$','orest')
proj_dt$Forest <- str_remove_all(proj_dt$Forest,'\\.')
admins$FORESTNAME <- str_remove_all(admins$FORESTNAME,'\\.')
proj_dt$LMU <- str_remove(str_remove(proj_dt$`Lead Management Unit`,'\\sAll Units'),'^\\W+')

proj_dt$FOREST_ID <- admins$FOREST_ID[match(tolower(proj_dt$Forest),tolower(admins$FORESTNAME))]
proj_dt$FOREST_ID[is.na(proj_dt$FOREST_ID )] <- admins$FOREST_ID[match(tolower(proj_dt$LMU[is.na(proj_dt$FOREST_ID)]),tolower(admins$FORESTNAME))]
proj_dt$FOREST_ID[is.na(proj_dt$FOREST_ID )] <- admins$FOREST_ID[match(tolower(str_remove(proj_dt$Forest[is.na(proj_dt$FOREST_ID)],';.*')),tolower(admins$FORESTNAME))]

fo_covs <- forest_covs[,.(FOREST_ID,CALENDAR_YEAR,Average_Yearly_Visits,mrp_mean,ACRES,Receipts_TimberMineralsGrazing_P4)]
setnames(fo_covs,'CALENDAR_YEAR','DECISON.YEAR')
proj_dt$DECISON.YEAR <- floor(proj_dt$Decision.Signed.Date)
fo_covs$FOREST_ID <- formatC(fo_covs$FOREST_ID,width = 4,flag = 0)
proj_dt <- proj_dt[,!duplicated(colnames(proj_dt)),with = F]
proj_dt <- merge(proj_dt,fo_covs)
proj_dt$diversity <- proj_dt$LCBD
proj_dt$novelty <- proj_dt$novel_citation_count
proj_dt$newness <- -proj_dt$median_ref_age
proj_dt$impact <- proj_dt$cites_per_year

library(vtable)
sumtable(proj_dt[,.(newness,impact,novelty,diversity)],add.median = T)
summary(proj_dt$diversity)
sd(proj_dt$diversity)

depvars <- c('newness','impact','novelty','diversity')
devars_std <- paste0('std.',depvars)
fema <- fread('usfs_bibliometrics/input/DisasterDeclarationsSummaries.csv')
fema$FIPS <- paste0(formatC(fema$fipsStateCode,width = 2,flag = 0),formatC(fema$fipsCountyCode,width = 3,flag = 0))
forest_counties <- fread('policypolitics/prepped_inputs/forest_county_overlap_props.csv')
forest_counties$FOREST_ID <- formatC(forest_counties$FOREST_ID,width = 4,flag = 0)
forest_counties <- forest_counties[round(forest_counties$Prop_Overlap,2)>0.00,]
fema$DATE <- decimal_date(ymd(fema$declarationDate))

proj_dt$Disaster_County_Count <- pbsapply(1:nrow(proj_dt),function(i) {
nrow(fema[FIPS %in% forest_counties$CFIPS[forest_counties$FOREST_ID==proj_dt$FOREST_ID[i]]&DATE<proj_dt$Decision.Signed.Date[i]&DATE>proj_dt$Decision.Signed.Date[i]-5,])
},cl = 5)

temp<-melt(proj_dt[,depvars,with = F])
temp$value[temp$variable=='novelty'&temp$value>500]<-500
ggplot(temp) + 
  ggtitle('Distribution of innovation measures by project')+
theme_bw()+
  geom_histogram(aes(x = value)) + 
    facet_wrap(~variable,scale = 'free')


h1_comp <- melt(proj_dt[,c('Project_Num','Analysis.Type','Decision.Signed.Date',depvars),with = F],id.vars = c('Project_Num','Analysis.Type','Decision.Signed.Date'))
h2_comp <- melt(proj_dt[,c('Project_Num','Analysis.Type','mrp_mean',depvars),with = F],id.vars = c('Project_Num','Analysis.Type','mrp_mean'))
h3_comp <- melt(proj_dt[,c('Project_Num','Analysis.Type','Disaster_County_Count',depvars),with = F],id.vars = c('Project_Num','Analysis.Type','Disaster_County_Count'))

gg_h1 <- ggplot(h1_comp,aes(color = Analysis.Type,y = value,x = Decision.Signed.Date)) + ggtitle('Reference innovation and date of decision') + 
  facet_wrap(~variable,scales = 'free') + theme_bw()  +
  theme(axis.title.y = element_blank()) + 
  geom_point(pch = 19,alpha = 0.5) + theme_bw() + geom_smooth(se = F)+
  theme(legend.position = 'bottom',axis.title.y = element_blank())+
  scale_color_colorblind(name = 'Assessment',labels = c('EA','EIS'))
ggsave(gg_h1,file = 'usfs_bibliometrics/output/figure1_h1.png',dpi = 450,units = 'in',width = 6,height = 6)

gg_h2 <- ggplot(h2_comp,aes(color = Analysis.Type,y = value,x = mrp_mean)) + 
  ggtitle('Reference innovation and local political ideology') + 
  facet_wrap(~variable,scales = 'free') + theme_bw()  +
  theme(axis.title.y = element_blank()) + 
  geom_point(pch = 19,alpha = 0.5) + theme_bw() + geom_smooth(se = F)+
  theme(legend.position = 'bottom',axis.title.y = element_blank())+
  scale_color_colorblind(name = 'Assessment',labels = c('EA','EIS'))
ggsave(gg_h2,file = 'usfs_bibliometrics/output/figure2_h2.png',dpi = 450,units = 'in',width = 6,height = 6)


h3_comp$Disaster_County_Count[h3_comp$Disaster_County_Count>50]<-50
gg_h3 <- ggplot(h3_comp,aes(color = Analysis.Type,y = value,x = Disaster_County_Count)) + 
  ggtitle('Reference innovation and social/enviro. pressure') + 
  facet_wrap(~variable,scales = 'free') + theme_bw()  +
  theme(axis.title.y = element_blank()) + 
  geom_point(pch = 19,alpha = 0.5) + theme_bw() + geom_smooth(se = F)+
  theme(legend.position = 'bottom',axis.title.y = element_blank())+
  scale_x_continuous(breaks = c(0,10,20,30,40,50),labels = c(0,10,20,30,40,'50+'))+
  scale_color_colorblind(name = 'Assessment',labels = c('EA','EIS'))
ggsave(gg_h3,file = 'usfs_bibliometrics/output/figure3_h3.png',dpi = 450,units = 'in',width = 6,height = 6)

ref_count <- mt5[,.N,by=.(Project_Num)]
proj_dt$Ref_Count <- ref_count$N[match(proj_dt$Project_Num,ref_count$Project_Num)]

proj_dt$std.ln.avg.yearly.visits <- scale(log(proj_dt$Average_Yearly_Visits))
proj_dt$std.ln.resource.receipts.p4 <- scale(log(proj_dt$Receipts_TimberMineralsGrazing_P4))
proj_dt$std.ln.acreage <- scale(log(proj_dt$ACRES))
proj_dt$std.mrp.mean <- scale(proj_dt$mrp_mean)
proj_dt$std.ln.disaster <- scale(proj_dt$Disaster_County_Count)
summary(proj_dt$Disaster_County_Count)

base_form <- '~ std.ln.acreage + std.ln.resource.receipts.p4 + 
std.ln.avg.yearly.visits + std.ln.refcount + Analysis.Type + 
std.mrp.mean + std.ln.disaster +
f(FOREST_ID,model = "iid",hyper =  pc.prec.used) + 
f(inla.group(Decision.Signed.Date,20), model = "rw1", constr = TRUE,hyper = list(theta = list(prior="pc.prec", param=c(u,0.01))))'

scl <- scale(proj_dt[,depvars,with = F])
colnames(scl) <- devars_std

proj_dt <- cbind(proj_dt,scl)
proj_dt <- proj_dt[!duplicated(proj_dt),]
proj_dt$std.ln.refcount <- scale(log(proj_dt$Ref_Count))
library(splines)
library(INLA)


bprior <- list(prior = 'gaussian', param = c(0,1))
cres <- list(return.marginals.predictor = FALSE, 
             return.marginals.random = FALSE)

mod_list <- lapply(colnames(scl),function(y){
  print(y)
  u <- 1
  u.sdres <- sd(residuals(lm(as.formula(paste(y,'~ 1 + std.ln.acreage + std.ln.resource.receipts.p4 + 
std.ln.avg.yearly.visits + std.ln.refcount + Analysis.Type')),data = proj_dt)))
  pc.prec.used = list(prec= list(prior = "pc.prec", param = c(u.sdres,0.01)))
  inla(as.formula(paste(y,base_form)),data = proj_dt,
       control.compute = list(waic=TRUE,dic=TRUE,config = F),
       control.predictor=list(compute=TRUE),
       control.fixed = list(expand.factor.strategy = "inla",prec = bprior))
})


library(INLAutils)

date_rw1 <- rbindlist(lapply(seq_along(mod_list),function(x) 
{tdt <- mod_list[[x]]$summary.random$`inla.group(Decision.Signed.Date, 20)`
tdt$inno <- colnames(scl)[x]
tdt
}))

#ggplot(data = proj_dt,aes(x = Decision.Signed.Date,y = std.diversity)) + geom_point()

gg_time_inno <- ggplot(date_rw1,aes(x = ID,y = mean,col = inno))+ 
  geom_ribbon(aes(x = ID,ymin = `0.025quant`,fill = inno,
                  ymax = `0.975quant`),alpha = 0.05,lty = 2,lwd = 0.2)+
  geom_path(lwd = 1.5) + 
  scale_color_colorblind(name = 'measure',labels = c('diversity','impact','newness','novelty')) +
  scale_fill_colorblind(name = 'measure',labels = c('diversity','impact','newness','novelty')) +
  scale_y_continuous(name = 'posterior mean + 95% cred. interval')+
  scale_x_continuous(breaks = 2010:2019,name = 'Decision date') + 
  theme_bw() + ggtitle('Predicted change in innovation by time') + 
  theme(legend.position = c(0.8,0.8))

ggsave(gg_time_inno,file = 'usfs_bibliometrics/output/figure_rw1_time.png',units = 'in',dpi = 450,height = 5,width = 6.5)



coef_mods <- rbindlist(lapply(seq_along(mod_list),function(x) 
{tdt <- mod_list[[x]]$summary.fixed
tdt$coef <- rownames(tdt)
tdt$inno <- colnames(scl)[x]
tdt
}))
coef_mods$coef <- fct_rev(fct_inorder(coef_mods$coef))
dodge <- position_dodge(width=0.5)

coef_mods$coef <- fct_recode(coef_mods$coef ,'EIS'='Analysis.TypeEnvironmental Impact Statement',
           'scale(ln(acreage))'='std.ln.acreage',
           'scale(ln(extraction receipts))'='std.ln.resource.receipts.p4',
           'scale(ln(avg. yearly visits))' = 'std.ln.avg.yearly.visits',
           'scale(ln(# matched references))' = 'std.ln.refcount',
           'scale(political ideology)' = 'std.mrp.mean',
           'scale(ln(disaster declarations))' = 'std.ln.disaster')

gg_coefs <- ggplot(data = coef_mods) + theme_bw() + 
  scale_color_colorblind(name = 'innovation\nmeasure',labels = c('diversity','impact','newness','novelty')) +
  theme(legend.position = c(0.2,0.2),axis.title.y = element_blank())+
  geom_vline(xintercept = 0,lty = 2,col = 'grey50')+
  ggtitle('Posterior estimates, linear predictors of innovation')+
  geom_errorbarh(aes(y = coef,x = mean,xmin = `0.025quant`,xmax = `0.975quant`,col = inno),position = dodge) + 
    geom_point(aes(y = coef,x = mean, col = inno),position = dodge ) +
  geom_point(data = coef_mods[`0.025quant`<0&`0.975quant`>0,],aes(y = coef,x = mean,group = inno),col = 'white',position = dodge ) 

ggsave(gg_coefs,file = 'usfs_bibliometrics/output/figure_coefs_intervals.png',units = 'in',dpi = 450,height = 4.75,width = 6.25)
  


group_rw1_form <- '~ std.ln.acreage + std.ln.resource.receipts.p4 + 
std.ln.avg.yearly.visits + std.ln.refcount + Analysis.Type + 
std.mrp.mean + std.ln.disaster +
f(FOREST_ID,model = "iid",hyper =  pc.prec.used) + 
f(inla.group(Decision.Signed.Date,20), model = "rw1", group = FOREST_INLA_NUM,control.group = list(scale.model = T,model = "ar1"),
constr = TRUE,hyper = list(theta = list(prior="pc.prec", param=c(u,0.01))))'

proj_dt$FOREST_INLA_NUM <- as.numeric(as.factor(proj_dt$FOREST_ID))

mod_list_rwgroups <- lapply(colnames(scl),function(y){
  print(y)
  u <- 1
  u.sdres <- sd(residuals(lm(as.formula(paste(y,'~ 1 + std.ln.acreage + std.ln.resource.receipts.p4 + 
std.ln.avg.yearly.visits + std.ln.refcount + Analysis.Type')),data = proj_dt)))
  pc.prec.used = list(prec= list(prior = "pc.prec", param = c(u.sdres,0.01)))
  inla(as.formula(paste(y,group_rw1_form )),data = proj_dt,
       control.compute = list(waic=TRUE,dic=TRUE,config = F),
       control.predictor=list(compute=TRUE),
       control.fixed = list(expand.factor.strategy = "inla",prec = bprior))
})

base_fit <- rbind(sapply(mod_list,'[[','waic'),
                 sapply(mod_list,'[[','dic'))

rw1_fit <- rbind(sapply(mod_list_rwgroups,'[[','waic'),
sapply(mod_list_rwgroups,'[[','dic'))

convert <- function(x) {round(as.numeric(unlist(x)),2)}
base_waic <- convert(base_fit[c('waic'),])
base_dic <- convert(base_fit[c('dic'),])
rw1_waic <- convert(rw1_fit[c('waic'),])
rw1_dic <- convert(rw1_fit[c('dic'),])

htmlTable::htmlTable(rbind(rbind(base_waic,base_dic),
rbind(rw1_waic,rw1_dic)))


tab_coef <- rbindlist(lapply(seq_along(mod_list),function(x) {
  temp <- mod_list[[x]]$summary.fixed
  temp$coef <- rownames(temp)
  temp$inno <- colnames(scl)[x]
  temp
}))

tab_coef$coef <- fct_recode(tab_coef$coef  ,'EIS'='Analysis.TypeEnvironmental Impact Statement',
                             'scale(ln(acreage))'='std.ln.acreage',
                             'scale(ln(extraction receipts))'='std.ln.resource.receipts.p4',
                             'scale(ln(avg. yearly visits))' = 'std.ln.avg.yearly.visits',
                             'scale(ln(# matched references))' = 'std.ln.refcount',
                             'scale(political ideology)' = 'std.mrp.mean',
                             'scale(ln(disaster declarations))' = 'std.ln.disaster')

tab_coef <- tab_coef[,.(coef,mean,`0.025quant`,`0.975quant`,inno)]
tab_coef <- tab_coef %>% mutate_if(is.numeric,round,2) 
tab_coef$posterior <- paste0(tab_coef$mean,' (',tab_coef$`0.025quant`,', ',tab_coef$`0.975quant`,')')

htmlTable::htmlTable(dcast(tab_coef,coef~inno,value.var = 'posterior'))




ranef_coef <- rbindlist(lapply(seq_along(mod_list),function(x) {
  temp <- mod_list[[x]]$summary.hyperpar
  temp$coef <- rownames(temp)
  temp$inno <- colnames(scl)[x]
  temp
}))

ranef_coef <- ranef_coef[,.(coef,mean,`0.025quant`,`0.975quant`,inno)]
ranef_coef <- ranef_coef %>% mutate_if(is.numeric,round,2) 
ranef_coef$posterior <- paste0(ranef_coef$mean,' (',ranef_coef$`0.025quant`,', ',ranef_coef$`0.975quant`,')')

htmlTable::htmlTable(dcast(ranef_coef,coef~inno,value.var = 'posterior'))


base_waic <- convert(base_fit[c('waic'),])
base_dic <- convert(base_fit[c('dic'),])
base_dic
summary(mod_list[[1]])
proj_dt$Ref_Count
summary(mod_list[[1]])
stargazer::stargazer(mod_list,style = 'AJPS',singlerow = T,omit = c('FOREST_ID|Decision.Signed.Date'),type = 'html',out = 'usfs_bibliometrics/output/first_regression.html')

proj_dt[,.N,by=.(FOREST_ID)][order(-N),]
# normalize age for best basis functions

summary()
# add predicted values for plotting
proj_dt[, pred1 := predict(mod_list[[1]])]
proj_dt[, pred2 := predict(mod_list[[2]])]
proj_dt[, pred3 := predict(mod_list[[3]])]
proj_dt[, pred4 := predict(mod_list[[4]])]



ggplot(data = proj_dt, aes(x=Decision.Signed.Date, y=std.impact)) +
  geom_point(color = "grey65", size = 0.75) +
  geom_line(aes(x=Decision.Signed.Date, y = pred2), color = "#1B9E77", size = 1)
ggplot(data = proj_dt, aes(x=Decision.Signed.Date, y=std.novelty)) +
  geom_point(color = "grey65", size = 0.75) +
  geom_line(aes(x=Decision.Signed.Date, y = pred3), color = "#1B9E77", size = 1)


depvars
geom_line(aes(x=age, y = pred.2deg), color = "#D95F02", size = 1) +
  geom_line(aes(x=age, y = pred.1deg), color = "#7570B3", size = 1) +
  geom_vline(xintercept = quantile(dt$age, knots)) +
  theme(panel.grid.minor = element_blank())


summary(mod_list[[1]])
plot(mod_list[[1]])
as.formula(paste(depvars[1],base_form))
           
update.formula(base_form,'test'.+)
lapply(depvars,function(x) {
  
  lm(as.name(novel_citation_count)~1 + Forest + Analysis.Type + Decision.Signed.Date,data = proj_dt)
})



htmlTable::htmlTable(mt5[,.N,by=.(match.journal_title)][order(-N),][1:10,])
htmlTable::htmlTable(mt5[,.N,by=.(match.authors)][order(-N),][1:10,])

author_meta <- readRDS('usfs_bibliometrics/scratch/index_author.rds')

author_splits <- unlist(str_split(author_meta$author.id[match(mt5$oa_id[mt5$pub_dec_date>2010],author_meta$oa_id)],';'))
top_authors <- as.data.table(table(author_splits))[order(-N),][1:10,]
top_authors <- str_extract(top_authors$author_splits,'[A-Z0-9]+$')
library(jsonlite)

quathors <- pblapply(top_authors,function(x) {
  temp <- fromJSON(openalexR::oa_query(identifier = x,mailto = 'tascott@ucdavis.edu'))
return(paste(temp$display_name,temp$last_known_institution$display_name,sep = ', '))
Sys.sleep(0.01)
})

qjournals <- as.data.table(table(mt5$match.journal_title[mt5$pub_dec_date>2010]))[order(-N),][1:10,]

oaids_to_query <- author_meta$author.id[match(mt5$oa_id[mt5$pub_dec_date>2010],author_meta$oa_id)]
author_id_splits <- str_split(oaids_to_query,';')
uq_authids <- unique(unlist(author_id_splits))
uq_authids <- str_extract(uq_authids,'[A-Z0-9]+$')
library(referenceBuild)
auth_inst_pairs <- pblapply(uq_authids,function(q){
temp <- fromJSON(openalexR::oa_query(identifier = q,mailto = 'tascott@ucdavis.edu'))
data.table(auth_id = q,inst_name = temp$last_known_institution$display_name)},cl = 3
)


rbindlist(auth_inst_pairs,fill =T,use.names = T)
bads <- which(!is.na(uq_authids) & sapply(auth_inst_pairs,class)=='try-error')
auth_inst_pairs2 <- pblapply(uq_authids[bads],function(q){
  temp <- fromJSON(openalexR::oa_query(identifier = q,mailto = 'tascott@ucdavis.edu'))
  data.table(q,inst_name <- temp$last_known_institution$display_name)},cl = 1
)

auth_inst_pairs[bads] <- auth_inst_pairs2 
bads <- which(!is.na(uq_authids) & sapply(auth_inst_pairs,class)=='try-error')
auth_inst_pairs <- auth_inst_pairs[-bads]
authinst <- rbindlist(auth_inst_pairs,fill = T,use.names = T)

bads
authinst
match(author_meta$author.id,authinst$q)


ggplot(data = proj_dt,aes(colour = Analysis.Type,x = Decision.Signed.Date,y = novel_citation_count)) + 
  ggtitle('# novel citations by project over time') + 
geom_point(pch = 19,alpha = 0.5) + theme_bw() + geom_smooth(se = F)+
  theme(legend.position = c(0.8,0.4))+
  scale_color_colorblind(name = 'Assessment',labels = c('EA','EIS'))+
  scale_x_continuous(name = 'Decision signing date',breaks = c(2010:2019),
                     labels = 2010:2019) + 
  scale_y_continuous(name = '# novel citations') 

gmod <- gam(novel_citation_count~1 + Forest + Analysis + s(Decision.Signed.Date),data = proj_dt)

pals <- fread('common_inputs/Copy of MYTR_National Level(1100).xlsx - Decision Data Set.csv')
test2 <- readRDS('common_inputs/FS_ongoing_projects_11-2019.rds')


docs[1:5,]
docs$forest
proj_dt$Forest_ID <- pals$FORESTmatch(proj_dt$Project_Num,pals$`PROJECT NUMBER`)




mt5$Project_Num[!mt5$Project_Num %in% test$`PROJECT NUMBER`]
mt5$Project_Num %in% test2$`PROJECT NUMBER`
library(mgcv)

mt5[,list(median(Decision.Signed.Date-pub_dec_date)),by=.(Project_Num)]

temp <- mt5[,list(median(Decision.Signed.Date-pub_dec_date),.N),by=.(Project_Num,Decision.Signed.Date,Forest,Analysis,Decision.Year)]



#temp <- mt4[,list(median(match.year),median(cited_by_count)),by=.(Project_Num,Decision.Year,Forest)]
setnames(temp,c('V1'),c('median.age'))#,'median.citation.count'))
temp[,mean(median.age),by=.(Decision.Year)][order(Decision.Year),]
library(ggthemes)
(g1 <- ggplot(temp,aes(x = Decision.Signed.Date,y = median.age,colour = Analysis)) + 
  geom_point(pch = 19,alpha = 0.5) + theme_bw() + geom_smooth(se = F)+
  theme(legend.position = c(0.8,0.1))+
  scale_color_colorblind(name = 'Assessment',labels = c('EA','EIS'))+
  scale_x_continuous(name = 'Decision signing date',breaks = c(2010:2019),
                     labels = 2010:2019) + 
  scale_y_continuous(name = 'Project median reference age') + 
  ggtitle('Age of references used by project')) 
ggsave(g1,file = 'usfs_bibliometrics/output/figure1_median_age_over_time.png',dpi = 450,units = 'in',width = 6,height = 5)

mt5$sm.SJR <- as.numeric(str_replace_all(mt5$sm.SJR,'\\,','.'))
temp <- mt5[,list(median(sm.SJR),.N,median(cited_by_count)),by=.(Project_Num,Decision.Signed.Date,Forest,Analysis,Decision.Year)]

#temp <- mt4[,list(median(match.year),median(cited_by_count)),by=.(Project_Num,Decision.Year,Forest)]
setnames(temp,c('V1','V3'),c('median.SJR','median.cited_by_count'))#,'median.citation.count'))
temp[,mean(median.SJR,na.rm  = T),by=.(Decision.Year)][order(Decision.Year),]=

library(ggthemes)
(g2a <- ggplot(temp,aes(x = Decision.Signed.Date,y = median.SJR,colour = Analysis)) + 
    geom_point(pch = 19,alpha = 0.5) + theme_bw() + geom_smooth(se = F)+
    theme(legend.position = c(0.8,0.6))+
    scale_color_colorblind(name = 'Assessment',labels = c('EA','EIS'))+
    scale_x_continuous(name = 'Decision signing date',breaks = c(2010:2019),
                       labels = 2010:2019) + 
    scale_y_continuous(name = 'Project median journal rank ') + 
    ggtitle('Ranks of journals used by project')) 
(g2b <- ggplot(temp,aes(x = Decision.Signed.Date,y = median.cited_by_count,colour = Analysis)) + 
    geom_point(pch = 19,alpha = 0.5) + theme_bw() + geom_smooth(se = F)+
    theme(legend.position = c(0.8,0.6))+
    scale_color_colorblind(name = 'Assessment',labels = c('EA','EIS'))+
    scale_x_continuous(name = 'Decision signing date',breaks = c(2010:2019),
                       labels = 2010:2019) + 
    scale_y_continuous(name = 'Project median reference citation count') + 
    ggtitle('Citations for references used by project')) 
ggsave(g2,file = 'usfs_bibliometrics/output/figure2_median_rank_over_time.png',dpi = 450,units = 'in',width = 6,height = 5)


head(mite)


table(is.na(project_concept_matrix[,1]))

summary(concept_occ)
rar_weights <- rWeights(concept_occ)
assemb_matrix <- as.matrix(project_concept_dt[,-'Project_Num'])


rownames(assemb_matrix) <- project_concept_dt$Project_Num
project_irr <- Rarity::Irr(assemblages = t(assemb_matrix),W = rar_weights,abundance = T)

test <- data.table(project_irr)[order(-Irr),]


concepts_by_work[,.N,by = (id)][order(-N),]

colSums(assemb_matrix)
assemb_matrix[1:5,1:5]
?beta.civ
citation('Rarity')
str(test)

# Input rarity weights
data(spid.occ)

# Example of a single scale dataset
regional.occ <- spid.occ$occurMA
names(regional.occ) <- rownames(spid.occ)
head(regional.occ)
head(spid.occ)
# Preparation of rarity weights
rarity.weights <- rWeights(regional.occ)


str(regional.occ)
?rWeights

?Irr
dim(as.matrix(work_concept_matrix[,-1]))
temp[median.cited_by_count>6000,]


work_concept_count <- concepts_by_work[,.N,by=.(id)]
mt4$unique_concepts <- work_concept_count$N[match(mt4$oa_id,work_concept_count$id)]

mt4$unique_concepts <- pbsapply(mt4$oa_id,function(x) {
length(unique(concepts_by_work$display_name[concepts_by_work$id == x]))
},cl = 4)

mt4$unique_concepts
summary(mt4$`sm.H index`)


mt4[,median(`sm.H index`,na.rm = T)]

temp <- mt4[,list(median(Decision.Signed.Date-pub_dec_date),.N),by=.(Project_Num,Decision.Signed.Date,Forest,Analysis,Decision.Year)]
#temp <- mt4[,list(median(match.year),median(cited_by_count)),by=.(Project_Num,Decision.Year,Forest)]
setnames(temp,c('V1'),c('median.age'))#,'median.citation.count'))
temp[,mean(median.age),by=.(Decision.Year)]



cept_dt
lapply(cp,'[[','id')
cp[[1]][[1]]
test <- lapply(qr,'[[','counts_by_year')

jsonlite::fromJSON(test[[1]])
list_data <- cbind(Map(as.data.frame, test[[1]]),id[1])

unlist(test[[1]])
lapply(lapply(qr,'[[','counts_by_year'),'[[','year')
cit_by_year <- rbind(cit_by_year,as.data.table(cbind(t(as.data.table(qr$counts_by_year)),unique_works[1])))
concepts_by_work <- rbind(data.table(unique_works[1],str_extract(sapply(qr$concepts,'[[','id'),'[A-Z0-9]{1,}$')))
pub_date <- rbind(pub_date,data.table(unique_works[1],qr$publication_date))
}


g2 <- ggplot(mt4,aes(x = Decision.Year,y = log(cited_by_count+1),group = Decision.Year)) + 
  geom_boxplot() + theme_bw() +
  scale_x_continuous(name = 'Decision Signed',breaks = c(2010:2019),
                     labels = 2010:2019) + 
  scale_y_continuous(name = 'ln(# citations as of 9/2022)') + 
  ggtitle('Current citation count of references signing year')



match(mt3$Forest,forvars$)

forvars <- fread('usfs_bibliometrics/input/national_forest_covariates.csv')



fread('usfs_bibliometrics/scratch/')


author_meta[25983,]
auth_ids[[[[25983]]]]
auth_work_ids[[25983]]

which(test==1)

class(auth_work_ids[[1]])
auth_work_ids[[1]]
test
author_meta$author.id[1:10]
auth_ids[1:10]
test



str(auth_work)
auth_work[[1]]
auth_work[[2]]
head(auth_ids)

head(author_meta)
top_papers_oaqua)id <- mt2[,.N,by=.(oa_id)][order(-N),][1:10,]



mt2$au

sc$Sourceid
sc$ISSN[[1]]
test[[1]]
head(test)


tt <- str_remove(mt$host.issn_l[20],'-')


head(sc)

any(sapply(sc$ISSN,function(x) any(x == tt)))

test <- unique(mt$host.issn_l)
table(test %in% sc$Issn)
head(test)
sc$Issn
head(test)


slibrary(tidyverse)
names(mt)
ggplot(mt,aes(x = match.score,y = stringsim_osa)) + 
  theme_bw() + 
  geom_point(pch = 19,alpha = 0.5) + 
  geom_hline(yintercept = 0.5,lty = 2,col = 'red') +
  geom_vline(xintercept = 20,lty = 2,col ='red')

mt[stringsim_osa==1&match.score<10,.(title,match.title)][5,]



summary(mt$match.score[mt$stringsim_osa>0.75&mt$match.score<20])
mt[stringsim_osa<0.6&stringsim_osa>0.5&match.score>20,][3,]


test <- ref_dt[journal_title=='Environmental Pollution',]
test[grepl('potential carbon sequestration by rangelands',tolower(test$title)),]

library(referenceBuild)
library(openalexR)
?openalexR::oa_query()

test <- oa_query(title.search = "Soil carbon dynamics and potential carbon sequestration by rangelands",entity = 'works')

test


[3,.(journal_title,match.title)]


mt[stringsim_osa>0.5&stringsim_osa<0.60,][,.(title,match.title)][10,]

mt$match.journal_title[is.na(mt$match.title)]
summary(mt$stringsim_osa)
  align_local(mt$title[i],mt$match.title[i],)
ag <- align_local(mt$title[i],mt$match.title[i]);ag$score}))


mc.cores = 4,mc.preschedule = T,mc.cleanup = T,mc.set.seed = 24))


summary(mt$title_align)
mt[str_count(mt$title,';')==3,][,.(title)]
table(str_count(mt$title,';'))
test <- mt[match.score>75,.(title,match.title,journal_title,match.journal_title)]

library(textreuse)



mt$title[grepl("X{3,}",tm::removePunctuation(mt$authors))]

ref_dt[title=='Trumpeter Swan no longer rare and endangered',]
mt %>% dplyr::select(-q) %>%
  dplyr::filter(match.title=='Trumpeter Swan no longer rare and endangered')%>%
  dplyr::select(title)

mt$title[!is.na(mt$authors) & nchar(mt$authors)>500]
names(mt)

summary(mt$match.score)
table(



na <- is.na(res_dt$oa_id)

table(is.na(extracts_filtered$title))
table(is.na(ref_dt$title))
res_dt[na,][,.(title,doi,authors)]

#saveRDS(object = res_dt,'usfs_bibliometrics/scratch/query_match_scores.RDS')
temp <- res_dt[score>20&!is.na(title),]

test <- temp[grepl('Fish and Wildlife',temp$authors),]

test
##### make overview
temp[,.N,by=.(journal_title)][order(-N),][!is.na(journal_title),][1:10,]
temp[,.N,by=.(oa_id)][order(-N),][!is.na(oa_id),][1:10,][1,]


tt <- search_collection(test, collection_name="oa_enviro1",topn = 5)
tt2 <- search_collection(test, collection_name="oa_enviro2",topn = 5)
tt2[1,]
test
tt2[1,]$authors
tt$score
tt2$score
dim(temp)

test <- temp[oa_id=='W4239972567',][903,]$q
test

fw <- grep('us fish',tm::removePunctuation(tolower(extracts_filtered$authors)))

extracts_filtered[fw,]$authors

reco <- extracts_filtered[grepl('Recovery Plan',extracts_filtered$title),]
reco2 <-
reco2$journal_title
dim(reco2)
table(reco2$journal_title)

extracts_filtered[match(test,queries),]$title

tt <- grepl('trumpeter swan no longer',tolower(extracts_filtered$title))

table(tt)

temp[,.N,by=.(journal_title)][order(-N),][N<200&N>100,]
table(is.na(temp$journal_title))
test <- temp[temp$title!=temp$ex_title,]
temp[is.na(journal_title),][1,]$oa_id
test[2,]
i = 4
(test[2,]$title==test[2,]$ex_title)

test[i,]$title
test[i,]$ex_title

dim(test)
conf <- which(temp$title!=temp$ex_title)
length(conf)
temp$title[1]==temp$ex_title[1]

conf[1]
temp[4,]

snag longevity in relation to wildfire and postfire salvage logging
snag longevity in relation to wildlife and postfire salvage logging


test <- temp[is.na(oa_id),][5,]



queries <- citationSearch::create_queries(extracts_filtered)



paste(test$title,test$doi)


test <- extracts_filtered[grepl('^Glyphosate-based herbicides produce teratogenic',title),]
test

dim(test)
test

test$title
test$title




q1 <- search_collection(queries[[i]], collection_name="openalex_enviroconcepts1",topn = 1)
q2 <- search_collection(queries[[i]], collection_name="openalex_enviroconcepts2",topn = 1)

ref_dt[grepl('^glyphosate based herbicides',tolower(title)),]
table(is.na(temp$oa_id))

mean(res_dt$score>20)
dim(res_dt)


temp[5,]$score
temp[5,]$q
res_dt$doi
table(res_dt$score>17)
mean(res_dt$score)


length(queries)

res_dt$title
res_list
})
results = list()
for(i in seq_along(queries)[1:50]){
  print(i)
  q1 <- search_collection(queries[[i]], collection_name="openalex_enviroconcepts1",topn = 1)
  q2 <- search_collection(queries[[i]], collection_name="openalex_enviroconcepts2",topn = 1)
  if(q1$score>q2$score){
    results[[i]] <- q1
  }else{
    results[[i]] <- q2
  }
  results[[i]]$query <- queries[[i]]
}
