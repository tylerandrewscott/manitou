
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
if(!require(INLA)){install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)}
packages = c('data.table','stringr','tidyverse','sf','lwgeom','ggthemes','tigris','lubridate','MASS','aod','matrixStats','stargazer')
not_installed = packages[!packages %in% installed.packages()[,'Package']]
if(length(not_installed)>0){lapply(not_installed,install.packages)}
lapply(packages,require,character.only = T)

start_year = 2005
end_year = 2018

##### this determines whether models run on per-congress (i.e., two year windows) #####
# for "CALENDAR_YEAR" or "congress"
period_type = 'CALENDAR_YEAR'

#### this determines which projects to include ######
keep_decisions = c('EIS','EA','CE')

#### this determines whether ot keep purpose or activity or both
keep_activities = FALSE
keep_purpose = TRUE


albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
admin_districts <- readRDS('policypolitics/prepped_inputs//admin_units_clean.RDS')
admin_districts <- st_transform(admin_districts,crs = st_crs(albersNA))

fs = readRDS('policypolitics/raw_curated_inputs/fs_PALS_cleaned_project_datatable.RDS')

#system('ln -s ~/Box/manitou/output/ ~/Documents/Github/manitou')
#system('ln -s ~/Box/manitou/input/ ~/Documents/Github/manitou')

states = tigris::states(class = 'sf')
states <- st_transform(states,crs = st_crs(albersNA))
test = st_within(st_centroid(admin_districts),states)
admin_districts$STATE = states[unlist(test),]$STUSPS

totcount = fs[,.N,by=.(FOREST_ID)]
admin_districts = left_join(admin_districts,totcount)

us_counties = counties(class = 'sf',year = 2017)
us_counties = st_transform(us_counties ,albersNA)


states = tigris::states(class = 'sf',year = 2015)
states = st_transform(states,albersNA)
states <- st_crop(states,st_bbox(admin_districts))
us_counties <- st_crop(us_counties,st_bbox(admin_districts))

nf = fread('policypolitics/prepped_inputs//national_forest_covariates.csv')
nf$FOREST_ID = formatC(nf$FOREST_ID,width = 4,flag = 0)

nf$FOREST_NAME = admin_districts$FORESTNAME[match(nf$FOREST_ID,admin_districts$FOREST_ID)]
nf = nf[congress %in% 108:115,]
nf[!nf$FOREST_ID %in% admin_districts$FOREST_ID,]
#install.packages("ggcorrplot")
require(ggcorrplot)

subvars = c('Type_Purpose_Extractive')

if(period_type=='congress')
{obs_grid = data.table(expand.grid(congress = 107:116,FOREST_ID = unique(fs$FOREST_ID),DECISION_TYPE= keep_decisions,Project_Type = subvars))}
if(period_type=='CALENDAR_YEAR')
{obs_grid = data.table(expand.grid(CALENDAR_YEAR = start_year:end_year,FOREST_ID = unique(fs$FOREST_ID),DECISION_TYPE= keep_decisions,Project_Type = subvars))}

subset_projtypes = lapply(subvars, function(x) {
  if(period_type=='congress'){
    temp = fs[get(x)==1,.N,by = .(FOREST_ID,`DECISION TYPE`,congress)]}
  if(period_type!='congress'){
    temp = fs[get(x)==1,.N,by = .(FOREST_ID,`DECISION TYPE`,CALENDAR_YEAR)]}
  setnames(temp,c('DECISION TYPE'),c('DECISION_TYPE'))
  temp$Project_Type <- x
  temp = merge(obs_grid[Project_Type==x,],temp,all = T)
  temp
})
counts_by_type = rbindlist(subset_projtypes,use.names = T)
counts_by_type$N[is.na(counts_by_type$N)]<-0


durations = fs[,list(mean(mdy(`DECISION SIGNED`) - mdy(`INITIATION DATE`),na.rm=T),median(mdy(`DECISION SIGNED`) - mdy(`INITIATION DATE`),na.rm=T)),by = .(`DECISION TYPE`)]
names(durations) <- c('Type','mean','median')

#counts_by_type[FOREST_ID=='1004']
nf= nf[nf$FOREST_ID%in%admin_districts$FOREST_ID,]
nf$USFS_REGION = admin_districts$REGION[match(nf$FOREST_ID,admin_districts$FOREST_ID)]

library(zoo)

center_continuous_cov = TRUE


#BASE RATIO: remove nominate_dim1 & all macro variables. remove L1_TOTAL_EA_EIS based on Manny's comments? your call
#Model 2: add macro variables (there are 4 of them -- demPres + demCongress  + ComLCV + ChairLCV
#Model 3: no macro variables, interact LCV_annual X percentD_H (edited) 
#Model 4: no macros, no interaction, interact democrat X Count_EorT_Species, democrat X Wilderness_Prop (edited) 
#Model 5: same as above, but instead of democrat use LCV_annual
#Model 6: same as above, but instead use percentD_H

if(period_type == 'congress'){
  all_combos = expand.grid(FOREST_ID = sort(unique(admin_districts$FOREST_ID)),congress = 109:115,DECISION_TYPE = c('CE','EA','EIS'))
}
if(period_type !='congress'){
  all_combos = expand.grid(FOREST_ID = sort(unique(admin_districts$FOREST_ID)),CALENDAR_YEAR = start_year:end_year,DECISION_TYPE = c('CE','EA','EIS'))
}

all_combos = data.table(all_combos)
subtypes = unique(counts_by_type$Project_Type)

nf$STATE = admin_districts$STATE[match(nf$FOREST_ID,admin_districts$FOREST_ID)]

dist_by_year = full_join(admin_districts,nf[,.(FOREST_ID,LCV_annual,CALENDAR_YEAR)])
dist_by_year = dist_by_year[!is.na(dist_by_year$CALENDAR_YEAR),]
# 
 tt = ggplot() + geom_sf(data = dist_by_year,aes(fill = LCV_annual,colour = LCV_annual)) + 
   facet_wrap(~CALENDAR_YEAR,ncol = 3) + 
   ggtitle('LCV annual by year and forest') + theme_map() + scale_fill_viridis_c() + 
   scale_colour_viridis_c()
#ggsave(tt,dpi = 300,filename = 'policypolitics/tables_figures/figures/LCV_by_forest_and_year.png',height = 12,width = 8,units = 'in')


dist_by_year = full_join(admin_districts,nf[,.(FOREST_ID,Unemp_Rate ,CALENDAR_YEAR)])
dist_by_year = dist_by_year[!is.na(dist_by_year$CALENDAR_YEAR),]

 ###### exclude all time-fixed (or nearly fixed) variables
base_linear_form = Y ~ -1 + mu.u + mu.y + 
  u_Unemp_Rate +  
  u_Perc_Extraction_Employ + 
  u_ln_County_naturalresource_GDP_1M + 
  u_ln_Receipts_Extraction_1M_P4 + 
 # u_Ln_ACRES + 
#  u_Wilderness_Perc + 
 # u_Ln_AVERAGE_YEARLY_VISITS + 
#  u_Count_EorT_Species + 
  u_Burned_Perc_Past5yrs  + 
 # u_Perc_WUI_Housing +
 # u_demPres + 
#  u_demCongress + 
  u_mrp_mean +
  u_LCV_annual + 
  y_Unemp_Rate +  y_Perc_Extraction_Employ + y_ln_County_naturalresource_GDP_1M + 
  y_ln_Receipts_Extraction_1M_P4 + 
#  y_Ln_ACRES + y_Wilderness_Perc + 
  y_Burned_Perc_Past5yrs  + 
 # y_Ln_AVERAGE_YEARLY_VISITS + y_Count_EorT_Species + 
#  y_Perc_WUI_Housing + 
#  y_demPres + y_demCongress +
  y_mrp_mean +
  y_LCV_annual 

  form0 = update.formula(base_linear_form, ~ . + 
    u_forest_id+ 
    u_congress_id+
    u_region_id+
    u_state_id+
    y_forest_id+
    y_congress_id+
    y_region_id+
    y_state_id)
  
  
  form0x = update.formula(form0, ~ . + u_Unemp_Rate:u_LCV_annual + y_Unemp_Rate:y_LCV_annual)

list_of_forms = grep('form0',ls(),value=T)


input_data = dcast(counts_by_type,get(period_type) + 
                     FOREST_ID + Project_Type ~ DECISION_TYPE,value.var = 'N',fill = 0)
input_data$Tot_Proj = input_data$CE + input_data$EA + input_data$EIS
if(period_type=='congress'){
  input_data = input_data[Project_Type=='Type_Purpose_Extractive',][period_type>108&period_type<116,]}
if(period_type!='congress'){
  input_data = input_data[Project_Type=='Type_Purpose_Extractive',][period_type>=start_year&period_type<=end_year,]}

yheight = input_data[,.N,by = .(Tot_Proj)][N==max(N),]$N

forest_index = data.table(forest_id = sort(unique(nf$FOREST_ID)),index = seq_along(unique(nf$FOREST_ID)))
region_index = data.table(region_id = sort(unique(nf$USFS_REGION)),index = seq_along(unique(nf$USFS_REGION)))
state_index = data.table(state_id = sort(unique(nf$STATE)),index = seq_along(unique(nf$STATE)))
congress_index = data.table(congress_id = sort(unique(nf$congress)),index = seq_along(unique(nf$congress)))

subtypes =  "Type_Purpose_Extractive" 
mod = subtypes; nf = nf; project_type_counts_for_model = counts_by_type;period = period_type
  temp_count = project_type_counts_for_model[Project_Type==mod,,]
  temp_count = dcast(temp_count ,get(period_type) + FOREST_ID ~ DECISION_TYPE,fill = 0,value.var = 'N')
  setnames(temp_count,'period_type',period_type)
  if(period=='congress'){
    temp_nf = nf[order(FOREST_ID,get(period_type)),][!duplicated(paste(FOREST_ID,congress)),]}
  if(period=='CALENDAR_YEAR'){
    temp_nf = nf[order(FOREST_ID,get(period_type)),]}
  temp_dt = merge(temp_count,temp_nf,by = c('FOREST_ID',period_type))#,all=T,by = c('FOREST_ID',congress))
  temp_dt$ln_Receipts_Extraction_1M_P4[is.na(temp_dt$ln_Receipts_Extraction_1M_P4)] <- 0
  
  
  

  n = nrow(temp_dt)
  u = rowSums(temp_dt[,c('CE','EA','EIS'),with = F])
  narep = length(u)
  y <- ifelse(u>0,temp_dt$CE,NA)
  
  idat <- list(Y=matrix(NA,2*n,2))
  idat$Y[1:n,1] <- u
  idat$Y[n+1:n,2] <- y
  idat$mu.u <- rep(c(1,NA), each=n)
  idat$mu.y <- rep(c(NA,1), each=n)


  idat$u_forest_id = c(temp_dt$FOREST_ID,rep(NA,length(y)))#temp_dt$FOREST_ID)

  idat$y_forest_id = c(rep(NA,length(y)),idat$u_forest_id[1:length(u)])
  
  idat$u_congress_id = c(temp_dt$congress,rep(NA,length(y)))

  idat$y_congress_id = c(rep(NA,length(y)),idat$u_congress_id[1:length(u)])
  
  idat$u_year_id = as.factor(as.character(c(temp_dt$CALENDAR_YEAR,rep(NA,length(y)))))

  idat$y_year_id =   c(rep(NA,length(y)),as.factor(as.character(idat$u_year_id[1:length(u)])))
  
  idat$u_state_id = c(temp_dt$STATE,rep(NA,length(y)))

  idat$y_state_id = c(rep(NA,length(y)),idat$u_state_id[1:length(u)])
  
  idat$u_region_id = c(temp_dt$USFS_REGION,rep(NA,length(y)))

  idat$y_region_id = c(rep(NA,length(y)),idat$u_region_id[1:length(u)])

  #### binomial EIS / [EA+EIS] model coefficients

  idat$y_Unemp_Rate = c(rep(NA,narep),scale(temp_dt$Unemp_Rate))
  idat$y_raw_Unemp_Rate = c(rep(NA,narep),temp_dt$Unemp_Rate)
  
  temp_dt$ln_Receipts_Extraction_1M_P4[is.na(temp_dt$ln_Receipts_Extraction_1M_P4)] <- 0
  idat$y_ln_Receipts_Extraction_1M_P4 = c(rep(NA,narep),scale(temp_dt$ln_Receipts_Extraction_1M_P4))
  idat$y_ln_Receipts_Recreation_1M_P4 = c(rep(NA,narep),scale(temp_dt$ln_Receipts_Recreation_1M_P4))

  idat$y_Perc_Extraction_Employ = c(rep(NA,narep),scale(temp_dt$Perc_Extraction_Employ))
  idat$y_Ln_ACRES = c(rep(NA,narep),scale(temp_dt$Ln_ACRES))
  idat$y_Wilderness_Perc = c(rep(NA,narep),scale(temp_dt$Wilderness_Perc)) 
  idat$y_Burned_Perc_Past5yrs = c(rep(NA,narep),scale(temp_dt$Burned_Perc_Past5yrs)) 
  idat$y_Ln_AVERAGE_YEARLY_VISITS = c(rep(NA,narep),scale(temp_dt$Ln_AVERAGE_YEARLY_VISITS)) 
  idat$y_Count_EorT_Species= c(rep(NA,narep),scale(temp_dt$Count_EorT_Species)) 

  idat$y_LCV_annual= c(rep(NA,narep),scale(temp_dt$LCV_annual))
  idat$y_raw_LCV_annual= c(rep(NA,narep),temp_dt$LCV_annual)
  idat$y_mrp_mean= c(rep(NA,narep),scale(temp_dt$mrp_mean))
  
  idat$y_demPres = c(rep(NA,narep),temp_dt$demPres)
  idat$y_demCongress = c(rep(NA,narep),temp_dt$demCongress)

  idat$y_Perc_WUI_Housing = c(rep(NA,narep),scale(temp_dt$Perc_WUI_Housing))
  
  idat$y_ln_County_naturalresource_GDP_1M = c(rep(NA,narep),scale(temp_dt$ln_County_naturalresource_GDP_1M))
  idat$y_Total_Receipts_4yr_Change_Perc = c(rep(NA,narep),scale(temp_dt$Total_Receipts_4yr_Change_Perc))
  idat$u_Unemp_Rate = c(scale(temp_dt$Unemp_Rate),rep(NA,narep))
  idat$u_Unemp_Rate_Demeaned = c(scale(temp_dt$Unemp_Rate),rep(NA,narep))
  
  idat$u_raw_Unemp_Rate = c(temp_dt$Unemp_Rate,rep(NA,narep))
  idat$u_ln_Receipts_Extraction_1M_P4 = c(scale(temp_dt$ln_Receipts_Extraction_1M_P4),rep(NA,narep))
  idat$u_ln_Receipts_Recreation_1M_P4 = c(scale(temp_dt$ln_Receipts_Recreation_1M_P4),rep(NA,narep))
  idat$u_Perc_Extraction_Employ = c(scale(temp_dt$Perc_Extraction_Employ),rep(NA,narep))
  idat$u_Total_Receipts_4yr_Change_Perc = c(scale(temp_dt$Total_Receipts_4yr_Change_Perc),rep(NA,narep))
  idat$u_Perc_WUI_Housing = c(scale(temp_dt$Perc_WUI_Housing),rep(NA,narep))
  idat$u_Ln_ACRES = c(scale(temp_dt$Ln_ACRES),rep(NA,narep))
  idat$u_Wilderness_Perc = c(scale(temp_dt$Wilderness_Perc),rep(NA,narep)) 
  idat$u_Burned_Perc_Past5yrs = c(scale(temp_dt$Burned_Perc_Past5yrs),rep(NA,narep)) 
  idat$u_Ln_AVERAGE_YEARLY_VISITS = c(scale(temp_dt$Ln_AVERAGE_YEARLY_VISITS),rep(NA,narep)) 
  idat$u_Count_EorT_Species= c(scale(temp_dt$Count_EorT_Species),rep(NA,narep)) 

  idat$u_LCV_annual= c(scale(temp_dt$LCV_annual),rep(NA,narep)) 
  idat$u_raw_LCV_annual= c(temp_dt$LCV_annual,rep(NA,narep)) 
  idat$u_mrp_mean = c(scale(temp_dt$mrp_mean),rep(NA,narep)) 

  idat$u_demPres = c(temp_dt$demPres,rep(NA,narep))
  idat$u_demCongress = c(temp_dt$demCongress,rep(NA,narep))

  idat$u_ln_County_naturalresource_GDP_1M = c(scale(temp_dt$ln_County_naturalresource_GDP_1M),rep(NA,narep))
  idat$n = n;idat$y = y;idat$u = u;





pcount = fs[fs$congress>=109&fs$congress<=115,]

counts_by_type$UNIT = admin_districts$FORESTNAME[match(counts_by_type$FOREST_ID,admin_districts$FOREST_ID)]

counts_by_type[Project_Type=='Type_Purpose_Extractive',][,sum(N)]

subtypes = grep('Extract',subtypes,value = T)

nb_mod = glm.nb(u~ -1 + u_Unemp_Rate + u_Perc_Extraction_Employ + u_ln_County_naturalresource_GDP_1M +
                  u_ln_Receipts_Extraction_1M_P4 +
                  u_Burned_Perc_Past5yrs + 
                  u_mrp_mean +
                  u_LCV_annual + as.factor(u_forest_id) + u_year_id ,data = lapply(idat,function(x) x[1:length(idat$u)]))

u.sdres <- sd(residuals(nb_mod))
temp_dat = as.data.frame(lapply(idat,function(x) x[length(idat$u)+{1:length(idat$y)}]))
keep_index = which(!is.na(idat$y))
drop_index = which(is.na(idat$y))
temp_dat = temp_dat[keep_index,]
temp_dat$y = idat$y[keep_index]
temp_dat$u = idat$u[keep_index]

### simple beta-bin model to develop prior for distribution
bin_mod = betabin(cbind(y,u-y) ~ -1 + 
                    y_Unemp_Rate + 
                    y_Perc_Extraction_Employ + 
                    y_ln_County_naturalresource_GDP_1M +
                    y_ln_Receipts_Extraction_1M_P4 + 
                    y_Burned_Perc_Past5yrs + 
                    y_mrp_mean +
                    y_LCV_annual + as.factor(y_forest_id) + y_year_id,~1,
                  data =  temp_dat,control = list(maxit = 10e3))

y.sdres <- sd(residuals(bin_mod))
pc.prec.used = list(prec= list(prior = "pc.prec", param = c(u.sdres,0.01)),
                    prec = list(prior = "pc.prec", param = c(y.sdres,0.01)))

u.resp <- sd(idat$u,na.rm = T)
y.resp <- sd(idat$y/idat$u,na.rm=T)

famcontrol = list(list(prior = "pcprec", param = c(u.resp,0.01)),
                  list(prior = "pcprec", param = c(y.resp,0.01)))

bprior <- list(prior = 'gaussian', param = c(0,1))


cres <- list(return.marginals.predictor = FALSE, 
               return.marginals.random = FALSE)



  
  idat$u_forest_id <- ifelse(is.na(forest_index$index[match(idat$u_forest_id,forest_index$forest_id)]),NA,paste0('forest_',forest_index$index[match(idat$u_forest_id,forest_index$forest_id)]))
  idat$y_forest_id <- ifelse(is.na(forest_index$index[match(idat$y_forest_id,forest_index$forest_id)]),NA,paste0('forest_',forest_index$index[match(idat$y_forest_id,forest_index$forest_id)]))

  
  
  idat$u_region_id <- ifelse(is.na(idat$u_region_id),NA,paste0('region_',idat$u_region_id))
  idat$y_region_id <-  ifelse(is.na(idat$y_region_id),NA,paste0('region_',idat$y_region_id))


  
  idat$u_state_id <- ifelse(is.na(state_index$index[match(idat$u_state_id,state_index$state_id)]),NA,paste0('state_',state_index$index[match(idat$u_state_id,state_index$state_id)]))
  idat$y_state_id <- ifelse(is.na(state_index$index[match(idat$y_state_id,state_index$state_id)]),NA,paste0('state_',state_index$index[match(idat$y_state_id,state_index$state_id)]))

  
  idat$u_congress_id <- ifelse(is.na(congress_index$index[match(idat$u_congress_id,congress_index$congress_id)]),NA,paste0('congress_',congress_index$index[match(idat$u_congress_id,congress_index$congress_id)]))
  idat$y_congress_id <- ifelse(is.na(congress_index$index[match(idat$y_congress_id,congress_index$congress_id)]),NA,paste0('congress_',congress_index$index[match(idat$y_congress_id,congress_index$congress_id)]))


  ######### not returning marginals greatly reduces object size #######
  cres <- list(return.marginals.predictor = FALSE, 
               return.marginals.random = FALSE)
  require(pbapply)
  mod_list = pblapply(list_of_forms,function(f) {print(f);gc();inla(get(f),
                                                                  family = c('nbinomial', 'betabinomial'),Ntrials = idat$u,
                                                                  control.fixed = list(expand.factor.strategy = "inla",prec = bprior),
                                                                  control.family = famcontrol,
                                                                
                                                                  control.results = cres,
                                                                  data=idat, control.compute = list(waic=TRUE,dic=TRUE,config = F),
                                                                  control.predictor=list(compute=TRUE),verbose=F)},cl = 1)
  
  names(mod_list) <- list_of_forms

saveRDS(mod_list,paste0('policypolitics/model_objects/models_',subtypes,'_FE.RDS'))

