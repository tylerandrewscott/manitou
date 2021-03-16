
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
#if(!require(INLA)){install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)}
require(INLA)
packages = c('data.table','stringr','tidyverse','sf','lwgeom','ggthemes','tigris','lubridate')
not_installed = packages[!packages %in% installed.packages()[,'Package']]
if(length(not_installed)>0){lapply(not_installed,install.packages)}
lapply(packages,require,character.only = T)

#run = 'joint_shared'
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


td = tempdir()
albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

admin_districts <- readRDS('policypolitics/prepped/admin_units_clean.RDS')

file.remove(list.files('policypolitics/tables_figures/tables/',pattern = 'coefs',full.names = T))
states = tigris::states(class = 'sf')
states <- st_transform(states,crs = st_crs(albersNA))
test = st_within(st_centroid(admin_districts),states)
admin_districts$STATE = states[unlist(test),]$STUSPS
fs = readRDS('policypolitics/prepped/fs_PALS_cleaned_project_datatable.RDS')


totcount = fs[,.N,by=.(FOREST_ID)]
admin_districts = left_join(admin_districts,totcount)

us_counties = counties(class = 'sf',year = 2017)
us_counties = st_transform(us_counties ,albersNA)


nf = fread('policypolitics/prepped/national_forest_covariates.csv')
nf$FOREST_ID = formatC(nf$FOREST_ID,width = 4,flag = 0)

nf$FOREST_NAME = admin_districts$FORESTNAME[match(nf$FOREST_ID,admin_districts$FOREST_ID)]
nf = nf[congress %in% 109:115,]

fs = fs[congress%in%109:115,]
fs = fs[FOREST_ID %in% admin_districts$FOREST_ID,]


subvars = 'Type_Purpose_Extractive'

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

nf$USFS_REGION = admin_districts$REGION[match(nf$FOREST_ID,admin_districts$FOREST_ID)]

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



#setnames(nf,'LAU','Unemp_Rate')

nf$STATE = admin_districts$STATE[match(nf$FOREST_ID,admin_districts$FOREST_ID)]

dist_by_year = full_join(admin_districts,nf[,.(FOREST_ID,LCV_annual,CALENDAR_YEAR)])
dist_by_year = dist_by_year[!is.na(dist_by_year$CALENDAR_YEAR),]







form_separate_u = Y[,1] ~ 0 + mu.u + 
  f(u_forest_id,model = 'iid',hyper = pc.prec.u) + 
  f(u_congress_id,model = 'iid',hyper = pc.prec.u) + 
  f(u_region_id, model = 'iid',hyper = pc.prec.u) + 
  f(u_state_id, model = 'iid',hyper = pc.prec.u) + 
  u_Unemp_Rate +  u_Perc_Extraction_Employ + u_ln_County_naturalresource_GDP_1M + 
  u_ln_Receipts_Extraction_1M_P4 + 
  u_Ln_ACRES + u_Wilderness_Perc + 
  u_Burned_Perc_Past5yrs  + 
  u_Ln_AVERAGE_YEARLY_VISITS + u_Count_EorT_Species + 
  u_Perc_WUI_Housing +
  u_demPres + u_demCongress + 
  u_mrp_mean +
  u_LCV_annual

form_separate_y = Y[,2] ~ 0 + mu.y +
  f(y_forest_id,model = 'iid',hyper = pc.prec.y) +
  f(y_congress_id,model = 'iid',hyper = pc.prec.y) +
  f(y_region_id,model = 'iid',hyper = pc.prec.y) +
  f(y_state_id,model = 'iid',hyper = pc.prec.y) +
  y_Unemp_Rate +  y_Perc_Extraction_Employ + y_ln_County_naturalresource_GDP_1M + 
  y_ln_Receipts_Extraction_1M_P4 + 
  y_Ln_ACRES + y_Wilderness_Perc + 
  y_Burned_Perc_Past5yrs  + 
  y_Ln_AVERAGE_YEARLY_VISITS + y_Count_EorT_Species + 
  y_Perc_WUI_Housing + 
    y_demPres + y_demCongress +
    y_mrp_mean +
  y_LCV_annual


  form_joint = Y ~ 0 + mu.u + 
    f(u_forest_id,model = 'iid',hyper = pc.prec.u) + 
    f(u_congress_id,model = 'iid',hyper = pc.prec.u) + 
    f(u_region_id, model = 'iid',hyper = pc.prec.u) + 
    f(u_state_id, model = 'iid',hyper = pc.prec.u) + 
    f(y_forest_id,model = 'iid',hyper = pc.prec.y) +
    f(y_congress_id,model = 'iid',hyper = pc.prec.y) +
    f(y_region_id,model = 'iid',hyper = pc.prec.y) +
    f(y_state_id,model = 'iid',hyper = pc.prec.y) +
    u_Unemp_Rate +  u_Perc_Extraction_Employ + u_ln_County_naturalresource_GDP_1M + 
    u_ln_Receipts_Extraction_1M_P4 + u_ln_Receipts_Recreation_1M_P4 +
    u_Total_Receipts_4yr_Change_Perc +
    u_Ln_ACRES + u_Wilderness_Perc + 
    u_Burned_Perc_Past5yrs  + 
    u_Ln_AVERAGE_YEARLY_VISITS + u_Count_EorT_Species + 
    u_Perc_WUI_Housing +
    u_demPres + u_demCongress + 
    u_mrp_mean +
    u_LCV_annual + 
    mu.y +
    y_Unemp_Rate +  y_Perc_Extraction_Employ + y_ln_County_naturalresource_GDP_1M + 
    y_ln_Receipts_Extraction_1M_P4 +
    y_Ln_ACRES + y_Wilderness_Perc + 
    y_Burned_Perc_Past5yrs  + 
    y_Ln_AVERAGE_YEARLY_VISITS + y_Count_EorT_Species + 
    y_Perc_WUI_Housing + 
    y_demPres + y_demCongress +
    y_mrp_mean +
    y_LCV_annual 


form_joint_shared = update.formula(form_joint, . ~ . +
    f(uc_forest_id,copy = 'u_forest_id',fixed = F,hyper = list(theta=bprior)) +
    f(uc_congress_id,copy = 'u_congress_id',fixed = F,hyper = list(theta=bprior))+
    f(uc_region_id,copy = 'u_region_id',fixed = F,hyper = list(theta=bprior)) +
    f(uc_state_id,copy = 'u_state_id',fixed = F,hyper = list(theta=bprior)))
   

form_shared_only = update.formula(form_joint_shared, . ~ . - 
    f(y_forest_id,model = 'iid',hyper = pc.prec.y)
    -f(y_congress_id,model = 'iid',hyper = pc.prec.y)
    -f(y_region_id,model = 'iid',hyper = pc.prec.y)
    -f(y_state_id,model = 'iid',hyper = pc.prec.y))
                                    
                                
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
idat$uc_forest_id = c(rep(NA,length(y)),idat$u_forest_id[1:length(u)])
idat$y_forest_id = c(rep(NA,length(y)),idat$u_forest_id[1:length(u)])

idat$u_congress_id = c(temp_dt$congress,rep(NA,length(y)))
idat$uc_congress_id =  c(rep(NA,length(y)),idat$u_congress_id[1:length(u)])
idat$y_congress_id = c(rep(NA,length(y)),idat$u_congress_id[1:length(u)])

idat$u_year_id = c(temp_dt$CALENDAR_YEAR,rep(NA,length(y)))
idat$uc_year_id =  c(rep(NA,length(y)),idat$u_year_id[1:length(u)])
idat$y_year_id =   c(rep(NA,length(y)),idat$u_year_id[1:length(u)])

idat$u_state_id = c(temp_dt$STATE,rep(NA,length(y)))
idat$uc_state_id = c(rep(NA,length(y)),idat$u_state_id[1:length(u)])
idat$y_state_id = c(rep(NA,length(y)),idat$u_state_id[1:length(u)])

idat$u_region_id = c(temp_dt$USFS_REGION,rep(NA,length(y)))
idat$uc_region_id = c(rep(NA,length(y)),idat$u_region_id[1:length(u)])
idat$y_region_id = c(rep(NA,length(y)),idat$u_region_id[1:length(u)])

#### binomial EIS / [EA+EIS] model coefficients

idat$y_Unemp_Rate = c(rep(NA,narep),scale(temp_dt$Unemp_Rate))

temp_dt$ln_Receipts_Extraction_1M_P4[is.na(temp_dt$ln_Receipts_Extraction_1M_P4)] <- 0
idat$y_ln_Receipts_Extraction_1M_P4 = c(rep(NA,narep),scale(temp_dt$ln_Receipts_Extraction_1M_P4))
idat$y_ln_Receipts_Recreation_1M_P4 = c(rep(NA,narep),scale(temp_dt$ln_Receipts_Recreation_1M_P4))

idat$y_Perc_Extraction_Employ = c(rep(NA,narep),scale(temp_dt$Perc_Extraction_Employ))
idat$y_Ln_ACRES = c(rep(NA,narep),scale(temp_dt$Ln_ACRES))
idat$y_Wilderness_Perc = c(rep(NA,narep),scale(temp_dt$Wilderness_Perc)) 
idat$y_Burned_Perc_Past5yrs = c(rep(NA,narep),scale(temp_dt$Burned_Perc_Past5yrs)) 
idat$y_Ln_AVERAGE_YEARLY_VISITS = c(rep(NA,narep),scale(temp_dt$Ln_AVERAGE_YEARLY_VISITS)) 
idat$y_Count_EorT_Species= c(rep(NA,narep),scale(temp_dt$Count_EorT_Species)) 
idat$y_percentD_H = c(rep(NA,narep),scale(temp_dt$percentD_H)) 

idat$y_LCV_annual= c(rep(NA,narep),scale(temp_dt$LCV_annual))
idat$y_mrp_mean= c(rep(NA,narep),scale(temp_dt$mrp_mean))

idat$y_demPres = c(rep(NA,narep),temp_dt$demPres)
idat$y_demCongress = c(rep(NA,narep),temp_dt$demCongress)
idat$y_ComLCV = c(rep(NA,narep),scale(temp_dt$ComLCV))
idat$y_ChairLCV = c(rep(NA,narep),scale(temp_dt$ChairLCV))
idat$y_Perc_WUI_Housing = c(rep(NA,narep),scale(temp_dt$Perc_WUI_Housing))

idat$y_ln_County_naturalresource_GDP_1M = c(rep(NA,narep),scale(temp_dt$ln_County_naturalresource_GDP_1M))
idat$y_Total_Receipts_4yr_Change_Perc = c(rep(NA,narep),scale(temp_dt$Total_Receipts_4yr_Change_Perc))
idat$u_Unemp_Rate = c(scale(temp_dt$Unemp_Rate),rep(NA,narep))

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
idat$u_percentD_H = c(scale(temp_dt$percentD_H),rep(NA,narep)) 

idat$u_LCV_annual= c(scale(temp_dt$LCV_annual),rep(NA,narep)) 
idat$u_mrp_mean = c(scale(temp_dt$mrp_mean),rep(NA,narep)) 
idat$u_democrat = c(temp_dt$democrat,rep(NA,narep))
idat$y_democrat = c(rep(NA,narep),temp_dt$democrat)
idat$u_demPres = c(temp_dt$demPres,rep(NA,narep))
idat$u_demCongress = c(temp_dt$demCongress,rep(NA,narep))
idat$u_ComLCV = c(scale(temp_dt$ComLCV),rep(NA,narep))
idat$u_ChairLCV = c(scale(temp_dt$ChairLCV),rep(NA,narep))
idat$u_ln_County_naturalresource_GDP_1M = c(scale(temp_dt$ln_County_naturalresource_GDP_1M),rep(NA,narep))
idat$n = n;idat$y = y;idat$u = u;



library(INLA)

#project_type_counts_for_model[,sum(N),by=.(Project_Type)]

#proj_count_input = dcast(counts_by_type,congress + FOREST_ID + Project_Type ~ DECISION_TYPE,value.var = 'N')
pcount = fs[fs$congress>=109&fs$congress<=115,]
library(ggrepel)

flist = list(form_joint,form_joint_shared,form_shared_only,list(form_separate_u,form_separate_y))
names(flist) <- c('joint','joint_shared','shared','separate')
names(flist$separate) <- c('nbinomial', 'betabinomial')


subtypes = subtypes[grepl('EXTRACTIVE',toupper(subtypes))]


#### code from Krainski et al. 2018
getfit <- function(r) {
  fam <- r$dic$family
  data.frame(dic = tapply(r$dic$local.dic, fam, sum), 
             waic = tapply(r$waic$local.waic, fam, sum), 
             cpo = tapply(r$cpo$cpo, fam, 
                          function(x) - sum(log(x), na.rm = TRUE)))
}

mod = subtypes[1]


idat <- list(Y=matrix(NA,2*n,2))
idat$Y[1:n,1] <- u
idat$Y[n+1:n,2] <- y
idat$mu.u <- rep(c(1,NA), each=n)
idat$mu.y <- rep(c(NA,1), each=n)


idat$u_forest_id = c(temp_dt$FOREST_ID,rep(NA,length(y)))#temp_dt$FOREST_ID)
idat$uc_forest_id = c(rep(NA,length(y)),idat$u_forest_id[1:length(u)])
idat$y_forest_id = c(rep(NA,length(y)),idat$u_forest_id[1:length(u)])

idat$u_congress_id = c(temp_dt$congress,rep(NA,length(y)))
idat$uc_congress_id =  c(rep(NA,length(y)),idat$u_congress_id[1:length(u)])
idat$y_congress_id = c(rep(NA,length(y)),idat$u_congress_id[1:length(u)])

idat$u_year_id = c(temp_dt$CALENDAR_YEAR,rep(NA,length(y)))
idat$uc_year_id =  c(rep(NA,length(y)),idat$u_year_id[1:length(u)])
idat$y_year_id =   c(rep(NA,length(y)),idat$u_year_id[1:length(u)])

idat$u_state_id = c(temp_dt$STATE,rep(NA,length(y)))
idat$uc_state_id = c(rep(NA,length(y)),idat$u_state_id[1:length(u)])
idat$y_state_id = c(rep(NA,length(y)),idat$u_state_id[1:length(u)])

idat$u_region_id = c(temp_dt$USFS_REGION,rep(NA,length(y)))
idat$uc_region_id = c(rep(NA,length(y)),idat$u_region_id[1:length(u)])
idat$y_region_id = c(rep(NA,length(y)),idat$u_region_id[1:length(u)])

#### binomial EIS / [EA+EIS] model coefficients

idat$y_Unemp_Rate = c(rep(NA,narep),scale(temp_dt$Unemp_Rate))

temp_dt$ln_Receipts_Extraction_1M_P4[is.na(temp_dt$ln_Receipts_Extraction_1M_P4)] <- 0
idat$y_ln_Receipts_Extraction_1M_P4 = c(rep(NA,narep),scale(temp_dt$ln_Receipts_Extraction_1M_P4))
idat$y_ln_Receipts_Recreation_1M_P4 = c(rep(NA,narep),scale(temp_dt$ln_Receipts_Recreation_1M_P4))

idat$y_Perc_Extraction_Employ = c(rep(NA,narep),scale(temp_dt$Perc_Extraction_Employ))
idat$y_Ln_ACRES = c(rep(NA,narep),scale(temp_dt$Ln_ACRES))
idat$y_Wilderness_Perc = c(rep(NA,narep),scale(temp_dt$Wilderness_Perc)) 
idat$y_Burned_Perc_Past5yrs = c(rep(NA,narep),scale(temp_dt$Burned_Perc_Past5yrs)) 
idat$y_Ln_AVERAGE_YEARLY_VISITS = c(rep(NA,narep),scale(temp_dt$Ln_AVERAGE_YEARLY_VISITS)) 
idat$y_Count_EorT_Species= c(rep(NA,narep),scale(temp_dt$Count_EorT_Species)) 
idat$y_percentD_H = c(rep(NA,narep),scale(temp_dt$percentD_H)) 

idat$y_LCV_annual= c(rep(NA,narep),scale(temp_dt$LCV_annual))
idat$y_mrp_mean= c(rep(NA,narep),scale(temp_dt$mrp_mean))

idat$y_demPres = c(rep(NA,narep),temp_dt$demPres)
idat$y_demCongress = c(rep(NA,narep),temp_dt$demCongress)
idat$y_ComLCV = c(rep(NA,narep),scale(temp_dt$ComLCV))
idat$y_ChairLCV = c(rep(NA,narep),scale(temp_dt$ChairLCV))
idat$y_Perc_WUI_Housing = c(rep(NA,narep),scale(temp_dt$Perc_WUI_Housing))

idat$y_ln_County_naturalresource_GDP_1M = c(rep(NA,narep),scale(temp_dt$ln_County_naturalresource_GDP_1M))
idat$y_Total_Receipts_4yr_Change_Perc = c(rep(NA,narep),scale(temp_dt$Total_Receipts_4yr_Change_Perc))
idat$u_Unemp_Rate = c(scale(temp_dt$Unemp_Rate),rep(NA,narep))

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
idat$u_percentD_H = c(scale(temp_dt$percentD_H),rep(NA,narep)) 

idat$u_LCV_annual= c(scale(temp_dt$LCV_annual),rep(NA,narep)) 
idat$u_mrp_mean = c(scale(temp_dt$mrp_mean),rep(NA,narep)) 
idat$u_democrat = c(temp_dt$democrat,rep(NA,narep))
idat$y_democrat = c(rep(NA,narep),temp_dt$democrat)
idat$u_demPres = c(temp_dt$demPres,rep(NA,narep))
idat$u_demCongress = c(temp_dt$demCongress,rep(NA,narep))
idat$u_ComLCV = c(scale(temp_dt$ComLCV),rep(NA,narep))
idat$u_ChairLCV = c(scale(temp_dt$ChairLCV),rep(NA,narep))
idat$u_ln_County_naturalresource_GDP_1M = c(scale(temp_dt$ln_County_naturalresource_GDP_1M),rep(NA,narep))
idat$n = n;idat$y = y;idat$u = u;


counts_by_type
u.sdres <- sd(idat$u,na.rm = T)
y.sdres <- sd(idat$y/idat$u,na.rm=T)
pc.prec.u = list(prec = list(prior = "pc.prec", param = c(3*u.sdres, 0.01)))
pc.prec.y = list(prec = list(prior = "pc.prec", param = c(3*y.sdres, 0.01)))
bprior <- list(prior = 'gaussian', param = c(0,1))
famcontrol = list(list(prior = "pcprec", param = c(3*u.sdres,0.01)),
                  list(prior = "pcprec", param = c(3*y.sdres,0.01)))
cinla <- list(strategy = 'adaptive', int.strategy = 'eb') 
cres <- list(return.marginals.predictor = FALSE, 
             return.marginals.random = FALSE)





mod.joint = inla(formula = form_joint,
            family = c('nbinomial', 'betabinomial'),Ntrials = idat$u,
            control.fixed = list(expand.factor.strategy = "inla"),
            control.family = famcontrol,
            control.inla = cinla,
            control.results = cres,
            data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE,config = TRUE),
            control.predictor=list(compute=TRUE),verbose=F)

mod.joint.shared = inla(formula = form_joint_shared,
                 family = c('nbinomial', 'betabinomial'),Ntrials = idat$u,
                 control.fixed = list(expand.factor.strategy = "inla"),
                 control.family = famcontrol,
                 control.inla = cinla,
                 control.results = cres,
                 data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE,config = TRUE),
                 control.predictor=list(compute=TRUE),verbose=F)

mod.shared.only = inla(formula = form_shared_only,
                 family = c('nbinomial', 'betabinomial'),Ntrials = idat$u,
                 control.fixed = list(expand.factor.strategy = "inla"),
                 control.family = famcontrol,
                 control.inla = cinla,
                 control.results = cres,
                 data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE,config = TRUE),
                 control.predictor=list(compute=TRUE),verbose=F)

mod.separate.u = inla(formula = form_separate_u,
                      family = c('nbinomial'),
                      control.fixed = list(expand.factor.strategy = "inla"),
                      control.family = famcontrol[[1]],
                      control.inla = cinla,
                      control.results = cres,
                      data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE,config = TRUE),
                      control.predictor=list(compute=TRUE),verbose=F)
mod.separate.y = inla(formula = form_separate_y,
                      family = c('betabinomial'),Ntrials = idat$u,
                      control.fixed = list(expand.factor.strategy = "inla"),
                      control.family = famcontrol[[2]],
                      control.inla = cinla,
                      control.results = cres,
                      data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE,config = TRUE),
                      control.predictor=list(compute=TRUE),verbose=F)

fwrite(data.table(rbind(joint = getfit(mod.joint), 
      joint.shared = getfit(mod.joint.shared), 
      shared.only = getfit(mod.shared.only),
      separate = rbind(getfit(mod.separate.u),
            getfit(mod.separate.y)))[c(1, 3, 5,7, 2, 4, 6.,8),],keep.rownames = T),
      file = paste0('output/policypolitics/tables/variance_gof_',mod,'.csv'))
}

extraction_gof = fread('output/policypolitics/tables/variance_gof_Type_Purpose_Extractive.csv')

sdn = c('dic','waic','cpo')
extraction_gof[,(sdn):=lapply(.SD,round,2),.SDcols = sdn]
htmlTable::htmlTable(extraction_gof[order(dic),])
