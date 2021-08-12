
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
if(!require(INLA)){install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)}

packages = c('data.table','stringr','tidyverse','sf','lwgeom','ggthemes','tigris','lubridate','matrixStats','stargazer','MASS','aod','pbapply')
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

td = tempdir()
albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
admin_districts <- readRDS('policypolitics/prepped_inputs/admin_units_clean.RDS')
st_crs(admin_districts) <- st_crs(albersNA)


fs = readRDS('policypolitics/raw_curated_inputs/fs_PALS_cleaned_project_datatable.RDS')
#system('ln -s ~/Box/manitou/output/ ~/Documents/Github/manitou')
#system('ln -s ~/Box/manitou/input/ ~/Documents/Github/manitou')
#file.remove(list.files('output/policypolitics/tables/',pattern = 'coefs',full.names = T))
states = tigris::states(class = 'sf',year = '2017')
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

nf = fread('policypolitics/prepped_inputs/national_forest_covariates.csv')
nf$FOREST_ID = formatC(nf$FOREST_ID,width = 4,flag = 0)
nf$FOREST_NAME = admin_districts$FORESTNAME[match(nf$FOREST_ID,admin_districts$FOREST_ID)]
nf = nf[congress %in% 108:115,]

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



nf$Num_Eco_Sections[nf$Num_Eco_Sections==0] <- 1
nf = nf[order(FOREST_ID,get(period_type)),]
nf = nf[, zoo::na.locf(.SD, na.rm = FALSE),by = .(FOREST_ID)]

# nf$Prop_Extraction_Employ = nf$Prop_Forestry_Employ + nf$Prop_Mining_Employ  
#  nf$Perc_Extraction_Employ = nf$Prop_Extraction_Employ*100
# nf$Prop_Outdoor_Employ = nf$Prop_HuntingFishing + nf$Prop_Recreation 
# nf$Wilderness_Prop = as.numeric(nf$Wilderness_Prop)
# nf$Wilderness_Perc = nf$Wilderness_Prop * 100
# nf$Limited_Use_Prop = as.numeric(nf$Limited_Use_Prop)
# nf$Prop_WUI_Housing = as.numeric(nf$Prop_WUI_Housing)
# nf$Perc_WUI_Housing = 100 * nf$Prop_WUI_Housing
# nf$Burned_Prop_Past5yrs = as.numeric(nf$Burned_Prop_Past5yrs)
# nf$Burned_Perc_Past5yrs = nf$Burned_Prop_Past5yrs * 100
# nf$ACRES = admin_districts$GIS_ACRES[match(nf$FOREST_ID,admin_districts$FOREST_ID)]
# nf$demCongress[nf$demCongress==2] <- 0
# nf$congress = as.character(nf$congress)

# nf$mrp_mean = nf$mrp_mean * -1

# nf$Ln_ACRES = log(nf$ACRES)
# nf$Ln_AVERAGE_YEARLY_VISITS = log(nf$Average_Yearly_Visits) 


nf$USFS_REGION = admin_districts$REGION[match(nf$FOREST_ID,admin_districts$FOREST_ID)]

library(zoo)
nf = nf[order(FOREST_ID,congress),]
nf = nf[order(FOREST_ID,congress), zoo::na.locf(.SD, na.rm = FALSE),by = .(FOREST_ID)]
nf = nf[order(FOREST_ID,congress), na.locf(.SD, na.rm = FALSE,fromLast=TRUE),by = .(FOREST_ID)]
nf$congress = as.numeric(nf$congress)
nf$ln_County_naturalresource_GDP_1M = log(nf$NaturalResources1M+1)
nf$Prop_Extraction_Employ = nf$Prop_NaturalResourceEmployment
nf$Perc_Extraction_Employ = nf$Prop_Extraction_Employ * 100
center_continuous_cov = TRUE

nf= nf[nf$FOREST_ID%in%admin_districts$FOREST_ID,]

if(period_type == 'congress'){
  all_combos = expand.grid(FOREST_ID = sort(unique(admin_districts$FOREST_ID)),congress = 109:115,DECISION_TYPE = c('CE','EA','EIS'))
}
if(period_type !='congress'){
  all_combos = expand.grid(FOREST_ID = sort(unique(admin_districts$FOREST_ID)),CALENDAR_YEAR = start_year:end_year,DECISION_TYPE = c('CE','EA','EIS'))
}

all_combos = data.table(all_combos)
subtypes = unique(counts_by_type$Project_Type)

nf$`Avg_MBF_Cut_1999-2004`[is.na(nf$`Avg_MBF_Cut_1999-2004`)]<-0
nf$Ln_Avg_MBF_Cut_1999_2004 = log(nf$`Avg_MBF_Cut_1999-2004`+0.001)

nf$ln_Receipts_Extraction_1M_P4 <- log(nf$Receipts_TimberMineralsGrazing_P4/1e6+1)
nf$ln_Receipts_Recreation_1M_P4 <- log(nf$Receipts_Recreation_P4/1e6+1)

nf = nf[order(FOREST_ID,CALENDAR_YEAR),Unemp_Rate:=lag(LAU_October),by = .(FOREST_ID)]
nf$STATE = admin_districts$STATE[match(nf$FOREST_ID,admin_districts$FOREST_ID)]

dist_by_year = full_join(admin_districts,nf[,.(FOREST_ID,LCV_annual,CALENDAR_YEAR)])
dist_by_year = dist_by_year[!is.na(dist_by_year$CALENDAR_YEAR),]

dist_by_year = full_join(admin_districts,nf[,.(FOREST_ID,Unemp_Rate ,CALENDAR_YEAR)])
dist_by_year = dist_by_year[!is.na(dist_by_year$CALENDAR_YEAR),]

base_linear_form = Y ~ -1 + mu.u + mu.y + 
  u_Unemp_Rate +  u_Perc_Extraction_Employ + u_ln_County_naturalresource_GDP_1M + 
  u_ln_Receipts_Extraction_1M_P4 + 
  u_Ln_ACRES + u_Wilderness_Perc + 
  u_Burned_Perc_Past5yrs  + 
  u_Ln_AVERAGE_YEARLY_VISITS + u_Count_EorT_Species + 
  u_Perc_WUI_Housing +
  u_demPres + u_demCongress + 
  u_mrp_mean +
  u_LCV_annual + 
  y_Unemp_Rate +  y_Perc_Extraction_Employ + y_ln_County_naturalresource_GDP_1M + 
  y_ln_Receipts_Extraction_1M_P4 + 
  y_Ln_ACRES + y_Wilderness_Perc + 
  y_Burned_Perc_Past5yrs  + 
  y_Ln_AVERAGE_YEARLY_VISITS + y_Count_EorT_Species + 
  y_Perc_WUI_Housing + 
  y_demPres + y_demCongress +
  y_mrp_mean +
  y_LCV_annual 

form0 = update.formula(base_linear_form, ~ . + 
                         f(u_forest_id,model = 'iid',hyper = iid_prior_u) + 
                         f(u_congress_id,model = 'iid',hyper = iid_prior_u) + 
                         f(u_region_id, model = 'iid',hyper = iid_prior_u) + 
                         f(u_state_id, model = 'iid',hyper = iid_prior_u) + 
                         f(y_forest_id,model = 'iid',hyper = iid_prior_y) +
                         f(y_congress_id,model = 'iid',hyper = iid_prior_y) +
                         f(y_region_id,model = 'iid',hyper = iid_prior_y) +
                         f(y_state_id,model = 'iid',hyper = iid_prior_y) )

form0x = update.formula(form0, ~ . + u_Unemp_Rate:u_LCV_annual + y_Unemp_Rate:y_LCV_annual)

list_of_forms = grep('form[0-2]',ls(),value=T)
raw_vars = unlist(lapply(list_of_forms,function(x)  grep('^u_',str_split(as.character(get(x)[[3]])[2],pattern = '\\s\\+\\s')[[1]],value=T) ))

uvars = grep('^u_',raw_vars,value = T)

uvars_no_int = grep(':',uvars,value = T,invert = T)
uvars_int = grep(':',uvars,value = T,invert = F)

uvars_interaction_products = as.data.table(sapply(str_split(uvars_int ,':'),function(x) rowProds(as.matrix(nf[FOREST_ID %in% fs$FOREST_ID &CALENDAR_YEAR %in% start_year:end_year,gsub('u_','',(unlist(x))),with = F]))))
names(uvars_interaction_products) <- gsub('u_','',uvars_int)

nf = nf[nf$congress%in%109:115,]

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

idat$u_year_id = c(temp_dt$CALENDAR_YEAR,rep(NA,length(y)))
idat$y_year_id =   c(rep(NA,length(y)),idat$u_year_id[1:length(u)])

idat$u_state_id = c(temp_dt$STATE,rep(NA,length(y)))
idat$y_state_id = c(rep(NA,length(y)),idat$u_state_id[1:length(u)])

idat$u_region_id = c(formatC(temp_dt$USFS_REGION,width = 2,flag=0),rep(NA,length(y)))
idat$y_region_id = c(rep(NA,length(y)),idat$u_region_id[1:length(u)])

#### binomial EIS / [EA+EIS] model coefficients
#idat$y_FOREST_LEVEL_DECISIONS = c(rep(NA,narep),scale(temp_dt$FOREST_LEVEL_DECISIONS))
idat$y_Unemp_Rate = c(rep(NA,narep),scale(temp_dt$Unemp_Rate))
#idat$y_Unemp_Rate_L1 = c(rep(NA,narep),scale(temp_dt$Unemp_Rate_L1))
idat$y_ln_Receipts_Extraction_1M_P4 = c(rep(NA,narep),scale(temp_dt$ln_Receipts_Extraction_1M_P4))

idat$y_Perc_Extraction_Employ = c(rep(NA,narep),scale(temp_dt$Perc_Extraction_Employ))
#idat$y_Perc_Extraction_Employ_L1 = c(rep(NA,narep),scale(temp_dt$Perc_Extraction_Employ_L1))
idat$y_Ln_ACRES = c(rep(NA,narep),scale(temp_dt$Ln_ACRES))
idat$y_Wilderness_Perc = c(rep(NA,narep),scale(temp_dt$Wilderness_Perc)) 
idat$y_Burned_Perc_Past5yrs = c(rep(NA,narep),scale(temp_dt$Burned_Perc_Past5yrs)) 
idat$y_Ln_AVERAGE_YEARLY_VISITS = c(rep(NA,narep),scale(temp_dt$Ln_AVERAGE_YEARLY_VISITS)) 
idat$y_Count_EorT_Species= c(rep(NA,narep),scale(temp_dt$Count_EorT_Species)) 

#idat$y_democrat = c(rep(NA,narep),scale(temp_dt$democrat))  
idat$y_LCV_annual= c(rep(NA,narep),scale(temp_dt$LCV_annual))
idat$y_mrp_mean= c(rep(NA,narep),scale(temp_dt$mrp_mean))

#idat$y_nominate_dim1 = c(rep(NA,narep),scale(temp_dt$nominate_dim1))
#idat$y_nominate_dim1_x_y_democrat =  c(rep(NA,narep),scale(temp_dt$nominate_dim1) * scale(temp_dt$democrat))
idat$y_demPres = c(rep(NA,narep),temp_dt$demPres)
idat$y_demCongress = c(rep(NA,narep),temp_dt$demCongress)

idat$y_Perc_WUI_Housing = c(rep(NA,narep),scale(temp_dt$Perc_WUI_Housing))

idat$y_ln_County_naturalresource_GDP_1M = c(rep(NA,narep),scale(temp_dt$ln_County_naturalresource_GDP_1M))
idat$y_Total_Receipts_4yr_Change_Perc = c(rep(NA,narep),scale(temp_dt$Total_Receipts_4yr_Change_Perc))
idat$u_Unemp_Rate = c(scale(temp_dt$Unemp_Rate),rep(NA,narep))

temp_dt$ln_Receipts_Extraction_1M_P4[is.na(temp_dt$ln_Receipts_Extraction_1M_P4)] <- 0
idat$u_ln_Receipts_Extraction_1M_P4 = c(scale(temp_dt$ln_Receipts_Extraction_1M_P4),rep(NA,narep))

idat$u_Perc_Extraction_Employ = c(scale(temp_dt$Perc_Extraction_Employ),rep(NA,narep))

idat$u_Perc_WUI_Housing = c(scale(temp_dt$Perc_WUI_Housing),rep(NA,narep))
idat$u_Ln_ACRES = c(scale(temp_dt$Ln_ACRES),rep(NA,narep))
idat$u_Wilderness_Perc = c(scale(temp_dt$Wilderness_Perc),rep(NA,narep)) 
idat$u_Burned_Perc_Past5yrs = c(scale(temp_dt$Burned_Perc_Past5yrs),rep(NA,narep)) 
idat$u_Ln_AVERAGE_YEARLY_VISITS = c(scale(temp_dt$Ln_AVERAGE_YEARLY_VISITS),rep(NA,narep)) 
idat$u_Count_EorT_Species= c(scale(temp_dt$Count_EorT_Species),rep(NA,narep)) 

#idat$u_democrat = c(scale(temp_dt$democrat),rep(NA,narep))  
idat$u_LCV_annual= c(scale(temp_dt$LCV_annual),rep(NA,narep)) 
idat$u_mrp_mean = c(scale(temp_dt$mrp_mean),rep(NA,narep)) 

idat$u_demPres = c(temp_dt$demPres,rep(NA,narep))
idat$u_demCongress = c(temp_dt$demCongress,rep(NA,narep))

idat$u_ln_County_naturalresource_GDP_1M = c(scale(temp_dt$ln_County_naturalresource_GDP_1M),rep(NA,narep))

# idat$u_ln_County_naturalresource_GDP_1M_L1 = c(scale(temp_dt$ln_County_naturalresource_GDP_1M_L1),rep(NA,narep))
idat$n = n;idat$y = y;idat$u = u;


pcount = fs[fs$congress>=109&fs$congress<=115,]


counts_by_type[Project_Type=='Type_Purpose_Extractive',][order(-N),][CALENDAR_YEAR==start_year&FOREST_ID=='0302',]
counts_by_type[Project_Type=='Type_Purpose_Extractive',sum(N),by=.(CALENDAR_YEAR,FOREST_ID)][order(-V1),][CALENDAR_YEAR==start_year&FOREST_ID=='0302',]


counts_by_type$UNIT = admin_districts$FORESTNAME[match(counts_by_type$FOREST_ID,admin_districts$FOREST_ID)]

counts_by_type[Project_Type=='Type_Purpose_Extractive',][,sum(N)]

subtypes = grep('Extract',subtypes,value = T)

u.sdres <- sd(idat$u,na.rm = T)
y.sdres <- sd(idat$y/idat$u,na.rm=T)

pc.prec.u = list(prec = list(prior = "pc.prec", param = c(3*u.sdres, 0.01)))
pc.prec.y = list(prec = list(prior = "pc.prec", param = c(3*y.sdres, 0.01)))
bprior <- list(prior = 'gaussian', param = c(0,1))

#cinla <- list(strategy = 'adaptive', int.strategy = 'eb') 
cres <- list(return.marginals.predictor = FALSE, 
             return.marginals.random = FALSE)

idat$u_forest_id <- forest_index$index[match(idat$u_forest_id,forest_index$forest_id)]
idat$y_forest_id <- forest_index$index[match(idat$y_forest_id,forest_index$forest_id)] + nrow(forest_index)


idat$u_region_id <- region_index$index[match(idat$u_region_id,region_index$region_id)]
idat$y_region_id <- region_index$index[match(idat$y_region_id,region_index$region_id)] + nrow(region_index)


idat$u_state_id <- state_index$index[match(idat$u_state_id,state_index$state_id)]
idat$y_state_id <- state_index$index[match(idat$y_state_id,state_index$state_id)] + nrow(state_index)


idat$u_congress_id <- congress_index$index[match(idat$u_congress_id,congress_index$congress_id)]
idat$y_congress_id <- congress_index$index[match(idat$y_congress_id,congress_index$congress_id)] + nrow(congress_index)



HN.prior = "expression:
  tau0 = 0.001;
  sigma = exp(-theta/2);
  log_dens = log(2) - 0.5 * log(2 * pi) + 0.5 * log(tau0);
  log_dens = log_dens - 0.5 * tau0 * sigma^2;
  log_dens = log_dens - log(2) - theta / 2;
  return(log_dens);  
"
HT.prior = "expression:
  sigma = exp(-theta/2);
  nu = 3;
  log_dens = 0 - 0.5 * log(nu * pi) - (-0.1207822);
  log_dens = log_dens - 0.5 * (nu + 1) * log(1 + sigma * sigma);
  log_dens = log_dens - log(2) - theta / 2;
  return(log_dens);
"

UN.prior = "expression:
  log_dens = 0 - log(2) - theta / 2;
  return(log_dens);
"

HC.prior  = "expression:
  sigma = exp(-theta/2);
  gamma = 25;
  log_dens = log(2) - log(pi) - log(gamma);
  log_dens = log_dens - log(1 + (sigma / gamma)^2);
  log_dens = log_dens - log(2) - theta / 2;
  return(log_dens);
"



 nb_mod = glm.nb(u~ -1 + mu.u + u_Unemp_Rate + u_Perc_Extraction_Employ + u_ln_County_naturalresource_GDP_1M +
   u_ln_Receipts_Extraction_1M_P4 + 
   u_Burned_Perc_Past5yrs + 
    u_mrp_mean +
   u_LCV_annual,data = lapply(idat,function(x) x[1:length(idat$u)]))

 u.sdres <- sd(residuals(nb_mod))
 
temp_dat = as.data.frame(lapply(idat,function(x) x[length(idat$u)+{1:length(idat$y)}]))
keep_index = which(!is.na(idat$y))
drop_index = which(is.na(idat$y))
temp_dat = temp_dat[keep_index,]
temp_dat$y = idat$y[keep_index]
temp_dat$u = idat$u[keep_index]

bin_mod = betabin(cbind(y,u-y) ~ 1 + 
                    y_Unemp_Rate + 
                 y_Perc_Extraction_Employ + 
                    y_ln_County_naturalresource_GDP_1M +
                 y_ln_Receipts_Extraction_1M_P4 + 
             
               y_Burned_Perc_Past5yrs + 
           
                y_mrp_mean +
                 y_LCV_annual,~1,data =  temp_dat,control = list(maxit = 10e3))

y.sdres <- sd(residuals(bin_mod))
u.resp <- sd(idat$u,na.rm = T)
y.resp <- sd(idat$y/idat$u,na.rm=T)


#pcprior <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))
famcontrol = list(list(prior = "pcprec", param = c(u.resp,0.01)),
                  list(prior = "pcprec", param = c(y.resp,0.01)))

prior.list = list(
  loggamma0.5 = list(prec = list(prior = "loggamma", param = c(0.5, 0.00005))),
  loggamma1 = list(prec = list(prior = "loggamma", param = c(1, 0.00005))),
  loggamma5 = list(prec = list(prior = "loggamma", param = c(5, 0.00005))),
  half.normal = list(prec = list(prior = HN.prior)),
  half.cauchy = list(prec = list(prior = HC.prior)),
  h.t = list(prec = list(prior = HT.prior)),
  uniform = list(prec = list(prior = UN.prior)),
  pc.prec.1 = list(prec = list(prior = "pc.prec", param = c(1,0.01))),
  pc.prec.3 = list(prec = list(prior = "pc.prec", param = c(3, 0.01))),
  pc.prec.5 = list(prec = list(prior = "pc.prec", param = c(5, 0.01))),
  pc.prec.10 = list(prec = list(prior = "pc.prec", param = c(10, 0.01))),
  pc.prec.used = list(prec= list(prior = "pc.prec", param = c(u.sdres,0.01)),
                      prec = list(prior = "pc.prec", param = c(y.sdres,0.01)))
) 

mod_list = pblapply(prior.list,function(f) {print(f);gc();
  if(length(f)==1){iid_prior_u = iid_prior_y = f}
  if(length(f)>1){
  iid_prior_u <- list(prec= list(prior = "pc.prec", param = c(3*u.sdres,0.01)))
  iid_prior_y <- list(prec = list(prior = "pc.prec", param = c(3*y.sdres,0.01)))
  }
  tmod = inla(form0x,family = c('nbinomial', 'betabinomial'),Ntrials = idat$u,
       control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
       control.family = famcontrol,
       control.results = cres,
       data=idat, control.compute = list(waic=TRUE,return.marginals=F),
       control.predictor=list(compute=TRUE),verbose=F)
  tmod$prior = f
  tmod
},cl = 1)

hpars = lapply(mod_list,'[[','summary.hyperpar')

hypers = rbindlist(hpars)
hypers$prior = c(mapply(function(x,y) rep(x,y), x = names(hpars),y = sapply(hpars,nrow),SIMPLIFY = T))
hypers$param = unlist(lapply(hpars,rownames))

hypers$prior[hypers$prior=='pc.prec.10'] <- 'PC prior (10)'
hypers$prior[hypers$prior=='pc.prec.3'] <- 'PC prior (3)'
hypers$prior[hypers$prior=='pc.prec.1'] <- 'PC prior (1)'
hypers$prior[hypers$prior=='pc.prec.5'] <- 'PC prior (5)'
require(forcats)

hypers$prior = as.factor(hypers$prior)
hypers$prior = fct_recode(hypers$prior,'PC prior, sd(resid.)'='pc.prec.used',
                        'Half t' ="h.t" , 'Half Cauchy'="half.cauchy"   ,'Half normal'  = "half.normal" ,         
                         'Log-gamma (0.5)'= "loggamma0.5" ,  'Log-gamma (1)'="loggamma1"  ,   'Log-gamma (5)'="loggamma5"  ,            
                  'Uniform'= "uniform")

hypers$prior <- fct_relevel(hypers$prior, "Uniform"  , "Half normal","Half t","Half Cauchy",
                            "Log-gamma (0.5)" , "Log-gamma (1)" , "Log-gamma (5)"   ,       
                            "PC prior (1)" ,  "PC prior (3)" , "PC prior (5)" ,   "PC prior (10)" , "PC prior, sd(residuals)" )

hypers$param <- gsub('Precision for u_(.+)_id$','\\1 (# projects)',hypers$param)
hypers$param <- gsub('Precision for y_(.+)_id$','\\1 (CE ratio)',hypers$param)


gg_priors = ggplot(hypers[!grepl('dispersion',param),]) + 
  geom_errorbarh(aes(xmin = `0.025quant`,xmax = `0.975quant`,y=prior)) + 
  facet_wrap(~param,scales='free',ncol = 2) + theme_bw() + ylab('Prior distribution') +
  xlab('Hyperparameter precision estimates')+
  ggtitle('Sensitivity of iid latent effects to prior distribution',
          '(scale varies by panel)')

ggsave(filename = 'policypolitics/tables_figures/figures/figures_in_paper/figureA1_prior_sensitivity.png',plot = gg_priors,width= 7,height = 9,dpi = 500,units = 'in')


res = rbindlist(lapply(seq_along(mod_list),function(x) data.table(prior = names(mod_list)[x],
                                                                  mod_list[[x]]$summary.fixed,coef = rownames(mod_list[[x]]$summary.fixed))))
res = res[grepl('Unemp|LCV',coef),]

res$lik = ifelse(grepl('^u_',res$coef),'# projects','CE ratio')

res$lik <- fct_inorder(as.factor(res$lik))
res$coef = ifelse(grepl(':',res$coef),'Unemp. % x LCV score',ifelse(grepl('Unemp',res$coef),'Unemployment %','LCV score'))

res$prior[res$prior=='pc.prec.10'] <- 'PC prior (10)'
res$prior[res$prior=='pc.prec.3'] <- 'PC prior (3)'
res$prior[res$prior=='pc.prec.1'] <- 'PC prior (1)'
res$prior[res$prior=='pc.prec.5'] <- 'PC prior (5)'

res$prior = fct_recode(res$prior,'PC prior, sd(resid.)'='pc.prec.used',
                          'Half t' ="h.t" , 'Half Cauchy'="half.cauchy"   ,'Half normal'  = "half.normal" ,         
                          'Log-gamma (0.5)'= "loggamma0.5" ,  'Log-gamma (1)'="loggamma1"  ,   'Log-gamma (5)'="loggamma5"  ,            
                          'Uniform'= "uniform")

res$prior <- fct_relevel(res$prior, "Uniform"  , "Half normal","Half t","Half Cauchy",
                            "Log-gamma (0.5)" , "Log-gamma (1)" , "Log-gamma (5)"   ,       
                            "PC prior (1)" ,  "PC prior (3)" , "PC prior (5)" ,   "PC prior (10)" , "PC prior, sd(residuals)" )


res$coef = as.factor(res$coef)
res$coef = fct_relevel(res$coef,'Unemployment %',after=1L)

focal_priors = ggplot(data = res,aes(x = prior,y = mean,ymin = `0.025quant`,ymax = `0.975quant`)) +
  geom_errorbar() + #geom_point() + 
  geom_hline(yintercept = 0,lty = 2,col = 'grey50')+
  facet_wrap(coef~lik,ncol = 2) + coord_flip() + theme_bw() +xlab('Prior distribution')+
  ylab('95% credible interval') + ggtitle('Focal parameter estimates w/ \ndifferent priors in iid latent effects')

ggsave(filename = 'policypolitics/tables_figures/figures/figures_in_paper/figureA2_focal_prior_sensitivity.png',plot = focal_priors,width= 5,height = 6.25,dpi = 500,units = 'in')








