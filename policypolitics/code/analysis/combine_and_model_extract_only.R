
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
#if(!require(INLA)){install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)}
require(INLA)
packages = c('data.table','stringr','tidyverse','sf','lwgeom','ggthemes','tigris','lubridate')
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


td = tempdir()
albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

admin_districts <- readRDS('policypolitics/prepped/admin_units_clean.RDS')
fs = readRDS('policypolitics/prepped/fs_PALS_cleaned_project_datatable.RDS')

#system('ln -s ~/Box/manitou/output/ ~/Documents/Github/manitou')
#system('ln -s ~/Box/manitou/input/ ~/Documents/Github/manitou')
file.remove(list.files('output/policypolitics/tables/',pattern = 'coefs',full.names = T))
states = tigris::states(class = 'sf')
states <- st_transform(states,crs = st_crs(albersNA))
test = st_within(st_centroid(admin_districts),states)
admin_districts$STATE = states[unlist(test),]$STUSPS

totcount = fs[,.N,by=.(FOREST_ID)]
admin_districts = left_join(admin_districts,totcount)

us_counties = counties(class = 'sf',year = 2017)
us_counties = st_transform(us_counties ,albersNA)


congress2015 = tigris::congressional_districts(year = 2015,class = 'sf')
congress2015  = st_transform(congress2015 ,albersNA)

states = tigris::states(class = 'sf',year = 2015)
states = st_transform(states,albersNA)
states <- st_crop(states,st_bbox(admin_districts))
us_counties <- st_crop(us_counties,st_bbox(admin_districts))

nf = fread('policypolitics/prepped/national_forest_covariates.csv')
nf$FOREST_ID = formatC(nf$FOREST_ID,width = 4,flag = 0)

nf$FOREST_NAME = admin_districts$FORESTNAME[match(nf$FOREST_ID,admin_districts$FOREST_ID)]
nf = nf[congress %in% 108:115,]

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

nf$Num_Eco_Sections[nf$Num_Eco_Sections==0] <- 1
nf = nf[order(FOREST_ID,get(period_type)),]
nf = nf[, zoo::na.locf(.SD, na.rm = FALSE),by = .(FOREST_ID)]

nf$Prop_Extraction_Employ = nf$Prop_Forestry_Employ + nf$Prop_Mining_Employ
nf$Perc_Extraction_Employ = nf$Prop_Extraction_Employ*100
nf$Prop_Outdoor_Employ = nf$Prop_HuntingFishing + nf$Prop_Recreation
nf$Wilderness_Prop = as.numeric(nf$Wilderness_Prop)
nf$Wilderness_Perc = nf$Wilderness_Prop * 100
nf$Prop_WUI_Housing = as.numeric(nf$Prop_WUI_Housing)
nf$Perc_WUI_Housing = 100 * nf$Prop_WUI_Housing
nf$Burned_Prop_Past5yrs = as.numeric(nf$Burned_Prop_Past5yrs)
nf$Burned_Perc_Past5yrs = nf$Burned_Prop_Past5yrs * 100
nf$ACRES = admin_districts$GIS_ACRES[match(nf$FOREST_ID,admin_districts$FOREST_ID)]
nf$demCongress[nf$demCongress==2] <- 0
nf$congress = as.character(nf$congress)
nf$nominate_dim1  = nf$nominate_dim1  * -1
nf$nominate_dim2  = nf$nominate_dim2  * -1
nf$mrp_mean = nf$mrp_mean * -1

nf$ComLCV = (nf$nrComLCV+nf$agComLCV)/2
nf$ChairLCV = (nf$nrChairLCV + nf$agChairLCV)/2

nf$Ln_ACRES = log(nf$ACRES)
nf$Ln_AVERAGE_YEARLY_VISITS = log(nf$Average_Yearly_Visits)
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

nf$ln_Receipts_Extraction_1M_P4 <- log(nf$Receipts_TimberMineralsGrazing_P4/1e6+1)
nf$ln_Receipts_Recreation_1M_P4 <- log(nf$Receipts_Recreation_P4/1e6+1)

#setnames(nf,'LAU','Unemp_Rate')
nf = nf[order(FOREST_ID,CALENDAR_YEAR),Unemp_Rate:=lag(LAU_October),by = .(FOREST_ID)]
nf$STATE = admin_districts$STATE[match(nf$FOREST_ID,admin_districts$FOREST_ID)]

dist_by_year = full_join(admin_districts,nf[,.(FOREST_ID,LCV_annual,CALENDAR_YEAR)])
dist_by_year = dist_by_year[!is.na(dist_by_year$CALENDAR_YEAR),]
# 
 tt = ggplot() + geom_sf(data = dist_by_year,aes(fill = LCV_annual,colour = LCV_annual)) + 
   facet_wrap(~CALENDAR_YEAR,ncol = 3) + 
   ggtitle('LCV annual by year and forest') + theme_map() + scale_fill_viridis_c() + 
   scale_colour_viridis_c()
ggsave(tt,dpi = 300,filename = 'policypolitics/tables_figures/figures/LCV_by_forest_and_year.png',height = 12,width = 8,units = 'in')


dist_by_year = full_join(admin_districts,nf[,.(FOREST_ID,Unemp_Rate ,CALENDAR_YEAR)])
dist_by_year = dist_by_year[!is.na(dist_by_year$CALENDAR_YEAR),]
 tt = ggplot() + geom_sf(data = dist_by_year,aes(fill = Unemp_Rate,colour = Unemp_Rate)) + 
   facet_wrap(~CALENDAR_YEAR,ncol = 3) + 
   ggtitle('Unemp. % by year and forest') + theme_map() + scale_fill_viridis_c() + 
   scale_colour_viridis_c()
ggsave(tt,dpi = 300,filename = 'policypolitics/tables_figures/figures/Unemp_by_forest_and_year.png',height = 12,width = 8,units = 'in')


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
    f(u_forest_id,model = 'iid',hyper = pc.prec.used[1]) + 
    f(u_congress_id,model = 'iid',hyper = pc.prec.used[1]) + 
    f(u_region_id, model = 'iid',hyper = pc.prec.used[1]) + 
    f(u_state_id, model = 'iid',hyper = pc.prec.used[1]) + 
    f(y_forest_id,model = 'iid',hyper = pc.prec.used[2]) +
    f(y_congress_id,model = 'iid',hyper = pc.prec.used[2]) +
    f(y_region_id,model = 'iid',hyper = pc.prec.used[2]) +
    f(y_state_id,model = 'iid',hyper = pc.prec.used[2]))
  
  form0x = update.formula(form0, ~ . + u_Unemp_Rate:u_LCV_annual + y_Unemp_Rate:y_LCV_annual)
  form1 = update.formula(form0, ~ . - u_LCV_annual - y_LCV_annual + u_percentD_H + y_percentD_H)
  form1x = update.formula(form1, ~ . + u_Unemp_Rate:u_percentD_H + y_Unemp_Rate:y_percentD_H)
  form2 = update.formula(form0, ~ . - u_LCV_annual - y_LCV_annual + u_democrat + y_democrat)
  form2x =  update.formula(form2, ~ . + u_Unemp_Rate:u_democrat + y_Unemp_Rate:y_democrat)
  

list_of_forms = grep('form[0-2]',ls(),value=T)
raw_vars = unlist(lapply(list_of_forms,function(x)  grep('^u_',str_split(as.character(get(x)[[3]])[2],pattern = '\\s\\+\\s')[[1]],value=T) ))
library(matrixStats)
uvars = grep('^u_',raw_vars,value = T)
library(stargazer)
uvars_no_int = grep(':',uvars,value = T,invert = T)
uvars_int = grep(':',uvars,value = T,invert = F)

uvars_interaction_products = as.data.table(sapply(str_split(uvars_int ,':'),function(x) rowProds(as.matrix(nf[FOREST_ID %in% fs$FOREST_ID &CALENDAR_YEAR %in% start_year:end_year,gsub('u_','',(unlist(x))),with = F]))))
names(uvars_interaction_products) <- gsub('u_','',uvars_int)

nf = nf[nf$congress%in%109:115,]

coef_vals = cbind(nf[FOREST_ID %in% fs$FOREST_ID & CALENDAR_YEAR %in% start_year:end_year,c('FOREST_ID','CALENDAR_YEAR',unique(gsub('u_','',uvars_no_int))),with = F],uvars_interaction_products)
swap_names = names(coef_vals)
swap_names = as.factor(swap_names)

swap_names = fct_recode(swap_names,
                        '(intercept)' = 'mu.u',
                        '% wilderness area' = 'Wilderness_Perc',
                        '% dem. vote share' = 'percentD_H',
                        '% housing in WUI' = 'Perc_WUI_Housing',
                        'Public ideology' = 'mrp_mean',
                        '# listed species' = 'Count_EorT_Species','ln(forest acreage)'='Ln_ACRES',
                        'ln(Resource receipts, last 4 yrs)' = 'ln_Receipts_Extraction_1M_P4',
                        'ln(Recreation receipts, last 4 yrs)' = 'ln_Receipts_Recreation_1M_P4',
                        '% burned last 5 years'='Burned_Perc_Past5yrs','LCV annual score'='LCV_annual',
                        'Unemployment %' = 'Unemp_Rate','% extraction employ.' = 'Perc_Extraction_Employ',
                        'ln(yearly visitation)' = 'Ln_AVERAGE_YEARLY_VISITS','ln($1M county NR GDP)' = 'ln_County_naturalresource_GDP_1M',
                        'Democratic president' = 'demPres','Democratic congress' = 'demCongress',
                        "% dem. vote x unemp. %" = "Unemp_Rate:percentD_H"   ,
                        'Dem. rep.' = 'democrat','Dem. rep. x unemp.  %' = "Unemp_Rate:democrat" ,
                        'LCV annual x unemp. %' = 'Unemp_Rate:LCV_annual',
                        'House committee LCV' = 'ComLCV','House chair LCV' = 'ChairLCV')
colnames(coef_vals) <- as.character(swap_names)


coef_html_table = stargazer(coef_vals,summary = T,out = 'policypolitics/tables_figures/tables/variable_summaries.html')

corr <- round(cor(coef_vals[,-c('FOREST_ID','CALENDAR_YEAR')],use = 'pairwise.complete.obs'), 2)

#install.packages("ggcorrplot")
require(ggcorrplot)
ggc = ggcorrplot(corr,method = 'circle',show.diag = F,type = 'upper',lab = F)
ggsave(plot = ggc,filename = 'policypolitics/tables_figures/figures/correlation_plot.png',width=  8,height = 8, units = 'in',dpi = 300)


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

pcount = fs[fs$congress>=109&fs$congress<=115,]

library(ggrepel)


counts_by_type[Project_Type=='Type_Purpose_Extractive',][order(-N),][CALENDAR_YEAR==start_year&FOREST_ID=='0302',]
counts_by_type[Project_Type=='Type_Purpose_Extractive',sum(N),by=.(CALENDAR_YEAR,FOREST_ID)][order(-V1),][CALENDAR_YEAR==start_year&FOREST_ID=='0302',]


counts_by_type$UNIT = admin_districts$FORESTNAME[match(counts_by_type$FOREST_ID,admin_districts$FOREST_ID)]

counts_by_type[Project_Type=='Type_Purpose_Extractive',][,sum(N)]

subtypes = grep('Extract',subtypes,value = T)



require(MASS)
require(aod)
nb_mod = glm.nb(u~ -1 + mu.u + u_Unemp_Rate + u_Perc_Extraction_Employ + u_ln_County_naturalresource_GDP_1M +
                  u_ln_Receipts_Extraction_1M_P4 + u_Ln_ACRES + u_Wilderness_Perc +
                  u_Burned_Perc_Past5yrs + u_Ln_AVERAGE_YEARLY_VISITS + u_Count_EorT_Species +
                  u_Perc_WUI_Housing + u_demPres + u_demCongress + u_mrp_mean +
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
                    y_Ln_ACRES + y_Wilderness_Perc +
                    y_Burned_Perc_Past5yrs + y_Ln_AVERAGE_YEARLY_VISITS + y_Count_EorT_Species +
                    y_Perc_WUI_Housing + 
                    y_demPres + y_demCongress + 
                    y_mrp_mean +
                    y_LCV_annual,~1,data =  temp_dat,control = list(maxit = 10e3))

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
  
  idat$u_forest_id <- forest_index$index[match(idat$u_forest_id,forest_index$forest_id)]
  idat$y_forest_id <- forest_index$index[match(idat$y_forest_id,forest_index$forest_id)] + nrow(forest_index)
  idat$uc_forest_id <-  idat$u_forest_id
  
  idat$u_region_id <- region_index$index[match(formatC(idat$u_region_id,width = 2,flag = 0),region_index$region_id)]
  idat$y_region_id <- region_index$index[match(formatC(idat$y_region_id,width = 2,flag = 0),region_index$region_id)] + nrow(region_index)
  idat$uc_region_id <-  idat$u_region_id
  
  idat$u_state_id <- state_index$index[match(idat$u_state_id,state_index$state_id)]
  idat$y_state_id <- state_index$index[match(idat$y_state_id,state_index$state_id)] + nrow(state_index)
  idat$uc_state_id <-  idat$u_state_id
  
  idat$u_congress_id <- congress_index$index[match(idat$u_congress_id,congress_index$congress_id)]
  idat$y_congress_id <- congress_index$index[match(idat$y_congress_id,congress_index$congress_id)] + nrow(congress_index)
  idat$uc_congress_id <-  idat$u_congress_id

  ######### not returning marginals greatly reduces object size #######
  cres <- list(return.marginals.predictor = FALSE, 
               return.marginals.random = FALSE)
  
  require(pbapply)
  mod_list = pblapply(list_of_forms,function(f) {print(f);gc();inla(get(f),
                                                                  family = c('nbinomial', 'betabinomial'),Ntrials = idat$u,
                                                                  control.fixed = list(expand.factor.strategy = "inla",prec = bprior),
                                                                  control.family = famcontrol,
                                                                  control.results = cres,
                                                                  data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE,config = TRUE),
                                                                  control.predictor=list(compute=TRUE),verbose=F)},cl = 5)
  names(mod_list) <- list_of_forms
    saveRDS(mod_list,paste0('policypolitics/model_objects/models_',subtypes,'.RDS'))

