
td = tempdir()

base = 'https://www2.census.gov/programs-surveys/cbp/datasets/'
zpfls = paste0(base,2004:2019,'/cbp',str_extract(2004:2019,'[0-9]{2}$'),'co.zip')

path_list = pblapply(zpfls,function(z){
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(z, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('.txt',fname$Name,value=T))
fpath},cl = 4)

fls_read = lapply(path_list,fread)
fls_read_add_year = mapply(function(x,y) {x$year <- y;names(x) <- tolower(names(x));return(x)},
       x = fls_read,y = paste0('20',str_extract(lapply(path_list,basename),'[0-9]{2}')))
cbp_all = rbindlist(fls_read_add_year,use.names = T,fill = T)
cbp_all$CFIPS = paste0(formatC(cbp_all$fipstate,width = 2,flag = '0'),formatC(cbp_all$fipscty,width = 3,flag = '0'))
table(cbp_all$emp_nf)
setnames(cbp_all,c("naics","emp",'year'),c('NAICS',"Number_employees",'YEAR'))

codes_to_keep = c('113///',"114///","21----","71----","------")
cbp_all = cbp_all[NAICS %in% codes_to_keep,]
cbp_dt = dcast(cbp_all,CFIPS + YEAR ~ NAICS,value.var = 'Number_employees')
cbp_dt = cbp_dt[!grepl('000$',cbp_dt$CFIPS),]
setnames(cbp_dt,codes_to_keep,c('forestry_logging','fishing_hunting',"mining","recreation_entertainment","all_employees"))
table(cbp_dt$YEAR,is.na(cbp_dt$fishing_hunting))


simulate_blurred_response = function(x){
  round(ifelse(is.na(x),NA,ifelse(!is.na(as.numeric(x)),as.numeric(x),ifelse(x=='a',runif(length(x),1,19),ifelse(x=='b',runif(length(x),20,99),
                                                                                                                 ifelse(x=='c',runif(length(x),100,249),ifelse(x=='e',runif(length(x),250,499),ifelse(x=='f',runif(length(x),500,999),
                                                                                                                                                                                                      ifelse(x=='g',runif(length(x),1000,2499),ifelse(x=='h',runif(length(x),2500,4999),NA))))))))))}
vnames = c('all_employees','mining','recreation_entertainment',
           'forestry_logging','fishing_hunting')
cbp_dt = cbp_dt[,(vnames):=lapply(.SD,simulate_blurred_response),.SDcols=vnames]
cbp_dt
cbp_dt$Prop_Forestry_Employ = cbp_dt$forestry_logging/cbp_dt$all_employees
cbp_dt$Prop_Mining_Employ = cbp_dt$mining/cbp_dt$all_employees
cbp_dt$Prop_Recreation =  cbp_dt$recreation_entertainment/cbp_dt$all_employees
cbp_dt$Prop_HuntingFishing = cbp_dt$fishing_hunting/cbp_dt$all_employees
cbp_dt$Prop_NaturalResourceEmployment <- replace_na(cbp_dt$Prop_HuntingFishing,0) + replace_na(cbp_dt$Prop_Forestry_Employ,0)+replace_na(cbp_dt$Prop_Mining_Employ,0)
setnames(cbp_dt,'Year','YEAR')


summary(cbp_dt$Prop_NaturalResourceEmployment)


county_econ = data.table(left_join(county_econ,cbp_dt[,.(CFIPS,YEAR,Prop_NaturalResourceEmployment)]))
county_econ[order(CFIPS,YEAR),Prop_NaturalResourceEmployment:=zoo::na.locf(Prop_NaturalResourceEmployment,na.rm=F),by = .(CFIPS)]

setkey(county_econ,CFIPS)
setkey(county_over,CFIPS)
county_econ = county_econ[county_over,]
forest_weighted_econ = county_econ[,lapply(.SD,weighted.mean,w=prop_in_county,na.rm = T), by=.(FOREST_ID,YEAR),.SDcols = c("LAU_January","LAU_October","NaturalResources1M",'Prop_NaturalResourceEmployment')]
setnames(forest_weighted_econ,'YEAR','CALENDAR_YEAR')


source('~/Documents/census_key')

call = 'https://api.census.gov/data/2019/cbp?get=NAME,NAICS2017_LABEL,LFO_LABEL,EMPSZES_LABEL,ESTAB,PAYANN,PAYQTR1,EMP&for=county:*&NAICS2017=72&LFO=001&EMPSZES=001&key='
call2 = 'https://api.census.gov/data/2013/cbp?get=NAME,NAICS2017_LABEL,LFO_LABEL,EMPSZES_LABEL,ESTAB,PAYANN,PAYQTR1,EMP&for=county:*&NAICS2017=72&LFO=001&EMPSZES=001&key='
library(jsonlite)

fromJSON(paste0(call,census_key))












