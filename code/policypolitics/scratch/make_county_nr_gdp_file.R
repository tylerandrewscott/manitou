#list.files('../../Downloads/CAGDP2/',patterh = 'ALL_AREAS')
require(data.table)
require(stringr)
cgp = fread('input/bea_data/CAGDP2/CAGDP2__ALL_AREAS_2001_2018.csv')
cgp$GeoFIPS = formatC(cgp$GeoFIPS,width = 5,flag = 0)
cgp = cgp[!grepl('000',GeoFIPS),]
cgp = cgp[IndustryClassification%in%c('11','21','11, 21')]

cgp[,Region:=NULL]
cgp[,TableName:=NULL]
cgp[,LineCode:=NULL]

cgp_melt = melt(cgp[,c('GeoFIPS','IndustryClassification',as.character(2001:2018)),with = F],id.vars = c('GeoFIPS','IndustryClassification'))
cgp_sum = cgp_melt[IndustryClassification!='11, 21',][,sum(as.numeric(value),na.rm = T),by=.(GeoFIPS,variable)]

cgp_sum$V1= cgp_sum$V1 * 1000
dt = cgp_sum
setnames(dt,c('GeoFIPS','variable','V1'),c('CFIPS','Year','County_naturalresource_GDP'))
dt = dt[order(CFIPS,Year),]
library(zoo)
dt[,County_naturalresource_GDP:=na.locf(County_naturalresource_GDP,na.rm = F),by = .(CFIPS)]
dt[,County_naturalresource_GDP:=na.locf(County_naturalresource_GDP,na.rm = F,fromLast = T),by = .(CFIPS)]
dt$STATE = str_remove(dt$GeoName,'.+\\,\\s')
dt$STATE = gsub('\\*','',dt$STATE)

fwrite(dt,'../manitou/input/cpb_data/naturalresource_gdp_by_county_2001-2018.csv')
