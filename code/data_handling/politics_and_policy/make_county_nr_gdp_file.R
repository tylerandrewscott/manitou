list.files('../../Downloads/CAGDP2/',patterh = 'ALL_AREAS')

cgp = fread('../../Downloads/CAGDP2/CAGDP2__ALL_AREAS_2001_2018.csv')
cgp$GeoFIPS = formatC(cgp$GeoFIPS,width = 5,flag = 0)
cgp = cgp[!grepl('000',GeoFIPS),]
cgp = cgp[grepl('mining|forest|oil|gas',tolower(cgp$Description)),]
cgp = cgp[IndustryClassification =="11, 21",]
cgp[,Region:=NULL]
cgp[,TableName:=NULL]
cgp[,LineCode:=NULL]
cgp[,IndustryClassification:=NULL]


dt = data.table::melt(cgp,id.vars = c('GeoFIPS','GeoName','Description','Unit'))
dt$value[dt$value=='(D)'] <- NA
dt$value[dt$value=='(NA)'] <- NA
dt$value = as.numeric(dt$value)
dt$value = dt$value * 1000
dt[,Unit:=NULL]
setnames(dt,c('GeoFIPS','variable','value'),c('CFIPS','Year','County_naturalresource_GDP'))
dt = dt[order(CFIPS,Year),]
library(zoo)
dt[,County_naturalresource_GDP:=na.locf(County_naturalresource_GDP,na.rm = F),by = .(CFIPS)]
dt[,County_naturalresource_GDP:=na.locf(County_naturalresource_GDP,na.rm = F,fromLast = T),by = .(CFIPS)]
dt$STATE = str_remove(dt$GeoName,'.+\\,\\s')
dt$STATE = gsub('\\*','',dt$STATE)
table(dt$Year)
fwrite(dt,'../manitou/input/cpb_data/naturalresource_gdp_by_county_2001-2018.csv')
