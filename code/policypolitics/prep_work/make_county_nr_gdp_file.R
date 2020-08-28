library(data.table)
library(tidyverse)
library(stringr)
cgp = fread('input/bea_data/CAGDP2/CAGDP2__ALL_AREAS_2001_2018.csv',stringsAsFactors = F)
cgp$GeoFIPS = formatC(cgp$GeoFIPS,width = 5,flag = 0)

state_totals = cgp[grepl('000$',GeoFIPS),]
cgp = cgp[!grepl('000$',GeoFIPS),]

#test for alaska
#state_totals = state_totals[GeoFIPS=='02000']
#cgp = cgp[grepl('^02',GeoFIPS)]

cgp = cgp[LineCode%in%c(2,3,6,87),]
state_totals = state_totals[LineCode%in%c(2,3,6,87),]

cgp[,Region:=NULL]
cgp[,TableName:=NULL]
cgp[,LineCode:=NULL]
cgp[,GeoName:=NULL]
cgp[,Unit:=NULL]
cgp[,IndustryClassification:=NULL]

state_totals[,Region:=NULL]
state_totals[,TableName:=NULL]
state_totals[,LineCode:=NULL]
state_totals[,GeoName:=NULL]
state_totals[,Unit:=NULL]
state_totals[,IndustryClassification:=NULL]

cgp_dt = data.table::melt(cgp,id.vars = c('GeoFIPS','Description'))
cgp_dt$value <- as.numeric(cgp_dt$value)
cgp_dt2 = dcast(cgp_dt,GeoFIPS + variable ~ Description,id.vars = 'value')
names(cgp_dt2) <- c('CFIPS','YEAR','AgrForestFishHunt1k','MineOilGas1k','PrivateIndustry','NaturalResources1k')
cgp_dt2$YEAR = as.numeric(as.character(cgp_dt2$YEAR))
cgp_dt2$STATE = paste0(str_extract(cgp_dt2$CFIPS,'^[0-9]{2}'),'000')
cgp_dt2 = cgp_dt2[YEAR>=2002]

state_dt = data.table::melt(state_totals,id.vars = c('GeoFIPS','Description'))
state_dt$value <- as.numeric(state_dt$value)
state_dt2 = dcast(state_dt,GeoFIPS + variable ~ Description,id.vars = 'value')
names(state_dt2) <- c('STATE','YEAR','State_AgrForestFishHunt1k','State_MineOilGas1k','PrivateIndustry','State_NaturalResources1k')
state_dt2$YEAR = as.numeric(as.character(state_dt2$YEAR))
state_dt2 = state_dt2[YEAR>=2002]

cgp_dt2[is.na(AgrForestFishHunt1k),PI_Multiple:=PrivateIndustry/sum(PrivateIndustry,na.rm=T),by=.(YEAR,STATE)]
cgp_dt2[,AccountedAgrForestFishHunt1k:=sum(AgrForestFishHunt1k,na.rm=T),by=.(YEAR,STATE)]
cgp_dt2 = data.table(left_join(cgp_dt2,state_dt2[,.(STATE,YEAR,State_AgrForestFishHunt1k)]))
cgp_dt2[,UnaccountedAgrForestFishHunt1k:=State_AgrForestFishHunt1k-AccountedAgrForestFishHunt1k]
cgp_dt2[,Imputed_AgrForestFishHunt1k:=UnaccountedAgrForestFishHunt1k*PI_Multiple]
cgp_dt2[,AgrForestFishHunt1k:=ifelse(is.na(AgrForestFishHunt1k),Imputed_AgrForestFishHunt1k,AgrForestFishHunt1k)]

cgp_dt2[is.na(MineOilGas1k),PI_Multiple:=PrivateIndustry/sum(PrivateIndustry,na.rm=T),by=.(YEAR,STATE)]
cgp_dt2[,AccountedMineOilGas1k:=sum(MineOilGas1k,na.rm=T),by=.(YEAR,STATE)]
cgp_dt2 = data.table(left_join(cgp_dt2,state_dt2[,.(STATE,YEAR,State_MineOilGas1k)]))
cgp_dt2[,UnaccountedMineOilGas1k:=State_MineOilGas1k-AccountedMineOilGas1k]
cgp_dt2[,Imputed_MineOilGas1k:=UnaccountedMineOilGas1k*PI_Multiple]
cgp_dt2[,MineOilGas1k:=ifelse(is.na(MineOilGas1k),Imputed_MineOilGas1k,MineOilGas1k)]
cgp_dt2[,NaturalResources1k:=ifelse(is.na(NaturalResources1k),MineOilGas1k+AgrForestFishHunt1k,NaturalResources1k)]

fwrite(cgp_dt2,'input/cpb_data/naturalresource_gdp_by_county_2002-2018.csv')
