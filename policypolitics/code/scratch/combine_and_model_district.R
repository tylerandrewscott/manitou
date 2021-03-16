library(lme4)
library(sjPlot)
library(glmnet)
library(lavaan)
library(cluster) 
library(INLA)
library(data.table)
library(stringr)
#fs = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vTPHD71fQvTZpNljqRNVI6lD4b26wW86ptP_dy3R_qWaxkXyOE2QCuyTzroL_38mw/pub?output=csv')
fs = fread('input/Copy of MYTR_National Level(1100).xlsx - Decision Data Set.csv')
#fs2 = fs
fs = fs2
fs = fs[fs$`UNIQUE DECISION?`=='Y',]
fs = fs[fs$`DECISION TYPE`!='PAD',]
fs = fs[!grepl('Visitor Center|Interpretive Center|Nursery|Air Center|Field Office|Recreation Area|Grassland|Mgt Unit|Scenic Area Unit',fs$`LMU (ACTUAL)`),]

fs$FISCAL_YEAR <- fs$`INITIATION FY`
fs$`LMU – DISTRICT`[fs$`LMU – DISTRICT`=="Buffalo Ranger District (11040306)"] <- "Blackrock Ranger District (11040306)"
fs$`LMU – DISTRICT`[fs$`LMU – DISTRICT`=="Powell Ranger District  (11010506)"] <- "Lochsa/Powell Ranger District (11011755)"
fs$`LMU – DISTRICT`[fs$`LMU – DISTRICT`=="Salmon River Ranger District (11050554)"] <- "Salmon-Scott Ranger District (11050554)"
fs$`LMU – DISTRICT`[fs$`LMU – DISTRICT`=="Paintrock Ranger District (11020204)"] <- "Salmon-Scott Ranger District (11020203)"
fs$`LMU – DISTRICT`[fs$`LMU – DISTRICT`=="Belt Creek Ranger District (11011503)"] <- "Belt Creek-White Sulphur Springs Ranger District (11011507)"


fs$DISTRICT_ID = str_extract(str_extract(fs$`LMU – DISTRICT`,'[0-9]{1,}'),'[0-9]{6}$')
fs$DISTRICT_ID = gsub('^0108','0111',fs$DISTRICT_ID)
fs$DISTRICT_ID = gsub('^0105','0117',fs$DISTRICT_ID)
fs$DISTRICT_ID = gsub('^0112','0115',fs$DISTRICT_ID)
fs$DISTRICT_ID[fs$DISTRICT_ID=='011702'] <- '011752'
fs$DISTRICT_ID[fs$DISTRICT_ID=='011703'] <- '011753'
fs$DISTRICT_ID[fs$DISTRICT_ID=='011501'] <- '011511'
fs$DISTRICT_ID[fs$DISTRICT_ID=='011504'] <- '011514'
fs$DISTRICT_ID[fs$DISTRICT_ID=='011104'] <- '011184'
fs$DISTRICT_ID[fs$DISTRICT_ID=='011502'] <- '011512'
fs$DISTRICT_ID[fs$DISTRICT_ID=='011102'] <- '011182'
fs$DISTRICT_ID[fs$DISTRICT_ID=='011706'] <- '011755'

fs$FOREST_ID <- str_extract(fs$DISTRICT_ID,'^[0-9]{4}')
fs$REGION_ID <- str_extract(fs$DISTRICT_ID,'^[0-9]{2}')
fs <- fs[fs$FISCAL_YEAR>=2004 & fs$FISCAL_YEAR<2019,]
#drop service wide programmatic study
fs = fs[fs$REGION_ID!='00',]
#drop northern research station
fs = fs[fs$REGION_ID!='24',]
#drop washington head office
fs = fs[fs$REGION_ID!='13',]
fs$DISTRICT_ID[grepl('00$',fs$DISTRICT_ID)] <- NA
fs$DISTRICT_NAME = str_squish(gsub('\\([0-9]{1,}\\)','',fs$`LMU – DISTRICT`))
fs$DISTRICT_NAME[grepl("All Units",fs$DISTRICT_NAME)] <- NA


fs[!fs$DISTRICT_ID %in% ranger_districts$DISTRICT_ID,]



test = as.matrix(table(fs$DISTRICT_ID,fs$FISCAL_YEAR))


dim(test[rowSums(test==0)>0,])
dim(test[rowSums(test==0)>0,])
fs[grep('Hayfork|',fs$`LMU – DISTRICT`),]




unique(fs$DISTRICT_NAME[fs$DISTRICT_ID%in%rownames(test[rowSums(test==0)>8,])])


dim(test)

test = fs[fs$DISTRICT_ID%in%c('091002','091001'),]

table(test$FISCAL_YEAR,test$DISTRICT_ID)
fs[fs$DISTRICT_ID%in%c('051454'),c('DISTRICT_ID','FISCAL_YEAR')][order(FISCAL_YEAR),]

ranger_districts[grep('011508',ranger_districts$DISTRICT_ID),]

fs[fs$DISTRICT_ID=='011508',]

ranger_districts[ranger_districts$FOR]


fs[fs$DISTRICT_ID=='011101',]


test = as.data.table(table(fs$DISTRICT_NAME,fs$DISTRICT_ID))
test = test[test$N>0,]
dubs = test$V1[duplicated(test$V1)]
test = test[test$V1 %in% dubs,]
test[order(V1),]


fs[fs$DISTRICT_ID %in% c('011704'),]
ranger_districts[ranger_districts$FOREST_ID=='0117',]
ranger_districts[grepl("Clearwater",ranger_districts$DISTRICTNA),]

admin_districts[grep('Bighorn',admin_districts$FORESTNAME,value=F),]

ranger_districts[grep("Medicine",ranger_districts$DISTRICTNA),]
ranger_districts[grep("Paintro",ranger_districts$DISTRICTNA),]

ranger_districts[ranger_districts$FOREST_ID=='020204',]
fs[fs$DISTRICT_ID %in% c('011706','011755'),][!duplicated(DISTRICT_ID),]

ranger_districts[ranger_districts$DISTRICT_ID %in% c('011706','040703'),]









test
ranger_districts[grepl('Ozark|Teton',ranger_districts$FORESTNAME),]
grep('Buffalo',ranger_districts$DISTRICTNA,value=T)
test = fs[fs$FISCAL_YEAR<=2011,]
t2 = fs[fs$FISCAL_YEAR>2011,]

grep("Buffalo",fs$`LMU – DISTRICT`,value=T)

te = unique(test$DISTRICT_ID)[!unique(test$DISTRICT_ID) %in% t2$DISTRICT_ID]


fs[fs$DISTRICT_ID=='041306',][order(FISCAL_YEAR,decreasing = T),]

test = 




ranger_districts[grepl("Unaka",ranger_districts$DISTRICTNA),]
ranger_districts[ranger_districts$DISTRICT_ID=='080405',]
test[37328,]
grep('Slashdown',test$`PROJECT NAME`,value=F)
ranger_districts[ranger_districts$DISTRICT_ID %in% te,]

fs[grepl('011704',fs$`LMU – DISTRICT`),'FISCAL_YEAR']

ranger_districts[grep('Nez|Clearwater',ranger_districts$FORESTNAME),]
ranger_districts[grep('Clearwater',ranger_districts$DISTRICTNA),]
table(fs$DISTRICT_ID,fs$FISCAL_YEAR)






fs$REGION_ID <- str_extract(fs$FOREST_ID,'^[0-9]{2}')






unique(fs$FOREST_ID)
admin_districts[admin_districts$FOREST_ID=='0500',]
table(fs$DISTRICT_ID,fs$FISCAL_YEAR)


fs$DISTRICT_ID[!is.na(fs$DISTRICT_ID) & !fs$DISTRICT_ID %in% ranger_districts$DISTRICT_ID]

ranger_districts[ranger_districts$FOREST_ID=='0112',]






fs$`DECISION TYPE`[fs$`DECISION TYPE`=='ROD'] <- 'EIS'
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='DN'] <- 'EA'
fs$`DECISION TYPE`[fs$`DECISION TYPE`=='DM'] <- 'CX'
fs$PROJECT_DECISON_LEVEL<-ifelse(is.na(fs$DISTRICT_ID),'NATIONAL FOREST','RANGER DISTRICT')
#fs = fs[fs$`DECISION TYPE`!='CX',]
#fs$EIS = (fs$`DECISION TYPE`=='EIS') + 0


nf = readRDS('input/prepped/national_forest_covariates.RDS')
rd = readRDS('input/prepped/ranger_district_covariates.RDS')
fs = fs[fs$FOREST_ID %in% nf$FOREST_ID,]
fs = fs[order(DECISON_ID),]
fs = fs[!duplicated(`PROJECT NUMBER`),]
setkey(nf,'FOREST_ID','FISCAL_YEAR')
setkey(fs,'FOREST_ID','FISCAL_YEAR')

proj_count = fs[,.N,by=.(DISTRICT_ID,`DECISION TYPE`,FISCAL_YEAR)]
proj_count[order(DISTRICT_ID,FISCAL_YEAR),]
setnames(proj_count,c('DECISION TYPE'),c('DECISION_TYPE'))
library(noncompliance)
tdt = expand.grid(sort(unique(rd$DISTRICT_ID)),2004:2018,c('CX','EA','EIS'))
tdt = data.table(tdt)
setnames(tdt,c('DISTRICT_ID',"FISCAL_YEAR",'DECISION_TYPE'))
setkey(tdt,       DISTRICT_ID,FISCAL_YEAR,DECISION_TYPE)
setkey(proj_count,DISTRICT_ID,FISCAL_YEAR,DECISION_TYPE)

temp = proj_count[tdt,]
temp$N[is.na(temp$N)]<-0
temp_dt = dcast(temp,DISTRICT_ID+FISCAL_YEAR~DECISION_TYPE,value.var = 'N')
temp_dt = left_join(temp_dt,rd)
setnames(nf,names(nf[,-c(1:2)]),paste0('NF_',names(nf[,-c(1:2)])))
temp_dt = left_join(temp_dt,nf)
temp_dt = data.table(temp_dt)
temp_dt$TOTAL = temp_dt$EA + temp_dt$EIS


inla(EIS ~ 1,family = 'zeroinflatedbinomial1',Ntrials = temp_dt$TOTAL, data = temp_dt)


table(temp_dt$FISCAL_YEAR,temp_dt$TOTAL==0)

fs

y = rbinom(50, size = 10, prob = 0.2)
y[ rbinom(n, size=1, prob=0.2) == 1 ] = 0
data = list(y=y,z=z)

inla.doc('zeroinflatedbinomial0')
table(temp_dt$EIS)
nf[nf$FOREST_ID=='1005',]


ranger_districts[ranger_districts$FOREST_ID=='1005',]
# to perform different types of hierarchical clustering
# package functions used: daisy(), diana(), clusplot()
gower.dist <- daisy(synthetic.customers[ ,2:7], metric = c("gower"))



test = glmer(EIS~1+PROJECT_DECISON_LEVEL+(1|`INITIATION FY`)+
               (1|FOREST_ID),family = 'binomial',data = fs)

head(fs)

table(fs$PROJECT_DECISON_LEVEL)

test = glmnet(cbind(1,fs$`CREATED FY`),y = fs$`DECISION TYPE`,family = 'multinomial',standardize = T)


proj_count = fs_sub[,.N,by=.(DISTRICT_ID,`DECISION TYPE`,`INITIATION FY`)]
proj_count$DISTRICT_ID = str_extract(proj_count$DISTRICT_ID,'[0-9]{6}$')
setnames(proj_count,c('INITIATION FY','DECISION TYPE'),c('FISCAL_YEAR','DECISION_TYPE'))
library(noncompliance)
tdt = expand.grid.DT(sort(unique(str_extract(ranger_districts$DISTRICTOR,'[0-9]{6}$'))),2004:2018,c('CX','EA','EIS'))
setnames(tdt,c('DISTRICT_ID',"FISCAL_YEAR",'DECISION_TYPE'))
setkey(tdt,       DISTRICT_ID,FISCAL_YEAR,DECISION_TYPE)
setkey(proj_count,DISTRICT_ID,FISCAL_YEAR,DECISION_TYPE)


temp_dt$REGION_ID = str_extract(temp_dt$DISTRICT_ID,'^[0-9]{2}')
temp_dt$FOREST_ID = str_extract(temp_dt$DISTRICT_ID,'^[0-9]{4}')

cor(temp_dt[,.(CX,EA,EIS)],method = 'kendall')