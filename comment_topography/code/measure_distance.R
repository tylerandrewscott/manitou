
packages = c('data.table','stringr','tidyverse','sf','lwgeom','tigris','textreuse')
not_installed = packages[!packages %in% installed.packages()[,'Package']]
if(length(not_installed)>0){lapply(not_installed,install.packages)}
lapply(packages,require,character.only = T)
albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

packages = c('data.table','stringr','tidyverse','sf','lwgeom','tigris','textreuse')
not_installed = packages[!packages %in% installed.packages()[,'Package']]
if(length(not_installed)>0){lapply(not_installed,install.packages)}
lapply(packages,require,character.only = T)

set.seed(24)
library(tidyverse)
library(vader)
library(tm)
library(data.table)
library(tokenizers)
library(quanteda)
library(quanteda.textstats)
library(vader)
library(pbapply)
library(pals)
library(udpipe)

metad = readRDS('comment_topography/input/cleaned_comment_meta.RDS')


library(textreuse)

project.name.key = data.table(Project.Number = unique(metad$Project.Number),Project =
                                c("Westside Fire Recovery",
                                  "Stibnite Gold Mine",
                                  "Sage-grouse Amendments",
                                  "Alaska Roadless Rule"))

metad <- left_join(metad,project.name.key)
metad <- metad[!is.na(Use.Zipcode),]
metad$Use.Zipcode <- str_sub(metad$Use.Zipcode,end = 5)
metad <- metad[!duplicated(paste(project.name.key,Use.Zipcode)),]

library(ggmaps)
library(ggthemes)
library(tigris)

states <- states(year = 2020,class = 'sf',cb = T)
#states <- st_transform(states,st_crs(albersNA))
zcta <- zctas(class = 'sf',year = 2010)
#zcta <- st_transform(zcta,st_crs(albersNA))

metad <- metad[,.(Project.Number,Project,Use.Zipcode)]


projs <- unique(metad$Project.Number)

zip_to_zcta <- fread('https://raw.githubusercontent.com/censusreporter/acs-aggregate/master/crosswalks/zip_to_zcta/zip_zcta_xref.csv')
zip_to_zcta$zip_code <- formatC(x = zip_to_zcta$zip_code,width = 5,flag = '0')
zip_to_zcta$zcta <- formatC(x = zip_to_zcta$zcta,width = 5,flag = '0')
#metad$ZCTA <- metad$new_zip %in% zcta$ZCTA5CE10

metad$ZCTA<-zip_to_zcta$zcta[match(metad$Use.Zipcode,as.character(zip_to_zcta$zip_code))]
metad <- metad[!is.na(ZCTA),]
td = tempdir()
#albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
# # 
adm_url = "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip"
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(adm_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
admin_districts <- st_read(fpath)
admin_districts <- st_transform(admin_districts,st_crs(zcta))
#admin_districts <- st_transform(admin_districts,crs = st_crs(albersNA))
# fix bad polygons
bad_polys = !st_is_valid(admin_districts)
admin_districts[bad_polys,] <- st_make_valid(admin_districts[bad_polys,])
admin_districts$FOREST_ID <- paste0(admin_districts$REGION,admin_districts$FORESTNUMB)
project.name.key$FORESTS <- list('0505','0412',c(paste0('04',admin_districts$FORESTNUMB[admin_districts$REGION=='04'])),c(paste0('10',admin_districts$FORESTNUMB[admin_districts$REGION=='10'])))

# library(rgdal)
# library(rgeos)
# zcta_sg = zctas(year = 2010,class = 'sp')
# admin_sg = readOGR(fpath)
# admin_sg_albers <- sp::spTransform(admin_sg,CRS(albersNA))
# admin_sg_albers$FOREST_ID <- paste0(admin_sg_albers$REGION,admin_sg_albers$FORESTNUMB)
# zcta_sg_albers <- sp::spTransform(zcta_sg,CRS(albersNA))
# gdist <- gDistance(zcta_sg_albers,admin_sg_albers,byid = T)
# 


library(ggthemes)
#####
proj_list <- lapply(seq(nrow(project.name.key)),function(p){
print(p)
  if(p!=4){
  proj <- metad[Project.Number == project.name.key$Project.Number[p],]
  setnames(proj,'ZCTA','ZCTA5CE10')
  #pzip <- zcta[zcta$ZCTA5CE10 %in% proj$new_zip,]
  zcta_temp <- zcta[zcta$ZCTA5CE10 %in% proj$ZCTA5CE10,]
  admin_temp <- admin_districts[admin_districts$FOREST_ID %in% project.name.key$FORESTS[[which(project.name.key$Project.Number[p]==project.name.key$Project.Number)]],]
  if(nrow(admin_temp)==1){
    which_nearest <- rep(1,nrow(zcta_temp))
  }
  if(nrow(admin_temp)>1){
    which_nearest <- pbsapply(1:nrow(zcta_temp),function(i) st_nearest_feature(x = zcta_temp[i,],y = admin_temp),cl = 4)
  }
  nearest_dist <- pblapply(seq_along(which_nearest),function(x) st_distance(zcta_temp[x,],admin_temp[which_nearest[x],]),cl = 4)
  zcta_temp$km_from_project <- unlist(nearest_dist)/1000
  proj$km_from_project <- zcta_temp$km_from_project[match(proj$ZCTA5CE10,zcta_temp$ZCTA5CE10)]
  return(proj)}
  if(p==4){
    proj <- metad[Project.Number == project.name.key$Project.Number[p],]
    setnames(proj,'ZCTA','ZCTA5CE10')
    #pzip <- zcta[zcta$ZCTA5CE10 %in% proj$new_zip,]
    zcta_temp <- zcta[zcta$ZCTA5CE10 %in% proj$ZCTA5CE10,]
    admin_temp <- admin_districts[admin_districts$FOREST_ID %in% project.name.key$FORESTS[[which(project.name.key$Project.Number[p]==project.name.key$Project.Number)]],]
    ak_zcta <- st_intersects(states[states$STUSPS=='AK',],zcta_temp )
    zcta_temp$AK <- NA
    zcta_temp$AK[unlist(ak_zcta)] <- T
    zcta_temp$AK[is.na(zcta_temp$AK)]<-F
    which_nearest <- ifelse(!zcta_temp$AK,2,NA)
    nearest_ak <- st_nearest_feature(zcta_temp[zcta_temp$AK==T,],admin_temp)
    which_nearest[is.na(which_nearest)]<-unlist(nearest_ak)
    
    dist_to_chugach <- st_distance(zcta_temp,admin_temp[admin_temp$FOREST_ID=='1004',])
    dist_to_tongass <- st_distance(zcta_temp,admin_temp[admin_temp$FOREST_ID=='1005',])
    nearest_dist <- ifelse(which_nearest==1,dist_to_chugach,dist_to_tongass)
    
    zcta_temp$km_from_project <- unlist(nearest_dist)/1000
    proj$km_from_project <- zcta_temp$km_from_project[match(proj$ZCTA5CE10,zcta_temp$ZCTA5CE10)]
    return(proj)}
})

proj_dt <- rbindlist(proj_list)

zip_distance <- proj_dt[,.(ZCTA5CE10,km_from_project)]

metad$km_from_project <- zip_distance$km_from_project[match(metad$ZCTA,zip_distance$ZCTA5CE10)]
#saveRDS(object = proj_dt,'comment_topography/input/distance.RDS')

zp <- metad[,.(Project.Number,Use.Zipcode,ZCTA,km_from_project)]

fwrite(x = zp,
       file = 'comment_topography/input/distance_to_project.txt',sep = '\t')

