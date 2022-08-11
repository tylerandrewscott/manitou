
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
projs <- unique(metad$Project.Number)
library(ggmap)
library(ggthemes)
library(tigris)

states <- states(year = 2020,class = 'sf',cb = T)
#states <- st_transform(states,st_crs(albersNA))

zcta <- zctas(class = 'sf',year = 2010,cb = F)
#zcta <- st_transform(zcta,st_crs(albersNA))


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

forests <- sort(unlist(project.name.key$FORESTS))

admin_temp <- admin_districts[admin_districts$FOREST_ID %in% forests,]

dist_mat <- st_distance(zcta,admin_temp)

colnames(dist_mat) <- admin_temp$FOREST_ID
rownames(dist_mat) <- zcta$ZCTA5CE10
dist_mat_km <- dist_mat/1000
dt <- data.table(dist_mat_km)
dt$ZCTA5CE10 <- rownames(dist_mat_km)
km_forest <- melt(dt,id.vars = 'ZCTA5CE10',variable.name = 'FOREST_ID',value.name = 'km_to_forest')

fwrite(x = km_forest,file = 'comment_topography/input/distance_zcta_to_forest.txt',sep = '\t')



# library(rgdal)
# library(rgeos)
# zcta_sg = zctas(year = 2010,class = 'sp')
# admin_sg = readOGR(fpath)
# admin_sg_albers <- sp::spTransform(admin_sg,CRS(albersNA))
# admin_sg_albers$FOREST_ID <- paste0(admin_sg_albers$REGION,admin_sg_albers$FORESTNUMB)
# zcta_sg_albers <- sp::spTransform(zcta_sg,CRS(albersNA))
# gdist <- gDistance(zcta_sg_albers,admin_sg_albers,byid = T)
# 

