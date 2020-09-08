library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(gthemes)
library(ggplot2)

packages = c('data.table','stringr','tidyverse','sf','lwgeom','ggthemes','lubridate','pbapply','parallel','zoo','readxl')
not_installed = packages[!packages %in% installed.packages()[,'Package']]
if(length(not_installed)>0){lapply(not_installed,install.packages)}
lapply(packages,require,character.only = T)
td = tempdir()
cores = detectCores()-2
first_year = 2000;last_year = 2020
# projection to use for all spatial data
albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

# create a temporary folder that stores downloaded shapefile, extract contents and load


td = tempdir()
admin_url = "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip"
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(admin_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
admin_districts <- st_read(fpath)
admin_districts <- st_transform(admin_districts,crs = st_crs(albersNA))
# fix bad polygons
bad_polys = !st_is_valid(admin_districts)
admin_districts[bad_polys,] <- st_make_valid(admin_districts[bad_polys,])

admin_districts$FORESTORGC = as.character(admin_districts$FORESTORGC)
admin_districts$FOREST_ID = admin_districts$FORESTORGC
admin_districts$FOREST_ID = formatC(admin_districts$FORESTORGC,width=4,flag = 0)
#admin_districts$FOREST_ID = formatC(admin_districts$FORESTORGC,width=4,flag = 0)



td = tempdir()
states_url = "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.ALPGeopoliticalUnit.zip"
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(states_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))

states <- st_read(fpath)
states <- st_transform(states,crs = st_crs(albersNA))
# fix bad polygons
bad_polys = !st_is_valid(states)
states[bad_polys,] <- st_make_valid(states[bad_polys,])

plot(st_geometry(states))
ggplot() + geom_sf(data = states)


gross_receipts <- read_excel('input/USFSGrossReceipts.xlsx',skip = 3)
gross_receipts <- data.table(gross_receipts )

g2017 = gross_receipts[Year==2017,]

g2017itemized = melt(g2017,id.vars = c('Year','National Forest Name','National Forest Code'))

g2017itemized = g2017itemized[grepl('^Class',variable),]

g2017_leading_source = g2017itemized[order(`National Forest Code`,-value),][!duplicated(`National Forest Code`),]
g2017_leading_source$FOREST_ID  = formatC(g2017_leading_source$`National Forest Code`,width = 4,flag = 0)

admin_districts$leading_revenue_source_2017 <- g2017_leading_source$variable[match(admin_districts$FOREST_ID,g2017_leading_source$FOREST_ID)]
admin_districts$leading_revenue_source_2017 <- gsub('^Class[0-9]\\s-\\s','',admin_districts$leading_revenue_source_2017)

nf_revenue_source = admin_districts[!is.na(admin_districts$leading_revenue_source_2017),]
gg_revenue_source<-ggplot() + 
  geom_sf(data = states) + 
  geom_sf(data = nf_revenue_source,aes(fill = leading_revenue_source_2017),lwd = 0.1) + 
  theme_map() + ggtitle('Leading source of revenue receipts by National Forest, 2017') +
  scale_fill_tableau(name = 'Source')
# need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html

require(sp)
us_aea = tigris::states(class = 'sp')
# convert it to Albers equal area
us_aea <- spTransform(us_aea, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

alaska <- us_aea[us_aea$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea)

# extract, then rotate & shift hawaii
hawaii <- us_aea[us_aea$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us_aea)


us_aea <- us_aea[!us_aea$STATEFP %in% c("02", "15", "72",'60','66','78','69'),]
us_aea <- rbind(us_aea, alaska, hawaii)

test = tigris::counties(state = 'MI',class = 'sp')
plot(test)
plot(us_aea)


grep('Plan',have_cara$`PROJECT NAME`,value = T)

# remove old states and put new ones back in; note the different order
# we're also removing puerto rico in this example but you can move it
# between texas and florida via similar methods to the ones we just used


require(tmap)
tmap_mode('plot')
tm <- tm_shape(admin_districts) + 
  tm_polygons(group = 'FOREST_ID',col = 'leading_revenue_source_2017') 

tm
# user  system elapsed 
# 0.000   0.000   0.001 
