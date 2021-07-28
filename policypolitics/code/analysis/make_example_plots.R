


packages = c('data.table','stringr','tidyverse','sf','lwgeom','ggthemes','tigris','lubridate','ggnewscale','ggrepel')
not_installed = packages[!packages %in% installed.packages()[,'Package']]
if(length(not_installed)>0){lapply(not_installed,install.packages)}
lapply(packages,require,character.only = T)

td = tempdir()
albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

admin_districts <- readRDS('policypolitics/prepped_inputs/admin_units_clean.RDS')

states = tigris::states(class = 'sf')
states <- st_transform(states,crs = st_crs(albersNA))
test = st_within(st_centroid(admin_districts),states)
admin_districts$STATE = states[unlist(test),]$STUSPS



congress2015 = tigris::congressional_districts(year = 2015,class = 'sf')
congress2015  = st_transform(congress2015 ,albersNA)
#congress_over_forest = st_intersects(congress2015,admin_districts)
#congress2015= congress2015[sapply(congress_over_forest,length)>0,]

states = tigris::states(class = 'sf',year = 2015)
states = st_transform(states,albersNA)
states <- st_crop(states,st_bbox(admin_districts))


oregon_districts = admin_districts[admin_districts$REGION=='06',]
oregon_districts = oregon_districts[sapply(st_intersects(oregon_districts,states[states$STUSPS=='OR',]),length)>0,]

oregon_house = congress2015[congress2015$STATEFP=='41',]
oregon_house$ID = paste0('OR-',oregon_house$CD114FP)
oregon_house$Dem_Rep = c('D','R','D','D','D')

nf = fread('policypolitics/prepped_inputs/national_forest_covariates.csv')
nf$FOREST_ID = formatC(nf$FOREST_ID,width = 4,flag = 0)
nf$FOREST_NAME = admin_districts$FORESTNAME[match(nf$FOREST_ID,admin_districts$FOREST_ID)]
nf = nf[congress %in% 108:115,]
nf115 = nf[congress==115,]
oregon_districts$LCV_annual = nf115$LCV_annual[match(oregon_districts$FOREST_ID,nf115$FOREST_ID)]




td = tempdir()
albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
# # 
adm_url = "https://www2.census.gov/geo/tiger/TIGER2017/COASTLINE/tl_2017_us_coastline.zip"
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(adm_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
coast <- st_read(fpath)
coast <- st_transform(coast,crs = st_crs(albersNA))


oregon_coast = st_crop(oregon_house,coast)


state_cartos = tigris::states(cb = T,class = 'sf')
state_cartos = state_cartos[state_cartos$STUSPS=='OR',]
state_cartos = st_transform(state_cartos,albersNA)
oregon_districts$short_name = gsub(' National (Forest|Forests)$','',oregon_districts$FORESTNAME)
oregon_districts$short_name = gsub(' National Scenic Area',' NSA',oregon_districts$short_name)
oregon_districts$short_name = gsub('-','-\n',oregon_districts$short_name,fixed = T)

oregon_house = st_make_valid(oregon_house)
oregon_districts = st_make_valid(oregon_districts)
oregon_house = st_transform(oregon_house,albersNA)
oregon_house = st_intersection(oregon_house,state_cartos)
oregon_house = oregon_house[order(oregon_house$ID),]
oregon_house$reps = c('Bonamici (D)','Walden (R)','Blumenauer (D)','DeFazio (D)','Schrader (D)')
oregon_house$LCV_annual = c(97,6,97,97,63)
house_centroids = st_centroid(oregon_house)
house_centroids[,c('X','Y')] <- st_coordinates(house_centroids$geometry)


y_adjust=c(0.25e5,-.5e5,0.15e5,0,-0.1e5)
x_adjust = c(0,-0.25e5,0,0,-0.25e5)
seed = 00
(gg_rep = ggplot() + ggtitle('House Representation (115th Congress)\n for National Forests in Oregon')+
    geom_sf(data = state_cartos,fill = NA)  + 
    geom_sf(data = oregon_house,fill = NA,lty = 2)+
    scale_fill_grey(start = 0.4,end = 0.7,name = 'House district') +
          #labels=c('Bonamici (D)','Walden (R)','Blumenauer (D)','DeFazio (D)','Schrader (D)')) +
    new_scale_fill() +
    geom_sf(data = oregon_districts,aes(fill = LCV_annual),col = 'black',alpha = 0.7,lwd = 0.25) + 
  #  scale_fill_gradient2_tableau(name = 'LCV annual',palette = "Orange-Blue Diverging",
   #                              limits = c(0,100)) +
  #  scale_color_gradient2_tableau(name = 'LCV annual',palette = "Orange-Blue Diverging",
   #                              limits = c(0,100)) +
    scale_fill_viridis_c(name = 'LCV annual',
                                 limits = c(0,100),option = 'E',direction = -1) +
    scale_color_viridis_c(name = 'LCV annual',
                                  limits = c(0,100),option = 'E',direction = -1) +
    
    geom_sf_label(data = oregon_districts,aes(label=short_name),nudge_y = 10000,lineheight=0.8,
                  nudge_x = 10000,label.padding = unit(0.12, "lines")) +
    #geom_sf_label(data = oregon_house,aes(label=gsub('0','',ID)),label.padding = unit(0.1, "lines")) + 
    #scale_fill_tableau(name = 'Representative',)) + 
    theme_map() + theme(panel.grid = element_line(colour='transparent'),legend.position = c(0.7,0.05),
                        legend.box = 'horizontal',
                        legend.background = element_rect(fill = alpha('white',0.5)),title = element_text(size = 15),
                        legend.text = element_text(size = 12),
                        legend.title = element_text(size =12))+

    geom_point(data = house_centroids,aes(x = X+x_adjust,y=Y+y_adjust,col = LCV_annual),size = 3)+
    geom_label_repel(max.time = 3,max.overlaps = 5,data = house_centroids,aes(X+x_adjust,y=Y+y_adjust,label = reps,fill = LCV_annual),
                     col = c('white','black',rep('white',3)),
                     segment.color = 'black',
                     min.segment.length = 0.25,force_pull = 0.2) + #nudge_x = runif(5,max=100000))+
NULL
    )


ggsave(plot = gg_rep,filename = 'policypolitics/tables_figures/figures/figure1_oregon_lcv_annual.tiff',width=7.5,height=6,units ='in',dpi = 500)

start_year = 2005
end_year = 2018
period_type = 'CALENDAR_YEAR'
#### this determines which projects to include ######
keep_decisions = c('EIS','EA','CE')

fs = readRDS('policypolitics/raw_curated_inputs/fs_PALS_cleaned_project_datatable.RDS')
fs = fs[FOREST_ID %in% admin_districts$FOREST_ID,]

subvars = c('Type_Purpose_All','Type_Purpose_Extractive','Type_Purpose_Recreation_Wildlife')

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


input_data = dcast(counts_by_type,get(period_type) + 
                     FOREST_ID + Project_Type ~ DECISION_TYPE,value.var = 'N',fill = 0)
input_data$Tot_Proj = input_data$CE + input_data$EA + input_data$EIS
if(period_type=='congress'){
  input_data = input_data[Project_Type=='Type_Purpose_Extractive',][period_type>108&period_type<116,]}
if(period_type!='congress'){
  input_data = input_data[Project_Type=='Type_Purpose_Extractive',][period_type>=start_year&period_type<=end_year,]}

yheight = input_data[,.N,by = .(Tot_Proj)][N==max(N),]$N


g1 = ggplot(data = input_data,aes(x = Tot_Proj)) + geom_histogram(bins = 100)+
  scale_y_continuous(name = '# forest-congress observations') +
  scale_x_continuous(name = '# projects by period') + theme_bw() + 
  ggtitle('# Extractive projects')

g2 = ggplot(data = input_data,aes(x = CE/Tot_Proj)) + 
  scale_x_continuous(name = '# CE / total NEPA analyses')+
  geom_density() + ggtitle('CE / total NEPA analyses')  +
  theme_bw() + scale_y_continuous(name = 'density') + 
  coord_flip()

g3 = ggplot(data = input_data,aes(x = Tot_Proj,y = CE)) + geom_jitter(pch = 21) + theme_bw() + 
  scale_x_continuous(name = '# projects in period') +
  scale_y_continuous(name = '# CEs issued') + 
  ggtitle('CEs vs. total extractive projects')
library(gridExtra)
ggsave(grid.arrange(g1,g2,g3,ncol = 2),filename = 'policypolitics/tables_figures/figures/figure2_DV_plot.tiff',width = 8,units = 'in',height= 8,dpi=350)







