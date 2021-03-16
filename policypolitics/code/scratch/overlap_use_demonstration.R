library(tidycensus)
k = "b5a388cd6162590fc939335ddc45787bcc61778c"
tidycensus::census_api_key(key = k,install=T)
vn = list("B02001_001E","B02001_002E","B06011_001E","B07013_001E","B07013_002E","B16010_001E","B16010_041E","B25035_001E","B25077_001E")

census_2017 = get_acs(geography = 'county',variables = vn)
census_2017$variable = as.factor(census_2017$variable )
census_2017$variable = fct_recode(census_2017$variable,
'Total_Population' = 'B02001_001','White_Population' = 'B02001_002','Median_Income'='B06011_001','Median_Year_Structure_Built' = 'B25035_001','MEDIAN_HOME_PRICE' = 'B25077_001',
       'Owner_Occupied_Households' = 'B07013_002', 'Total_Households' = 'B07013_001','Bach_Degree_Or_Higher' = 'B16010_041','Pop_Over_25' = 'B16010_001')

county_overlaps = fread('input/gis_overlap_props/rangerdistrict_county_overlap_props.csv',stringsAsFactors = F)
county_overlaps$CFIPS = formatC(county_overlaps$CFIPS,width=5,flag = 0)
county_overlaps$DISTRICTOR = formatC(county_overlaps$DISTRICTOR,width=6,flag=0)
albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

ranger ="https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.RangerDistrict.zip"
temp <- tempfile()
download.file(ranger ,temp)
cat = unzip(temp)
fname <- (grep('shp',cat,value=T))
ranger_districts <- st_read(fname)
ranger_districts <- st_transform(ranger_districts,crs = st_crs(albersNA))
ranger_districts  <- st_make_valid(ranger_districts)
ranger_districts$DISTRICTOR = as.character(ranger_districts$DISTRICTOR)
total_vars = c('Total_Population','White_Population','Total_Households','Owner_Occupied_Households','Pop_Over_25','Bach_Degree_Or_Higher')
summary_vars = c('Median_Income','MEDIAN_HOME_PRICE','Median_Year_Structure_Built')
census_2017_long = dcast(as.data.table(census_2017), GEOID ~ variable ,value.var = 'estimate')

census_weighted_results = pblapply(seq_along(ranger_districts$DISTRICTOR),function(x) {
    sub_portions = county_overlaps[county_overlaps$DISTRICTOR == ranger_districts$DISTRICTOR[x],]
  sub_df = census_2017_long[census_2017_long$GEOID %in% sub_portions$CFIPS,]
  sub_df$Multiplier = sub_portions$Prop_Overlap[match(sub_df$GEOID,sub_portions$CFIPS)]
  data.table(DISTRICTOR = ranger_districts$DISTRICTOR[x],
             cbind(t(as.data.frame(apply( sub_df[,total_vars,with=F],2,function(x) { sum(x * sub_df$Multiplier,na.rm = T)}))),
                   t(as.data.frame(apply( sub_df[,summary_vars,with=F],2,function(x) {weighted.mean(x,w = sub_df$Multiplier,na.rm=T)})))))})


rangerdistrict_demos_2017 = rbindlist(census_weighted_results)



