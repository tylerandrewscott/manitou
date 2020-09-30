  
  if(!require(data.table)){install.packages('data.table');require(data.table)}
  if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
  if(!require(INLA)){install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE);require(INLA)}
  if(!require(forcats)){install.packages('forcats');library(forcats)}
  if(!require(ggstance)){install.packages('ggstance');library(ggstance)}
  if(!require(ggthemes)){install.packages('ggthemes');library(ggthemes)}
  
  if(!require(scales)){install.packages('scales');library(scales)}
  if(!require(R2HTML)){install.packages('R2HTML');library(R2HTML)}
  if(!require(sf)){install.packages('sf');library(sf)}
  
  td = tempdir()
  # albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
  # 
  # admin_url = "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip"
  # tf = tempfile(tmpdir=td, fileext=".zip")
  # download.file(admin_url, tf)
  # fname = unzip(tf, list=TRUE)
  # unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
  # fpath = file.path(td, grep('shp$',fname$Name,value=T))
  # admin_districts <- st_read(fpath)
  # admin_districts <- st_transform(admin_districts,crs = st_crs(albersNA))
  # fix bad polygons
  
  # bad_polys = !st_is_valid(admin_districts)
  #  admin_districts[bad_polys,] <- st_make_valid(admin_districts[bad_polys,])
  
  admin_districts <- readRDS('scratch/admin_units_clean.RDS')
  

  
  admin_districts$FORESTORGC = as.character(admin_districts$FORESTORGC)
  admin_districts$FOREST_ID = admin_districts$FORESTORGC
  admin_districts$FOREST_ID = formatC(admin_districts$FORESTORGC,width=4,flag = 0)
  file.remove(list.files('output/policypolitics/tables/',pattern = 'coefs',full.names = T))
  locs = 'output/policypolitics/model_objects/'
  spec_names = data.table(specification = 1:6,name = c('Annual LCV score','LCV x % unemp','% dem. vote','% dem. x % unemp','Dem. rep.','Dem. rep. % unemp.'))
  model_sets = list.files('output/policypolitics/model_objects/','Purpose.*Ext.*drop')
  model_names = str_remove(model_sets,'models_Type_Purpose_')

  mod_name_sets = list(#grep('noCX_activities',model_sets,value = T),
  #intersect(grep('activities',model_sets,value = T,invert = T),grep('noCX',model_sets,value = T,invert = F)),
  intersect(grep('activities',model_sets,value = T,invert = T),grep('noCX',model_sets,value = T,invert = T)))
  #intersect(grep('activities',model_sets,value = T,invert = F),grep('noCX',model_sets,value = T,invert = T)))
  

  names(mod_name_sets) <- c('')

  # lapply(seq_along(mod_name_sets),function(mod){
  mod = 1
  model_list_of_lists = lapply(mod_name_sets[[mod]],function(x) readRDS(paste0(locs,x)))
  names(model_list_of_lists) <- mod_name_sets[[mod]]

  temp = (model_list_of_lists$models_Type_Purpose_Extractive.RDS[[2]])
 congressA = ggplot(temp$summary.random$u_congress_id ,aes(x = as.factor(ID),y = mean,ymin = `0.025quant`,ymax = `0.975quant`)) + geom_point() + geom_errorbar() + theme_bw() +
   ggtitle('modeled intercept by congress for total project')
 congressB = ggplot(temp$summary.random$y_congress_id ,aes(x = as.factor(ID),y = mean,ymin = `0.025quant`,ymax = `0.975quant`)) + geom_point() + geom_errorbar() + theme_bw() + 
   ggtitle('modeled intercept by congress for CE/total projects')
 library(gridExtra)
 grid.arrange(congressA,congressB,ncol = 2)
 
 waic_table = sapply(model_list_of_lists,function(x) sapply(x,function(y) y$waic$waic))
  colnames(waic_table) <- mod_name_sets[[mod]]
  fwrite(waic_table,'output/policypolitics/tables/waic_table.csv')
  
  coef_df = rbindlist(lapply(seq_along(model_list_of_lists), function(y) {
    rbindlist(lapply(seq_along(model_list_of_lists[[y]]),function(x) {
    model_list_of_lists[[y]][[x]]$summary.fixed[,c(1,3,5)] %>% mutate(coef = rownames(.), mod = x,DV = names(model_list_of_lists)[y])}))}))
  coef_df$cred = paste(formatC(round(coef_df$mean,3),digits = 3,flag = 0),paste0('(',
        formatC(round(coef_df$`0.025quant`,3),digits = 3,flag = 0),', ',
        formatC(round(coef_df$`0.975quant`,3),digits = 3,flag = 0),')'))
  
  coef_df_s1 = coef_df[grepl('Extr',DV),.(coef,cred,mod)][grepl('^u|mu\\.u',coef),]
  coef_df_s2 = coef_df[grepl('Extr',DV),.(coef,cred,mod)][!grepl('^u|mu\\.u',coef),]
  
  coef_df_s3 = coef_df[grepl('Rec',DV),.(coef,cred,mod)][grepl('^u|mu\\.u',coef),]
  coef_df_s4 = coef_df[grepl('Rec',DV),.(coef,cred,mod)][!grepl('^u|mu\\.u',coef),]
  

  coef_df$coef <- fct_inorder(coef_df$coef)
  coef_cast1 = dcast(coef_df_s1,coef ~ mod,value.var = 'cred',fill = '---')
  coef_cast2 = dcast(coef_df_s2,coef ~ mod,value.var = 'cred',fill = '---')
  coef_cast3 = dcast(coef_df_s3,coef ~ mod,value.var = 'cred',fill = '---')
  coef_cast4 = dcast(coef_df_s4,coef ~ mod,value.var = 'cred',fill = '---')
  library(R2HTML)
  
  simple_table = cbind(coef_cast1[,c(1:3)],  coef_cast2[,c(2:3)])
  simple_table$coef <- gsub('^y_|^u_','',simple_table$coef )
  simple_table$coef <- gsub(':y_|:u_','x',simple_table$coef)
  simple_table$coef = as.factor(simple_table$coef)
  simple_table$coef = fct_recode(simple_table$coef,
                                 '(intercept)' = 'mu.u',
                                 '% wilderness area' = 'Wilderness_Perc',
                                 '% dem. vote share' = 'percentD_H',
                                 '% housing in WUI' = 'Perc_WUI_Housing',
                                 '# listed species' = 'Count_EorT_Species','ln(forest acreage)'='Ln_ACRES',
                                 '% burned (last 5 years)'='Burned_Perc_Past5yrs','LCV annual score'='LCV_annual',
                                 'Unemployment %' = 'Unemp_Rate','% extraction employ.' = 'Perc_Extraction_Employ',
                                 'ln(yearly visitation)' = 'Ln_AVERAGE_YEARLY_VISITS','ln(county NR GDP ($1M))' = 'ln_County_naturalresource_GDP_1M',
                                 'ln(avg. board feet, 1999 to 2004)' = "Ln_Avg_MBF_Cut_1999_2004" ,
                                 'NEPA grazing actions, 1993 to 2004' = "ALLOTMENT_NEPA_1993_2004",
                                 'Mining claim actions, 1993 to 2004' = "MINING_CLAIM_ACTIONS_1993_2004" ,
                                 'Democratic president' = 'demPres','Democratic congress' = 'demCongress',
                                 "% dem. vote x unemp. %" = "Unemp_RatexpercentD_H"   ,
                                 'Dem. rep.' = 'democrat','Dem. rep. x unemp. %' = "Unemp_Ratexdemocrat" ,
                                 'LCV annual x unemp. %' = 'Unemp_RatexLCV_annual',
                                 'House committee LCV' = 'ComLCV','House chair LCV' = 'ChairLCV')
  
  simple_table$coef  = fct_relevel( simple_table$coef ,'ln(forest acreage)','ln(yearly visitation)',
              'ln(avg. board feet, 1999 to 2004)' ,
              'NEPA grazing actions, 1993 to 2004' ,
              'Mining claim actions, 1993 to 2004',
              '% wilderness area','# listed species','% burned (last 5 years)','% housing in WUI',
              '% extraction employ.','ln(county NR GDP ($1M))',
              'Democratic president','Democratic congress',
              '% dem. vote share','LCV annual score','Dem. rep.', 'Unemployment %',
              "% dem. vote x unemp. %",'LCV annual x unemp. %','Dem. rep. x unemp. %')

htmlTable::htmlTable(simple_table[order(coef,)])


  simple_table = cbind(coef_cast3[,c(1:3)],  coef_cast4[,c(2:3)])
  simple_table$coef <- gsub('^y_|^u_','',simple_table$coef )
  simple_table$coef <- gsub(':y_|:u_','x',simple_table$coef )
  simple_table$coef = as.factor(simple_table$coef)

  simple_table$coef = fct_recode(simple_table$coef, '(intercept)' = 'mu.u',
                                 '% wilderness area' = 'Wilderness_Perc',
                                 '% dem. vote share' = 'percentD_H',
                                 '% housing in WUI' = 'Perc_WUI_Housing',
                                 '# listed species' = 'Count_EorT_Species','ln(forest acreage)'='Ln_ACRES',
                                 '% burned (last 5 years)'='Burned_Perc_Past5yrs','LCV annual score'='LCV_annual',
                                 'Unemployment %' = 'Unemp_Rate','% extraction employ.' = 'Perc_Extraction_Employ',
                                 'ln(yearly visitation)' = 'Ln_AVERAGE_YEARLY_VISITS','ln(county NR GDP ($1M))' = 'ln_County_naturalresource_GDP_1M',
                                 'ln(avg. board feet, 1999 to 2004)' = "Ln_Avg_MBF_Cut_1999_2004" ,
                                 'NEPA grazing actions, 1993 to 2004' = "ALLOTMENT_NEPA_1993_2004",
                                 'Mining claim actions, 1993 to 2004' = "MINING_CLAIM_ACTIONS_1993_2004" ,
                                 'Democratic president' = 'demPres','Democratic congress' = 'demCongress',
                                 "% dem. vote x unemp. %" = "Unemp_RatexpercentD_H"   ,
                                 'Dem. rep.' = 'democrat','Dem. rep. x unemp. %' = "Unemp_Ratexdemocrat" ,
                                 'LCV annual x unemp. %' = 'Unemp_RatexLCV_annual',
                                 'House committee LCV' = 'ComLCV','House chair LCV' = 'ChairLCV')
  simple_table$coef  = fct_relevel( simple_table$coef ,'(intercept)','ln(forest acreage)','ln(yearly visitation)',
                                    'ln(avg. board feet, 1999 to 2004)' ,
                                    'NEPA grazing actions, 1993 to 2004' ,
                                    'Mining claim actions, 1993 to 2004',
                                    '% wilderness area','# listed species','% burned (last 5 years)','% housing in WUI',
                                    '% extraction employ.','ln(county NR GDP ($1M))',
                                    'Democratic president','Democratic congress',
                                    '% dem. vote share','LCV annual score','Dem. rep.', 'Unemployment %',
                                    "% dem. vote x unemp. %",'LCV annual x unemp. %','Dem. rep. x unemp. %')
  
  htmlTable::htmlTable( simple_table[order(coef,)])
  

  HTML(coef_cast1, file = paste0('output/policypolitics/tables/extractive_coefs',names(mod_name_sets[mod]),'.html'),row.names = F)
  HTML(coef_cast2, file = paste0('output/policypolitics/tables/extractive_coefs',names(mod_name_sets[mod]),'.html'),row.names = F)
  
  HTML(coef_cast3, file = paste0('output/policypolitics/tables/recreation_coefs',names(mod_name_sets[mod]),'.html'),row.names = F)
  HTML(coef_cast4, file = paste0('output/policypolitics/tables/recreation_coefs',names(mod_name_sets[mod]),'.html'),row.names = F)
  
  coef_results = rbindlist(lapply(seq_along(model_list_of_lists),function(m){
  rbindlist(lapply(seq_along(model_list_of_lists[[m]]),function(x) model_list_of_lists[[m]][[x]]$summary.fixed[,c(1,3,5)] %>%
                     mutate(specification = x,coef = rownames(.),  DV = mod_name_sets[[mod]][m])))}))
  
  coef_results = coef_results[!coef%in%c('mu.u','mu.y')]
  #coef_results = coef_results[specification!=3,]


coef_results$specification <-   spec_names$name[match(coef_results$specification,spec_names$specification)]
coef_results$specification <- fct_inorder(coef_results$specification)

coef_results$lik <- ifelse(grepl('^u_|mu\\.u',coef_results$coef),'# projects','CEs/total analyses')
coef_results$coef <- gsub('^y_|^u_','',coef_results$coef)
coef_results$coef <- gsub(':y_|:u_','x',coef_results$coef)

coef_results$coef = fct_recode(coef_results$coef, '% wilderness area' = 'Wilderness_Perc',
                               '% dem. vote share' = 'percentD_H',
                               '% housing in WUI' = 'Perc_WUI_Housing',
           '# listed species' = 'Count_EorT_Species','ln(forest acreage)'='Ln_ACRES',
           '% burned (last 5 years)'='Burned_Perc_Past5yrs','LCV annual score'='LCV_annual',
           'Unemployment %' = 'Unemp_Rate','% extraction employ.' = 'Perc_Extraction_Employ',
           'ln(yearly visitation)' = 'Ln_AVERAGE_YEARLY_VISITS','ln(county NR GDP ($1M))' = 'ln_County_naturalresource_GDP_1M',
            'ln(avg. board feet, 1999 to 2004)' = "Ln_Avg_MBF_Cut_1999_2004" ,
           'NEPA grazing actions, 1993 to 2004' = "ALLOTMENT_NEPA_1993_2004",
           'Mining claim actions, 1993 to 2004' = "MINING_CLAIM_ACTIONS_1993_2004" ,
           'Democratic president' = 'demPres','Democratic congress' = 'demCongress',
           "% dem. vote x unemp. %" = "Unemp_RatexpercentD_H"   ,
           'ln(Resource receipts, last 4 yrs)' = 'ln_Receipts_Extraction_1M_P4',
           'ln(Recreation receipts, last 4 yrs)' = 'ln_Receipts_Recreation_1M_P4',
           'Public ideology' = 'mrp_mean',
           '% change in receipts (vs. t-4)' = 'Total_Receipts_4yr_Change_Perc',
           'Dem. rep.' = 'democrat','Dem. rep. x unemp. %' = "Unemp_Ratexdemocrat" ,
           'LCV annual x unemp. %' = 'Unemp_RatexLCV_annual',
           'House committee LCV' = 'ComLCV','House chair LCV' = 'ChairLCV')
    
#coef_results$coef <- fct_inorder(coef_results$coef)
coef_results$coef <- fct_relevel(coef_results$coef,'ln(forest acreage)','ln(yearly visitation)',
                                 'ln(avg. board feet, 1999 to 2004)' ,
                                 'ln(Resource receipts, last 4 yrs)',
                                 'ln(Recreation receipts, last 4 yrs)' ,
                                 '% change in receipts (vs. t-4)',
                                 'NEPA grazing actions, 1993 to 2004' ,
                                 'Mining claim actions, 1993 to 2004',
                                 '% wilderness area','# listed species','% burned (last 5 years)','% housing in WUI',
                                 '% extraction employ.','ln(county NR GDP ($1M))',
                                 'Democratic president','Democratic congress',   'Public ideology' ,
                                 '% dem. vote share','LCV annual score','Dem. rep.', 'Unemployment %',
                                 "% dem. vote x unemp. %",'LCV annual x unemp. %','Dem. rep. x unemp. %')

coef_results$coef <- fct_rev(coef_results$coef)
coef_results$sig <- (!(coef_results$`0.025quant`<0 & coef_results$`0.975quant`>0)) + 0


base_coefs = coef_results[specification%in%'Annual LCV score',]
#base_coefs$DV <- ifelse(grepl('All',base_coefs$DV),'All',ifelse(grepl('Recreat',base_coefs$DV),'Wildlife/recreation','Extractive'))

base_coefs$DV <- ifelse(grepl('nodrops',base_coefs$DV),'No drops','w/ drops')
#base_coefs$specification = ifelse(base_coefs$specification==1,'Restricted model','w/ national leadership')
base_all = ggplot(base_coefs,aes(x = mean,xmin = `0.025quant`,xmax = `0.975quant`,
                                                                         y = coef,col = lik,fill = as.factor(sig),group = lik)) + 
  geom_vline(xintercept = 0,lty = 2,col = 'grey40') + 
  geom_errorbarh(height = 0.1,position = position_dodgev(0.5)) + 
  geom_point(position = position_dodgev(0.5),pch = 19,size = 1.5) + 
  geom_point(position = position_dodgev(0.5),pch = 21,size = 1.5) + 
  facet_grid(~DV,scales = 'fixed') + theme_bw() + 
  theme(legend.position = 'bottom',axis.title.y = element_blank()) +
  scale_x_continuous(name = '95% credible interval') + 
  # scale_shape_manual(values = c(19,21))
  #scale_fill_manual(name = "outcome",values = c('white','orange','white','green'),labels = c('# projects','EIS/total')) + 
  #scale_color_manual(name = "outcome",values = c('orange','orange','green','green'),labels = c('# projects','EIS/total')) + 
  scale_fill_manual(values = c('white',NA)) + 
  scale_color_tableau(name = 'Outcome') + 
  guides(shape = FALSE,fill = FALSE) + 
  ggtitle('Predicing projects by forest and Congress') +
  NULL


ggsave(base_all,filename = paste0('output/policypolitics/figures/coefplot_compare_projtypes',names(mod_name_sets)[[mod]],'.png'),dpi = 300,width = 7.5,height = 7,units = 'in')

extract_coefs = coef_results[grepl("Extract",DV),]
# 
# extract_coefs2$CX = 1
# extract_coefs$CX = 0
# extract_coefs$CX  = ifelse(extract_coefs$CX == 'count w/CX','w/CX','EA + EIS only')
# extract_coefs = rbind(extract_coefs,extract_coefs2)
# extract_coefs = extract_coefs[extract_coefs$specification == 1,]
# test = ggplot(extract_coefs,aes(x = mean,xmin = `0.025quant`,xmax = `0.975quant`,
#                          y = coef,col = lik,fill = as.factor(sig),group = lik)) + 
#   geom_vline(xintercept = 0,lty = 2,col = 'grey40') + 
#   geom_errorbarh(height = 0.1,position = position_dodgev(0.5)) + 
#   geom_point(position = position_dodgev(0.5),pch = 19,size = 1.5) + 
#   geom_point(position = position_dodgev(0.5),pch = 21,size = 1.5) + 
#   facet_grid(~CX,scales = 'fixed') + theme_bw() + 
#   theme(legend.position = 'bottom',axis.title.y = element_blank()) +
#   scale_x_continuous(name = '95% credible interval') + 
#   # scale_shape_manual(values = c(19,21))
#   #scale_fill_manual(name = "outcome",values = c('white','orange','white','green'),labels = c('# projects','EIS/total')) + 
#   #scale_color_manual(name = "outcome",values = c('orange','orange','green','green'),labels = c('# projects','EIS/total')) + 
#   scale_fill_manual(values = c('white',NA)) + 
#   scale_color_tableau(name = 'Stage',labels =  c('procedural','distributive')) + 
#   guides(shape = FALSE,fill = FALSE) + 
#   ggtitle('Extractive projects') +
#   NULL
# 
# ggsave(test,filename = 'test.withdrops.png',dpi = 300,height = 6,width= 6,units = 'in')


variations = c('LCV','Dem. rep','% dem')
varnames = c('LCV','demRep','demVote')


lapply(seq_along(variations),function(x) {
extract_comp = ggplot(extract_coefs[grepl(variations[x],specification)&grepl('withdrops',DV),],aes(x = mean,xmin = `0.025quant`,xmax = `0.975quant`,
                                 y = coef,col = lik,fill = as.factor(sig),group = lik)) + 
  geom_vline(xintercept = 0,lty = 2,col = 'grey40') + 
  geom_errorbarh(height = 0.1,position = position_dodgev(0.5)) + 
  geom_point(position = position_dodgev(0.5),pch = 19,size = 1.5) + 
  geom_point(position = position_dodgev(0.5),pch = 21,size = 1.5) + 
  facet_grid(~specification,scales = 'fixed') + theme_bw() + 
  theme(legend.position = 'bottom',axis.title.y = element_blank(),
        axis.text = element_text(size = 12),strip.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(name = '95% credible interval') + 
  # scale_shape_manual(values = c(19,21))
  #scale_fill_manual(name = "outcome",values = c('white','orange','white','green'),labels = c('# projects','EIS/total')) + 
  #scale_color_manual(name = "outcome",values = c('orange','orange','green','green'),labels = c('# projects','EIS/total')) + 
  scale_fill_manual(values = c('white',NA)) + 
  scale_color_tableau(name = 'Outcome',labels=c('# projects','CEs/total analyses')) + 
  guides(shape = FALSE,fill = FALSE) + 
  ggtitle('Extractive projects') +
  NULL

ggsave(extract_comp,filename = paste0('output/policypolitics/figures/coefplot_extraction_',varnames[x],names(mod_name_sets)[[mod]],'.withdrops.png'),dpi = 300,width = 7.5,height = 8,units = 'in')
})


temp = extract_coefs[grepl('Annual LCV',specification),]
temp$specification = 'Posterior estimates'
(lcv1 = ggplot(temp,aes(x = mean,xmin = `0.025quant`,xmax = `0.975quant`,
                        y = coef,col = lik,fill = as.factor(sig),group = lik)) + 
    geom_vline(xintercept = 0,lty = 2,col = 'grey40') + 
    geom_errorbarh(height = 0.1,position = position_dodgev(0.5)) + 
    geom_point(position = position_dodgev(0.5),pch = 19,size = 1.5) + 
    geom_point(position = position_dodgev(0.5),pch = 21,size = 1.5) + 
    # facet_grid(~specification,scales = 'fixed') + 
    theme_bw() + 
    theme(legend.position = c(0.3,-0.14),axis.ticks.y = element_blank(),
          legend.direction = 'horizontal',axis.title.y = element_blank(),
          
          text = element_text(family = 'Times'),plot.margin = unit(c(.1,.1,1.1,.1),units = 'cm'),
          # legend.title.align = -1,legend.justification = -1,
          legend.background = element_rect(fill = NA),
          axis.text = element_text(size = 12),strip.text = element_text(size = 12),
          legend.text = element_text(size = 12),legend.title = element_blank()) +
    scale_x_continuous(name = '95% credible interval',limits = c(-0.9,0.9)) + 
    # scale_shape_manual(values = c(19,21))
    #scale_fill_manual(name = "outcome",values = c('white','orange','white','green'),labels = c('# projects','EIS/total')) + 
    #scale_color_manual(name = "outcome",values = c('orange','orange','green','green'),labels = c('# projects','EIS/total')) + 
    scale_fill_manual(values = c('white',NA)) + 
    scale_color_tableau(name = 'Outcome',labels=c('# projects','CEs/total analyses')) + 
    guides(shape = FALSE,fill = FALSE) + 
    ggtitle('Extractive projects') +
    NULL)
ggsave(plot = lcv1,filename = 'output/policypolitics/figures/coefplot_LCV_linear.withdrops.png',
       width = 5,height =6, units = 'in',dpi = 300)


temp = extract_coefs[grepl('LCV x',specification),]
temp$specification = 'Posterior estimates'
(lcv2 = ggplot(temp,aes(x = mean,xmin = `0.025quant`,xmax = `0.975quant`,
                                                                    y = coef,col = lik,fill = as.factor(sig),group = lik)) + 
  geom_vline(xintercept = 0,lty = 2,col = 'grey40') + 
  geom_errorbarh(height = 0.1,position = position_dodgev(0.5)) + 
  geom_point(position = position_dodgev(0.5),pch = 19,size = 1.5) + 
  geom_point(position = position_dodgev(0.5),pch = 21,size = 1.5) + 
 # facet_grid(~specification,scales = 'fixed') + 
    theme_bw() + 
  theme(legend.position = c(0.3,-0.14),axis.ticks.y = element_blank(),
        legend.direction = 'horizontal',axis.title.y = element_blank(),
        
      text = element_text(family = 'Times'),plot.margin = unit(c(.1,.1,1.1,.1),units = 'cm'),
     # legend.title.align = -1,legend.justification = -1,
     legend.background = element_rect(fill = NA),
        axis.text = element_text(size = 12),strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),legend.title = element_blank()) +
  scale_x_continuous(name = '95% credible interval',limits = c(-0.9,0.9)) + 
  # scale_shape_manual(values = c(19,21))
  #scale_fill_manual(name = "outcome",values = c('white','orange','white','green'),labels = c('# projects','EIS/total')) + 
  #scale_color_manual(name = "outcome",values = c('orange','orange','green','green'),labels = c('# projects','EIS/total')) + 
  scale_fill_manual(values = c('white',NA)) + 
  scale_color_tableau(name = 'Outcome',labels=c('# projects','CEs/total analyses')) + 
  guides(shape = FALSE,fill = FALSE) + 
  ggtitle('Extractive projects') +
  NULL)
ggsave(plot = lcv2,filename = 'output/policypolitics/figures/coefplot_LCV_interaction.withdrops.png',
       width = 5,height =6, units = 'in',dpi = 300)

# 
# all_coefs = coef_results[grepl("All",DV),]
# all_comp = ggplot(all_coefs,aes(x = mean,xmin = `0.025quant`,xmax = `0.975quant`,
#                                          y = coef,col = lik,fill = as.factor(sig),group = lik)) + 
#     geom_vline(xintercept = 0,lty = 2,col = 'grey40') + 
#     geom_errorbarh(height = 0.1,position = position_dodgev(0.5)) + 
#     geom_point(position = position_dodgev(0.5),pch = 19,size = 1.5) + 
#     geom_point(position = position_dodgev(0.5),pch = 21,size = 1.5) + 
#     facet_grid(~specification,scales = 'fixed') + theme_bw() + 
#     theme(legend.position = 'bottom',axis.title.y = element_blank()) +
#     scale_x_continuous(name = '95% credible interval') + 
#     # scale_shape_manual(values = c(19,21))
#     #scale_fill_manual(name = "outcome",values = c('white','orange','white','green'),labels = c('# projects','EIS/total')) + 
#     #scale_color_manual(name = "outcome",values = c('orange','orange','green','green'),labels = c('# projects','EIS/total')) + 
#     scale_fill_manual(values = c('white',NA)) + 
#     scale_color_tableau(name = 'Stage',labels =  c('procedural','distributive')) + 
#     guides(shape = FALSE,fill = FALSE) + 
#     ggtitle('All projects') +
#     NULL
# 
# ggsave(all_comp,filename = paste0('output/policypolitics/figures/coefplot_allprojects',names(mod_name_sets)[[mod]],'.withdrops.png'),dpi = 300,width = 7.5,height = 8,units = 'in')
# 
# 
# wildlife_coefs = coef_results[grepl("Wild",DV),]
# 
# lapply(seq_along(variations),function(x) {
# wildlifecomp = ggplot(wildlife_coefs[grepl(variations[x],specification),],aes(x = mean,xmin = `0.025quant`,xmax = `0.975quant`,
#                                  y = coef,col = lik,fill = as.factor(sig),group = lik)) + 
#     geom_vline(xintercept = 0,lty = 2,col = 'grey40') + 
#     geom_errorbarh(height = 0.1,position = position_dodgev(0.5)) + 
#     geom_point(position = position_dodgev(0.5),pch = 19,size = 1.5) + 
#     geom_point(position = position_dodgev(0.5),pch = 21,size = 1.5) + 
#     facet_grid(~specification,scales = 'fixed') + theme_bw() + 
#   theme(legend.position = 'bottom',axis.title.y = element_blank(),
#         axis.text = element_text(size = 12),strip.text = element_text(size = 12),
#         legend.text = element_text(size = 12)) +scale_x_continuous(name = '95% credible interval') + 
#     # scale_shape_manual(values = c(19,21))
#     #scale_fill_manual(name = "outcome",values = c('white','orange','white','green'),labels = c('# projects','EIS/total')) + 
#     #scale_color_manual(name = "outcome",values = c('orange','orange','green','green'),labels = c('# projects','EIS/total')) + 
#     scale_fill_manual(values = c('white',NA)) + 
#     scale_color_tableau(name = 'Stage',labels =  c('procedural','distributive')) + 
#     guides(shape = FALSE,fill = FALSE) + 
#     ggtitle('Wildlife/rec. projects') +
#     NULL
# 
# ggsave(wildlifecomp,filename = paste0('output/policypolitics/figures/coefplot_wildlife_',varnames[x],names(mod_name_sets)[[mod]],'.withdrops.png'),dpi = 300,width = 7,height = 8,units = 'in')
# })
# 
# 


temp = rbind(model_list_of_lists[[1]][[1]]$summary.random$u_forest_id %>% mutate(group = '# projects'),
             model_list_of_lists[[1]][[1]]$summary.random$y_forest_id %>% mutate(group = 'CE/total analyses'))

nms = gsub('\\sNational Forest($|s$)|National Forests in\\s','',admin_districts$FORESTNAME[match(temp$ID,admin_districts$FOREST_ID)])
nms = gsub('National Recreation Area','NRA',nms)
nms = gsub('National Scenic Area','NSA',nms)
nms = gsub('Manaement Unit','MU',nms)
temp$nm = nms
temp$nm <- fct_rev(temp$nm)

forest_re =  ggplot(data = temp) + 
  geom_errorbar(aes(ymin = `0.025quant`,ymax =`0.975quant`,x = nms,col = group,group = group),position = position_dodge(width = 1)) + 
  geom_point(aes(y = mean,x = nms,col = group,group = group),position = position_dodge(width = 1)) + theme_bw() + 
  scale_colour_colorblind(name = 'outcome') + coord_flip() + 
  theme(axis.title = element_blank(),legend.position = 'bottom',legend.direction = 'horizontal',axis.ticks = element_blank()) + 
  scale_y_continuous(name = '95% credible interval') + 
  ggtitle('Modeled intercepts by local administrative unit',subtitle = 'Extractive projects') 
ggsave(forest_re,filename = 'output/policypolitics/figures/random_intercepts_forest_extraction.withdrops.png',width = 8,height = 12,units = 'in',dpi = 300)



temp = rbind(model_list_of_lists[[2]][[1]]$summary.random$u_forest_id %>% mutate(group = '# projects'),
             model_list_of_lists[[2]][[1]]$summary.random$y_forest_id %>% mutate(group = 'CE/total analyses'))

nms = gsub('\\sNational Forest($|s$)|National Forests in\\s','',admin_districts$FORESTNAME[match(temp$ID,admin_districts$FOREST_ID)])
nms = gsub('National Recreation Area','NRA',nms)
nms = gsub('National Scenic Area','NSA',nms)
nms = gsub('Manaement Unit','MU',nms)
temp$nm = nms
temp$nm <- fct_rev(temp$nm)

forest_re =  ggplot(data = temp) + 
  geom_errorbar(aes(ymin = `0.025quant`,ymax =`0.975quant`,x = nms,col = group,group = group),position = position_dodge(width = 1)) + 
  geom_point(aes(y = mean,x = nms,col = group,group = group),position = position_dodge(width = 1)) + theme_bw() + 
  scale_colour_colorblind(name = 'outcome') + coord_flip() + 
  theme(axis.title = element_blank(),legend.position = 'bottom',legend.direction = 'horizontal',
        axis.ticks = element_blank(),axis.text.y = element_text(angle = 45)) + 
  scale_y_continuous(name = '95% credible interval') + 
  ggtitle('Modeled intercepts by local administrative unit',subtitle = 'Wildlife/recreation projects') 
ggsave(forest_re,filename = 'output/policypolitics/figures/random_intercepts_forest_rec_wildlife.withdrops.png',width = 9,height = 10,units = 'in',dpi = 300)

forest_re

temp = rbind(model_list_of_lists[[1]][[1]]$summary.random$u_congress_id %>% mutate(group = '# projects'),
             model_list_of_lists[[1]][[1]]$summary.random$y_congress_id %>% mutate(group = 'CE/total analyses'))


congress_re =  ggplot(data = temp) + 
  geom_errorbar(aes(ymin = `0.025quant`,ymax =`0.975quant`,x = as.factor(ID),col = group,group = group),position = position_dodge(width = 1)) + 
  geom_point(aes(y = mean,x = as.factor(ID),col = group,group = group),position = position_dodge(width = 1)) + theme_bw() + 
  scale_colour_colorblind(name = 'outcome') + coord_flip() + 
  theme(axis.title = element_blank(),legend.position = 'bottom',legend.direction = 'horizontal',axis.ticks = element_blank()) + 
  scale_y_continuous(name = '95% credible interval') + 
  ggtitle('Modeled intercepts by Congress',subtitle = 'Extractive projects') 
ggsave(congress_re,filename = 'output/policypolitics/figures/random_intercepts_congress_extraction.withdrops.png',width = 4,height = 6,units = 'in',dpi = 300)


temp = rbind(model_list_of_lists[[2]][[1]]$summary.random$u_congress_id %>% mutate(group = '# projects'),
             model_list_of_lists[[2]][[1]]$summary.random$y_congress_id %>% mutate(group = 'CE/total analyses'))

congress_re =  ggplot(data = temp) + 
  geom_errorbar(aes(ymin = `0.025quant`,ymax =`0.975quant`,x = as.factor(ID),col = group,group = group),position = position_dodge(width = 1)) + 
  geom_point(aes(y = mean,x = as.factor(ID),col = group,group = group),position = position_dodge(width = 1)) + theme_bw() + 
  scale_colour_colorblind(name = 'outcome') + coord_flip() + 
  theme(axis.title = element_blank(),legend.position = 'bottom',legend.direction = 'horizontal',axis.ticks = element_blank()) + 
  scale_y_continuous(name = '95% credible interval') + 
  ggtitle('Modeled intercepts by Congress',subtitle = 'Wildlife/recreation projects') 
ggsave(congress_re,filename = 'output/policypolitics/figures/random_intercepts_congress_rec_wildlife.withdrops.png',width = 4,height = 6,units = 'in',dpi = 300)



htmlTable::htmlTable(round(do.call(rbind,list(
model_list_of_lists[[1]][[1]]$summary.hyperpar[,c(1,3,5)],
model_list_of_lists[[1]][[2]]$summary.hyperpar[,c(1,3,5)],
model_list_of_lists[[2]][[1]]$summary.hyperpar[,c(1,3,5)],
model_list_of_lists[[2]][[2]]$summary.hyperpar[,c(1,3,5)])),2))


