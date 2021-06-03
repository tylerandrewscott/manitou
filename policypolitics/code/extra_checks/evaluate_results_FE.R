  
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
   albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
 
  admin_districts <- readRDS('prepped/admin_units_clean.RDS')
  locs = 'output/policypolitics/model_objects/'
  spec_names = data.table(specification = 1:6,name =c('Annual LCV score','LCV x % unemp','% dem. vote','% dem. x % unemp','Dem. rep.','Dem. rep. % unemp.'))

  
  mod_list = readRDS(paste0('policypolitics/model_objects/models_Type_Purpose_Extractive_FE.RDS'))

  sapply(mod_list,function(x) x$waic$waic)
  temp = (mod_list[[2]])
 congressA = ggplot(temp$summary.random$u_congress_id ,aes(x = as.factor(ID),y = mean,ymin = `0.025quant`,ymax = `0.975quant`)) + geom_point() + geom_errorbar() + theme_bw() +
   ggtitle('modeled intercept by congress for total project')
 congressB = ggplot(temp$summary.random$y_congress_id ,aes(x = as.factor(ID),y = mean,ymin = `0.025quant`,ymax = `0.975quant`)) + geom_point() + geom_errorbar() + theme_bw() + 
   ggtitle('modeled intercept by congress for CE/total projects')
 library(gridExtra)
 grid.arrange(congressA,congressB,ncol = 2)
 
 (waic_table = as.data.table(lapply(mod_list,function(y) y$waic$waic)))
  colnames(waic_table) <- names(model_list_of_lists)
  
 fwrite(waic_table,'policypolitics/tables_figures/tables/waic_table_FE.csv')

  coef_df = rbindlist(lapply(seq_along(mod_list), function(y) {
    mod_list[[y]]$summary.fixed[,c(1,3,5)] %>% mutate(coef = rownames(.), mod = y,DV = names(mod_list)[y])}))
  coef_df$cred = paste(formatC(round(coef_df$mean,3),digits = 3,flag = 0),paste0('(',
        formatC(round(coef_df$`0.025quant`,3),digits = 3,flag = 0),', ',
        formatC(round(coef_df$`0.975quant`,3),digits = 3,flag = 0),')'))
  coef_df$DV <- 'Extractive'
  coef_df$coef <- fct_inorder(coef_df$coef)
  coef_df = coef_df[mod%in%c(1,2),]
  
  coef_df_s1 = coef_df[grepl('Extr',DV),.(coef,cred,mod)][grepl('^u|mu\\.u',coef),]
  coef_df_s2 = coef_df[grepl('Extr',DV),.(coef,cred,mod)][!grepl('^u|mu\\.u',coef),]
  

  coef_cast1 = dcast(coef_df_s1,coef ~ mod,value.var = 'cred',fill = '---')
  coef_cast2 = dcast(coef_df_s2,coef ~ mod,value.var = 'cred',fill = '---')
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
                                 'ln(yearly visitation)' = 'Ln_AVERAGE_YEARLY_VISITS',
                                 'ln(county NR GDP ($1M))' = 'ln_County_naturalresource_GDP_1M',
                                 'Democratic president' = 'demPres','Democratic congress' = 'demCongress',
                                 "% dem. vote x unemp. %" = "Unemp_RatexpercentD_H"   ,
                                 'Public ideology' = 'mrp_mean',
                                 'Dem. rep.' = 'democrat','Dem. rep. x unemp. %' = "Unemp_Ratexdemocrat" ,
                                 'LCV annual x unemp. %' = 'Unemp_RatexLCV_annual')
  
  simple_table$coef  = fct_relevel( simple_table$coef ,'ln(forest acreage)','ln(yearly visitation)',
              '% wilderness area','# listed species','% burned (last 5 years)','% housing in WUI',
              '% extraction employ.','ln(county NR GDP ($1M))',
              'Democratic president','Democratic congress',
              '% dem. vote share','LCV annual score','Dem. rep.', 'Unemployment %',
              "% dem. vote x unemp. %",'LCV annual x unemp. %','Dem. rep. x unemp. %')


temp_coef_table = simple_table[!grepl('_id',coef),]
library(tableHTML)
  ht = tableHTML(temp_coef_table,rownames = F,footer = paste0('WAIC scores-Model 1: ',round(as.numeric(waic_table[1,1])),'; Model 2: ',round(as.numeric(waic_table[1,2]))), headers = c('parameter','Model 1: baseline','Model 2: LCV x % unemp.','Model 1: baseline','Model 2: LCV x % unemp.'),
          second_headers = list(c(1,2,2),c('','# projects (neg. binomial)','CE ratio (beta-binomial)')))
write_tableHTML(ht, file = 'policypolitics/tables_figures/tables/tableB2_coefficient_estimates_FE.html')


  coef_results = rbindlist(lapply(seq_along(mod_list),function(x) mod_list[[x]]$summary.fixed[,c(1,3,5)] %>%
           mutate(specification = x,coef = rownames(.),  form =   names(mod_list)[x])))
  
  coef_results = coef_results[!coef%in%c('mu.u','mu.y')]
  #coef_results = coef_results[specification!=3,]


coef_results$specification <-   spec_names$name[match(coef_results$specification,spec_names$specification)]
coef_results$specification <- fct_inorder(coef_results$specification)

coef_results$lik <- ifelse(grepl('^u_|mu\\.u',coef_results$coef),'# projects','CEs/total analyses')
coef_results$coef <- gsub('^y_|^u_','',coef_results$coef)
coef_results$coef <- gsub(':y_|:u_','x',coef_results$coef)



coef_results = coef_results[!grepl('_id',coef),]
coef_results$coef = fct_recode(coef_results$coef, '% wilderness area' = 'Wilderness_Perc',
                               '% dem. vote share' = 'percentD_H',
                               '% housing in WUI' = 'Perc_WUI_Housing',
           '# listed species' = 'Count_EorT_Species','ln(forest acreage)'='Ln_ACRES',
           '% burned (last 5 years)'='Burned_Perc_Past5yrs','LCV annual score'='LCV_annual',
           'Unemployment %' = 'Unemp_Rate',
           '% extraction employ.' = 'Perc_Extraction_Employ',
           'ln(yearly visitation)' = 'Ln_AVERAGE_YEARLY_VISITS',
           'ln(county NR GDP ($1M))' = 'ln_County_naturalresource_GDP_1M',
           'Democratic president' = 'demPres','Democratic congress' = 'demCongress',
           "% dem. vote x unemp. %" = "Unemp_RatexpercentD_H"   ,
           'ln(Resource receipts, last 4 yrs)' = 'ln_Receipts_Extraction_1M_P4',
           'ln(Recreation receipts, last 4 yrs)' = 'ln_Receipts_Recreation_1M_P4',
           'Public ideology' = 'mrp_mean',
           'Dem. rep.' = 'democrat','Dem. rep. x unemp. %' = "Unemp_Ratexdemocrat" ,
           'LCV annual x unemp. %' = 'Unemp_RatexLCV_annual',
           'House committee LCV' = 'ComLCV','House chair LCV' = 'ChairLCV')
    
#coef_results$coef <- fct_inorder(coef_results$coef)
coef_results$coef <- fct_relevel(coef_results$coef,'ln(forest acreage)','ln(yearly visitation)',
                                 'ln(Resource receipts, last 4 yrs)',
                                 '% wilderness area','# listed species','% burned (last 5 years)','% housing in WUI',
                                 '% extraction employ.','ln(county NR GDP ($1M))',
                                 'Democratic president','Democratic congress',   'Public ideology' ,
                                 '% dem. vote share','LCV annual score','Dem. rep.', 'Unemployment %',
                                 "% dem. vote x unemp. %",'LCV annual x unemp. %','Dem. rep. x unemp. %')

coef_results$coef <- fct_rev(coef_results$coef)
coef_results$sig <- (!(coef_results$`0.025quant`<0 & coef_results$`0.975quant`>0)) + 0


base_coefs = coef_results[specification%in%'Annual LCV score',]

extract_coefs = coef_results


variations = c('LCV','Dem. rep','% dem')
varnames = c('LCV','demRep','demVote')

lapply(seq_along(variations),function(x) {
  print(x)
  extract_comp = ggplot(extract_coefs[grepl(variations[x],specification),],aes(x = mean,xmin = `0.025quant`,xmax = `0.975quant`,
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
if(varnames[x]=='LCV'){
ggsave(extract_comp,filename = paste0('policypolitics/tables_figures/figures/figure3_coefplot_extraction_FE_',varnames[x],'.png'),dpi = 500,width = 7.5,height = 8,units = 'in')
}
  if(varnames[x]!='LCV'){
    ggsave(extract_comp,filename = paste0('policypolitics/tables_figures/figures/coefplot_extraction_FE_',varnames[x],'.png'),dpi = 500,width = 7.5,height = 8,units = 'in')
  }
  })


temp = rbind(mod_list[[2]]$summary.random$u_forest_id %>% mutate(group = '# projects'),
             mod_list[[2]]$summary.random$y_forest_id %>% mutate(group = 'CE/total analyses'))

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
ggsave(forest_re,filename = 'policypolitics/tables_figures/figures/random_intercepts_forest_extraction.png',width = 8,height = 12,units = 'in',dpi = 300)

temp = rbind(mod_list[[2]]$summary.random$u_forest_id %>% mutate(group = '# projects'),
             mod_list[[2]]$summary.random$y_forest_id %>% mutate(group = 'CE/total analyses'))

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
ggsave(forest_re,filename = 'policypolitics/tables_figures/figures/random_intercepts_forest_rec_wildlife_FE.png',width = 9,height = 10,units = 'in',dpi = 300)

temp = rbind(mod_list[[2]]$summary.random$u_congress_id %>% mutate(group = '# projects'),
             mod_list[[2]]$summary.random$y_congress_id %>% mutate(group = 'CE/total analyses'))

congress_re =  ggplot(data = temp) + 
  geom_errorbar(aes(ymin = `0.025quant`,ymax =`0.975quant`,x = as.factor(ID),col = group,group = group),position = position_dodge(width = 1)) + 
  geom_point(aes(y = mean,x = as.factor(ID),col = group,group = group),position = position_dodge(width = 1)) + theme_bw() + 
  scale_colour_colorblind(name = 'outcome') + coord_flip() + 
  theme(axis.title = element_blank(),legend.position = 'bottom',legend.direction = 'horizontal',axis.ticks = element_blank()) + 
  scale_y_continuous(name = '95% credible interval') + 
  ggtitle('Modeled intercepts by Congress',subtitle = 'Extractive projects') 
ggsave(congress_re,filename = 'policypolitics/tables_figures/figures/random_intercepts_congress_extraction.png',width = 4,height = 6,units = 'in',dpi = 300)


temp_tab = (round(do.call(rbind,list(
mod_list[[1]]$summary.hyperpar[,c(1,3,5)],
mod_list[[2]]$summary.hyperpar[,c(1,3,5)],
mod_list[[3]]$summary.hyperpar[,c(1,3,5)],
mod_list[[4]]$summary.hyperpar[,c(1,3,5)],
mod_list[[5]]$summary.hyperpar[,c(1,3,5)],
mod_list[[6]]$summary.hyperpar[,c(1,3,5)])),2))

temp_coef_table = temp_tab
temp_coef_table

(rem1 = apply(round(mod_list[[1]]$summary.hyperpar[,c(1,3,5)],3),2,formatC,format = 's',drop0trailing = F,digits = 3,flag = 0))
(rem2 = apply(round(mod_list[[2]]$summary.hyperpar[,c(1,3,5)],3),2,formatC,format = 's',drop0trailing = F,digits = 3,flag = 0))
rem1 = data.table(rem1)
rem2 = data.table(rem2)
rem1$ci = str_replace(str_replace_all(paste0(rem1$mean,' (',rem1$`0.025quant`,', ',rem1$`0.975quant`,')'),'\\s{1,}',' '),'^\\s','')
rem2$ci = str_replace(str_replace_all(paste0(rem2$mean,' (',rem2$`0.025quant`,', ',rem2$`0.975quant`,')'),'\\s{1,}',' '),'^\\s','')

library(tableHTML)
remboth = data.table(hyper = rownames(mod_list[[1]]$summary.hyperpar),cbind(rem1[,ci],rem2[,ci]))
remboth$hyper = rownames(mod_list[[1]]$summary.hyperpar)
remboth$hyper <- gsub('(u|y)_forest_id','forest latent effect',remboth$hyper)
remboth$hyper <- gsub('(u|y)_congress_id','congress latent effect',remboth$hyper)
remboth$hyper <- gsub('(u|y)_state_id','state latent effect',remboth$hyper)
remboth$hyper <- gsub('(u|y)_region_id','region latent effect',remboth$hyper)

rem_table = tableHTML(remboth,rownames = F,
          headers = c('hyperparameter','Model 1 (baseline)','Model 2 (LCV x % unemp.)'))

write_tableHTML(rem_table, file = 'policypolitics/tables_figures/tables/tableB3_random_effect_estimates.html')




