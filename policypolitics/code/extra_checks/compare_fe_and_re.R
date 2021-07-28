#setwd('../manitou')
if(!require(data.table)){install.packages('data.table');require(data.table)}
if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
if(!require(INLA)){install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE);require(INLA)}
if(!require(INLAutils)){devtools::install_github('timcdlucas/INLAutils');require(INLAutils)}
if(!require(matrixStats)){install.packages('matrixStats');require(matrixStats)}

# From github
library(devtools)

library(ggthemes)
empty_list = data.table()
locs = 'policypolitics/model_objects/models_Type_Purpose_Extractive_FE.RDS'

fe_mod = readRDS(locs)
re_mod = readRDS('policypolitics/model_objects/models_Type_Purpose_Extractive.RDS')

#focus exclusively on primary, LCV * unemployemnt models#
mod_list = list(re_mod[[2]],fe_mod[[2]])

coef_results = rbindlist(lapply(seq_along(mod_list),function(x) mod_list[[x]]$summary.fixed[,c(1,3,5)] %>%
                                  mutate(specification = ifelse(x == 1,'random intercepts','fixed intercepts'),coef = rownames(.),  form =   names(mod_list)[x])))

coef_results = coef_results[!coef%in%c('mu.u','mu.y')]
#coef_results = coef_results[specification!=3,]
#drop out fixed effects
coef_results = coef_results[!grepl("_id",coef),]

coef_results$lik <- ifelse(grepl('^u_|mu\\.u',coef_results$coef),'# projects','CEs/total analyses')
coef_results$coef <- gsub('^y_|^u_','',coef_results$coef)
coef_results$coef <- gsub(':y_|:u_','x',coef_results$coef)

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

  (extract_comp = ggplot(extract_coefs,aes(x = mean,xmin = `0.025quant`,xmax = `0.975quant`,
        y = coef,col = specification,fill = as.factor(sig))) + 
    facet_wrap(~lik) + 
    geom_vline(xintercept = 0,lty = 2,col = 'grey40') + 
    geom_errorbarh(height = 0.1,position = position_dodge(0.5)) + 
    geom_point(position = position_dodge(0.5),pch = 19,size = 1.5) + 
    geom_point(position = position_dodge(0.5),pch = 21,size = 1.5) + 
   theme_bw() + 
    theme(legend.position = 'bottom',axis.title.y = element_blank(),legend.title = element_blank(),
          axis.text = element_text(size = 12),strip.text = element_text(size = 12),
          legend.text = element_text(size = 12)) +
    scale_x_continuous(name = '95% credible interval') + 
    # scale_shape_manual(values = c(19,21))
    #scale_fill_manual(name = "outcome",values = c('white','orange','white','green'),labels = c('# projects','EIS/total')) + 
    #scale_color_manual(name = "outcome",values = c('orange','orange','green','green'),labels = c('# projects','EIS/total')) + 
    scale_fill_manual(values = c('white',alpha('white',0))) + 
    scale_color_tableau(name = 'Specification') + 
    guides(shape = 'none',fill = 'none') + 
    ggtitle('Comparing coefficient estimates from random and fixed intercept models') +
    NULL)


ggsave(extract_comp,filename = paste0('policypolitics/tables_figures/figures/figureB_re_vs_fe_coefplot.tiff'),dpi = 350,width = 7.5,height = 8,units = 'in')
  

subtab = extract_coefs[grepl("LCV|Unemp",coef)]
subtab$CI =   paste(formatC(round(subtab$mean,3),digits = 3,flag = 0),paste0('(',
              formatC(round(subtab$`0.025quant`,3),digits = 3,flag = 0),', ',
               formatC(round(subtab$`0.975quant`,3),digits = 3,flag = 0),')'))

require(htmlTable)

comp_coef = htmlTable(dcast(subtab[,.(CI,specification,coef,lik)],coef ~ specification + lik,value.var = 'CI')[order(-coef)],
          header=c('Parameter',rep(c('# projects','CE Ratio'),2)),n.cgroup = c(1,2,2),cgroup = c('','Random effects model','Fixed effects model'),
          caption = '95% credible intervals for LCV score * unemployment %, random and fixed effect models')
outdir.tables = "policypolitics/tables_figures/tables/" 
tableout <-htmlTable(comp_coef)
sink(paste0(outdir.tables,"appendix_compare_credible_intervals.html"))
print(tableout,type="html",useViewer=F)
sink()


