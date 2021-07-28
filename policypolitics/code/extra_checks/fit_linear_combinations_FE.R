

#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
if(!require(INLA)){install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)}

packages = c('data.table','stringr','tidyverse','sf','lwgeom','ggthemes','tigris','lubridate','aod','MASS')
not_installed = packages[!packages %in% installed.packages()[,'Package']]
if(length(not_installed)>0){lapply(not_installed,install.packages)}
lapply(packages,require,character.only = T)

# From github

empty_list = data.table()
locs = 'policypolitics/model_objects/models_Type_Purpose_Extractive_FE.RDS'
  
fe_mod = readRDS(locs)
re_mod = readRDS('policypolitics/model_objects/models_Type_Purpose_Extractive.RDS')



model_list = readRDS(locs)
uy_ex = model_list[[1]]$.args$data$Y
eu = as.numeric(uy_ex[1:nrow(uy_ex)/2,1])
ey = as.numeric(uy_ex[(1:nrow(uy_ex)/2) + nrow(uy_ex)/2,2])

bprior <- list(prior = 'gaussian', param = c(0,1))

allvars = lapply(model_list,function(x) x$model.matrix@Dimnames[[2]])
intervars = lapply(allvars,function(x)  grep('LCV_annual|Unemp_Rate|percentD_H|democrat$',x,value=T))
cres <- list(return.marginals.predictor = FALSE, 
             return.marginals.random = FALSE)


idat = model_list[[1]]$.args$data

nb_mod = glm.nb(u~  u_Unemp_Rate + u_Perc_Extraction_Employ + u_ln_County_naturalresource_GDP_1M +
                  u_ln_Receipts_Extraction_1M_P4 +
                  #u_Ln_ACRES + u_Wilderness_Perc +
                  u_Burned_Perc_Past5yrs + 
                  #u_Ln_AVERAGE_YEARLY_VISITS + 
                  #u_Count_EorT_Species +
                  #u_Perc_WUI_Housing + u_demPres + u_demCongress + 
                  u_mrp_mean +
                  u_LCV_annual,data = lapply(idat,function(x) x[1:length(idat$u)]))


u.sdres <- sd(residuals(nb_mod))
temp_dat = as.data.frame(lapply(idat,function(x) x[length(idat$u)+{1:length(idat$y)}]))
keep_index = which(!is.na(idat$y))
drop_index = which(is.na(idat$y))
temp_dat = temp_dat[keep_index,]
temp_dat$y = idat$y[keep_index]
temp_dat$u = idat$u[keep_index]

bin_mod = betabin(cbind(y,u-y) ~
                    y_Unemp_Rate + 
                    y_Perc_Extraction_Employ + 
                    y_ln_County_naturalresource_GDP_1M +
                    y_ln_Receipts_Extraction_1M_P4 + 
                  #  y_Ln_ACRES + y_Wilderness_Perc +
                    y_Burned_Perc_Past5yrs + 
                   # y_Ln_AVERAGE_YEARLY_VISITS + y_Count_EorT_Species +
                  #  y_Perc_WUI_Housing + 
                  #  y_demPres + y_demCongress + 
                    y_mrp_mean +
                    y_LCV_annual,~1,data =  temp_dat,control = list(maxit = 10e3))

y.sdres <- sd(residuals(bin_mod))
pc.prec.used = list(prec= list(prior = "pc.prec", param = c(u.sdres,0.01)),
                    prec = list(prior = "pc.prec", param = c(y.sdres,0.01)))


u.resp <- sd(idat$u,na.rm = T)
y.resp <- sd(idat$y/idat$u,na.rm=T)

famcontrol = list(list(prior = "pcprec", param = c(u.resp,0.01)),
                  list(prior = "pcprec", param = c(y.resp,0.01)))

bprior <- list(prior = 'gaussian', param = c(0,1))

which_quantiles = function(v) {if(grepl('Unemp',v)){x1seq}else{x2seq}}

for(i in seq_along(intervars)){
  #  if(length(intervars[[i]])>0){
      tdt = as.data.table(as.matrix(model_list[[i]]$model.matrix))
      iname = lapply(intervars[[i]],function(k)  k)
      #ivars = lapply(intervars[[i]],function(k)  c(unlist(str_split(k,':'))))
      cols = unique(unlist(iname))
      constituents = grep(':',cols,value = T,invert = T)
      temp_vars = data.table(tdt[mu.u==1,grep('^u_',cols,value = T),with = F],
      tdt[mu.u==0,grep('^y_',cols,value = T),with = F])
      x1seq = seq(0.10,0.90,0.01)
    
      if(any(grepl('u_democrat',constituents))){x2seq = c(0,1)}
      if(!any(grepl('u_democrat',constituents))){x2seq = c(0.05,0.95)}
     
        quants_to_use = lapply(constituents,which_quantiles)

        quant_values = mapply(function(c,q) quantile(c,q),c = lapply(constituents,function(v) temp_vars[[v]]),q = quants_to_use)
        names(quant_values) = constituents
        u_combos = expand.grid(quant_values[grepl('u_',names(quant_values))])
        y_combos = expand.grid(quant_values[grepl('y_',names(quant_values))])
         if(any(grepl('^u_.*:',cols))){
           u_combos[[grep('^u_.*:',cols,value = T)]] <- u_combos[,1] * u_combos[,2]
           y_combos[[grep('^y_.*:',cols,value = T)]] <- y_combos[,1] * y_combos[,2]
          }
      #lcomb_data = data.table(u_combos[grep('Unemp',names(u_combos),value = T)],y_combos[grep('Unemp',names(y_combos),value = T)])
      lcomb_data = data.table(u_combos,y_combos)
      
      lcomb_data = lcomb_data[rowSums(lcomb_data)!=0,]
      lcom_data = lcomb_data[!duplicated(lcomb_data),]
      lcu = inla.make.lincombs(lcomb_data[,grepl('^u',names(lcomb_data)),with = F])
      lcy = inla.make.lincombs(lcomb_data[,grepl('^y',names(lcomb_data)),with = F])
      # 
      # lcu <- inla.make.lincombs(
      #   'l1' = lcomb_data[[ivars[[1]][1]]],
      #   "l2" = lcomb_data[[ivars[[1]][2]]],
      #   "l3"  = lcomb_data[[ivars[[1]][1]]] * lcomb_data[[ivars[[1]][2]]])
      # for(n in seq_along(lcu)){
      #   names(lcu[[n]][[1]]) <- ivars[[1]][1]
      #   names(lcu[[n]][[2]]) <- ivars[[1]][2]
      #   names(lcu[[n]][[3]]) <- iname[1]
      # }
      # 
      # lcy <- inla.make.lincombs(
      #   'l1' = lcomb_data[[ivars[[2]][1]]],
      #   "l2" = lcomb_data[[ivars[[2]][2]]],
      #   "l3"  = lcomb_data[[ivars[[2]][1]]] * lcomb_data[[ivars[[2]][2]]])
      # for(n in seq_along(lcy)){
      #   names(lcy[[n]][[1]]) <- ivars[[2]][1]
      #   names(lcy[[n]][[2]]) <- ivars[[2]][2]
      #   names(lcy[[n]][[3]]) <- iname[2]
      # }
      
      names(lcu) = paste('u_', names(lcu), sep="")
      names(lcy) = paste('y_', names(lcy), sep="")
      lc = c(lcu, lcy)

      newmodel = inla(formula = model_list[[i]]$.args$formula ,
                      control.compute = list(waic=TRUE,dic=TRUE),
                      family = model_list[[i]]$.args$family,
                      Ntrials = model_list[[i]]$.args$Ntrials,
                      control.family = famcontrol,
                      control.results = cres,
                      control.fixed = list(expand.factor.strategy = "inla"),
                      data=model_list[[i]]$.args$data,lincomb = lc,
                      control.update = list(result = model_list[[i]]),
                      control.predictor=list(compute=TRUE),verbose=F)
      
      lcd = newmodel$summary.lincomb.derived
      
      lcd$id = rownames(lcd)
      lcd$group = str_extract(lcd$id,'^[u-y]')
      lcd$group = ifelse(lcd$group== 'u','Project count','CE/total NEPA analyses')
      lcd$group <- fct_rev(as.factor(lcd$group))
      
      if(ncol(lcomb_data)==2){
        qvals = lcomb_data[,1:2]
        names(qvals) <- c('scale_val1','scale_val2')
        lcd$x1_quantile = unlist(replicate(1,rep(x1seq,nrow(lcd)/2/length(x1seq)),simplify = F))
      }
      if(ncol(lcomb_data)>2){
        qvals = rbind(lcomb_data[,1:2],lcomb_data[,3:4],use.names = F)
        names(qvals) <- c('scale_val1','scale_val2')
        lcd$x1_quantile = unlist(replicate(2,rep(x1seq,nrow(lcd)/2/length(x1seq)),simplify = F))
        lcd$x2_quantile = unlist(replicate(2,rep(x2seq,each = nrow(lcd)/2/length(x2seq)),simplify = F))
      }

      lcd = cbind(lcd,qvals)

      axnames = unique(str_remove(names(temp_vars),'^[u-y]_'))
      lcd[[axnames[[1]]]]<-lcomb_data[[names(temp_vars)[1]]]
      lcd[[axnames[[2]]]]<-lcomb_data[[names(temp_vars)[2]]]
      intername = gsub(':','x',str_remove_all(iname[[1]],'u_'))
      figname = paste('Extraction',intername,sep='_')
      lcd = data.table(lcd)
      #lcd = lcd[!duplicated(paste(scale_val1,scale_val2,group)),]
      lcd$i = i
      lcd$DV <- 'Extraction'
      lcd$form <-  names(model_list)[i]
      empty_list = rbind(empty_list,lcd,fill = T,use.names = T)

  #  }
  }


#fwrite(empty_list,'policypolitics/tables_figures/tables/interaction_results.csv')


#empty_list$outcome = name_matcher$outcome[match(empty_list$i,name_matcher$i)]


#empty_list = fread('output/policypolitics/interaction_results.csv')


qvals_LCV = c('0.05','0.95')
qval_labels_lcv = c('~0','~95')

ext_dt = empty_list
ext_dt$sig = ifelse(ext_dt$`0.025quant`<0&ext_dt$`0.975quant`>0,0,1)


ext_dt_lcv_linear = ext_dt[form == 'form0',]
ext_dt_lcv_interact = ext_dt[form == 'form0x',]

require(htmlTable)

htmlTable(ext_dt_lcv_interact[,.(group,x1_quantile,x2_quantile,mean = round(mean,3),`0.025quant` = round(`0.025quant`,3),`0.975quant` = round(`0.975quant`,3))],
          file = 'policypolitics/tables_figures/tables/extra_tables/interaction_combination_values_FE.html')


((gg_lcv_vs_unemp_extraction_interact_count = ggplot(data = ext_dt_lcv_interact[group=='Project count'&!grepl('alt',form)][order(x1_quantile)],
       aes(x = x1_quantile,y = mean,ymin = `0.025quant`,fill = as.factor(x2_quantile),
           ymax = `0.975quant`,group = as.factor(x2_quantile),
           col = as.factor(x2_quantile)))  + 
 # facet_wrap(~ group, scales = 'free_y',ncol = 2) + 
  #geom_path(position = position_dodge(0.05)) +
  geom_ribbon(aes(fill = as.factor(x2_quantile)),alpha = 0.2,lty= 2)+
   geom_path(aes(col = as.factor(x2_quantile)))+
 # geom_errorbar(position = position_dodge(0.01),alpha = 0.4) + 
  #geom_point(position = position_dodge(0.01),pch = 19) + 
  #geom_point(position = position_dodge(0.01),pch = 21) + 
  scale_fill_colorblind()+
  scale_x_continuous(name = paste('% unemployment quantile')) +
  scale_y_continuous(name = '95% credible interval') +#limits = c(0.65,1.3))+
  scale_color_colorblind(name = 'annual LCV score',labels=qval_labels_lcv) +
  #scale_color_viridis_d(name = 'annual LCV score',option = 'D',labels=qval_labels_lcv) + 
  guides(fill = FALSE) + 
  ggtitle('# extractive projects',subtitle ='LCV annual x unemployment %')+
  theme_bw() + theme(legend.position = c(0.2,0.15),legend.direction = 'vertical',
                     legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25)))))


ggsave(gg_lcv_vs_unemp_extraction_interact_count,dpi = 350,width = 6,height = 4.5, units = 'in',
       filename = paste0('policypolitics/tables_figures/figures/figures_in_paper/figureA4_interaction_extraction_projcount_lcv_vs_unemp_FE.tiff'))


(gg_lcv_vs_unemp_extraction_interact_ce = ggplot(data = ext_dt_lcv_interact[group=="CE/total NEPA analyses"&!grepl('alt',form),][order(x1_quantile),],
                                     aes(x = x1_quantile,y = mean,ymin = `0.025quant`,fill = as.factor(x2_quantile),
                                         ymax = `0.975quant`,group = as.factor(x2_quantile),
                                         col = as.factor(x2_quantile)))  + 
    # facet_wrap(~ group, scales = 'free_y',ncol = 2) + 
    #geom_path(position = position_dodge(0.05)) +
    geom_ribbon(aes(fill = as.factor(x2_quantile)),alpha = 0.2,lty= 2)+
    geom_path(aes(col = as.factor(x2_quantile)))+
    scale_fill_colorblind()+
    scale_x_continuous(name = paste('% unemployment quantile')) +
    scale_y_continuous(name = '95% credible interval',limits = c(-0.9,0.9))+
    #scale_color_viridis_d(name = 'annual LCV score',option = 'D',labels=qval_labels_lcv) + 
    scale_color_colorblind(name = 'annual LCV score',labels=qval_labels_lcv) +
    guides(fill = FALSE) + 
    ggtitle('CEs/total NEPA analyses',subtitle ='LCV annual x unemployment %')+
    theme_bw() + theme(legend.position = c(0.2,0.15),legend.direction = 'vertical',
                       legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25))))


ggsave(gg_lcv_vs_unemp_extraction_interact_ce,dpi = 350,width = 6,height = 4.5, units = 'in',
       filename = paste0('policypolitics/tables_figures/figures/figures_in_paper/figureA5_interaction_extraction_CEratio_lcv_vs_unemp_FE.tiff'))

