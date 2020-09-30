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
locs = 'output/policypolitics/model_objects/'
model_sets = list.files('output/policypolitics/model_objects/','models')
model_sets <- model_sets[which(grepl('Purpose.*Extract.*drop',model_sets))]
model_list_of_lists = lapply(model_sets,function(x) readRDS(paste0(locs,x)))

uy_ex = model_list_of_lists[[1]][[1]]$.args$data$Y
uy_rec = model_list_of_lists[[2]][[1]]$.args$data$Y

#correlation between extraction and rec counts
cor(uy_ex[,1],uy_rec[,1],use = 'pairwise.complete.obs')
#correlation between extraction and rec CE ratios
cor(uy_ex[,2],uy_rec[,2],use = 'pairwise.complete.obs')

uy_ex[which(is.na(uy_ex[,2])),1]
eu = as.numeric(uy_ex[1:nrow(uy_ex)/2,1])
ey = as.numeric(uy_ex[(1:nrow(uy_ex)/2) + nrow(uy_ex)/2,2])

cor(cbind(eu,ey),use = 'pairwise.complete.obs')

#cor(eu,ey,use = 'pairwise.complete.obs')
#cor(uy_ex[1:nrow(uy_ex)/2,1],
#uy_ex[(1:nrow(uy_ex)/2) + nrow(uy_ex)/2,2],use = 'pairwise.complete.obs')
#uy_rec[which(is.na(uy_ex[,1])),2]

#cor(uy_ex[!which(is.na(uy_ex[,1])),1],uy_rec[which(is.na(uy_ex[,1])),2],use = 'pairwise.complete.obs')

#modnames = str_remove(str_remove(str_extract(model_sets,'models_[A-Z-a-z_]+'),'models_Type_Purpose'),'models_Type_')
mod_names = gsub('\\.RDS','',str_remove(model_sets,'models_Type_Purpose_'))

bprior <- list(prior = 'gaussian', param = c(0,1))

allvars = lapply(model_list_of_lists,function(x) sapply(x,function(y) y$model.matrix@Dimnames[[2]]))


intervars = lapply(allvars,function(x) lapply(x,function(y) grep(':',y,value=T)))

for(i in seq_along(intervars)){
  for(j in seq_along(intervars[[i]])){
    if(length(intervars[[i]][[j]])>0){
      print(j)
      tdt = as.data.table(as.matrix(model_list_of_lists[[i]][[j]]$model.matrix))
      iname = lapply(intervars[[i]][[j]],function(k)  k)
      ivars = lapply(intervars[[i]][[j]],function(k)  c(unlist(str_split(k,':'))))
      temp_vars = data.table(tdt[mu.u ==1,ivars[[1]],with = F],
                             tdt[mu.u ==0,ivars[[2]],with = F])
      cols = unlist(ivars)
      x1seq = seq(0.05,0.95,0.05)
      x2seq = seq(0.05,0.95,0.05)
     
        u_combos = expand.grid(apply(temp_vars[,ivars[[1]][1],with=F],2,quantile,x1seq),
                               apply(temp_vars[,ivars[[1]][2],with=F],2,quantile,x2seq))
        names(u_combos) = ivars[[1]]
        y_combos = expand.grid(apply(temp_vars[,ivars[[2]][1],with=F],2,quantile,x1seq),
                               apply(temp_vars[,ivars[[2]][2],with=F],2,quantile,x2seq))
        names(y_combos) = ivars[[2]]
      lcomb_data = data.table(u_combos,y_combos)
      lcomb_data = lcomb_data[rowSums(lcomb_data)!=0,]

     # lcomb_data = lcomb_data[u_democrat!=0,]
      l1 = list(lcomb_data[[ivars[[1]][1]]])
      names(l1) <- ivars[[1]][1]
      lcu <- inla.make.lincombs(
        'l1' = lcomb_data[[ivars[[1]][1]]],
        "l2" = lcomb_data[[ivars[[1]][2]]],
        "l3"  = lcomb_data[[ivars[[1]][1]]] * lcomb_data[[ivars[[1]][2]]])
      for(n in seq_along(lcu)){
        names(lcu[[n]][[1]]) <- ivars[[1]][1]
        names(lcu[[n]][[2]]) <- ivars[[1]][2]
        names(lcu[[n]][[3]]) <- iname[1]
      }
      
      lcy <- inla.make.lincombs(
        'l1' = lcomb_data[[ivars[[2]][1]]],
        "l2" = lcomb_data[[ivars[[2]][2]]],
        "l3"  = lcomb_data[[ivars[[2]][1]]] * lcomb_data[[ivars[[2]][2]]])
      for(n in seq_along(lcy)){
        names(lcy[[n]][[1]]) <- ivars[[2]][1]
        names(lcy[[n]][[2]]) <- ivars[[2]][2]
        names(lcy[[n]][[3]]) <- iname[2]
      }
      
      names(lcu) = paste('u', 1:length(lcu), sep="")
      names(lcy) = paste('y', 1:length(lcy), sep="")
      lc = c(lcu, lcy)
      
      u.sdres <- sd(model_list_of_lists[[i]][[j]]$.args$data$u,na.rm = T)#sd(y_like[is.finite(y_lik)])
      y.sdres <- sd(model_list_of_lists[[i]][[j]]$.args$data$y/model_list_of_lists[[i]][[j]]$.args$data$u,na.rm=T)
      pc.prec.u = list(prec = list(prior = "pc.prec", param = c(3*u.sdres, 0.01)))
      pc.prec.y = list(prec = list(prior = "pc.prec", param = c(3*y.sdres, 0.01)))
     # famcontrol = list(list(prior = "pcprec", param = c(3*u.sdres,0.01)),
      #                  list(prior = "pcprec", param = c(3*y.sdres,0.01)))
   
      newmodel = inla(formula = model_list_of_lists[[i]][[j]]$.args$formula ,control.compute = list(waic=TRUE,dic=TRUE),
                      c('poisson', 'binomial'),Ntrials = model_list_of_lists[[i]][[j]]$.args$Ntrials,
                      # control.inla= list(strategy = "gaussian", int.strategy = "eb"),
                      #control.family = famcontrol,
                      control.fixed = list(expand.factor.strategy = "inla"),
                      data=model_list_of_lists[[i]][[j]]$.args$data,lincomb = lc,
                      control.update = list(result = model_list_of_lists[[i]][[j]]),
                      control.predictor=list(compute=TRUE),verbose=F)
      
      lcd = newmodel$summary.lincomb.derived
      
      lcd$id = rownames(lcd)
      lcd$group = str_extract(lcd$id,'^[u-y]')
      lcd$group = ifelse(lcd$group== 'u','Project count','CE/total NEPA analyses')
      lcd$group <- fct_rev(as.factor(lcd$group))
      
      
      qvals = rbind(lcomb_data[,1:2],lcomb_data[,3:4],use.names = F)
      names(qvals) <- c('scale_val1','scale_val2')
      lcd$x1_quantile = unlist(replicate(2,rep(x1seq,nrow(lcd)/2/length(x1seq)),simplify = F))
      lcd$x2_quantile = unlist(replicate(2,rep(x2seq,each = nrow(lcd)/2/length(x2seq)),simplify = F))
      
      lcd = cbind(lcd,qvals)
 
      axnames = unique(str_remove(names(temp_vars),'^[u-y]_'))
      lcd[[axnames[[1]]]]<-lcomb_data[[names(temp_vars)[1]]]
      lcd[[axnames[[2]]]]<-lcomb_data[[names(temp_vars)[2]]]
      
      intername = gsub(':','x',str_remove_all(iname[[1]],'u_'))
      figname = paste(mod_names[i],intername,sep='_')
      lcd = data.table(lcd)
      #lcd = lcd[!duplicated(paste(scale_val1,scale_val2,group)),]
      lcd$i = i
      lcd$j = j
      lcd$DV <- mod_names[i]
      empty_list = rbind(empty_list,lcd,fill = T,use.names = T)
    }
  }
}


fwrite(empty_list,'output/policypolitics/interaction_results_withdrops.csv')


#empty_list$outcome = name_matcher$outcome[match(empty_list$i,name_matcher$i)]

empty_list = fread('output/policypolitics/interaction_results_withdrops.csv')

#qvals = c('0.05','0.25','0.5','0.75','0.95')
qvals_LCV = c('0.05','0.95')
qval_labels_lcv = c('~0','~95')
qvals_demVS = c('0.05','0.95')
qval_labels_dmVS = c('15%','65%')
qvals_Dem = c(0.05,0.90)
qval_labels_DEM = c('Republican','Democrat')



ext_dt = empty_list[DV=='Extractive.withdrops',]
ext_dt$sig = ifelse(ext_dt$`0.025quant`<0&ext_dt$`0.975quant`>0,0,1)

ext_dt_lcv = ext_dt[!is.na(LCV_annual)& x2_quantile %in% qvals_LCV,]
ext_dt_dem = ext_dt[!is.na(percentD_H)&x2_quantile %in% qvals_demVS,]
ext_dt_rep = ext_dt[!is.na(democrat)&x2_quantile %in% qvals_Dem,]

((gg_lcv_vs_unemp_extraction = ggplot(data = ext_dt_lcv[group=='Project count'],
       aes(x = x1_quantile,y = mean,ymin = `0.025quant`,fill = as.factor(sig),
           ymax = `0.975quant`,group = as.factor(x2_quantile),
           col = as.factor(x2_quantile)))  + 
 # facet_wrap(~ group, scales = 'free_y',ncol = 2) + 
  #geom_path(position = position_dodge(0.05)) +
  geom_errorbar(position = position_dodge(0.05)) + 
  geom_point(position = position_dodge(0.05),pch = 19) + 
  geom_point(position = position_dodge(0.05),pch = 21) + 
  scale_fill_manual(values = c('white',NA)) + 
  scale_x_continuous(name = paste('% unemployment quantile')) +
  scale_y_continuous(name = '95% credible interval')+
  scale_color_colorblind(name = 'annual LCV score',labels=qval_labels_lcv) +
  #scale_color_viridis_d(name = 'annual LCV score',option = 'D',labels=qval_labels_lcv) + 
  guides(fill = FALSE) + 
  ggtitle('# extractive projects',subtitle ='LCV annual x unemployment %')+
  theme_bw() + theme(legend.position = c(0.2,0.15),legend.direction = 'vertical',
                     legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25)))))

ggsave(gg_lcv_vs_unemp_extraction,dpi = 300,width = 6,height = 4.5, units = 'in',
       filename = paste0('output/policypolitics/figures/interaction_extraction_projcount_lcv_vs_unemp.withdrops.png'))


(gg_lcv_vs_unemp_extraction = ggplot(data = ext_dt_lcv[group=="CE/total NEPA analyses",],
                                     aes(x = x1_quantile,y = mean,ymin = `0.025quant`,fill = as.factor(sig),
                                         ymax = `0.975quant`,group = as.factor(x2_quantile),
                                         col = as.factor(x2_quantile)))  + 
    # facet_wrap(~ group, scales = 'free_y',ncol = 2) + 
    #geom_path(position = position_dodge(0.05)) +
    geom_errorbar(position = position_dodge(0.05)) + 
    geom_point(position = position_dodge(0.05),pch = 19) + 
    geom_point(position = position_dodge(0.05),pch = 21) + 
    scale_fill_manual(values = c('white',NA)) + 
    scale_x_continuous(name = paste('% unemployment quantile')) +
    scale_y_continuous(name = '95% credible interval')+
    #scale_color_viridis_d(name = 'annual LCV score',option = 'D',labels=qval_labels_lcv) + 
    scale_color_colorblind(name = 'annual LCV score',labels=qval_labels_lcv) +
    guides(fill = FALSE) + 
    ggtitle('CEs/total NEPA analyses',subtitle ='LCV annual x unemployment %')+
    theme_bw() + theme(legend.position = c(0.2,0.15),legend.direction = 'vertical',
                       legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25))))

ggsave(gg_lcv_vs_unemp_extraction,dpi = 300,width = 6,height = 4.5, units = 'in',
       filename = paste0('output/policypolitics/figures/interaction_extraction_CEratio_lcv_vs_unemp.withdrops.png'))


(gg_percentD_H_vs_unemp_extraction = ggplot(data = ext_dt_dem[group=='Project count',],
                                    aes(x = x1_quantile,y = mean,ymin = `0.025quant`,
                                        fill = as.factor(sig),
                                        ymax = `0.975quant`,group = as.factor(x2_quantile),
                                        col = as.factor(x2_quantile)))  + 
  facet_wrap(~ group, scales = 'free_y',ncol = 2) + 
  geom_errorbar(position = position_dodge(0.05)) + 
  geom_point(position = position_dodge(0.05),pch = 19) + 
  geom_point(position = position_dodge(0.05),pch = 21) + 
  scale_x_continuous(name = paste('% unemployment quantile')) +
  scale_y_continuous(name = '95% credible interval')+
 # scale_color_viridis_d(name = 'dem. vote share quantile',option = 'D',labels=qval_labels_dmVS) + 
  scale_fill_manual(values = c('white',NA)) + 
  scale_colour_colorblind(name = 'dem. vote share',labels=qval_labels_dmVS) + 
  guides(fill = F) + 
  ggtitle('# extractive projects',subtitle= 'Dem. vote share x unemployment %')+
  theme_bw() + theme(legend.position = c(0.2,0.15),legend.direction = 'vertical',
                     legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25))))
ggsave(gg_percentD_H_vs_unemp_extraction,dpi = 300,width = 5,height = 4, units = 'in',
       filename = paste0('output/policypolitics/figures/interaction_extraction_projcount_percentD_H_vs_unemp.withdrops.png'))




(gg_percentD_H_vs_unemp_extraction = ggplot(data = ext_dt_dem[group=="CE/total NEPA analyses",],
                                           aes(x = x1_quantile,y = mean,ymin = `0.025quant`,fill = as.factor(sig),
                                               ymax = `0.975quant`,group = as.factor(x2_quantile),
                                               col = as.factor(x2_quantile)))  + 
  facet_wrap(~ group, scales = 'free_y',ncol = 2) + 
  geom_errorbar(position = position_dodge(0.05)) + 
  geom_point(position = position_dodge(0.05),pch = 19) + 
  geom_point(position = position_dodge(0.05),pch = 21) + 
  scale_x_continuous(name = paste('% unemployment quantile')) +
  scale_y_continuous(name = '95% credible interval')+
  scale_fill_manual(values = c('white',NA)) + 
  scale_colour_colorblind(name = 'dem. vote share',labels=qval_labels_dmVS) + 
  guides(fill = F) + 
  ggtitle('CEs/total NEPA analyses',subtitle= 'Dem. vote share x unemployment %')+
  theme_bw() + theme(legend.position = c(0.2,0.15),legend.direction = 'vertical',
                     legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25))))
ggsave(gg_percentD_H_vs_unemp_extraction,dpi = 300,width = 5,height = 4, units = 'in',
       filename = paste0('output/policypolitics/figures/interaction_extraction_CEratio_percentD_H_vs_unemp.withdrops.png'))


(gg_demRep_vs_unemp_extraction = ggplot(data = ext_dt_rep[group=='Project count',],
                                           aes(x = x1_quantile,y = mean,ymin = `0.025quant`,
                                               fill= as.factor(sig),
                                               ymax = `0.975quant`,group = as.factor(x2_quantile),
                                               col = as.factor(x2_quantile)))  + 
  facet_wrap(~ group, scales = 'free_y',ncol = 2) + 
  geom_errorbar(position = position_dodge(0.05)) + 
  geom_point(position = position_dodge(0.05),pch = 19) + 
  geom_point(position = position_dodge(0.05),pch = 21) + 
  scale_x_continuous(name = paste('% unemployment quantile')) +
  scale_y_continuous(name = '95% credible interval')+
  scale_fill_manual(values = c('white',NA)) + 
  scale_colour_colorblind(name = 'Representative',labels=qval_labels_DEM) + 
  guides(fill = F) + 
  ggtitle('# extractive projects',subtitle ='Dem. rep. x unemployment %')+
  theme_bw() + theme(legend.position = c(0.2,0.15),legend.direction = 'vertical',
                     legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25))))
ggsave(gg_demRep_vs_unemp_extraction,dpi = 300,width = 5,height = 4, units = 'in',
       filename = paste0('output/policypolitics/figures/interaction_extraction_projcount_demRep_vs_unemp_extraction.withdrops.png'))




gg_demRep_vs_unemp_extraction = ggplot(data = ext_dt_rep[group=="CE/total NEPA analyses",],
                                       aes(x = x1_quantile,y = mean,ymin = `0.025quant`,
                                           fill = as.factor(sig),
                                           ymax = `0.975quant`,group = as.factor(x2_quantile),
                                           col = as.factor(x2_quantile)))  + 
  facet_wrap(~ group, scales = 'free_y',ncol = 2) + 
  geom_errorbar(position = position_dodge(0.05)) + 
  geom_point(position = position_dodge(0.05),pch = 19) + 
  geom_point(position = position_dodge(0.05),pch = 21) + 
  scale_x_continuous(name = paste('% unemployment quantile')) +
  scale_y_continuous(name = '95% credible interval')+
  scale_fill_manual(values = c('white',NA)) + 
  scale_colour_colorblind(name = 'Representative',labels=qval_labels_DEM) + 
  guides(fill = F) + 
  ggtitle('CEs/total NEPA analyses',subtitle ='Dem. rep. x unemployment %')+
  theme_bw() + theme(legend.position = c(0.2,0.15),legend.direction = 'vertical',
                     legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25)))
ggsave(gg_demRep_vs_unemp_extraction,dpi = 300,width = 5,height = 4, units = 'in',
       filename = paste0('output/policypolitics/figures/interaction_extraction_CEratio_demRep_vs_unemp_extraction.withdrops.png'))



rec_ct = empty_list[DV== "Recreation_Wildlife",]
rec_ct$sig = ifelse(rec_ct$`0.025quant`<0&rec_ct$`0.975quant`>0,0,1)

rec_ct_lcv = rec_ct[!is.na(LCV_annual)&x2_quantile %in% qvals_LCV,]
rec_ct_dem = rec_ct[!is.na(percentD_H)&x2_quantile %in% qvals_demVS,]
rec_ct_rep = rec_ct[!is.na(democrat)&x2_quantile %in% qvals_Dem,]
exp(0.12066251 )

rec_ct[!is.na(LCV_annual)&x1_quantile==0.75&x2_quantile %in% qvals_LCV,]
quantile(nf$Unemp_Rate,0.75)
exp(0.083)

(gg_lcv_vs_unemp_rec = ggplot(data = rec_ct_lcv[group=='Project count'],
                                     aes(x = x1_quantile,y = mean,ymin = `0.025quant`,fill = as.factor(sig),
                                         ymax = `0.975quant`,group = as.factor(x2_quantile),
                                         col = as.factor(x2_quantile)))  + 
    # facet_wrap(~ group, scales = 'free_y',ncol = 2) + 
    #geom_path(position = position_dodge(0.05)) +
    geom_errorbar(position = position_dodge(0.05)) + 
    geom_point(position = position_dodge(0.05),pch = 19) + 
    geom_point(position = position_dodge(0.05),pch = 21) + 
    scale_x_continuous(name = paste('% unemployment quantile')) +
    scale_y_continuous(name = '95% credible interval')+
    scale_fill_manual(values = c('white',NA)) + 
    scale_colour_colorblind(name = 'annual LCV score',labels=qval_labels_lcv) + 
    guides(fill = F) + 
    ggtitle('# rec./wildlife projects',subtitle ='LCV annual x unemployment %')+
    theme_bw() + theme(legend.position = c(0.2,0.2),legend.direction = 'vertical',
                       legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25))))

ggsave(gg_lcv_vs_unemp_rec,dpi = 300,width = 6,height = 4.5, units = 'in',
       filename = paste0('output/policypolitics/figures/interaction_recwildlife_projcount_lcv_vs_unemp.withdrops.png'))



# 
# 
# 
# 
# 
# 
# rec_dt = empty_list[DV=='Recreation_Wildlife']
# rec_dt_lcv = rec_dt[!is.na(LCV_annual),]
# rec_dt_dem = rec_dt[!is.na(percentD_H),]
# gg_lcv_vs_unemp_rec_wildlife = ggplot(data = rec_dt_lcv,
#                                     aes(x = x1_quantile,y = mean,ymin = `0.025quant`,
#                                         ymax = `0.975quant`,group = as.factor(x2_quantile),
#                                         col = as.factor(x2_quantile)))  + 
#   facet_wrap(~ group, scales = 'free_y',ncol = 2) + 
#   geom_path(position = position_dodge(0.05)) +
#   geom_errorbar(position = position_dodge(0.05)) + 
#   scale_x_continuous(name = paste('% unemployment quantile')) +
#   scale_y_continuous(name = '95% credible interval')+
#   scale_color_viridis_d(name = 'LCV annual quantile',option = 'D') + 
#   ggtitle('Rec./wildlife: LCV annual x unemployment %')+
#   theme_bw() + theme(legend.position = c(0.2,0.15),legend.direction = 'vertical',
#                      legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25)))
# ggsave(gg_lcv_vs_unemp_rec_wildlife,dpi = 300,width = 6,height = 3.5, units = 'in',
#        filename = paste0('output/policypolitics/figures/interaction_lcv_vs_unemp_rec_wildlife.withdrops.png'))
# 
# gg_percentD_H_vs_unemp_rec_wildlife = ggplot(data = rec_dt_dem,
#                                            aes(x = x1_quantile,y = mean,ymin = `0.025quant`,
#                                                ymax = `0.975quant`,group = as.factor(x2_quantile),
#                                                col = as.factor(x2_quantile)))  + 
#   facet_wrap(~ group, scales = 'free_y',ncol = 2) + 
#   geom_path(position = position_dodge(0.05)) +
#   geom_errorbar(position = position_dodge(0.05)) + 
#   scale_x_continuous(name = paste('% unemployment quantile')) +
#   scale_y_continuous(name = '95% credible interval')+
#   scale_color_viridis_d(name = 'dem. vote share quantile',option = 'D') + 
#   ggtitle('Rec./wildlife: LCV annual x unemployment %')+
#   theme_bw() + theme(legend.position = c(0.2,0.15),legend.direction = 'vertical',
#                      legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25)))
# ggsave(gg_percentD_H_vs_unemp_rec_wildlife,dpi = 300,width = 6,height = 3.5, units = 'in',
#        filename = paste0('output/policypolitics/figures/interaction_percentD_H_vs_unemp_rec_wildlife.withdrops.png'))
# 
# 
# rec_side_by_side_dt = empty_list[DV=='Recreation_Wildlife'&group == 'Project count',]
# rec_side_by_side_dt$Var = ifelse(is.na(rec_side_by_side_dt$percentD_H),'LCV annual','% dem. vote')
# 
# gg_rec_side_by_side = ggplot(data = rec_side_by_side_dt,
#                              aes(x = x1_quantile,y = mean,ymin = `0.025quant`,
#                                  ymax = `0.975quant`,group = as.factor(x2_quantile),
#                                  col = as.factor(x2_quantile)))  + 
#   facet_wrap(~ Var, scales = 'fixed',ncol = 2) + 
#   geom_path(position = position_dodge(0.05)) +
#   geom_errorbar(position = position_dodge(0.05)) + 
#   scale_x_continuous(name = paste('% unemployment quantile')) +
#   scale_y_continuous(name = '95% credible interval')+
#   scale_color_viridis_d(name = 'quantile',option = 'D') + 
#   ggtitle('Recreation: % dem.|LCV annual x unemployment %')+
#   theme_bw() + theme(legend.position = c(0.35,0.15),legend.direction = 'vertical',
#                      legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25)))
# ggsave(gg_rec_side_by_side,dpi = 300,width = 6,height = 3.5, units = 'in',
#        filename = paste0('output/policypolitics/figures/interaction_projcount_unemp_rec_wildlife.withdrops.png'))
# 
# 
# 
# lcv_unemp_side_by_side_dt = empty_list[group == 'Project count'&!is.na(LCV_annual)&DV!='All',]
# lcv_unemp_side_by_side_dt$Var = ifelse(lcv_unemp_side_by_side_dt$DV=="Recreation_Wildlife",'Rec./wildlife','Extractive')
# 
# (gg_lcv_unemp_side_by_side_dt= ggplot(data = lcv_unemp_side_by_side_dt,
#                              aes(x = x1_quantile,y = mean,ymin = `0.025quant`,
#                                  ymax = `0.975quant`,group = as.factor(x2_quantile),
#                                  col = as.factor(x2_quantile)))  + 
#   facet_wrap(~ Var, scales = 'fixed',ncol = 2) + 
#   geom_path(position = position_dodge(0.05)) +
#   geom_errorbar(position = position_dodge(0.05)) + 
#   scale_x_continuous(name = paste('% unemployment quantile')) +
#   scale_y_continuous(name = '95% credible interval')+
#   scale_color_viridis_d(name = 'LCV annual quantile',option = 'D') + 
#   ggtitle('LCV annual x unemployment %')+
#   theme_bw() + theme(legend.position = c(0.2,0.15),legend.direction = 'vertical',
#                      legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25))))
# 
# ggsave(gg_lcv_unemp_side_by_side_dt,dpi = 300,width = 6,height = 3.5, units = 'in',
#        filename = paste0('output/policypolitics/figures/interaction_projcount_unemp_lcv_ext_vs_rec.withdrops.png'))
# 
# 
# 
# 
# lcv_unemp_side_by_side_dt = empty_list[group != 'Project count'&!is.na(LCV_annual)&DV!='All',]
# lcv_unemp_side_by_side_dt$Var = ifelse(lcv_unemp_side_by_side_dt$DV=="Recreation_Wildlife",'Rec./wildlife','Extractive')
# 
# (gg_lcv_unemp_side_by_side_dt= ggplot(data = lcv_unemp_side_by_side_dt,
#                                       aes(x = x1_quantile,y = mean,ymin = `0.025quant`,
#                                           ymax = `0.975quant`,group = as.factor(x2_quantile),
#                                           col = as.factor(x2_quantile)))  + 
#     facet_wrap(~ Var, scales = 'fixed',ncol = 2) + 
#     geom_path(position = position_dodge(0.05)) +
#     geom_errorbar(position = position_dodge(0.05)) + 
#     scale_x_continuous(name = paste('% unemployment quantile')) +
#     scale_y_continuous(name = '95% credible interval')+
#     scale_color_viridis_d(name = 'LCV annual quantile',option = 'D') + 
#     ggtitle('CE ratio: LCV annual x unemployment %')+
#     theme_bw() + theme(legend.position = c(0.2,0.15),legend.direction = 'vertical',
#                        legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25))))
# 
# ggsave(gg_lcv_unemp_side_by_side_dt,dpi = 300,width = 6,height = 3.5, units = 'in',
#        filename = paste0('output/policypolitics/figures/interaction_CEratio_unemp_lcv_ext_vs_rec.withdrops.png'))
# 
# 
# demvote_unemp_side_by_side_dt = empty_list[group == 'Project count'&is.na(LCV_annual)&DV!='All',]
# demvote_unemp_side_by_side_dt$Var = ifelse(demvote_unemp_side_by_side_dt$DV=="Recreation_Wildlife",'Rec./wildlife','Extractive')
# 
# (gg_demvote_unemp_side_by_side_dt= ggplot(data = demvote_unemp_side_by_side_dt,
#                                       aes(x = x1_quantile,y = mean,ymin = `0.025quant`,
#                                           ymax = `0.975quant`,group = as.factor(x2_quantile),
#                                           col = as.factor(x2_quantile)))  + 
#     facet_wrap(~ Var, scales = 'fixed',ncol = 2) + 
#     geom_path(position = position_dodge(0.05)) +
#     geom_errorbar(position = position_dodge(0.05)) + 
#     scale_x_continuous(name = paste('% unemployment quantile')) +
#     scale_y_continuous(name = '95% credible interval')+
#     scale_color_viridis_d(name = 'Dem. vote quantile',option = 'D') + 
#     ggtitle('Dem. vote share x unemployment %')+
#     theme_bw() + theme(legend.position = c(0.2,0.15),legend.direction = 'vertical',
#                        legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25))))
# 
# ggsave(gg_demvote_unemp_side_by_side_dt,dpi = 300,width = 6,height = 3.5, units = 'in',
#        filename = paste0('output/policypolitics/figures/interaction_projcount_unemp_demvote_ext_vs_rec.withdrops.png'))
# 
# 
# 
# 
# 
# demvote_unemp_side_by_side_dt = empty_list[group != 'Project count'&is.na(LCV_annual)&DV!='All',]
# demvote_unemp_side_by_side_dt$Var = ifelse(demvote_unemp_side_by_side_dt$DV=="Recreation_Wildlife",'Rec./wildlife','Extractive')
# 
# (gg_demvote_unemp_side_by_side_dt= ggplot(data = demvote_unemp_side_by_side_dt,
#                                           aes(x = x1_quantile,y = mean,ymin = `0.025quant`,
#                                               ymax = `0.975quant`,group = as.factor(x2_quantile),
#                                               col = as.factor(x2_quantile)))  + 
#     facet_wrap(~ Var, scales = 'fixed',ncol = 2) + 
#     geom_path(position = position_dodge(0.05)) +
#     geom_errorbar(position = position_dodge(0.05)) + 
#     scale_x_continuous(name = paste('% unemployment quantile')) +
#     scale_y_continuous(name = '95% credible interval')+
#     scale_color_viridis_d(name = 'Dem. vote quantile',option = 'D') + 
#     ggtitle('CE ratio: Dem. vote share x unemployment %')+
#     theme_bw() + theme(legend.position = c(0.2,0.15),legend.direction = 'vertical',
#                        legend.title=element_text(size = 10),legend.background = element_rect(fill = alpha('white',0.25))))
# 
# ggsave(gg_demvote_unemp_side_by_side_dt,dpi = 300,width = 6,height = 3.5, units = 'in',
#        filename = paste0('output/policypolitics/figures/interaction_CEratio_unemp_demvote_ext_vs_rec.withdrops.png'))
# 
# 



