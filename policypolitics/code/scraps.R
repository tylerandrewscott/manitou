
# 
# subnf = nf[congress==115,.(FOREST_ID,percentD_H,LCV_annual,Count_EorT_Species,Wilderness_Prop,Prop_Extraction_Employ,ln_County_naturalresource_GDP_1M)]
# admin_districts$percentD_H115 = subnf[match(admin_districts$FOREST_ID,FOREST_ID),]$percentD_H
# admin_districts$LCV_annual115 = subnf[match(admin_districts$FOREST_ID,FOREST_ID),]$LCV_annual
# admin_districts$Prop_Extraction_Employ = subnf[match(admin_districts$FOREST_ID,FOREST_ID),]$Prop_Extraction_Employ
# admin_districts$ln_County_naturalresource_GDP_1M = subnf[match(admin_districts$FOREST_ID,FOREST_ID),]$ln_County_naturalresource_GDP_1M
# admin_districts$Wilderness_Prop = subnf[match(admin_districts$FOREST_ID,FOREST_ID),]$Wilderness_Prop
# admin_districts$Count_EorT_Species = subnf[match(admin_districts$FOREST_ID,FOREST_ID),]$Count_EorT_Species
# g1 = ggplot(admin_districts) + geom_sf(aes(fill = percentD_H115,colour = percentD_H115)) + theme_map() + 
#   ggtitle('percentD_H, 115th congress by  unit') + 
#   scale_color_viridis_c() + scale_fill_viridis_c()+ 
#   theme(legend.title  = element_blank(),legend.position = c(0.1,0.2),text = element_text(size = 12))
# g2 = ggplot(admin_districts) + geom_sf(aes(fill = LCV_annual115,colour = LCV_annual115)) + theme_map()+ 
#   ggtitle('LCV Annual, 115th congress by unit')+ 
#   scale_color_viridis_c(option = 'C') + scale_fill_viridis_c(option = 'C') + 
#   theme(legend.title  = element_blank(),legend.position = c(0.1,0.2),text = element_text(size = 12))
# #install.packages('gridExtra')
# library(gridExtra)
# grb = grid.arrange(g1,g2,ncol = 1)
# #ggsave(grb,filename = 'output/policypolitics/figures/choropleth_politics_by_forest.png',width = 9,height = 11,units = 'in',dpi = 300)
# 
# g3 = ggplot(admin_districts) + geom_sf(aes(fill = Wilderness_Prop,colour = Wilderness_Prop)) + theme_map() + 
#   ggtitle('Wilderness %, 115th congress by unit') + 
#   scale_color_viridis_c(option = 'A') + scale_fill_viridis_c(option = 'A')+ 
#   theme(legend.title  = element_blank(),legend.position = c(0.1,0.2),text = element_text(size = 12))
# g4 = ggplot(admin_districts) + geom_sf(aes(fill = ,colour = Count_EorT_Species)) + theme_map()+ 
#   ggtitle('# ESA lists, 115th congress by unit')+ 
#   scale_color_viridis_c(option = 'B') + scale_fill_viridis_c(option = 'B') + 
#   theme(legend.title  = element_blank(),legend.position = c(0.1,0.2),text = element_text(size = 12))
# #install.packages('gridExtra')
# 
# grb2 = grid.arrange(g3,g4,ncol = 1)
# #ggsave(grb2,filename = 'output/policypolitics/figures/choropleth_enviro_by_forest.png',width = 9,height = 11,units = 'in',dpi = 300)
# 
# 
# g5 = ggplot(admin_districts) + geom_sf(aes(fill = Prop_Extraction_Employ,colour = Prop_Extraction_Employ)) + theme_map() + 
#   ggtitle('Extraction employ., 115th congress by unit') + 
#   scale_color_viridis_c(option = 'A') + scale_fill_viridis_c(option = 'A')+ 
#   theme(legend.title  = element_blank(),legend.position = c(0.1,0.2),text = element_text(size = 12))
# g6 = ggplot(admin_districts) + geom_sf(aes(fill = ln_County_naturalresource_GDP_1M,colour = ln_County_naturalresource_GDP_1M)) + theme_map()+ 
#   ggtitle('County resource GDP, 115th congress by unit')+ 
#   scale_color_viridis_c(option = 'B') + scale_fill_viridis_c(option = 'B') + 
#   theme(legend.title  = element_blank(),legend.position = c(0.1,0.2),text = element_text(size = 12))
# #install.packages('gridExtra')
# 
# grb3 = grid.arrange(g5,g6,ncol = 1)
# #ggsave(grb3,filename = 'output/policypolitics/figures/choropleth_extractecon_by_forest.png',width = 9,height = 11,units = 'in',dpi = 300)
# 





# waic_scores = data.table(sapply(list_of_results,function(x) sapply(x,function(y) y$waic$waic)))
# waic_scores$formula <- list_of_forms
# colnames(waic_scores)[1:length(subtypes)]<-subtypes
# library(htmlTable)
# waic_scores[,1:length(subtypes)] <- round(waic_scores[,1:length(subtypes)],3)
# htmlTable(waic_scores)
# 
# waic_scores$spec = c("baseline","national_politics","lcv_x_demvoteshare","lcv_x_unemployment",
#   "lcv_x_prop_extraction_employ","lcv_x_naturalresourceGDP","lcv_x_wilderness",
# "lcv_x_species","demvote_x_wilderness","demvote_x_species")
# if('CE' %in% keep_decisions & !keep_activities ){fwrite(waic_scores,'output/policypolitics/tables/waic_table.csv')}
# if(!'CE' %in% keep_decisions & !keep_activities){fwrite(waic_scores,'output/policypolitics/tables/waic_table_noCE.csv')}
# if('CE' %in% keep_decisions & keep_activities ){fwrite(waic_scores,'output/policypolitics/tables/waic_table_activities.csv')}
# if(!'CE' %in% keep_decisions & keep_activities){fwrite(waic_scores,'output/policypolitics/tables/waic_table_noCE_activities.csv')}
# 


lcv_vs_unemp = ggplot(nf,aes(x = LCV_annual,y = Unemp_Rate)) + geom_point(pch = 21,alpha = 0.5) + theme_bw() + 
  scale_x_continuous(name = 'annual LCV score') + scale_y_continuous(name = 'Unemployment %') + 
  ggtitle('Unemployment vs. LCV score', subtitle = 'administrative unit-year observations')

ggsave(lcv_vs_unemp,filename = 'output/policypolitics/figures/IV_plot.png',dpi=300,width = 6, height = 6, units = 'in')



