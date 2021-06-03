require(data.table)
require(tidyverse)

votes=fread('input/politics/1976-2018-house.csv')
votes = data.table(votes)
votes = votes[year %in% 2005:2018,]
votes = votes[!is.na(party),]

votes[,voterank:=rank(candidatevotes,ties.method = 'first'),by=.(year,state,district)]
votes = votes[voterank<=2,]
votes$candidatevotes <- as.numeric(votes$candidatevotes)
votes = votes[candidatevotes<=totalvotes,]

votes$voteperc = votes$candidatevotes/votes$totalvotes
votes$invert = ifelse(votes$voterank==1,votes$voteperc,votes$voteperc*-1)
votes[,absdiff:=sum(invert),by=.(year,state_fips,district)]

votes = votes[!duplicated(paste(state,district,year,sep = '_')),]


votes[,diffrank:=rank(absdiff,ties.method = 'first'),by=.(year)]

ggplot(data = votes) + geom_point(aes(x = diffrank,y = absdiff),pch = 21) + facet_wrap(~year) + 
  geom_hline(yintercept = c(0.1,-0.1)) + theme_bw() + 
  xlab('(ordered by % difference)') + 
  scale_y_continuous('% of votes for winner - % for runner-up',breaks = c(-1,-0.5,-0.1,0.1,0.5,1)) + 
  ggtitle('Election closeness by year and house district')

table(votes$absdiff<0.1&votes$absdiff>-0.1,votes$year)

require(tigris)
require(sf)
cds = tigris::congressional_districts(year = '2013',class='sf')
forests = st_read('https://opendata.arcgis.com/datasets/09e4c1162a4d4af3a84163cbc76108c4_1.geojson')
cds_transformed = st_transform(cds,st_crs(forests))


head(cds_transformed)

head(votes)

votes$CD_ID = paste0(formatC(votes$state_fips,width = 2,flag = 0),formatC(votes$district,width = 2,flag = 0))

over_a_forest = st_overlaps(cds_transformed,forests)

votes2012forest = votes[year==2012&CD_ID %in% cds_transformed$GEOID[sapply(over_a_forest,length)>0],]

votes2012forest[,diffrank := rank(absdiff,ties.method = 'first')]
ggplot(data = votes2012forest) + geom_point(aes(x = diffrank,y = absdiff),pch = 21) + facet_wrap(~year) + 
  geom_hline(yintercept = c(0.1,-0.1)) + theme_bw() + 
  xlab('(ordered by % difference)') + 
  scale_y_continuous('% of votes for winner - % for runner-up',breaks = c(-1,-0.5,-0.1,0.1,0.5,1)) + 
  ggtitle('Election closeness, 2012 house elections','Districts overlapping a forest') 


votes[year==2012&!CD_ID%in%cds_transformed$GEOID,]


cds_transformed[cds_transformed$STATEFP=='04',]


votes$CD_ID[votes$year==2012][!votes$CD_ID[votes$year==2012] %in% cds_transformed$GEOID]


votes[state_fips==48&year==2012,]

head(votes)


head(cds)
head()








