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

