
library(data.table)
joy_files = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQ1WucNsHU3_J4ULnFFuiGVKGd9UjqbIMPdHonmNcOA3x8JdonSjqUjS5jTtkF0qVvZzhn9H3HWToGU/pub?gid=1231467735&single=true&output=csv'
junna_files = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRhJwKWApRu66d9Rmk8phNsa4hk4pZh5po_c3B1jgsvU571bkWrU7R-LkLvqWV4UaMHTjeP_IpVImBT/pub?gid=0&single=true&output=csv'
luke_files = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vTExyNBeAoTbnThp2YeHUEseg84tWqdaAcucSw4nj8bvp7mbCVwGcosPL2e8Ld6LrNN4lf9VuNUn4ZY/pub?gid=942728448&single=true&output=csv'
nicole_files = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQ5j-AgeIrqpv2loJMPU8Q2T8RgBytI6ZE2rqMsFIXx07HTMGAXqsgWlmEnktQuGD79MScYpxCciCf8/pub?gid=0&single=true&output=csv'
joy_codes = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQ1WucNsHU3_J4ULnFFuiGVKGd9UjqbIMPdHonmNcOA3x8JdonSjqUjS5jTtkF0qVvZzhn9H3HWToGU/pub?gid=1019233508&single=true&output=csv'
junna_codes = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRhJwKWApRu66d9Rmk8phNsa4hk4pZh5po_c3B1jgsvU571bkWrU7R-LkLvqWV4UaMHTjeP_IpVImBT/pub?gid=2008450315&single=true&output=csv'
luke_codes = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vTExyNBeAoTbnThp2YeHUEseg84tWqdaAcucSw4nj8bvp7mbCVwGcosPL2e8Ld6LrNN4lf9VuNUn4ZY/pub?gid=1641294735&single=true&output=csv'
nicole_codes = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQ5j-AgeIrqpv2loJMPU8Q2T8RgBytI6ZE2rqMsFIXx07HTMGAXqsgWlmEnktQuGD79MScYpxCciCf8/pub?gid=184604661&single=true&output=csv'

intern_files = list(joy_files,junna_files,nicole_files,luke_files)
intern_codes = list(joy_codes,junna_codes,nicole_codes,luke_codes)

pals = fread('input/Copy of MYTR_National Level(1100).xlsx - Decision Data Set.csv')
docs = fread('input/forest_service_document_2019-08-12.csv')
proj = fread('input/forest_service_project_detail_2019-08-12.csv')

library(stringr)
good_files = lapply(intern_files,function(f){
file = fread(f)
file = file[grepl('^[0-9]',PDF_NAME),]
file$PDF_NAME = gsub('\\s$','',gsub('\\(.*','',file$PDF_NAME))
file$PDF_NAME = str_extract(file$PDF_NAME,'[0-9]{1,}_.*[0-9]{4,}')
file = file[!is.na(PDF_NAME),]
file
})

hand_coded_files = rbindlist(good_files,fill = T)
hand_refs = lapply(intern_codes,function(f){
  file = fread(f)
  file
})
hand_coded_refs = rbindlist(hand_refs,fill = T)

#fls = (unlist(sapply(good_files,function(x) x$PDF_NAME)))
#fls = fls[!is.na(fls)]
fwrite(data.table(fls = hand_coded_files$PDF_NAME),'input/intern_coded_file_names.csv')

found_refs = fread('input/reference_set_df_10-14-19.csv',header=T)
comp_refs = found_ref[File %in% unlist(fls),]

hand_coded_refs = rbindlist(hand_refs,fill = T)
hand_coded_refs = hand_coded_refs[full_citation != '',]
hand_coded_refs$PDF_NAME = hand_coded_files$PDF_NAME[match(hand_coded_refs$project_num,hand_coded_files$ROW_NUM)]
hand_coded_refs = hand_coded_refs[!is.na(hand_coded_refs$PDF_NAME),]


comps = intersect(hand_coded_refs$PDF_NAME,found_refs$File)
hand_coded_refs = hand_coded_refs[PDF_NAME %in% comps,]
sub_found_refs = found_refs[File %in% comps]
sub_found_refs = sub_found_refs[Title!='',]

hand_count = hand_coded_refs[,.N,by = .(PDF_NAME)]
setnames(hand_count,c('PDF_NAME','N'),c('File',"Hand_Count"))
machine_count = sub_found_refs[,.N,by = .(File)]
setnames(machine_count ,c('N'),c("Machine_Count"))

compare_counts = merge(hand_count,machine_count)


library(tidyverse)
gg1 = ggplot(compare_counts) + geom_abline(intercept = 0, slope= 1,lty = 2,col = 'grey50') + theme_bw()+
  geom_point(aes(x = Hand_Count,y = Machine_Count),size = 3) +
  scale_y_continuous(limits = c(0,500),name = '# extracted') + scale_x_continuous(limits = c(0,500),'# coded') +
  ggtitle('# hand coded\n vs. extracted citations')
  
title_machine = sub_found_refs$Title
title_hand = hand_coded_refs$title
library(tm)
library(tokenizers)

title_hand = stripWhitespace(removePunctuation(tolower(title_hand)))
title_machine = stripWhitespace(removePunctuation(tolower(title_machine)))
title_machine = gsub('^\\s{1,}|\\s${1,}','',title_machine)
title_hand = gsub('^\\s{1,}|\\s${1,}','',title_hand)

library(stringdist)
dt_titles = data.table(title_machine, amatch_vec = amatch(title_machine,title_hand,method = 'cosine',maxDist = 0.25),exact_match_vec = match(title_machine,title_hand))

sub_found_refs$title_machine = title_machine
sub_found_refs$title_matches_hand = sapply(sub_found_refs$title_machine,function(x) any(grepl(x,title_hand)))

title_matches=sub_found_refs[,list(sum(title_matches_hand)/.N,.N),by = .(File)]

gg2 = ggplot(title_matches) + geom_point(aes(x = N,y = V1),size = 3) + theme_bw()+
  #geom_point(aes(x = Hand_Count,y = Machine_Count),size = 3) +
  scale_y_continuous(limits = c(0,1),name = '% matching') + scale_x_continuous(name = '# extracted',limits = c(0,500)) +
  ggtitle('% titles extracted matching\n hand-coded samples')

library(gridExtra)
grid.arrange(gg1,gg2,ncol = 2)


pubs = found_refs[,.N,by=.(Publication)][order(-N)][Publication!='',]
pubs = pubs[nchar(Publication)<60,]
pubs = pubs[N>2,]
pubs$Publication=tolower(pubs$Publication)
pubs$Publication = gsub('can\\.','canadian',pubs$Publication)
pubs$Publication = gsub('for\\.','forest',pubs$Publication)
pubs$Publication = gsub('j\\.','journal',pubs$Publication)
pubs$Publication = gsub(' res$',' research',pubs$Publication)
pubs$Publication = gsub('geophys\\.','geophysical',pubs$Publication)
pubs = pubs[,sum(N),by = .(Publication)]
pubs = pubs[!grepl('unpublished',pubs$Publication),]
pubs = pubs[!grepl('special',pubs$Publication),]
pubs$Publication = gsub('\\(.*','',pubs$Publication)
impacts = scholar::get_impactfactor(journals = pubs$Publication)

pubs = cbind(pubs,impacts)
pubs = pubs[!is.na(Journal),]
pubs = pubs[!grepl('NUCLEAR|EPIDEMI|MATERIALS|CA-A CANCER JOURNAL FOR CLINICIANS|LANCET|MEDICAL|MEDICINE|Nanotech|Photonics',Journal),]

pubs$Journal[pubs$Journal=='NORTH AMERICAN JOURNAL OF FISHERIES MANAGEMENT']<-'JOURNAL OF FISHERIES MANAGEMENT'
library(ggrepel)
ggplot() + geom_point(aes(x =ImpactFactor,y = V1),data = pubs) +
  geom_label_repel(aes(x =ImpactFactor,y = V1,label = Journal),data = pubs[ImpactFactor>7.5,]) +
  geom_label_repel(aes(x =ImpactFactor,y = V1,label = Journal),data = pubs[V1>75,]) + 
  theme_bw() + scale_y_continuous(name = 'Observed citations') +
  scale_x_continuous(name = 'Impact Factor') + 
  ggtitle('Impact factor vs. use in EISs') + theme(axis.title = element_text(size = 14),
                                                   title = element_text(size = 14),
                                                   axis.text = element_text(size = 14))
library(tm)
library(quanteda)
library(lubridate)
found_refs$Date = str_extract(found_refs$Date,'[0-9]{4}')

found_refs$Project_Num = docs$Project_Num[match(found_refs$File,docs$File_Name)]
found_refs$Decision_Date = proj$Decision.Signed.Date[match(found_refs$Project_Num,proj$Proj_Num)]
found_refs$Decision_Date = decimal_date(ymd(found_refs$Decision_Date))
found_refs$Publication_Year =as.numeric(found_refs$Date)
found_refs = found_refs[Publication_Year<2020,]
found_refs = found_refs[Publication_Year>1900,]


found_refs[Publication_Year>1900,]
summary(found_refs$Publication_Year)
ggplot(data = found_refs) + geom_density(aes(x = Decision_Date-Publication_Year),trim=T) + 
  theme_bw() + ggtitle('Time from publication to use in EIS') + 
  scale_x_continuous('EIS date - citation date') + 
  scale_y_continuous(name = 'density') +
  theme(axis.text.y = element_blank(),text = element_text(size = 12))



as.numeric(found_refs$Date)

other_refs = found_refs[!Publication%in%pubs$Publication,]
library(tidytext)
library(dplyr)
library(tidytext)
library(janeaustenr)

title_trigrams <- other_refs %>%
  unnest_tokens(trigram, Title , token = "ngrams", n = 4)

title_trigrams %>%
  count(trigram, sort = TRUE)
head(other_refs)


pubs[grepl('NUCLEA',Journal),]
  get_impactfactor('ecological applications')
gsub('\\(.*','',pubs$Publication)


found_refs[,length(unique(File)),by =.(Publication)][order(-V1)]
  
library(GoogleScholarScrapeR)
library(scholar)
nchar("Defoliation by western spruce budworm in Oregon and Washington from 1980 through 1994")
impacts = scholar::get_impactfactor(journals = pubs$Publication[1:20])
impacts



nchar( "development of coarse scale spatial data for wildland fire and fuel management")

GoogleScholarScrapeR
  .N,by=.(Publication)][order(-N)][Publication!='',][1:10,]



found_refs$Publication
titles_matched = sub_found_refs[,list(.N,sum(title_machine%in%title_hand)),by = .(File)]
titles_matched = titles_matched[File!='49057_103909_FSPLT3_3968523',]

ggplot(titles_matched) + geom_point(N)

devtools::install_github('ropensci/rcrossref')
library(rcrossref)
?rcrossref::cr_journals()
sub_found_refs[File=='49057_103909_FSPLT3_3968523',]

hand_coded_refs[PDF_NAME=='49057_103909_FSPLT3_3968523',]




table(title_hand %in% title_machine)
cr_journals(query='conservation biology',
            sort='score', order="asc")

dt_titles$amatch_title = title_hand[dt_titles$amatch_vec]

dt_titles[2,]
gsub('^\\s{1,}','',title_machine)
title_machine[!title_machine %in% title_hand][10]

grep('aspen and conifer',title_hand,value=T)
notfound = unique(hand_coded_refs$project_num)[!unique(hand_coded_refs$project_num) %in% hand_coded_files$ROW_NUM]

hand_refs[[1]]
good_files[[1]]
hand_coded_files[ROW_NUM%in%notfound,]
notfound
hand_coded_files$ROW_NUM 

table(hand_coded_refs$project_num)
hand_coded_refs[753,]
match(hand_coded_refs$project_num,hand_coded_files$ROW_NUM)

hand_coded_files$ROW_NUM
hand_coded_files[ROW_NUM=='33097',]

pals[33097,]
hand_coded_files

good_refs[[2]][project_num =='17568']


17568
comp_refs


library(stringr)
good_refs = lapply(intern_codes,function(f){
  file = fread(f)
  file
})

good_dt = rbindlist(good_refs,fill = T)
intern_files_dt = rbindlist(good_files,fill = T)
good_dt$PDF_NAME = intern_files_dt$PDF_NAME[match(good_dt$project_num,intern_files_dt$ROW_NUM)]
      
      
good_dt[is.na(PDF_NAME),]




files[PDF_NAME=='40648_92374_FSPLT3_2422552',]

#codes[project_num=='25760',]
found_ref[File=='37038_82052_FSPLT2_286509',]


docs[docs$File_Name=='37038_82052_FSPLT2_286509.pdf',]

37038_82052_FSPLT2_135344


found_ref
37038_82052_FSPLT2_286509


codes$project_num
codes

docs[Pr==28760,]
docs$File_Name = gsub('\\.pdf$','',docs$File_Name)
docs[docs$File_Name %in% files$PDF_NAME,]





files$PDF_NAME 
found_ref[File %in% files$PDF_NAME]

