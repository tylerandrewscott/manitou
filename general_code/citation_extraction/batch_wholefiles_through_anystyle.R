require(data.table)
require(pdftools)
require(pbapply)
require(stringr)
dt =  readRDS('scratch/usfs_files_with_reference_section.rds')
fls = list.files('extracts/',full.names = T,recursive = T,pattern = 'json')
dt = dt[!gsub('txt','json',basename(dt)) %in% basename(fls)]
dt = gsub('txt','pdf',gsub('text_as_datatable','documents',dt))

dir = dirname(dt)
fil = str_remove(basename(dt),'^[0-9]{1,}--')

locs = paste(dir,fil,sep = '/')
locs = locs[file.exists(locs)]

locs = locs[!gsub('pdf','json',basename(locs)) %in% list.files('extracts2/')]

pblapply(locs,function(s){
  system(paste('anystyle --overwrite -f json,xml,csl find --no-layout',s,' extracts2'))
  },cl = 3)



# pdf_version = gsub('text_as_datatable','documents',dt)
# pdf_version = gsub('txt$','pdf',pdf_version)
# pdf_version = gsub('[0-9]{1,}--','',pdf_version)
# stringcombos = c('Works cited','Works Cited','References','Bibliography','Citations')
# stc = paste(c(stringcombos,toupper(stringcombos)),collapse='|')
# #test = sample(pdf_version,1)
# #temp= pdftools::pdf_text(test)
# refs = which(grepl(stc,temp)&!grepl('Table of Contents|TABLE OF CONTENTS|DOCUMENT ORGANIZATION',temp))

list.files('../eis_documents/agency_nepa_libraries/usfs/documents/NA/',pattern = '43854_98196_FSPLT3_2395395.pdf',full.names = T)


file.exists( "../eis_documents/agency_nepa_libraries/usfs/documents/NA//43854_98196_FSPLT3_2395395.pdf")




library(jsonlite)
dim(fromJSON('extracts/43854--43854_98196_FSPLT3_2395395.json'))


file.exists('../eis_documents/agency_nepa_libraries/usfs/documents//2012/21152_38941_FSPLT2_123667.pdf')
require(data.table)
test = fread('../eis_documents/agency_nepa_libraries/usfs/metadata/forest_service_project_overview.csv')
test = fread('../eis_documents/agency_nepa_libraries/usfs/metadata/forest_service_project_detail.csv')
test
test[grepl('51910',Project_Page),]
anystyle --overwrite -f json,xml,csl find ../eis_documents/agency_nepa_libraries/usfs/documents//2012/21152_38941_FSPLT2_123667.pdf
anystyle --overwrite -f json,xml,csl find ../eis_documents/agency_nepa_libraries/usfs/documents//2018/43770_98094_FSPLT3_4112344.pdf


anystyle --overwrite -f json,xml,csl find ../eis_documents/agency_nepa_libraries/usfs/documents/NA//43854_98196_FSPLT3_2395395.pdf


fls1 = list.files('scratch/',full.names = T,recursive = T,pattern = 'anycite.*rds$')
ref_list = lapply(fls1,readRDS)
ref_dt = rbindlist(ref_dt)

require(jsonlite)
ref_list = pblapply(fls,function(f){
temp_js = fromJSON(f)
temp_js = temp_js[nchar(temp_js$title)<500,]
if(nrow(temp_js)>0){temp_js$File = basename(f);temp_js}},cl = 4)

ref_dt = rbindlist(ref_list,fill = T,use.names = T)


ref_dt[grepl('13377',File),]
summary(nchar(ref_dt$title))

dim(ref_dt)
head(temp_js)



names(temp_js)

gc()


