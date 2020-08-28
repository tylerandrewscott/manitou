require(data.table)
require(pdftools)
require(pbapply)

dt =  readRDS('scratch/usfs_files_with_reference_section.rds')
pdf_version = gsub('text_as_datatable','documents',dt)
pdf_version = gsub('txt$','pdf',pdf_version)
pdf_version = gsub('[0-9]{1,}--','',pdf_version)
stringcombos = c('Works cited','Works Cited','References','Bibliography','Citations')
stc = paste(c(stringcombos,toupper(stringcombos)),collapse='|')
#test = sample(pdf_version,1)
#temp= pdftools::pdf_text(test)
refs = which(grepl(stc,temp)&!grepl('Table of Contents|TABLE OF CONTENTS|DOCUMENT ORGANIZATION',temp))
pblapply(dt,function(s){
system(paste('anystyle --overwrite -f json,xml,csl find',s,' extracts'))
},cl = 3)






test
require(stringr)


system()


getwd()
anystyle -f csl,xml find ../eis_documents/agency_nepa_libraries/usfs/documents//2016/*.pdf extract2016



test

tt = sapply(temp,str_extract_all,'\\n')
tt[[140]]
