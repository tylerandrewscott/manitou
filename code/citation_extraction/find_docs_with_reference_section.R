
require(boxr)
require(data.table)
require(utf8)
library(pbapply)
#fwrite(data.table(File = character()),file = 'scratch/boilerplate/reference_files.csv')
#fwrite(data.table(Box_ID = character(), File = character()),file = 'scratch/files_with_references.csv')
#dir_create("~/.boxr-auth", mode = 700)
box_auth_service()
eis_doc_id = '94759130991'
eis_dir = boxr::box_ls(eis_doc_id)
agency_dirs = as.data.table(eis_dir)[name=='agency_nepa_libraries',]$id
usfs_subd_id = as.data.table(box_ls(agency_dirs))[name=='usfs',]$id
usfs_text_subds_id = as.data.table(box_ls(usfs_subd_id))[name=='text_as_datatable',]$id
text_subdirs = as.data.table(box_ls(usfs_text_subds_id))

stringcombos = c('Works cited','Works Cited','References','Bibliography','Citations','Literature Cited')
stc = paste(c(stringcombos,toupper(stringcombos)),collapse='|')
tmpdir = tempdir()
library(doParallel)

setwd('../manitou/')
require(boxr)
require(data.table)
require(utf8)
library(pbapply)
#fwrite(data.table(File = character()),file = 'scratch/boilerplate/reference_files.csv')
#fwrite(data.table(Box_ID = character(), File = character()),file = 'scratch/files_with_references.csv')
#dir_create("~/.boxr-auth", mode = 700)


library(doParallel)

stringcombos = c('Works Cited','References','Works cited','Bibliography')
stc = paste(c(stringcombos,toupper(stringcombos)),collapse='|')

flist = list.files('../eis_documents/agency_nepa_libraries/usfs/text_as_datatable/',full.names = T,recursive = T)

list_of_ref_docs = pblapply(flist,function(f) {
  have = F
  while(!have){
    temp = tryCatch(fread(f),error = function(e) NULL)
    if(!is.null(temp)){have = T}
  }
  ref = any(grepl(stc,temp$text)&!grepl('Table of Contents|TABLE OF CONTENTS|DOCUMENT ORGANIZATION',temp$text))
  if(ref){f}
},cl = 8)

refs = unlist(list_of_ref_docs)
saveRDS(refs,file = 'scratch/usfs_files_with_reference_section.rds')











