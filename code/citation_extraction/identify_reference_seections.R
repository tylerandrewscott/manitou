setwd('../manitou/')
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

stringcombos = c('Works cited','Works Cited','References','Bibliography','Citations')
stc = paste(c(stringcombos,toupper(stringcombos)),collapse='|')
tmpdir = tempdir()
library(doParallel)

for(y in text_subdirs$name[text_subdirs$name>2000]){
  print(y)
file_set = box_ls(text_subdirs[name==y,]$id)
file_set_dt = as.data.table(file_set)
file_set_dt = file_set_dt[!grepl('xml',name),]
file_set_dt = file_set_dt[!is.na(as.numeric(file_set_dt$id)),]
fname = paste0('docs_with_references_',y,'.csv')
#file.remove(fname)
ref_sections = mclapply(1:nrow(file_set_dt),function(f) {
   success = F
   tries = 0
   while(!success|tries<5){
   temp = tryCatch({box_read_tsv(file_set_dt$id[f],fread = T,data.table = T)},error = function(e) NULL)
   success = !is.null(temp)
   tries = tries + 1
   }
   refs = any(grepl(stc,temp$text)&!grepl('Table of Contents|TABLE OF CONTENTS',temp$text))
   if(refs){data.table(id = file_set_dt$id[f],name = file_set_dt$name[f])}
},mc.cores = 12)
ref_dt = rbindlist(ref_sections)
if(nrow(ref_dt)>0){write.csv(x = ref_dt,file = fname)}
flist = list.files(tmpdir,full.names=T)
flist = flist[basename(flist) %in% file_set_dt$name]
file.remove(flist)
}



