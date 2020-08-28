
require(data.table)
require(pdftools)
comment_pages = readRDS('input/temp_pg.no_comments.rds')
dir.create('input/comment_docs/')

flist = list.files('../eis_documents/agency_nepa_libraries/usfs/documents/',recursive = T,full.names = T)
need_still = which(!file.exists(paste0('input/comment_docs/',comment_pages$File_Name)))
for(i in need_still){
  print(i)
fl = grep(comment_pages$File_Name[i],flist,value = T)
if(length(fl)>0){
start = comment_pages$Start[i]
end = comment_pages$End[i]
pdftools::pdf_subset(input = fl,pages = start:end,paste0('input/comment_docs/',basename(fl)))
}
}
