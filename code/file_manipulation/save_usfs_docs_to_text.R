
library(pdftools)
library(stringr)
library(tidyverse)
#nohup R --no-save <win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.R >& win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.Rout &
library(tm)
library(SnowballC)
library(stringr)
library(RCurl)
library(lubridate)
library(pbapply)
library(jsonlite)
library(pdfsearch) 
library(pbapply)
library(data.table)
proj = fread('input/forest_service_project_overview_2019-03-10.csv')
proj_forest = gsub('projects\\/|\\/landmanagement','',str_extract(proj$NF,'projects/.+/landmanagement'))

flist = list.files(paste0('../../../../net/tmp/tscott1/manitou_scratch/scratch/usfs_project_documents/',unique(proj_forest)),pattern = 'pdf',full.names = T)

#test = tabulizer::extract_text(paste0('scratch/eis_documents/',f))
#tt = tokenizers::tokenize_sentences(test)

readPDF2<-function (engine = c("xpdf", "Rpoppler", "ghostscript", "Rcampdf","custom"), control = list(info = NULL, text = NULL)) {
  stopifnot(is.character(engine), is.list(control))
  engine <- match.arg(engine)
  pdf_info <- switch(engine, xpdf = function(x) tm:::pdf_info_via_xpdf(x,control$info), Rpoppler = Rpoppler::PDF_info, ghostscript = tm:::pdf_info_via_gs,Rcampdf = Rcampdf::pdf_info, custom = control$info)
  pdf_text <- switch(engine, xpdf = function(x) system2("pdftotext", c(control$text, shQuote(x), "-"), stdout = TRUE), Rpoppler = Rpoppler::PDF_text, ghostscript = pdf_text_via_gs, Rcampdf = Rcampdf::pdf_text, custom = control$text)
  if (!is.function(pdf_info) || !is.function(pdf_text)) 
    stop("invalid function for PDF extraction")
  function(elem, language, id) {
    uri <- processURI2(elem$uri)
    #meta <- pdf_info(uri)
    meta<-list()
    content <- pdf_text(uri)
    content<-iconv(enc2utf8(content), sub = "byte")
    tm::PlainTextDocument(content, meta$Author, meta$CreationDate, 
                          meta$Subject, meta$Title, basename(elem$uri), language, 
                          meta$Creator)}}
processURI2 <-function(uri) {
  uri <- as.character(uri)
  if (identical(substr(uri, 1, 7), "file://"))
    uri <- substr(uri, 8, nchar(uri))
  uri
}


test= list.files('scratch/eis_documents_plaintext/')
regex_line_number_string = "([0-9]{1,2}\\s){3,}"

read_engine <- readPDF2(engine=c("Rpoppler"),control = list(info = '-f'))
floc = '../../../../net/tmp/tscott1/manitou_scratch/scratch/usfs_project_documents/'
readable = list.files('../../../../net/tmp/tscott1/manitou_scratch/scratch/usfs_documents_plaintext/')
library(pdftools)

#sapply(unique(proj_forest ),function(x) {dir.create(paste0('../../../../net/tmp/tscott1/manitou_scratch/scratch/usfs_documents_plaintext/',x))})

file_status = do.call(rbind,pblapply(seq_along(flist),function(i){

  dfile   = paste0(flist[i])
  if(!file.exists(gsub('pdf','txt',gsub('usfs_project_documents','usfs_documents_plaintext',dfile)))){
  rr <- tryCatch(read_engine(elem = list(uri = dfile), language = "en") ,error = function(e) NULL)
  read_status = ifelse(is.null(rr),'Bad read',"Good read")
  read_df = data.frame(DOC = flist[i],PROJ_NUMBER = str_extract(flist[i],'^[0-9]{1,}'),RESULT = read_status)
  if(!is.null(rr)){
    rr <- stringr::str_replace_all(unlist(rr),"[\\s]+", " ")
    rr <- stringr::str_replace_all(unlist(rr),regex_line_number_string, "")
    write.table(rr, file=gsub('pdf','txt',gsub('usfs_project_documents','usfs_documents_plaintext',dfile)),
                quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
    rm(rr)
  }
  read_df}
},cl = 2))

write.csv(file = 'input/batch_usfs_text_to_pdf_record.csv',x = file_status)



