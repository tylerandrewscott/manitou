library(data.table)
library(stringr)
library()
scif <- list.files('usfs_bibliometrics/scratch/scimago/',full.names = T)
scif <- grep('201[0-9]',scif,value = T)
scid_issn_dt <- data.table()
scimag_dt <- data.table()
for(x in scif){
  print(x)
  sc <- fread(x)
  sc$Year <- str_extract(x,'[0-9]{4}')
  scimag_dt <- rbind(scimag_dt,sc,fill = T,use.names = T)
  sc$ISSN <- str_split(sc$Issn,'\\,\\s')
  source_issn <- lapply(seq_along(sc$ISSN),function(x) {
    data.table(Sourceid = as.character(sc$Sourceid[x]),ISSN = sc$ISSN[[x]])}
  )
  issn_dt <- rbindlist(source_issn,fill = T)
  scid_issn_dt <- rbind(scid_issn_dt,issn_dt,fill = T,use.names = T)
}
scid_issn_dt <- scid_issn_dt[!duplicated(scid_issn_dt),]
saveRDS(object = scid_issn_dt,file = 'usfs_bibliometrics/input/scimago_issn.RDS')
saveRDS(object = scimag_dt,file = 'usfs_bibliometrics/input/scimago_journal_data.RDS')

