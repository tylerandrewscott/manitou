require(rvest)
require(httr)
old = fread('input/usfs_internal_data/Copy of MYTR_National Level(1100).xlsx - Decision Data Set.csv')
current = readRDS('input/usfs_internal_data/FS_ongoing_projects_11-2019.rds')
projects = rbind(old,current,use.names = T,fill = T)
base = 'https://cara.ecosystem-management.org/Public//ReadingRoom?Project='


projects[`PROJECT NUMBER`=='50185',]
projects$CARA = NA

for(i in which(projects$`DECISION TYPE`=='DN')){
  if(i < 40904){next}
  print(i)
  temp_url = paste0(base,projects$`PROJECT NUMBER`[i])
  test = read_html(temp_url) %>% html_text(trim = T) 
  projects$CARA[i] <- !grepl('System.Web.HttpUnhandledException|Public Reading Room is not available',test)
}


have_cara = projects[CARA==T,]
have_cara$CARA_PAGE = paste0(base,have_cara$`PROJECT NUMBER`)

saveRDS(have_cara,'scratch/projects_with_CARA_page.RDS')
