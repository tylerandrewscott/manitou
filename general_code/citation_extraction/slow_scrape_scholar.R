library(data.table)
library(rvest)

dt = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vQYWiNWT5yI-Wgl1po1oWf9CIkU_9oCLSP7mKRU5h8Lftrl1FBntcsW19nYXauwSbArApbQ_8WqFqqu/pub?output=csv')

dt$Q = sapply(dt$Title,URLencode)
#dt$tquery = gsub(' ','+',dt$Title,fixed = T)
#dt$tquery = gsub(':','%3A',dt$tquery,fixed = T)
#dt$tquery = gsub('?','%3F',dt$tquery,fixed = T)

URLencode(dt$Title)
baseq = 'https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=title%3A'
library(httr)

citations = data.table()
for(i in 1:nrow(dt)){
  if(dt$Title[i]!=''){
    print(i)
    Sys.sleep(2)
    q = paste0(baseq,'"',dt$Q[10],'"')
    res = read_html(q)
    if(grepl('did not match any articles',res %>% html_text(trim=T))){next}
    nodes = res %>% html_nodes('a') 
    citing_papers_node = which(grepl("/scholar?cites",nodes %>% html_attr('href'),fixed = T))
    preview_abstract = res %>% html_nodes('.gs_rs') %>% html_text(trim=T)
    byline = res %>% html_nodes('.gs_a') %>% html_text(trim=T)
    current_citations = res %>% html_nodes('.gs_or_cit+ a') %>% html_text(trim=T)
    citing_papers_href = nodes %>% html_attr('href') %>% .[[citing_papers_node]]
    google_id = res %>% html_nodes('h3') %>% html_nodes('a') %>% html_attr('id')
    main_link = res %>% html_nodes('h3') %>% html_nodes('a') %>% html_attr('href')
    open_source_href = res %>% html_nodes('.gs_or_ggsm a')  %>% html_attr('href')
    Sys.sleep(5)
    citations = rbind(citations,data.table(index = i,preview_abstract,byline,current_citations,main_link,open_source_href,google_id,citing_papers_href))}
}


