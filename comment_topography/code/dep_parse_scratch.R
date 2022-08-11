
#packages = c('data.table','stringr','tidyverse','sf','lwgeom','tigris','sentimentr')
#not_installed = packages[!packages %in% installed.packages()[,'Package']]
#if(length(not_installed)>0){lapply(not_installed,install.packages)}
#lapply(packages,require,character.only = T)

set.seed(24)
library(tidyverse)
library(vader)
library(tm)
library(data.table)
library(tokenizers)
library(quanteda)
library(quanteda.textstats)
library(pbapply)
dt = readRDS('comment_topography/input/cleaned_comment_text.RDS')
meta = readRDS('comment_topography/input/cleaned_comment_meta.RDS')
form <- fread('comment_topography/input/form_letter_designation.txt')
form$UQID <- as.character(form$UQID)
meta <- left_join(meta,form)
ptest <- meta$Project.Number[1]
dt <- dt[UQID %in% meta$UQID[meta$Project.Number==ptest],]
meta <- meta[Project.Number == ptest,]
meta <- meta[!duplicated(GROUP),]
dt <- dt[dt$UQID %in% meta$UQID,]
dt <- dt[nchar(dt$Letter.Text.Clean)<100e3,]
meta <- meta[UQID %in% dt$UQID,]

sent <- fread('comment_topography/input/sentiment_scores.txt')
sent$UQID <- as.character(sent$UQID)

meta <- left_join(meta,sent)

library(stm)
library(tm)
library(spacyr)
library(tokenizers)

doc <- spacyr::spacy_parse(dt$Letter.Text.Clean[1],lemma = FALSE, entity = TRUE, nounphrase = TRUE)
#doc <- data.table(doc)

nounphrase_consolidate(doc)


dt$Letter.Text.Clean[3]

parsedtxt <- spacy_parse(txt, lemma = FALSE, entity = TRUE, nounphrase = TRUE)
spacy_parse('The ranger cut down the forest.')
spacy_parse('Please reject the proposal.')
unique(doc$pos)

doc[sentence_id==2,]
doc[,any(pos=='SUBJ'),by=.(sentence_id)]

doc[doc$sentence_id==5,]






sents[[1]]

doc$sentence_id

dt$Letter.Text.Clean[1]doc[doc$doc_id==5,]


"Please reject the request of the Klamath National Forest to begin clear cutting these forests prior to any meaningful review process." 


doc$pos

meta$UQID <- as.character(meta$UQID)
dt$UQID <- as.character(dt$UQID)
tot <- left_join(dt,meta)
tot$text <- tolower(tot$Letter.Text.Clean)
tot$text <- tm::removeWords(tot$text,tm::stopwords())
tot_corpus <- corpus(tot, text_field = "text")
dfm <- quanteda::dfm(tot_corpus)
dfm <- dfm[,grepl('[A-Za-z]',colnames(dfm))]
library(quanteda)

dfm <- quanteda::dfm_trim(dfm,min_termfreq = 5)

k_search <- searchK(dfm,K = c(5,10,20,40,80),
                    N = .2 * nrow(dfm),
                    prevalence = ~ + FORM,
                    data = docvars(tot_corpus))
k_search$results

setnames(dt,c('UQID','text'),c('doc_id','text'))
src <- DataframeSource(x = dt)

gadarian_corpus <- corpus(gadarian, text_field = "text")
corp <- Corpus(src)
dtm <- DocumentTermMatrix(corp)
stm_corp <- stm::readCorpus(dtm,type = 'dtm')




stm(stm_corp,)


?stm
stm(stm_corp,K = 20,

?tm::Corpus()
str(proc)
tm::DocumentTermMatrix()
corp <- readCorpus(proc,type = 'dtm')

?readCorpus


#dt = dt[meta$FORM==0,]
library(spacyr)
library(udpipe)
spacyr::spacy_initialize('en_core_web_sm')




test <- tokenizers::tokenize_sentences(dt$Letter.Text.Clean)
test_dt <- data.table(doc_id = dt$UQID,
                      text = dt$Letter.Text.Clean)
ud_tokens <- udpipe(x = test_dt, object = 'english')

ud_tokens[ud_tokens$doc_id=='73',]

sentence_objects <- as.data.table(table(tolower(ud_tokens[ud_tokens$dep_rel=='obj',]$token)))
sentence_objects
#sentence_subjects <- as.data.table(table(tolower(ud_tokens[ud_tokens$dep_rel=='obj',]$token)))


sentence_objects

unique(ud_tokens$dep_rel)


library(vader)
sent_df <- vader::vader_df(dt$Letter.Text.Clean)


library(rsyntax)

library(udpipe)
library(textrank)
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = dt$Letter.Text.Clean, doc_id = dt$UQID,trace = TRUE)
x <- as.data.table(x)
x$FORM <- meta$FORM[match(x$doc_id,meta$UQID)]
x$FORM <- ifelse(x$FORM==1,'form','unique')


x


## Collocation (words following one another)
stats <- keywords_collocation(x = x[x$FORM=='form',], 
                              term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)
## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
stats <- cooccurrence(x = subset(x[x$FORM=='form',], upos %in% c("NOUN", "ADJ")), 
                      term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = x[x$FORM=='form',]$lemma, 
                      relevant = x[x$FORM=='form',]$upos %in% c("NOUN", "ADJ"))
## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
form_stats <- cooccurrence(x = x[x$FORM=='form',]$lemma, 
                      relevant = x[x$FORM=='form',]$upos %in% c("NOUN", "ADJ"), skipgram = 2)
head(stats)

## Collocation (words following one another)
stats <- keywords_collocation(x = x[x$FORM!='form',], 
                              term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)
## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
stats <- cooccurrence(x = subset(x[x$FORM!='form',], upos %in% c("NOUN", "ADJ")), 
                      term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = x[x$FORM!='form',]$lemma, 
                      relevant = x[x$FORM!='form',]$upos %in% c("NOUN", "ADJ"))
## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
uq_stats <- cooccurrence(x = x[x$FORM!='form',]$lemma, 
                      relevant = x[x$FORM!='form',]$upos %in% c("NOUN", "ADJ"), skipgram = 2)



stats <- keywords_rake(x = x[x$FORM!='form',], 
                       term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                       relevant = x[x$FORM!='form',]$upos %in% c("NOUN", "ADJ"),
                       ngram_max = 4)
head(subset(stats, freq > 3))


form_stats[1:10,]
uq_stats[1:10,]



noun_count <- x[upos=='NOUN',.N,by=.(lemma,FORM)]
noun_count[,rank:=rank(-N),by=.(FORM)]


g1 <- ggplot(noun_count[rank<=10&FORM!='form',][order(rank),] %>%
               mutate(lemma = fct_inorder(lemma))) + 
  # facet_wrap(~FORM,scales = 'free') + 
  geom_bar(aes(x = lemma,y = N),
           stat = 'identity') + 
  theme_bw() + coord_flip() + 
  ggtitle('unique letters')
g2 <- ggplot(noun_count[rank<=10&FORM=='form',][order(rank),] %>%
         mutate(lemma = fct_inorder(lemma))) + 
 # facet_wrap(~FORM,scales = 'free') + 
  geom_bar(aes(x = lemma,y = N),
           stat = 'identity') + 
  theme_bw() + coord_flip() +
  ggtitle('form letters')
library(gridExtra)





ggplot(x[upos=='NOUN',])
stats <- subset(x, upos %in% "NOUN")


stats <- txt_freq(x = stats$lemma)
library(lattice)
stats$key <- factor(stats$key, levels = rev(stats$key))

stats
head(stats)

barchart(key ~ freq, data = head(stats, 30), col = "cadetblue", main = "Most occurring nouns", xlab = "Freq")

doc <- spacyr::spacy_parse(test)

doc







