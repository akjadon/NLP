
#update.packages()
#### Removing all the existing objects ######

rm(list=ls())

### install required packages ####

pack <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("ggplot2","wordcloud","tm","tidyverse","tidytext","topicmodels","SnowballC","wordcloud","RColorBrewer","LDAvis",
              "magrittr","Rgraphviz","data.table","lubridate","RWeka","RWekajars","lda")

pack(packages)

#install.packages("data.table", type = "source",repos = "http://Rdatatable.github.io/data.table")


#source("https://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

#######    importing all the data data_1aug,data_3aug,data_4aug, data_5aug,data_6aug,data_7aug in R

colsToKeep <- c("MEDICARECLAIMNUMBER","PYRESOLVEDTIMESTAMP_CST","WRAPUPCOMMENTS")

folder <- "C:\\Users\\ajadon1\\Desktop\\projects\\topic_modelling\\data\\"      # path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*aug.csv") # create list of all .csv files in folder

dt <- do.call(rbind,lapply(file_list,  function(x)  fread(paste(folder,x,sep = ''),header=TRUE, sep=",",
                                                          select=colsToKeep,verbose=TRUE, na.strings = c(""," ","  ","NA","N/A","na","n/a")) ))

dt$PYRESOLVEDTIMESTAMP_CST <- substr(dt$PYRESOLVEDTIMESTAMP_CST ,1,9)
## removing duplicates
dt = unique(dt)
dt <- na.omit(dt)

dt1 <- dt %>%   group_by(MEDICARECLAIMNUMBER,PYRESOLVEDTIMESTAMP_CST) %>%   mutate(GRP_WRAPUPCOMMENTS = paste0(WRAPUPCOMMENTS, collapse = ",")) 

dt2 <- dt1[,c('MEDICARECLAIMNUMBER','PYRESOLVEDTIMESTAMP_CST','GRP_WRAPUPCOMMENTS')]
dt2 = unique(dt2)



##Conversing to lower case *****************************
dt2$GRP_WRAPUPCOMMENTS <- tolower(dt2$GRP_WRAPUPCOMMENTS)
#considering only text data
dt2$new_comment <- lapply(dt2$GRP_WRAPUPCOMMENTS, FUN=function(x) gsub("[^a-zA-Z\\s]", " ", x))

#### looking word count in each column  
dt2$txt_count <- sapply(dt2$new_comment, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))

### removing rows having less than 2 text in it.

dt2 <- dt2[which(dt2$txt_count > 1), ]
dt2$txt_count <- NULL


# Making corpus 
cool_corpus <- Corpus(VectorSource(dt2$new_comment))
#inspect(cool_corpus)
# Remove numbers 
cool_corpus <- tm_map(cool_corpus, removeNumbers)
#Remove Punctuation *************************************
cool_corpus <- tm_map(cool_corpus, removePunctuation)
#change words
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="caller", replacement= "call")
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="doctors", replacement= "doctor")
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="problems", replacement= "problem")
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="prescriptions", replacement= "prescription")
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="questions", replacement= "question")
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="don not|didn", replacement= "dont")
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="hipv|hpa|hippa|hipaav|hipaa|hip", replacement= "hipaa")
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="uhc|unitedhealthcare", replacement= "unitedhealthcare")
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="authorizationorization|authorized|auth|aauthorization|authorizationorizations|authorizationorization", replacement= "authorization")
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="answered|answering", replacement= "answer")
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="completed|complete", replacement= "complete")
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="understood|understand", replacement= "understand")

## steming 
#cool_corpus <- tm_map(cool_corpus, stemDocument)
# Remove english stop words **********************
cool_corpus <- tm_map(cool_corpus, removeWords, stopwords("SMART"))
# Remove own stopwords *************************************
cool_corpus <- tm_map(cool_corpus, removeWords, c("ooa","cs","vh","sc","ivr","aiv","ms","va","hvms","vai","hv","na","can", "info", "n/a", "nza","ver","vfd",
                                                  "ver'd", "verified", "sel", "member","calls","called","calling",
                                                  "mbr","wasnt", "wants", "change", "changed", "september", "soon", "may",
                                                  "also", "will", "takeoverhipaa", "related","see","doesnt","outbound","name","ive","just",
                                                  "much","thank","bye","didnt","shes","cant","went","dont","everything","get","always",
                                                  "anything","can","cant","done","dont","far","getting","give","going","havent","just",
                                                  "keep","make","news","nothing","nwo","one","really","say","see","send","thats","theyre",
                                                  "thing","think","year","every","lot","know","use","never","people","things","time","now","got"
                                                  ,"can", "say","one","way","use","also","howev","tell","will","much","need","take","tend","even",
                                                  "like","particular","rather","said","get","well","make","ask","come","end",
                                                  "first","two","help","often","may","might","see","someth","thing","point",
                                                  "post","look","right","now","think","'ve","'re" ,"anoth","put","set","new","good",
                                                  "want","sure","kind","larg","yes,","day","etc","quit","sinc","attempt","lack","seen","awar",
                                                  "littl","ever","moreov","though","found","abl","enough","far","earli","away","achiev","draw",
                                                  "last","never","brief","bit","entir","brief","great","lot"
                                                  ,"id","ee","rep","tt","dont","isn","4","ar","pa","iv","fe","cc","il","in","wa","ny","vg",
                                                  "adv","fl","ca","ssa","ma","si","az","ci","nj","ga","call","members","gave","spoke","stated"))

# Remove extra space **********************************
cool_corpus <- tm_map(cool_corpus, stripWhitespace)

#cool_corpus <- lexicalize(cool_corpus )

dataframe <- data.frame(text=sapply(cool_corpus, identity),  stringsAsFactors=F)
#### getting back to text dataframe
dt3 <- cbind.data.frame(dt2,dataframe)
dt4_all <- dt3[!( dt3$text=="" | dt3$text==" " ), ]

dt4 <- dt4_all[ (dt4_all$PYRESOLVEDTIMESTAMP_CST=="01-AUG-17" ), ]


####################################################################################################################

# Creating  term document matrix
dt4 <- na.omit(dt4)


myCorpus <- Corpus(VectorSource(dt4$text))
# convert corpus to a Plain Text Document

tdm <- TermDocumentMatrix(myCorpus,control = list(minWordLength = 3))
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, scale=c(4,.5),
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


## frequent terms and their associations
tdm

findFreqTerms(tdm, lowfreq = 50)

findAssocs(tdm, terms = "jci", corlimit = 0.3)

## Plot word frequencies

barplot(d[1:15,]$freq, las = 2, names.arg = d[1:15,]$word,
        col ="lightblue", 
        main =" Most frequent words   ",
        ylab = "Word frequencies")


############################################################################### done ###################
########  bi gramm
##Conversing to lower case *****************************
dt5 <- dt4[1:8000,]
dt_top_bi <- as.data.frame(dt5[,'text'])

dt_top_bi <- unlist(dt_top_bi)

# Making corpus ******************************************

corpus_bi <- VCorpus(VectorSource(dt_top_bi))

corpus_bi <- tm_map(corpus_bi, stripWhitespace)

# convert corpus to a Plain Text Document

corpus_bi <- tm_map(corpus_bi,PlainTextDocument)

#strwrap(corpus_bi)

# Creating document term matrix
#tdm_bi <- TermDocumentMatrix(corpus_bi)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.bigram = TermDocumentMatrix(corpus_bi,control = list(tokenize = BigramTokenizer,weighting = weightTf))


#freq_bi = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
#freq_df_bi = data.frame(word=names(freq_bi), freq=freq_bi)
#head(freq_df_bi, 50)


#bi_ww <- as.data.frame(head(freq_df_bi, 1000))
#bi_ww
#write.csv(bi_ww, file = "C:\\Users\\ajadon1\\Desktop\\projects\\topic_modelling\\data\\bi_ww_out.csv")

#wordcloud(freq_df_bi$word,freq_df_bi$freq, max.words=50,random.order = F, colors=brewer.pal(8, "Dark2"),scale=c(8,.3))



####################################################################################################################
#### topic modelling
##Conversing to lower case *****************************
dt_top <- as.data.frame(tolower(dt4[,'text']))
dt_top <- unlist(dt_top)

#stop_words <- stopwords("SMART")
reviews<-dt_top
# pre-processing:
reviews <- gsub("'", "", reviews)  # remove apostrophes
reviews <- gsub("[[:punct:]]", " ", reviews)  # replace punctuation with space
reviews <- gsub("[[:cntrl:]]", " ", reviews)  # replace control characters with space
reviews <- gsub("^[[:space:]]+", "", reviews) # remove whitespace at beginning of documents
reviews <- gsub("[[:space:]]+$", "", reviews) # remove whitespace at end of documents
reviews <- tolower(reviews)  # force to lowercase


head(reviews)

# tokenize on space and output as a list:
doc.list <- strsplit(reviews, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)


# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (13356)
W <- length(vocab)  # number of terms in the vocab (2720)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [1]   3   3   3   4  40   3   2   2  83   3  11 
N <- sum(doc.length)  # total number of tokens in the data (171590)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]


# MCMC and model tuning parameters:
K <- 4
G <- 1000
alpha <- 0.02
eta <- 0.02

# Fit the model:

#ldaModel=lda.collapsed.gibbs.sampler(documents$documents,K=4,vocab=documents$vocab,burnin=500,num.iterations=10000,alpha=.02,eta=0.02
                                #     ,compute.log.likelihood = TRUE)



fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab,num.iterations = G, alpha = alpha,eta = eta, 
                                   initial = NULL, burnin = 0,compute.log.likelihood = TRUE)


####  posterior of the document-topic distribution  ( theta)
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
####  posterior of the topic-term distribution  (phi)
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
Reviews <- list(phi = phi,
                theta = theta,
                doc.length = doc.length,
                vocab = vocab,
                term.frequency = term.frequency)


library(LDAvis)

# create the JSON object to feed the visualization:
json <- createJSON(phi = Reviews$phi, 
                   theta = Reviews$theta, 
                   doc.length = Reviews$doc.length, 
                   vocab = Reviews$vocab, 
                   term.frequency = Reviews$term.frequency)



#serVis(json, out.dir = 'vis', open.browser =TRUE ) 




####################################################################################################################
#### topic modelling
#corpusLDA <- lexicalize(dt4$text )

#ldaModel=lda.collapsed.gibbs.sampler(corpusLDA$documents,K=4,vocab=corpusLDA$vocab,burnin=500,num.iterations=1000,alpha=.02,eta=0.02
#                                     ,compute.log.likelihood = TRUE)

####  posterior of the document-topic distribution  ( theta)
#theta <- t(apply(ldaModel$document_sums + alpha, 2, function(x) x/sum(x)))
####  posterior of the topic-term distribution  (phi)
#phi <- t(apply(t(ldaModel$topics) + eta, 2, function(x) x/sum(x)))


###
#doc.length <- sapply(corpusLDA, function(x) sum(x[1, ]))  # number of tokens per document

#term.table <- table(unlist(corpusLDA))
#term.table <- sort(term.table, decreasing = TRUE)
# remove terms that are stop words or occur fewer than 5 times:
#del <- names(term.table) %in% stop_words | term.table < 5
#term.table <- term.table[!del]
#vocab <- names(term.table)
#term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

#Reviews <- list(phi = phi,
#                theta = theta,
#                doc.length = corpusLDA,
#                vocab = vocab,
#                term.frequency = term.frequency)

#library(LDAvis)

# create the JSON object to feed the visualization:
#json <- createJSON(phi = Reviews$phi, 
   #                theta = Reviews$theta, 
   #                doc.length = Reviews$doc.length, 
   ##                vocab = Reviews$vocab, 
      #             term.frequency = Reviews$term.frequency)

#serVis(json, out.dir = 'vis', open.browser =FALSE ) 

#################################################################

top.words <- top.topic.words(fit$topics, 50, by.score=TRUE)
top.words
ww <- as.data.frame(top.words)
colnames(ww) <- c("Topic1","Topic2","Topic3","Topic4")
#write.csv(ww, file = "C:\\Users\\ajadon1\\Desktop\\projects\\topic_modelling\\data\\top_word.csv")

wt <- as.data.frame(t(fit$topics))
colnames(wt) <- c("Topic1","Topic2","Topic3","Topic4")
#write.csv(wt, file = "C:\\Users\\ajadon1\\Desktop\\projects\\topic_modelling\\data\\word_topic_dist.csv")

### posterior probability of document-topic distribution
theta_out <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))*10
theta_out

aa <- as.data.frame(theta_out)
colnames(aa) <- c("Topic1","Topic2","Topic3","Topic4")

### final data outcome

final_out <- cbind(dt4[,c('MEDICARECLAIMNUMBER','PYRESOLVEDTIMESTAMP_CST','GRP_WRAPUPCOMMENTS'),],aa)
#final_out
#write.csv(final_out, file = "C:\\Users\\ajadon1\\Desktop\\projects\\topic_modelling\\data\\final_out.csv")


######################################################################################################################
###  appling 

#test <- dt4[9000:10000,]

test <- dt4_all[ dt4_all$PYRESOLVEDTIMESTAMP_CST=="03-AUG-17" , ]
rownames(test) = NULL

#test1 <- subset(dt4_all, PYRESOLVEDTIMESTAMP_CST=="03-AUG-17")

test <- na.omit(test)

corpusLDA_test <- lexicalize(test$text )
corpusLDA <- lexicalize(dt4$text )

#ldaRes=lda.collapsed.gibbs.sampler(corpusLDA_test$documents,K=4,initial= list(topics=ldaModel$topics,topic_sums=ldaModel$topic_sums),
 #                                  vocab=corpusLDA$vocab,burnin=500,num.iterations=1000,alpha=.02,eta=0.2, freeze.topics=TRUE)


ldaRes=lda.collapsed.gibbs.sampler(corpusLDA_test$documents,K=4,initial= list(topics=fit$topics,topic_sums=fit$topic_sums),
                        vocab=corpusLDA$vocab,burnin=500,num.iterations=1000,alpha=.02,eta=0.2, freeze.topics=TRUE)

thetaRes <- t(apply(ldaRes$document_sums +alpha, 2, function(x) x/sum(x)))# 1 is alpha
phiRes <- t(apply(t(ldaRes$topics) + eta, 2, function(x) x/sum(x)))#0.1 is eta


top_words_test <- top.topic.words(ldaRes$topics, 50, by.score=TRUE)
top_words_test
ww_test <- as.data.frame(top_words_test)
colnames(ww_test) <- c("Topic1","Topic2","Topic3","Topic4")
write.csv(ww_test, file = "C:\\Users\\ajadon1\\Desktop\\projects\\topic_modelling\\data\\top_word_test.csv")

wt_test <- as.data.frame(t(ldaRes$topics))
colnames(wt_test) <- c("Topic1","Topic2","Topic3","Topic4")
write.csv(wt_test, file = "C:\\Users\\ajadon1\\Desktop\\projects\\topic_modelling\\data\\word_topic_dist_test.csv")

### posterior probability of document-topic distribution
theta_out_test <- t(apply(ldaRes$document_sums + alpha, 2, function(x) x/sum(x)))*10
theta_out_test

aa_test <- as.data.frame(theta_out_test)
colnames(aa_test) <- c("Topic1","Topic2","Topic3","Topic4")

### final data outcome

final_out_test <- cbind(test[,c('MEDICARECLAIMNUMBER','PYRESOLVEDTIMESTAMP_CST','GRP_WRAPUPCOMMENTS'),],aa_test)

write.csv(final_out_test, file = "C:\\Users\\ajadon1\\Desktop\\projects\\topic_modelling\\data\\final_out_test.csv")



