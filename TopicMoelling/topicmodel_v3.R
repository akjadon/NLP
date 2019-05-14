
#update.packages()
#### Removing all the existing objects ######

rm(list=ls())

# Start writing to an output file
#sink('C:\\Users\\ajadon1\\Desktop\\projects\\featureEngineering\\topic_modelling\\data\\analysis-output.txt')


### install required packages ####

pack <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("ggplot2","wordcloud","tm","tidyverse","tidytext","topicmodels","SnowballC","wordcloud","RColorBrewer","LDAvis",
              "magrittr","Rgraphviz","data.table","lubridate","RWeka","RWekajars","lda","textstem","sotu")

pack(packages)

#install.packages("data.table", type = "source",repos = "http://Rdatatable.github.io/data.table")


#source("https://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

#######    importing all the data data_1aug,data_3aug,data_4aug, data_5aug,data_6aug,data_7aug in R

colsToKeep <- c("MEDICARECLAIMNUMBER","PYRESOLVEDTIMESTAMP_CST","WRAPUPCOMMENTS")

folder <- "C:\\Users\\ajadon1\\Desktop\\projects\\featureEngineering\\topic_modelling\\data\\"      # path to folder that holds multiple .csv files
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

### removing rows having less than 5 text in it.

dt2 <- dt2[which(dt2$txt_count > 5), ]
dt2$txt_count <- NULL


# Making corpus 
cool_corpus <- Corpus(VectorSource(dt2$new_comment))
#inspect(cool_corpus)
# Remove numbers 
cool_corpus <- tm_map(cool_corpus, removeNumbers)
#Remove Punctuation *************************************
cool_corpus <- tm_map(cool_corpus, removePunctuation)


#change words
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="don not|didn", replacement= "dont")
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="hipv|hpa|hippa|hipaav|hipaa|hip", replacement= "hipaa")
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="uhc|unitedhealthcare", replacement= "unitedhealthcare")
cool_corpus <- tm_map(cool_corpus,content_transformer(gsub), pattern ="authorizationorization|authorized|auth|aauthorization|authorizationorizations",replacement= "authorization")
cool_corpus <-tm_map(cool_corpus, lemmatize_strings)
## steming 
#cool_corpus <- tm_map(cool_corpus, stemDocument)
# Remove english stop words **********************
cool_corpus <- tm_map(cool_corpus, removeWords, stopwords("SMART"))
cool_corpus <- tm_map(cool_corpus, stripWhitespace)
# Remove own stopwords *************************************
cool_corpus <- tm_map(cool_corpus, removeWords, c("ooa","cs","vh","sc","ivr","aiv","ms","va","hvms","vai","hv","na","can", "info", "n/a","ver","vfd",
                                                  "ver'd", "verified", "sel", "member","calls","called","calling", "nza",
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
                                                  "adv","fl","ca","ssa","ma","si","az","ci","nj","ga","call","members","gave","spoke","stated","st",
                                                  "ave","ste","mapd","disconnect","hang","rr"))

# Remove extra space **********************************
cool_corpus <- tm_map(cool_corpus, stripWhitespace)

#cool_corpus <- lexicalize(cool_corpus )

dataframe <- data.frame(text=sapply(cool_corpus, identity),  stringsAsFactors=F)
#### getting back to text dataframe
dt3 <- cbind.data.frame(dt2,dataframe)
dt4_all <- dt3[!( dt3$text=="" | dt3$text==" " ), ]

dt4 <- dt4_all[ (dt4_all$PYRESOLVEDTIMESTAMP_CST=="01-AUG-17" |dt4_all$PYRESOLVEDTIMESTAMP_CST=="04-AUG-17"), ]
test <- dt4_all[ dt4_all$PYRESOLVEDTIMESTAMP_CST=="03-AUG-17" , ]

####################################################################################################################

# Creating  term document matrix
dt4 <- na.omit(dt4)

myCorpus <- Corpus(VectorSource(dt4$text))
# convert corpus to a Plain Text Document

tdm0 <- TermDocumentMatrix(myCorpus,control = list(minWordLength = 2))
tdm <- removeSparseTerms(tdm0,sparse=.998)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, scale=c(4,.5),
          max.words=50, random.order=FALSE, rot.per=0.35, 
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




############################################################################### done ###############################
####################################################################################################################
#### topic modelling
##Conversing to lower case *****************************

#### topic modelling

corpusLDA <- lexicalize(dt4$text ,lower=TRUE)
## Only keep words that appear at least twice:
#to.keep <- corpusLDA$vocab[word.counts(corpusLDA$documents, corpusLDA$vocab) >= 5]
## Re-lexicalize, using this subsetted vocabulary
#corpusLDA <- lexicalize(dt4$text, lower=TRUE, vocab=to.keep)


alpha <- 0.02
eta <- 0.02

library("topicmodels")

ldaModel=lda.collapsed.gibbs.sampler(corpusLDA$documents,K=4,vocab=corpusLDA$vocab,burnin=500,
                                     num.iterations=1000,alpha=.02,eta=0.02 ,compute.log.likelihood = TRUE)

## scoring
test <- dt4_all[ dt4_all$PYRESOLVEDTIMESTAMP_CST=="03-AUG-17" , ]
rownames(test) = NULL
test <- na.omit(test)

corpusLDA_test <- lexicalize(test$text )


ldaRes=lda.collapsed.gibbs.sampler(corpusLDA_test$documents,K=4,initial= list(topics=ldaModel$topics,topic_sums=ldaModel$topic_sums),
                                   vocab=corpusLDA$vocab,burnin=500,num.iterations=1000,alpha=.02,eta=0.2, freeze.topics=TRUE)


####################################################################################################################
########  train & Test outcome #####################################################################################

top_words = top.topic.words(ldaModel$topics, 50, by.score=TRUE)

ww <- as.data.frame(top_words)
colnames(ww) <- c("Topic1","Topic2","Topic3","Topic4")
colnames(ww) <- c("general","provider_coverage","demographic","finance")
write.csv(ww, file = "C:\\Users\\ajadon1\\Desktop\\projects\\featureEngineering\\topic_modelling\\data\\top_word_train.csv")

wt <- as.data.frame(t(ldaModel$topics))
colnames(wt) <- c("Topic1","Topic2","Topic3","Topic4")
write.csv(wt, file = "C:\\Users\\ajadon1\\Desktop\\projects\\featureEngineering\\topic_modelling\\data\\word_topic_train.csv")

### posterior probability of document-topic distribution
theta_train <- t(apply(ldaModel$document_sums + alpha, 2, function(x) x/sum(x)))*10

aa <- as.data.frame(theta_train)
colnames(aa) <- c("general","provider_coverage","demographic","finance")


### final data outcome
final_out_train <- cbind(dt4[,c('MEDICARECLAIMNUMBER','PYRESOLVEDTIMESTAMP_CST','GRP_WRAPUPCOMMENTS'),],aa)
write.csv(final_out_train, file = "C:\\Users\\ajadon1\\Desktop\\projects\\featureEngineering\\topic_modelling\\data\\final_out_train.csv")


#########  test ###########################
theta_test <- t(apply(ldaRes$document_sums + alpha, 2, function(x) x/sum(x)))*10
aa_test <- as.data.frame(theta_test)
colnames(aa_test) <- c("general","provider_coverage","demographic","finance")
### final data outcome
final_out_test <- cbind(test[,c('MEDICARECLAIMNUMBER','PYRESOLVEDTIMESTAMP_CST','GRP_WRAPUPCOMMENTS'),],aa_test)
write.csv(final_out_test, file = "C:\\Users\\ajadon1\\Desktop\\projects\\featureEngineering\\topic_modelling\\data\\final_out_test.csv")

####################################################################################################################
# Append to the file
#sink('C:\\Users\\ajadon1\\Desktop\\projects\\featureEngineering\\topic_modelling\\data\\analysis-output.txt', append=TRUE)
#cat("Some more stuff here...\n")
#sink()








####################################################################################################################
########################################visualization##############################################################
VisualizeText(dt4[,'text'], .50, label_degree_cut=3)

####################################################################################################################
#################  visualization ###################################################################################

dt_top <- as.data.frame(tolower(dt4[,'text']))
dt_top <- unlist(dt_top)
 
reviews<-dt_top
# pre-processing:
reviews <- gsub("'", "", reviews)  # remove apostrophes
reviews <- gsub("[[:punct:]]", " ", reviews)  # replace punctuation with space
reviews <- gsub("[[:cntrl:]]", " ", reviews)  # replace control characters with space
reviews <- gsub("^[[:space:]]+", "", reviews) # remove whitespace at beginning of documents
reviews <- gsub("[[:space:]]+$", "", reviews) # remove whitespace at end of documents
reviews <- tolower(reviews)  # force to lowercase

# tokenize on space and output as a list:
doc.list <- strsplit(reviews, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
#del <- names(term.table) %in% stop_words | term.table < 5
#term.table <- term.table[!del]
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



#doc.length = colSums( as.matrix(tdm) > 0 )[!empty.docs]


library(LDAvis)
theta <- t(apply(ldaModel$document_sums + alpha,  2,   function(x) x/sum(x)))
phi <- t(apply(t(ldaModel$topics) + eta,        2,        function(x) x/sum(x)))

# create the JSON object to feed the visualization:
json <- createJSON(phi = phi, 
                   theta = theta,
                   vocab = corpusLDA$vocab, 
                   doc.length = doc.length,
                   term.frequency = term.frequency)

serVis(json, out.dir = 'vis',  open.browser = TRUE)








