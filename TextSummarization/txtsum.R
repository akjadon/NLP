
rm(list=ls())
pack <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("rvest","lexRankr","selectr","ggplot2","wordcloud","tm","tidyverse","tidytext","topicmodels","SnowballC","wordcloud","RColorBrewer","LDAvis",
              "magrittr","Rgraphviz","data.table","lubridate","textmineR")

#source("https://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

pack(packages)


icube <- read.csv(file = "C:\\Users\\ajadon1\\Desktop\\projects\\NLP\\textSummarisation\\icue.csv",na.strings=c("", "NA"), header = T)

#icube <- icube[7000:7000,]

#text=as.character(icube$COMMENTS[1])
 
txt_sum <- function(text) {
  top_3 = lexRankr::lexRank(text,
                                 #only 1 article; repeat same docid for all of input vector
                                 docId = rep(1, length(text)),
                                 #return 3 sentences to mimick /u/autotldr's output
                                 #n = 3,
                                 #returnTies = F,
                                 sentencesAsDocs = T,
                                 threshold = 0.1,
                                 usePageRank=T,
                                 continuous = TRUE)
  #damping = 0.1)
  #reorder the top 3 sentences to be in order of appearance in article
  order_of_appearance = order(as.integer(gsub("_","",top_3$sentenceId)))
  #extract sentences in order of appearance
  ordered_top_3 = top_3[order_of_appearance, "sentence"]
  ordered_top_3=as.data.frame(ordered_top_3)
  y=t(ordered_top_3)
  y1 = paste(y, collapse="  ")
  return(y1)
  
}

list1 = c()

for (i in c(1:length(icube$COMMENTS))) 
  {
  tryCatch({ list1[i] <-  txt_sum(as.character(icube$COMMENTS[i])) }, 
           error = function(e) { list1[i] <-  icube$COMMENTS[i] }
            )
}
listd1 <- as.data.frame(list1)


list = c()

for (i in c(1:length(icube$COMMENTS))) 
{
  tryCatch({ list[i] <-  txt_sum(as.character(icube$COMMENTS[i])) }, 
           error = identity
  )
  
}


listd <- as.data.frame(list)

###

icube$New_comment <- listd$list


### writing csv

write.csv(icube, file = "C:\\Users\\ajadon1\\Desktop\\projects\\NLP\\textSummarisation\\t.csv")


text <- as.character(icube$COMMENTS)

top_3 = lexRankr::lexRank(text,
                          #only 1 article; repeat same docid for all of input vector
                          docId = rep(1, length(text)),
                          #return 3 sentences to mimick /u/autotldr's output
                          #n = 3,
                          #returnTies = F,
                          #sentencesAsDocs = T,
                          #threshold = 0.1,
                          #usePageRank=T,
                          continuous = TRUE)
#damping = 0.1)
#reorder the top 3 sentences to be in order of appearance in article
order_of_appearance = order(as.integer(gsub("_","",top_3$sentenceId)))
#extract sentences in order of appearance
ordered_top_3 = top_3[order_of_appearance, "sentence"]
ordered_top_3=as.data.frame(ordered_top_3)
y=t(ordered_top_3)
y1 = paste(y, collapse=" ; ")


list <-  icube$COMMENTS
