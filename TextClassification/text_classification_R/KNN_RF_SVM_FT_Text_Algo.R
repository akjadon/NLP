library(readxl)
data <- read_excel("C:/Users/adimudga/Desktop/Learning/R/Text data for R.xlsx")
View(data)

df<- data[c("Description", "Label")]
df$id<- seq(1:length(df$Description))
df2<- df[c("id", "Label", "Description")]
df2$Label<- as.factor(df2$Label)
levels(df2$Label)
df2$Label<- tolower(df2$Label)
df2$Label<- as.factor(df2$Label)
levels(df2$Label)

############## Data Cleaning ###########################################

clean_data <- function(x) {
  stopifnot(is.character(x))
  x <- gsub("[[:cntrl:]]", " ", x)
  x <- gsub("<.*>", "", x, perl = TRUE)  ## remove xml tags
  x <- gsub("&amp", "&", x, perl = TRUE) ## decode URL encoded chars
  x <- gsub("&lt", "<", x, perl = TRUE)
  x <- gsub("&gt", ">", x, perl = TRUE)
  x <- gsub("<ref[^<]*<\\/ref>", "", x, perl = TRUE) ## remove references <ref...> ... </ref>
  x <- gsub("<[^>]*>", "", x, perl = TRUE)           ## remove xhtml tags
  x <- gsub("\\[http:[^] ]*", "[", x, perl = TRUE)   ## remove normal url, preserve visible text
  x <- gsub("\\|thumb", "", x, perl = TRUE) ## remove images links, preserve caption
  x <- gsub("\\|left", "", x, perl = TRUE)
  x <- gsub("\\|right", "", x, perl = TRUE)
  x <- gsub("\\|\\d+px", "", x, perl = TRUE)
  x <- gsub("\\[\\[image:[^\\[\\]]*\\|", "", x, perl = TRUE)
  x <- gsub("\\[\\[category:([^|\\]]*)[^]]*\\]\\]", "[[\\1]]", x, perl = TRUE) ## show categories without markup
  x <- gsub("\\[\\[[a-z\\-]*:[^\\]]*\\]\\]", "", x, perl = TRUE) ## remove links to other languages
  x <- gsub("\\[\\[[^\\|\\]]*\\|", "[[", x, perl = TRUE) ## remove wiki url, preserve visible text
  x <- gsub("\\{\\{[^\\}]*\\}\\}", "", x, perl = TRUE) ## remove {{icons}} and {tables}
  x <- gsub("\\{[^\\}]*\\}", "", x, perl = TRUE)
  x <- gsub("\\[", "", x, perl = TRUE) ## remove [ and ]
  x <- gsub("\\]", "", x, perl = TRUE)
  x <- gsub("&[^;]*;", " ", x, perl = TRUE) ## remove URL encoded chars
  # convert to lowercase letters and spaces, spell digits
  x <- tolower(x)
  x <- gsub("0", " zero ", x, perl = TRUE)
  x <- gsub("1", " one ", x, perl = TRUE)
  x <- gsub("2", " two ", x, perl = TRUE)
  x <- gsub("3", " three ", x, perl = TRUE)
  x <- gsub("4", " four ", x, perl = TRUE)
  x <- gsub("5", " five ", x, perl = TRUE)
  x <- gsub("6", " six ", x, perl = TRUE)
  x <- gsub("7", " seven ", x, perl = TRUE)
  x <- gsub("8", " eight ", x, perl = TRUE)
  x <- gsub("9", " nine ", x, perl = TRUE)
  x <- gsub("[[:punct:]]", " ", x)
  x
}


library(stopwords)
library(quanteda)
library(tm)
stopwords<-stopwords::stopwords()
tm.stopwords<- tm::stopwords()
quant.stopwords<- quanteda::stopwords()
Exhaustive.stopwords<- c(stopwords, tm.stopwords, quant.stopwords)
df.stopwords<- as.data.frame(Exhaustive.stopwords)
df.stopwords<- df.stopwords$Exhaustive.stopwords[!duplicated(df.stopwords$Exhaustive.stopwords)]
df2$Description<- clean_data(df2$Description)
df2$Description<- removeWords(df2$Description, df.stopwords)


library(textstem)
df3<- df2
df3$Description<- tolower(df3$Description)
df3$Description<- lemmatize_strings(df3$Description)
df3$Description<- lemmatize_words(df3$Description)
df3$Description<- stem_words(df3$Description)
#df3$Description<- stem_strings(df3$Description)
head(df3)

############ Data Preparation ##############################################
library(slam)
Corpus<- tm::Corpus(VectorSource(df3$Description))
txt= DocumentTermMatrix(Corpus)
txt

txt1<- removeSparseTerms(txt, 0.9999)
txt1
#DTM <- tm::as.DocumentTermMatrix(txt1)
txt_df<- cbind(Label= df3$Label, as.data.frame(as.matrix(txt1)))
names(txt_df)[c(239, 456, 678, 908)]
txt_matrix<- as.matrix(txt1)
colnames(txt_matrix)[1:50]
dim(txt_matrix)
View(txt_matrix[1:20, 1:100])
#names(txt_df)<- make.names(names = txt_df)

#term_tfidf <- tapply(txt1$v/row_sums(txt1)[txt1$i], txt1$j, mean) * log2(nDocs(txt1)/col_sums(txt1 > 0))
rm(Text_Algo_data, data, df, df2, stopwords, df.stopwords, tm.stopwords, 
   quant.stopwords, Corpus,Exhaustive.stopwords)

t.f<- function(row){
  row/sum(row)
}
i.d.f<- function(col){
  corpus.size<- length(col)
  doc.count<- length(which(col>0))
  log10(corpus.size/doc.count)
}
tf.idf<- function(tf, idf){
  tf*idf
  
}
train_t.f<- apply(txt_matrix, 1, t.f)
dim(train_t.f)
View(train_t.f[1:10, 1:20])

train_i.d.f<- apply(txt_matrix, 2, i.d.f)
str(train_i.d.f)

train_tf.idf<- apply(train_t.f, 2, tf.idf, idf=train_i.d.f)
dim(train_tf.idf)
View(train_tf.idf[1:10, 1:20])

train_tf.idf<- t(train_tf.idf)
dim(train_tf.idf)
View(train_tf.idf[1:10, 1:20])

#incomplete cases
incom.cases<- which(!complete.cases(train_tf.idf))
df3$Description[incom.cases]

train_tf.idf[incom.cases, ]<- rep(0.0, ncol(train_tf.idf))
dim(train_tf.idf)
sum(which(!complete.cases(train_tf.idf)))

train_df<- cbind(Label=df3$Label, data.frame(train_tf.idf))
#write.csv(train_df, file = "data_tf_idf.csv")

###### remove unecessary Data ##########################################################
rm(train_df, train_t.f, train_tf.idf, txt_df, txt_matrix, txt1)

############################### Data Partition ################################################
Prep_data<- read.csv(file = "C:\\Users\\adimudga\\Desktop\\Learning\\R\\data_tf_idf.csv", header = T)
Prep_data$X<- NULL

library(plyr)
tr_prop = 0.70
seed= 1234
dt.train = ddply(Prep_data, .(Label), function(., seed) { set.seed(seed); .[sample(1:nrow(.), trunc(nrow(.) * tr_prop)), ] }, seed = 1234)
dt.test = ddply(Prep_data, .(Label), function(., seed) { set.seed(seed); .[-sample(1:nrow(.), trunc(nrow(.) * tr_prop)), ] }, seed = 1234)

# check that proportions are equal across Prep_datasets
ddply(Prep_data, .(Label), function(.) nrow(.)/nrow(Prep_data) )
ddply(dt.train, .(Label), function(.) nrow(.)/nrow(dt.train) )
ddply(dt.test, .(Label), function(.) nrow(.)/nrow(dt.test) )
c(nrow(dt.train), nrow(dt.test), nrow(Prep_data)) # lengths of sets

########## Models ########################################################################
############ KNN #########################################################################
library(e1071)
library(class)
library(caret)
sqrt(9809)

#start.time<- Sys.time()
#knn<- knn(dt.train[ ,-1], test = dt.test[, -1], dt.train[,1], k = 99)
#Sys.time() - start.time
#knn  
#saveRDS(knn, file = "knn.rds")
knn= readRDS("C:\\Users\\adimudga\\Desktop\\Learning\\R\\knn.rds")
summary(knn)

confusionMatrix(knn, dt.test$Label)

############### Random Forest #################################
library(randomForest)
rf.start.time<- Sys.time()
#rf<- randomForest(dt.train$Label~., data = dt.train)
#Sys.time()- rf.start.time
#saveRDS(rf, file = "rf.rds")
rf= readRDS("C:\\Users\\adimudga\\Desktop\\Learning\\R\\rf.rds")
rf
rf_pred<- predict(rf, newdata = dt.test)
confusionMatrix(rf_pred, dt.test$Label)
plot(rf, main= "Error Rate")

#rf<- tuneRF(dt.train[ ,-1], dt.train[,1], stepFactor = 1, plot = T, improve = 0.01)
#rf

#new_rf<- randomForest(dt.train$Label~., data = dt.train, ntree=200,
#                      mtry=44, importance= T, proximity=T)

#saveRDS(new_rf, file = "new_rf.rds")
new_rf= readRDS("C:\\Users\\adimudga\\Desktop\\Learning\\R\\new_rf.rds")
new_rf
new_rf_pred<- predict(new_rf, newdata = dt.test)
confusionMatrix(new_rf_pred, dt.test$Label)

######################### SVM ##################################
library(e1071)
svm.start.time<- Sys.time()
#SVM<- svm(Label~., data = dt.train, kernel="radial")
Sys.time()- svm.start.time
#write.svm(object = SVM, svm.file = "svm_classifier.svm")

#saveRDS(SVM, file = "SVM_model.rds")
SVM<- readRDS("C:\\Users\\adimudga\\Desktop\\Learning\\R\\SVM_model.rds")
summary(SVM)

svm.test<- predict(SVM, dt.test)
confusionMatrix(svm.test, dt.test$Label)


############## Fast Text #####################################################################
library(plyr)
tr_prop = 0.70
seed= 1234
View(df3)

ft.train = ddply(df3, .(Label), function(., seed) { set.seed(seed); .[sample(1:nrow(.), trunc(nrow(.) * tr_prop)), ] }, seed = 1234)
ft.test = ddply(df3, .(Label), function(., seed) { set.seed(seed); .[-sample(1:nrow(.), trunc(nrow(.) * tr_prop)), ] }, seed = 1234)

# check that proportions are equal across df3sets
ddply(df3, .(Label), function(.) nrow(.)/nrow(df3) )
ddply(ft.train, .(Label), function(.) nrow(.)/nrow(ft.train) )
ddply(ft.test, .(Label), function(.) nrow(.)/nrow(ft.test) )
c(nrow(ft.train), nrow(ft.test), nrow(df3)) # lengths of sets


train_labels <- paste0("__label__", ft.train$Label)
train_texts <- ft.train$Description
train_to_write <- paste(train_labels, train_texts)
train_tmp_file_txt <- tempfile()
writeLines(text = train_to_write, con = train_tmp_file_txt)
head(train_to_write)

test_labels <- paste0("__label__", ft.test$Label)
test_texts <- ft.test$Description
test_to_write <- paste(test_labels, test_texts)


library(fastTextR)
a<- normalize(train_to_write)
writeLines(a, con = "a.train")
b<- normalize(test_to_write)
writeLines(b, con = "b.test")
cntrl<- ft.control(word_vec_size = 10L, learning_rate = 0.01, max_len_ngram = 3L,
                   min_count = 1L, nbuckets = 10000000L, epoch = 100L, nthreads = 20L)

ft.start.time<- Sys.time()
fast_text <- fasttext(input = "a.train", method = "supervised", control = cntrl)
Sys.time()-ft.start.time

#fast_text
#save.fasttext(model = fast_text, "fast_text")
fast_text<- read.fasttext("C:\\Users\\adimudga\\Desktop\\Learning\\R\\fast_text.bin")

predictions <- predict(fast_text, newdata = test_to_write, unlock_empty_predictions = T)
print(head(predictions))

#Probabilities<- predict(fast_text, newdata = test_to_write, unlock_empty_predictions = T, prob = T)
#print(head(Probabilities))

New_data<- data.frame()
New_data<- NULL

New_data$Actual<- ft.test$Label
New_data$Predicted<- predictions

New_data<- as.data.frame(New_data)
print(head(New_data))
New_data$Predicted<- gsub("__label__",  "", New_data$Predicted )
print(head(New_data))
confusionMatrix(as.factor(New_data$Actual), as.factor(New_data$Predicted))


