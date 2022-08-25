#https://www.youtube.com/watch?v=e7eWvbFSiPI
setwd("C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script")

#library(caret)
#library(RTextTools)
#library(dplyr)
#library(tm)

df = read.csv("10labelmanual.csv")
#View(df)
glimpse(df)
summary(df)
corpus <- VCorpus(VectorSource(df$x))
corpus
df$x<- as.factor(df$x)
d.tdm <- DocumentTermMatrix(corpus,control = list(weighting = weightTfIdf))
d.tdm
inspect(d.tdm)

set.seed(111)
d.tdm <-  removeSparseTerms(d.tdm, 0.99)
dim(d.tdm)
d.tdm
#d.class <- as.factor(df$klasifikasi)
#levels(d.class)

train_freq = 0.90
train_idx <- sample.int(nrow(d.tdm), size = 
                          ceiling(nrow(d.tdm) * train_freq),
                        replace = FALSE)
train_idx
train_idx <- sort(train_idx)


test_idx <- setdiff(1:nrow(d.tdm), train_idx)
test_idx

df.train <- df[train_idx,]
#View(df.train)
df.test <- df[test_idx,]
#View(df.test)
write.csv(df.test, file = "C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/testlabelknntfidfmanual.csv")
write.csv(df.train, file = "C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/trainknntfidfmanual.csv")
df.test <- read.csv("testlabelknnbowmanual.csv")
df.train<- read.csv("trainknnbowmanual.csv")
View(df.test)
table(df.test$klasifikasi)
#table(df.test1$klasifikasi)
table(df.train$klasifikasi)

d.tdm.train <- d.tdm[train_idx,]
d.tdm.test <- d.tdm[test_idx,]

#d.tdm.train <- d.tdm[df.train$X,]
#d.tdm.test <- d.tdm[df.test$X,]

#d.class.train <- d.class[train_idx]
#d.class.test <- d.class[test_idx]
#d.class.test <- d.class[df.test1$klasifikasi]
#View(d.class.test)

d.tdm.train
d.tdm.test
inspect(d.tdm.test)


#library(rpart)
#d.frame.train <- data.frame(as.matrix(d.tdm.train))
#d.frame.trainclass <- as.factor(d.class.train)

#library(class)
#library(caret)
set.seed(156)
knn_res <- knn(d.tdm.train, d.tdm.test, df.train$klasifikasi, 
               k=4, prob=TRUE)
knn.table <- table(knn_res, df.test$klasifikasi, dnn = list('predicted', 'actual'))
knn.table

hasil = confusionMatrix(table(knn_res, df.test$klasifikasi))
hasil
#hasil$byClass


#print data test
class.df <- as.data.frame(knn_res)
colnames(class.df) <- c("Predicted")
datatest_class.df <- cbind(df.test1, class.df)
View(datatest_class.df)
write.csv(datatest_class.df, file = "C:/Users/ibrahim/Downloads/latihan TA/knnusetfidfacc6685.csv")
