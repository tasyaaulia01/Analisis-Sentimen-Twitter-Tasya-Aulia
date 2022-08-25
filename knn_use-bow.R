#https://www.youtube.com/watch?v=e7eWvbFSiPI
setwd("C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script")

#library(caret)
#library(RTextTools)
library(dplyr)
library(tm)

df = read.csv("10labelmanual.csv")
#View(df)
table(df$klasifikasi)
glimpse(df)
summary(df)
corpus <- VCorpus(VectorSource(df$x))
corpus
df$x<- as.factor(df$x)
d.tdm <- DocumentTermMatrix(corpus)
d.tdm
inspect(d.tdm)

set.seed(10)
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
#View(test_idx)

df.train <- df[train_idx,]
#View(df.train)
df.test <- df[test_idx,]
#View(df.test)
write.csv(df.test, file = "C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/testlabelknnbowmanual1.csv")
write.csv(df.train, file = "C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/trainknnbowmanual1.csv")
df.test <- read.csv("testlabelknnbowmanual1.csv")
df.train<- read.csv("trainknnbowmanual1.csv")
#View(df.test)
table(df.test$klasifikasi)
#table(df.test1$klasifikasi)
table(df.train$klasifikasi)

d.tdm.train <- d.tdm[train_idx,]
d.tdm.test <- d.tdm[test_idx,]

#d.tdm.train <- d.tdm[df.train$x,]
#d.tdm.test <- d.tdm[df.test$x,]

#d.class.train <- d.class[train_idx]
#d.class.test <- d.class[test_idx]
#d.class.test <- d.class[df.test1$klasifikasi]
#View(d.class.test)

inspect(d.tdm.train)
inspect(d.tdm.test)


#library(rpart)
#d.frame.train <- data.frame(as.matrix(d.tdm.train))
#d.frame.trainclass <- as.factor(d.class.train)

library(class)
set.seed(144)
knn_res <- knn(d.tdm.train, d.tdm.test, df.train$klasifikasi, 
               k=4, prob=TRUE)
knn.table <- table(knn_res, df.test$klasifikasi, dnn = list('predicted', 'actual'))
knn.table

hasil = confusionMatrix(table(knn_res, df.test$klasifikasi))
hasil
hasil$byClass



#print data test
class.df <- as.data.frame(knn_res)
View(class.df)
colnames(class.df) <- c("Predicted")
datatest_class.df <- cbind(df.test1, class.df)
View(datatest_class.df)
write.csv(datatest_class.df, file = "C:/Users/ibrahim/Downloads/latihan TA/knnusebowacc6464.csv")
