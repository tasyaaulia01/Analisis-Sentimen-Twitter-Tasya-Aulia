setwd("C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script")

library(caret)
library(RTextTools)
library(dplyr)
library(tm)

##===== CONSTRUCT DATA =====##
df <- read.csv("C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/10labelmanual.csv", stringsAsFactors = FALSE)
library(dplyr)
glimpse(df) #informasi terkit data
summary(df)
#View(df)
#check table
table(df$klasifikasi) #jumlah klasifikasi negatif, netral positif

#check classes distribution
prop.table(table(df$klasifikasi))

set.seed(12)
df <- df[sample(nrow(df)), ]
#df <- df[sample(nrow(df)), ]
#View(df)
df$klasifikasi<- as.factor(df$klasifikasi) #mengubah type data
#library(tm)
corpus <- VCorpus(VectorSource(df$x))
corpus
#inspect(corpus[1:5])

## TRANSFORM DATA INTO DOCUMENT TERM MATRIX
dtm <- DocumentTermMatrix(corpus)
#dtm
#inspect(dtm)

findFreqTerms(dtm, 5) #mencari kata yang mempunyai frekuensi muncul lebih dari 5 kali

#tdm <- TermDocumentMatrix(corpus)
#tdm
#inspect(tdm)

#remove the infrequently used words
#start by removing sparse terms:
dtms <- removeSparseTerms(dtm, 0.99) #this makes a matrix that is 99% empty space, maximum
inspect(dtms)

#Explore your data
freq <- colSums(as.matrix(dtms))
length(freq)
ord <- order(freq)
m <- as.matrix(dtms) #if you prefer to export the matrix to Excel
dim(m)
#write.csv(m, file = "C:/Users/ibrahim/Downloads/latihan TA/Skripsi/vaksinsinovaclowerslangcleandupstemlabeldtmnbbow.csv")

#check out the frequency of frequencies
head(table(freq), 20) #the 20 indicates that we only want the first 20 frequencies. feel free to change that number
tail(table(freq), 20)
freq <- colSums(as.matrix(dtms))
freq

#an alternate view of term frequency:
findFreqTerms(dtm, lowfreq = 50)

#another way to do this
wf <- data.frame(word=names(freq), freq = freq)
head(wf)

#plot word frequencies
#library(ggplot2)
#p <- ggplot(subset(wf, freq>10), aes(x = reorder(word, -freq), y = freq)) +geom_bar(stat = "identity") +theme(axis.text.x = element_text(angle = 45, hjust = 1))
#p

## SPLITTING DATA
library(e1071)  #package for classification
set.seed(111)
n <- nrow(df)
df.train <- df[1:round(.90 * n),]  #percentage split for 75:25, do to another composition 80:20, 70:30, 60:40, and 50:50
df.test  <- df[(round(.90 * n)+1):n,]
write.csv(df.test, file = "C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/80testmanual.csv")
write.csv(df.train, file = "C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/80trainmanual.csv")
df.test <- read.csv("80testmanual.csv")
df.train <- read.csv("80trainmanual.csv")
#View(df.test)
table(df.test$klasifikasi)
table(df.train$klasifikasi)

nn <- length(corpus)
#nn
#View(nn)
corpus.train <- corpus[1:round(.90 * nn)]
corpus.test  <- corpus[(round(.90 * nn)+1):nn]

nnn <- nrow(dtm)
#nnn
dtm.train <- dtm[1:round(.90 * nnn),]
dtm.test  <- dtm[(round(.90 * nnn)+1):nnn,]
#dim(dtm.train)

fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))
fivefreq

# Use only 5 most frequent words (fivefreq) to build the DTM

dtm.train.nb <- DocumentTermMatrix(corpus.train, control=list(dictionary = fivefreq))
#dim(dtm.train.nb)
dtm.test.nb <- DocumentTermMatrix(corpus.test, control=list(dictionary = fivefreq))
#dim(dtm.test.nb)

# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("Yes", "No"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
trainNB <- apply(dtm.train, 2, convert_count)
testNB <- apply(dtm.test, 2, convert_count)
#View(trainNB)

# Train the classifier
set.seed(111)
classifier <- naiveBayes(trainNB, df.train$klasifikasi, laplace = 1)

# Use the NB classifier we built to make predictions on the test set.
pred <- predict(classifier, newdata=testNB)

# Create a truth table by tabulating the predicted class labels with the actual class labels 
#library(gmodels)
#table("Predictions"= pred,  "Actual" = df.test$klasifikasi )

#CrossTable(pred, df.test$klasifikasi, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))

# Prepare the confusion matrix
#library(caret)
conf.mat <- confusionMatrix(pred, df.test$klasifikasi)
conf.mat

#conf.mat$byClass

#print data test
class.df <- as.data.frame(pred)
View(class.df)
colnames(class.df) <- c("Predicted")
datatest_class.df <- cbind(df.test, class.df)
View(datatest_class.df)
write.csv(datatest_class.df, file = "C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/visual.csv")
