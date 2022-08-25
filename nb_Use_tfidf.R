setwd("C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script")

library(caret)
#library(RTextTools)
library(dplyr)
library(tm)

df <- read.csv("C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/10labelmanual.csv", stringsAsFactors = FALSE)
glimpse(df)
summary(df)
#View(df)
#check table
table(df$klasifikasi)

#check classes distribution
prop.table(table(df$klasifikasi))

set.seed(1)
df <- df[sample(nrow(df)), ]
#df <- df[sample(nrow(df)), ]
#glimpse(df)
df$klasifikasi<- as.factor(df$klasifikasi)
corpus <- VCorpus(VectorSource(df$x))
#corpus
#inspect(corpus[1:3])

## TRANSFORM DATA INTO DOCUMENT TERM MATRIX
dtm <- DocumentTermMatrix(corpus,control = list(weighting = weightTfIdf))
#dtm
inspect(dtm)

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
#write.csv(m, file = "C:/Users/ibrahim/Downloads/latihan TA/vaksinsinovaclowerslangcleandupstemlabeldtmnbtfidf.csv")
#View(m)

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

## SPLITTING DATA
library(e1071)  #package for classification
set.seed(150)
n <- nrow(df)
df.train <- df[1:round(.90 * n),]  #percentage split for 90:10, do to another composition 80:20, 70:30, 60:40, and 50:50
df.test  <- df[(round(.90 * n)+1):n,]
write.csv(df.test, file = "C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/testlabelnbtfidfmanual.csv")
write.csv(df.train, file = "C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/trainnbtfidfmanual.csv")
df.test <- read.csv("80testmanual.csv")
df.train <- read.csv("80trainmanual.csv")
#View(df.test)
table(df.test$klasifikasi)
table(df.train$klasifikasi)

nn <- length(corpus)
corpus.train <- corpus[1:round(.90 * nn)]
corpus.test  <- corpus[(round(.90 * nn)+1):nn]

nnn <- nrow(dtm)
dtm.train <- dtm[1:round(.90 * nnn),]
dtm.test  <- dtm[(round(.90 * nnn)+1):nnn,]
dim(dtm.train)

fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))
fivefreq

# Use only 5 most frequent words (fivefreq) to build the DTM

dtm.train.nb <- DocumentTermMatrix(corpus.train, control=list(weighting = weightTfIdf))
#dim(dtm.train.nb)
dtm.test.nb <- DocumentTermMatrix(corpus.test, control=list(weighting = weightTfIdf))
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

# Train the classifier
set.seed(119)
system.time( classifier <- naiveBayes(trainNB, df.train$klasifikasi, laplace = 1) )

# Use the NB classifier we built to make predictions on the test set.
system.time( pred <- predict(classifier, newdata=testNB) )


# Create a truth table by tabulating the predicted class labels with the actual class labels 
#library(gmodels)
#table("Predictions"= pred,  "Actual" = df.test$klasifikasi )

#CrossTable(pred, df.test$klasifikasi,prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))

# Prepare the confusion matrix
conf.mat <- confusionMatrix(pred, df.test$klasifikasi)
conf.mat

#conf.mat$byClass

conf.mat$overall
conf.mat$overall['Accuracy']

#see model probabilities
print(classifier)

#print data test
class.df <- as.data.frame(pred)
colnames(class.df) <- c("Predicted")
datatest_class.df <- cbind(df.test, class.df)
View(datatest_class.df)
write.csv(datatest_class.df, file = "C:/Users/ibrahim/Downloads/latihan TA/nbusetfidfacc7486.csv")
