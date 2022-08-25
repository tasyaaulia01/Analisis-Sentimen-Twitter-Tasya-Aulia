setwd("C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script")

library(twitteR)
library(rtweet)
library(wordcloud)
library(tm)


#API 25September 2021
API_Key = "cMsLNig9PttvjNlS9u96vke43"
API_Secret = "gNa9b4UAeV8TJcaxG2hJDbNA4jZXfmbsN1VrNf6BOirvJGwQ7e"
access_token = "1108376517514395652-t4PEFf0cD3okfg9DlbL1CW8wOg8OXr"
access_secret = "J3hn54alqYrooa6naOgYAvoSZDUMfxITyxxJUwQa8tCNC"
setup_twitter_oauth(API_Key, API_Secret, access_token, access_secret)

#Generate Tweet
araya <-searchTwitter('vaksin sinovac', lang = 'id', n = 2500, since="2021-09-30", until = "2021-10-07")
twitaraya <- twListToDF(araya) #Mengubah list menjadi data frame

#Save Tweet as CSV
write.csv(twitaraya, file = 'C:/Users/ibrahim/Downloads/latihan TA/Skripsi/tasya/SkripsiBismillah/vaksinsinovac.csv', 
          row.names = F)


#Load the data
teks <- read.csv("1vaksinsinovac.csv", header = T)
#View(teks)
tasya <- iconv(teks$text, to="UTF-8") #Pengenal Character UTF-8
#View(tasya)
#write.csv(tasya, file = 'C:/Users/ibrahim/Downloads/latihan TA/Skripsi/tasya/SkripsiBismillah/2vaksinsinovacteks.csv', row.names = F)

#Case Folding
library(tm)
tasya2 <- Corpus(VectorSource(tasya))
tasya2 <- tm_map(tasya2, tolower)
inspect(tasya2[10:20])
#View(tasya2)
#write.csv(tasya2, file = 'C:/Users/ibrahim/Downloads/latihan TA/Skripsi/tasya/SkripsiBismillah/3vaksinsinovactekslow.csv', row.names = F)

##cleaning data
#Remove URL
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
tasya <- tm_map(tasya2, removeURL)

inspect(tasya[10:15])

#Remove Mention
removeAT <- function(x) gsub("@([A-Za-z0-9_]+)", "", x)
tasya <- tm_map(tasya,removeAT)

inspect(tasya[30:35])

#Remove Emoji
removeemoji <- function(x) gsub("<([A-Z+a-z0-9_]+)>", "", x)
tasya <- tm_map(tasya,removeemoji)

inspect(tasya[59:65])

#Remove Hashtag
removehash <- function(x) gsub("#([A-Za-z0-9_]+)", "", x)
tasya <- tm_map(tasya,removehash)

#Hapus Kata yang tidak diperlukan
removeNL<- function(y) gsub("\n", " ", y)
tasya <- tm_map(tasya, removeNL)

removeRT <- function(y) gsub("rt", "", y)
tasya <- tm_map(tasya, removeRT) 


#Hapus Angka & Tanda Baca
tasya <- tm_map(tasya, removePunctuation)
tasya <- tm_map(tasya, removeNumbers)


inspect(tasya[1:5])
inspect(tasya[196:200])
inspect(tasya[20:25])


tasya <- tm_map(tasya, removeWords, c('jeni','²','gtgtgt','ufd','ufduff', 'samp', 'amp', 
                                      'jenisjenis', 'kanisu', 'teruk','f',
                                    'elak...', 'jerung','ff','kamufdkamuff','ppp',
                                    'kamukamukamukamu', 'ff', 'wkwkwk', 'gitugitugitu'))

tasya <- tm_map(tasya, stripWhitespace)
tasya3 <- unlist(tasya)
View(tasya3)

write.csv(tasya3, file = 'C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/4vaksinsinovaclowerclean.csv', row.names = F)

##Normalisasi
library(textclean)
slangw <- read.csv("slang.csv")
#View(slangw)
tasya4 <-replace_internet_slang(tasya3, slang = paste0("\\b", slangw$sebelum, "\\b"),
                              replacement = slangw$setelah, ignore.case = TRUE)
View(tasya4)
#write.csv(tasya4, file = 'C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/5vaksinsinovaclowercleanslang.csv', row.names = F)


#Remove Duplicate Rows
df <- read.csv("5vaksinsinovaclowercleanslang.csv")
#View(df)
duplicated(df)
df[duplicated(df), ]
tasya5 <- unlist(df[!duplicated(df), ])
View(tasya5)
#write.csv(tasya5, file = 'C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/6vaksinsinovaclowercleanslangdup.csv', row.names = F)


###Label data (https://medium.com/@17611087/cara-membuat-sentiment-analysis-menggunakan-r-97ca5cd4b681)
data <- read.csv("6vaksinsinovaclowercleanslangdup.csv", header = T)
#View(data)
kata.positif <- scan("positifword.txt",what="character",comment.char=";")
kata.negatif <- scan("negatifword.txt",what="character",comment.char=";")

score.sentiment = function(data, kata.positif, kata.negatif, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(data, function(data, kata.positif, kata.negatif) {
    
    list.kata = str_split(data, '\\s+')
    kata2 = unlist(list.kata)
    positif.matches = match(kata2, kata.positif)
    negatif.matches = match(kata2, kata.negatif)
    positif.matches = !is.na(positif.matches)
    negatif.matches = !is.na(negatif.matches)
    score = sum(positif.matches) - (sum(negatif.matches))
    return(score)
  }, kata.positif, kata.negatif, .progress=.progress )
  scores.df = data.frame(score=scores, text=data)
  return(scores.df)
}

tasya6 = score.sentiment(data$x, kata.positif, kata.negatif)
tasya6$klasifikasi<- ifelse(tasya6$score>0,"Positif",ifelse(tasya6$score<0,"Negatif",
                                                        "Netral"))
View(tasya6)
table(tasya6$klasifikasi)
#str(tasya6)

write.csv(tasya6, file = 'C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/7vaksinsinovaclowercleanslangdupbel.csv', row.names = F)

#Stopword
tasya7 <- read.csv('labelmanual.csv')
#View(tasya7)
tasya7 <- iconv(tasya7$text)
tasya7 <- Corpus(VectorSource(tasya7))
inspect(tasya7[20:25])

stopwordID <- "C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/stopword_id.txt"
cStopwordID <- readLines(stopwordID)

tasya7 <- tm_map(tasya7, removeWords, cStopwordID)
inspect(tasya7[10:15])
inspect(tasya7[10:25])
tasya7 <- tm_map(tasya7, stripWhitespace)
tasya7 <- unlist(tasya7)
View(tasya7)

write.csv(tasya7, file = 'C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/8labelmanual.csv', row.names = F)
#View(tasya7)

#Stemming
tasya8 = read.csv("8labelmanual.csv")
#View(tasya8)
#str(tasya8)
stem <- as.character(tasya8$x)
stem[20:25]

library(katadasaR)
stemming <- function(x){
  paste(lapply(x,katadasar),collapse = " ")}

library(tokenizers)
tweets <- lapply(tokenize_words(stem[]), stemming)

tweets[1:5]
tweets[20:25]
tweets[196:200]
tasya8<- unlist(tweets)
#View(tasya8)

write.csv(tasya8, file = 'C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/9labelmanual.csv', row.names = F)

##
data =  read.csv("labelmanual.csv")
#View(data)
data2 = read.csv("9labelmanual.csv", header=TRUE)
#View(data2)
str(data2)
klasifikasi <- data$klasifikasi
datatest_class.df <- cbind(klasifikasi, data2)

View(datatest_class.df)
write.csv(datatest_class.df, file = 'C:/Users/ibrahim/Desktop/1 Sidang Skripsi/Script/10labelmanual.csv', row.names = F)


###
hasil = read.csv("visual.csv")
#View(hasil)
tasya <- iconv(hasil$x, to="UTF-8") #Pengenal Character UTF-8
#View(tasya)
head(tasya)
library(tm)
tasya <- Corpus(VectorSource(tasya))
inspect(tasya[1:5])

#BOW
tdm <- TermDocumentMatrix(tasya)
tdm 
m <- as.matrix(tdm)
#m[1:30, 1:20]
v <- sort(rowSums(m), decreasing= T)
#d_bow <- data.frame(word = names(v),freq=v)
#View(d_bow)

#TFIDF
tdm_tfidf <- TermDocumentMatrix(tasya,
                                control = list(weighting = weightTfIdf))
m_tfidf <- as.matrix(tdm_tfidf)
m_tfidf
#View(m_tfidf)
v_tfidf <- sort(rowSums(m_tfidf),decreasing=TRUE)
View(v_tfidf)
#d_tfidf <- data.frame(word = names(v_tfidf),freq=v_tfidf)
#View(d_tfidf)



######WordCloud

####WordCloud negatif
hasil = read.csv("visual.csv")
data.neg <- hasil[hasil$Predicted=="Negatif",]
aya <- iconv(data.neg$x, to="UTF-8") #Pengenal Character UTF-8
View(aya)
head(aya)
library(tm)
aya <- Corpus(VectorSource(aya))
inspect(aya[1:2])

tdm <- TermDocumentMatrix(aya)
tdm 
m <- as.matrix(tdm)
m[1:30, 1:20]



#Bar plot
bar <- rowSums(m)
bar
bar <- subset(bar, bar>= 25)
bar
barplot(bar, las = 2, col = rainbow(50), cex.names = 1)

#WordCloud 
set.seed(140)
library(wordcloud)
v <- sort(rowSums(m), decreasing= T)
wordcloud(words = names(v), freq = v, max.words = 200, min.freq = 1,
          colors = brewer.pal(8, 'Dark2'), scale = c(3, 0.3), rot.per = 0.25)

#wordCloud2
library(wordcloud2)
tasya <- data.frame(names(v), v)
colnames(tasya) <- c('word', 'freq')
head(tasya)
wordcloud2(tasya)

wordcloud2(tasya, size = 2, shape = 'box', rotateRatio = 1, minSize = 3)

####WordCloud Positif
hasil = read.csv("visual.csv")
data.pos<- hasil[hasil$Predicted=="Positif",]
aya <- iconv(data.pos$x, to="UTF-8") #Pengenal Character UTF-8
#View(aya)
head(aya)
library(tm)
aya <- Corpus(VectorSource(aya))
inspect(aya[1:2])

tdm <- TermDocumentMatrix(aya)
tdm 
m <- as.matrix(tdm)
m[1:30, 1:20]



#Bar plot
bar <- rowSums(m)
bar
bar <- subset(bar, bar>= 25)
bar
barplot(bar, las = 2, col = rainbow(50), cex.names = 1)

#WordCloud 
set.seed(140)
library(wordcloud)
v <- sort(rowSums(m), decreasing= T)
wordcloud(words = names(v), freq = v, max.words = 100, min.freq = 5,
          colors = brewer.pal(8, 'Dark2'), scale = c(3, 0.3), rot.per = 0.25)

#wordCloud2
library(wordcloud2)
tasya <- data.frame(names(v), v)
colnames(tasya) <- c('word', 'freq')
head(tasya)
wordcloud2(tasya)

wordcloud2(tasya, size = 2, shape = 'box', rotateRatio = 0.5, minSize = 2)


####WordCloud Netral
hasil = read.csv("10Vaksinfin.csv")
data.net <- hasil[hasil$Predicted=="Netral",]
aya <- iconv(data.net$x, to="UTF-8") #Pengenal Character UTF-8
#View(aya)
head(aya)
library(tm)
aya <- Corpus(VectorSource(aya))
inspect(aya[1:2])

tdm <- TermDocumentMatrix(aya)
tdm 
m <- as.matrix(tdm)
m[1:30, 1:20]



#Bar plot
bar <- rowSums(m)
bar
bar <- subset(bar, bar>= 25)
bar
barplot(bar, las = 2, col = rainbow(50), cex.names = 1)

#WordCloud 
set.seed(140)
library(wordcloud)
v <- sort(rowSums(m), decreasing= T)
wordcloud(words = names(v), freq = v, max.words = 100, min.freq = 5,
          colors = brewer.pal(8, 'Dark2'), scale = c(3, 0.3), rot.per = 0.25)

#wordCloud2
library(wordcloud2)
tasya <- data.frame(names(v), v)
colnames(tasya) <- c('word', 'freq')
head(tasya)
wordcloud2(tasya)

wordcloud2(tasya, size = 2, shape = 'box', rotateRatio = 0.5, minSize = 2)
wordcloud2(tasya, size = 5, shape = 'box', rotateRatio = 0.5, minSize = 2)
