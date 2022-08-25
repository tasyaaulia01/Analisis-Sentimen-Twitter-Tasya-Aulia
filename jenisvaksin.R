#Jenis Vaksin
setwd("C:/Users/ibrahim/Downloads/latihan TA/SKripsi/tasya/SkripsiBismillah")

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
araya <-searchTwitter('jenis vaksin', lang = 'id', n = 2500, since="2021-09-30", until = "2021-10-07")
twitaraya <- twListToDF(araya) #Mengubah list menjadi data frame

#Save Tweet as CSV
write.csv(twitaraya, file = 'C:/Users/ibrahim/Downloads/latihan TA/jenisvaksin.csv', row.names = F)

#Load the data
teks <- read.csv("jenisvaksin.csv", header = T)
View(teks)

tasya <- iconv(teks$text, to="UTF-8") #Pengenal Character UTF-8
library(tm)
tasya <- Corpus(VectorSource(tasya))


##cleaning data
aya <- tm_map(aya2, tolower)
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
aya <- tm_map(aya, removeURL)

removeAT <- function(x) gsub("@([A-Za-z0-9_]+)", "", x)
aya <- tm_map(aya,removeAT)

removehash <- function(x) gsub("#([A-Za-z0-9_]+", "", x)
aya <- tm_map(aya,removehash)

inspect(aya[1:5])
inspect(aya[196:200])

#Remove Emoji
removeemoji <- function(x) gsub("<([A-Z+a-z0-9_]+)>", "", x)
aya <- tm_map(aya,removeemoji)

removeNL<- function(y) gsub("\n", " ", y)
aya <- tm_map(aya, removeNL)


removeRT <- function(y) gsub("rt", "", y)
aya <- tm_map(aya, removeRT) 

aya <- tm_map(aya, removePunctuation)
aya <- tm_map(aya, removeNumbers)

##Normalisasi
library(textclean)
slangw <- read.csv("slang.csv")
View(slangw)

aya <-replace_internet_slang(aya, slang = paste0("\\b", slangw$sebelum, "\\b"),
                              replacement = slangw$setelah, ignore.case = TRUE)
head(aya)

#Stopword
stopwordID <- "C:/Users/ibrahim/Downloads/latihan TA/Skripsi/tasya/SkripsiBismillah/stopword_id.txt"
cStopwordID <- readLines(stopwordID)

aya <- tm_map(aya, removeWords, cStopwordID)
aya <- tm_map(aya, removeWords, c('jeni', 'samp', 'amp', 'jenisjenis', 'vaksin', 'kanisu', 'teruk',
                                  'elak...', 'jerung', 'lepa', 'iaitu,', 'kalitegar', 'kesan',
                                  'mbeng', 'the', 'molek'))
aya <- tm_map(aya, stripWhitespace)
aya <- tm_map(aya, stemDocument)

aya <- unlist(aya)
str(aya)

write.csv(aya, file = 'C:/Users/ibrahim/Downloads/latihan TA/Skripsi/tasya/SkripsiBismillah/cleanjenisvaksin.csv', row.names = F)
View(aya)

###Label dat

kalimat2 <- read.csv("cleanjenisvaksin.csv", header = T)

kata.positif <- scan("positifword.txt",what="character",comment.char=";")
kata.negatif <- scan("negatifword.txt",what="character",comment.char=";")

score.sentiment = function(kalimat2, kata.positif, kata.negatif, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(kalimat2, function(kalimat2, kata.positif, kata.negatif) {
    kalimat2 = gsub('[[:punct:]]', '', kalimat2)
    kalimat2 = gsub('[[:cntrl:]]', '', kalimat2)
    kalimat2 = gsub('\\d+', '', kalimat2)
    kalimat2 = tolower(kalimat2)
    
    list.kata = str_split(kalimat2, '\\s+')
    kata2 = unlist(list.kata)
    positif.matches = match(kata2, kata.positif)
    negatif.matches = match(kata2, kata.negatif)
    positif.matches = !is.na(positif.matches)
    negatif.matches = !is.na(negatif.matches)
    score = sum(positif.matches) - (sum(negatif.matches))
    return(score)
  }, kata.positif, kata.negatif, .progress=.progress )
  scores.df = data.frame(score=scores, text=kalimat2)
  return(scores.df)
}

hasil = score.sentiment(kalimat2$x, kata.positif, kata.negatif)
View(hasil)

hasil$klasifikasi<- ifelse(hasil$score<0, "Negatif",ifelse(hasil$score==0,"Netral","Positif"))
hasil$klasifikasi
View(hasil)
str(hasil)

write.csv(hasil, file = 'C:/Users/ibrahim/Downloads/latihan TA/labelcleanjenisvaksin.csv', row.names = F)


###
hasil = read.csv("cleanjenisvaksin.csv")
aya <- iconv(hasil$x, to="UTF-8") #Pengenal Character UTF-8
View(aya)
head(aya)
aya <- Corpus(VectorSource(aya))
inspect(aya[1:2])

tdm <- TermDocumentMatrix(aya)
tdm 
m <- as.matrix(tdm)
m[1:30, 1:20]

#Bar plot
bar <- rowSums(m)
bar
bar <- subset(bar, bar>= 19)
bar
barplot(bar, las = 2, col = rainbow(50), cex.names = 1)

v <- sort(rowSums(m), decreasing= T)
View(v)
tasya <- data.frame(names(v), v)
colnames(tasya) <- c('word', 'freq')
View(tasya)
write.csv(tasya, file = 'C:/Users/ibrahim/Downloads/latihan TA/Skripsi/tasya/SkripsiBismillah/hasiljenisvaksin.csv', row.names = F)



#WordCloud
set.seed(140)
library(wordcloud)
v <- sort(rowSums(m), decreasing= T)
View(v)
wordcloud(words = names(v), freq = v, max.words = 100, min.freq = 15,
          colors = brewer.pal(8, 'Dark2'), scale = c(3, 0.3), rot.per = 0.25)

#wordCloud2
library(wordcloud2)
tasya <- data.frame(names(v), v)
colnames(tasya) <- c('word', 'freq')
head(tasya)
wordcloud2(tasya)

wordcloud2(tasya, size = 0.8, shape = 'star', rotateRatio = 0.5, minSize = 1)
