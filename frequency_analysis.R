library(tm)
library(Rcpp)
library(wordcloud)
library(topicmodels)
library(lsa)
library(ggplot2)

wd <- setwd("C:/Users/ZOSIA/Desktop/Social Network and Sentiment Analysis/projekt_zaliczeniowy/text/")

#Pre-processing
docs <- Corpus(DirSource(wd))
docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, tolower) 
docs <- tm_map(docs, removeWords, stopwords("English"))

StW <- read.table("C:/Users/ZOSIA/Desktop/Social Network and Sentiment Analysis/projekt_zaliczeniowy/StopWords.txt")
StWW <- as.character(StW$V1)
docs <- tm_map(docs,removeWords, StWW)

for (j in seq(docs)) {
  docs[[j]] <- gsub("[^A-Za-z ]", " ", docs[[j]])
}

for (j in seq(docs)) {
  docs[[j]]<-stemDocument(docs[[j]], language = "english")
} 

docs <- tm_map(docs,removeWords, StWW)
dtm <- DocumentTermMatrix(docs)
inspect(dtm)

filenames <- list.files(getwd(), pattern ="*.txt")
filenames <- c(filenames)
rownames(dtm) <- filenames
freq <- colSums(as.matrix(dtm))
length(freq)

ord <- order(freq, decreasing = TRUE)
freq[ord]
write.csv(freq[ord], "C:/Users/ZOSIA/Desktop/Social Network and Sentiment Analysis/projekt_zaliczeniowy/docs/word_freq.csv")

tdm <- TermDocumentMatrix(docs)

dtmr <- DocumentTermMatrix(docs, control=list(wordLengths=c(3, 20), bounds = list(global = c(2, Inf))))

filenames <- list.files(getwd(), pattern="*.txt")
filenames <- c(filenames)
rownames(dtmr) <- filenames

dtmr1 = removeSparseTerms(dtmr, 0.70)

nn <- rowSums(as.matrix(dtm))
dtm_Norm <- dtm/nn

m0 <- as.matrix(dtm)
write.csv(m0, file="C:/Users/ZOSIA/Desktop/Social Network and Sentiment Analysis/projekt_zaliczeniowy/docs/DocumentTermMatrix.csv")
m1 <- as.matrix(dtm_Norm)
write.csv(m0, file="C:/Users/ZOSIA/Desktop/Social Network and Sentiment Analysis/projekt_zaliczeniowy/docs/DocumentTermMatrixNorm.csv")
m2 <- as.matrix(dtmr)
write.csv(m0, file="C:/Users/ZOSIA/Desktop/Social Network and Sentiment Analysis/projekt_zaliczeniowy/docs/DocumentTermMatrix_1.csv")
m3 <- as.matrix(dtmr1)
write.csv(m0, file="C:/Users/ZOSIA/Desktop/Social Network and Sentiment Analysis/projekt_zaliczeniowy/docs/SparseDocumentTermMatrix.csv")

#General corpus analysis
freqr <- colSums(as.matrix(dtm))
freq <- sort(freqr, decreasing = TRUE)

#findAssocs(dtmr, "tramadol", 0.6)
#findAssocs(dtmr, "acetaminophen", 0.6)
#findAssocs(dtmr, "oxycodon", 0.6)

#Zipf's law
freqr <- colSums(as.matrix(dtmr))
freq <- sort(freqr, decreasing = TRUE)
mk <- min(head(freq, 30))
wf = data.frame(word = names(freq), freq=freq)

p <- ggplot(subset(wf, freq>mk), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#Topic modeling
burnin <- 4000
iter <- 2000
thin <- 500
seed <- list(2003, 5, 63, 100001, 765)
nstart <- 5 
best <- TRUE

k<-2

ldaOut <- LDA(dtm, k, method = "Gibbs", control=list(nstart=nstart, seed=seed, best = best, burnin = burnin, iter = iter, thin = thin))
str(ldaOut)
ldaOut.terms <- as.matrix(terms(ldaOut,6))
ldaOut.terms

topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities, file = paste("LDAGibbs", k, "TopicProbabilities.csv"))

ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k, "DocsToTopics.csv"))

#MDS
dtmr = removeSparseTerms(dtm, 0.50)
dist.mat <- dist(dtmr)
dist.mat
fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])

p<-ggplot(points, aes(x = x, y = y))
p<-p + geom_point(data = points, aes(x = x, y = y))
p<-p + geom_text(data = points, aes(x = x, y = y+0.2, label=rownames(dtmr)))
p

d <- dist(fit$points, method="euclidian") 

kfit11 <- kmeans(d, 2) 
kfit11 
library(cluster)
clusplot(as.matrix(d), kfit11$cluster, color=T, shade=T, labels=2, lines=0)
