library(tidyverse)
library(tidytext)
library(glue)
library(stringr)


files <- list.files("C:/Users/ZOSIA/Desktop/Social Network and Sentiment Analysis/projekt_zaliczeniowy/text/")


GetSentiment <- function(file){
  fileName <- glue("C:/Users/ZOSIA/Desktop/Social Network and Sentiment Analysis/projekt_zaliczeniowy/text/", file, sep=" ")
  fileName <- trimws(fileName)
  
  fileText <- glue(read_file(fileName))
  fileText <- gsub("[^A-Za-z ]", "", fileText)
  fileText <- gsub("pain", "", fileText)
  fileText <- tolower(fileText)
  
  tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
  
  sentiment <- tokens %>%
    inner_join(get_sentiments("bing")) %>%
    count(sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative) %>%
    mutate(file = file) %>%
    mutate(year =  as.numeric(str_match(file, "\\d{4}"))) %>%
    mutate(medicine = substr(file, 1, nchar(file)-9))
  
  return(sentiment)
  
}

sentiments <- data_frame()

for(i in files){
  sentiments <- rbind(sentiments, GetSentiment(i))
}

summary(sentiments)


ggplot(sentiments, aes(x = as.numeric(year), y = sentiment)) +
  geom_point(aes(color = medicine)) +
  geom_smooth(method = "auto")


ggplot(sentiments, aes(x = medicine, y = sentiment, color = medicine)) + 
  geom_boxplot() 

