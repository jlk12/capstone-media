#Load Libraries

library(dplyr)
library(tidyr)
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(ape)
library(cluster)
library(readr)

#Load Cleaned Dataset
Podcast_Dataset <- read_csv("~/Documents/Podcast_Dataset.csv")

#Perform Transformations

#Convert Text to Lowercase
Podcast_Dataset <- tm_map (podcast_corpus, tolower)
#Remove Stopwords
Podcast_Dataset <- tm_map (podcast_corpus, removeWords, stopwords("english"))
#Remove Numbers
Podcast_Dataset <- tm_map (podcast_corpus, removeNumbers)
#Remove Punctuation
Podcast_Dataset <- tm_map (podcast_corpus, removePunctuation)
#Stem Text
Podcast_Dataset <- tm_map (podcast_corpus, stemDocument)
str(Podcast_Dataset)

#Create DTM
Podcast_Dataset <- DocumentTermMatrix (podcast_corpus)
dim(Podcast_Dataset)

#Find Frequent Words 

#Define Frequency Variables
min_freq <- 20
#Frequent words
findFreqTerms(Podcast_Dataset, lowfreq = min_freq)
# Plot frequent words
term_freq <- colSums(as.matrix(Podcast_Dataset))
term_freq <- subset(term_freq, term_freq >= min_freq)
freq_words_df <- data.frame(term = names(term_freq), freq = term_freq)
ggplot(data = freq_words_df, aes(x = reorder(term, freq), y = freq, colour = freq)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  ggtitle("Frequency of Most-Used Terms") +
  xlab("Terms") +
  ylab("Frequency") + 
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10))

# Wordcloud
wordcloud (podcast_corpus, scale = c(2, 0.5), colors = brewer.pal(8, "Paired"),  random.color = TRUE, random.order = FALSE, max.words = 250)

