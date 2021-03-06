#Load Libraries

library(tidyverse)
library(tm)
library(wordcloud)

#Load Cleaned Dataset
Podcast_Dataset <- read_csv("~/Documents/Podcast_Dataset.csv")
glimpse(Podcast_Dataset)

#Create Corpus
podcast_corpus <- Corpus(VectorSource(Podcast_Dataset))

#Create a Histogram for Episode Popularity

ggplot(Podcast_Dataset, aes(`Popularity Rating`)) +
  geom_histogram(binwidth = 1, fill = "#377EB8") 

#Create a Histogram for Release Date

ggplot(Podcast_Dataset, aes(`Release Date`)) +
  geom_histogram(binwidth = 1, fill = "#377EB8")

# Create a scatterplot for Release Date and Popularity Rating
ggplot(Podcast_Dataset, aes(x = `Release Date`, y = `Popularity Rating`)) +
  geom_point(position = position_jitter(0.5))

# Create a time-series plot for each Podcast's Popularity Rating and Running Time
ggplot(Podcast_Dataset, aes(x = `Popularity Rating`, y = `Running Time`, color = `Podcast`)) +
  geom_line(position = position_fill(0.5)) +facet_wrap(~Podcast, ncol=4)

#Perform Transformations

#Convert Text to Lowercase
podcast_corpus <- tm_map (podcast_corpus, tolower)
#Remove Stopwords
podcast_corpus <- tm_map (podcast_corpus, removeWords, stopwords("english"))
#Remove Numbers
podcast_corpus <- tm_map (podcast_corpus, removeNumbers)
#Remove Punctuation
podcast_corpus <- tm_map (podcast_corpus, removePunctuation)
#Stem Text
podcast_corpus <- tm_map (podcast_corpus, stemDocument)
#Strip Whitespace
podcast_corpus <- tm_map (podcast_corpus, stripWhitespace)
str(podcast_corpus)

#Machine Learning

#Create Dendrogram
distances <- dist(Podcast_Dataset[ ,6], method = "euclidean")
clusterPodcasts <- hclust(distances, method = "ward.D")
plot(clusterPodcasts)

#Analyze Clusters
clusterGroups <- cutree(clusterPodcasts, k = 4)
tapply(Podcast_Dataset$`Popularity Rating`, clusterGroups)
cluster1 <- subset(Podcast_Dataset, clusterGroups == 1)
cluster1[1:6]

#Create DTM
podcast_corpus <- tm_map(podcast_corpus, PlainTextDocument)
podcast_corpus <- Corpus(VectorSource(podcast_corpus))
podcast_DTM <- DocumentTermMatrix(podcast_corpus)

dim(podcast_DTM)

#Find Frequent Words 

#Define Frequency Variables
min_freq <- 20
term_freq <- colSums(as.matrix(podcast_DTM))
term_freq <- subset(term_freq, term_freq >= min_freq)
freq_words_df <- data.frame(term = names(term_freq), freq = term_freq)
findFreqTerms(podcast_DTM, lowfreq = min_freq)

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
