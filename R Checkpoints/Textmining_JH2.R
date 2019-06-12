## Assignment #2: Text Mining
#Version 1 contains the base data cleaning and model development code created using android dataset only.
#Version 2 begins incorporating apple reviews
library(qdap) #replace_contraction
library(data.table) #fread
library(tm) # text minint (Corpus)
library(dplyr) #select
library(tidytext) #stop_words
library(tibble)
library(tidyverse)
library(stringr)
library(SnowballC) #stemming
library(pacman)#Stemming/Lemmatize (textstem)
library(textstem) #stem/Lem word
library(textclean) #stem/lem string

#Load Files
setwd("C:/Users/Jessee/Desktop/York University/CSDA1040/Text Mining DF/Data")
review_android = read.csv("reviews_googleplay_android.csv",stringsAsFactors=FALSE)
review_apple = read.csv("reviews_itunes_apple.csv")
str(review_android)
str(review_apple)

#Remove unnecessary columns
review_android = select(review_android, -c(AppID,Language, Author, Title, TranslatedTitle, TranslatedReview, ReplyDate, DeveloperReply, User, Device, DeviceType, Tags, Notes, Link))
review_apple = select(review_apple, -c(AppID, Country, Author, Title, TranslatedTitle, TranslatedReview, ReplyDate, DeveloperReply, User, Tags, Notes, Link))

#Change data types
review_android <-
  review_android %>%
  mutate(Rating = as.factor(Rating),
         Date = as.Date(Date),
         Review = as.vector(Review))
str(review_android)

review_apple <-
  review_apple %>%
  mutate(Rating = as.factor(Rating),
         Date = as.Date(Date),
         Review = as.vector(Review),
         AppName = as.character(AppName))
str(review_apple)

## merge reviews
library(gtools)
review_apple$Type = "apple"
review_android$Type = "android"
review_all = smartbind(review_android, review_apple)
review_all = review_all %>%
  mutate(Date = as.Date(Date))
str(review_all)

#test
review_all$Review = textclean::replace_contraction(review_android$Review) 
review_all$Review <- stem_strings(review_all$Review)
review_all$Review = lemmatize_strings(review_all$Review)
data("stop_words")
review_all$Review = removeWords(review_android$Review, stopwords("English"))
head(review_all$Review)

#review_android$Review <- stem_words(review_android$Review)
#review_android$Review <- lemmatize_words(review_android$Review)
#review_android$Review <- lemmatize_strings(review_android$Review)

#Data Cleaning Phase 1:
review_all$Review <- replace_contraction(review_all$Review) #Convert contractions to base
review_all$Review <- tolower(review_all$Review)# convert to lowercase
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)#remove puncuation function
review_all$Review <- removePunctuation(review_all$Review)# remove punctuation
review_all$Review <- gsub("[[:punct:]]", "",review_all$Review)
review_all$Review <- removeNumbers(review_all$Review)# remove numbers
review_all$Review <- removeNumPunct(review_all$Review)# remove non latin characters except space

#split cleaned dataset back into android and apple datasets
review_apple = review_all[!grepl("android", review_all$Type),]
review_android = review_all[!grepl("apple", review_all$Type),]

rv_android1 = review_android[grepl("1", review_android$Rating),]
rv_android2 = review_android[grepl("2", review_android$Rating),]
rv_android3 = review_android[grepl("3", review_android$Rating),]
rv_android4 = review_android[grepl("4", review_android$Rating),]
rv_android5 = review_android[grepl("5", review_android$Rating),]

#=======================#
#***MODEL DEVELOPMENT***#
#=======================#

# Turn dataframe to tibble, number the reviews
tbl_reviews <- as_tibble(review_all) %>%
  mutate(review_no = row_number())
class(tbl_reviews)

# Print data
tbl_reviews

# one-token-per-row format
tidy_reviews <- tbl_reviews %>%
  unnest_tokens(word, Review)

#now that each text string has been split into a separate token, perform stemming/lemmatizing on a per word basis.
tidy_reviews$Review = stem_words(tidy_reviews$word)
tidy_reviews$Review = lemmatize_words(tidy_reviews$word)

# remove stop words
data("stop_words")
tidy_reviews <- tidy_reviews %>%
  anti_join(stop_words)

#count words
tidy_reviews %>%
  count(word, sort = TRUE)

# graph words
library(ggplot2)
tidy_reviews %>%
  count(word, sort = TRUE) %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
#do not remove app as stop word as it could have meaning in trigram/bigram analysis

tidy_reviews%>%
  count(word, sort = TRUE)

#========================#
#***SENTIMENT ANALYSIS***#
#========================#
library(tidytext)
sentiments
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#Lexicon Descriptions
#NRC categorizes words in a binary fashion ("yes"/"no") into categories of positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust. 
#Bing lexicon categorizes words in a binary fashion into positive and negative categories. 
#AFINN lexicon assigns words with a score that runs between -5 and 5, with negative scores indicating negative sentiment and positive scores indicating positive sentiment.

# comparing 3 sentiment dictionaries (plot all lexicon results)
afinn <- tidy_reviews %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = review_no %/% 80) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  tidy_reviews %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  tidy_reviews %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive",
                                         "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, index = review_no %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn,
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
#Can clearly see that Bing et al is more unforgiving in classifying sentiment.


###=Use Case=###
#Bing et al uses binary categories of positive and negative. These broader categories will make it enable us to;
#1. Clearly identify negative reviews or areas of improvement
#2. Identify any negative comments across all apps to see what we can do to be a trend setter
#3. Identify areas/functions already developed by competitors (catch up)
#4. Identify what differentiates our app for marketing/branding
#5. Both positive and Negative to determine what the users/markets ultimately want from an App

bing_word_counts <- tidy_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#vizualization: plot Bing sentitment
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#Vizualization: Word cloud based on Bing sentitment
install.packages("tm")
library(reshape2)
library(tm)
tidy_reviews %>%
  inner_join(get_sentiments("bing"))%>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),max.words = 100)

#==========================================================================================================================#
# Can see negative review are very "emotional". Need to get Noun verb pairings to extract more meaning.
# Use Udpipe (pre-trained) model for english Link:https://towardsdatascience.com/easy-text-analysis-on-abc-news-headlines-b434e6e3b5b8
#install.packages("udpipe")
library(udpipe)
#model <- udpipe_download_model(language = "english") #This will download most updated version so may be different from line 178
udmodel_english <- udpipe_load_model(file = 'english-ewt-ud-2.3-181115.udpipe')


s <- udpipe_annotate(udmodel_english, review_android$Review)
x <- data.frame(s)

#Universal Parts of Speech (UPOS)
install.packages(lattice)
library(lattice)
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

## NOUNS
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")


## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Freq")


## VERB
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "gold", 
         main = "Most occurring Verbs", xlab = "Freq")
stats <-stats[!(stats$key =="ря" ),]
stats <-stats[!(stats$keyword =="ря" ),]
#****Automated Keywords Extraction with RAKE****
#RAKE is one of the most popular (unsupervised) algorithms for extracting keywords in Information retrieval. 
#RAKE short for Rapid Automatic Keyword Extraction algorithm, is a domain independent keyword extraction algorithm 
#which tries to determine key phrases in a body of text by analyzing the frequency of word appearance and its co-occurrence 
#with other words in the text.

## Using RAKE
#stats <-stats[!(stats$=="ря" ),]

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats = stats[!grepl("ря", stats$keyword),]
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")


#==========================================================================================================================#
#N-Grams
##################BIGRAMS/TRIGRAMS######################

#Bigrams
#Note: need to remove "NA"

review_bigrams <- review_android %>%
  unnest_tokens(bigram, Review, token = "ngrams", n = 2)
bigrams_separated <- review_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united %>%
  count(bigram, sort = TRUE)

#Trigrams
review_trigrams <- review_android %>%
  unnest_tokens(trigram, Review, token = "ngrams", n = 3)

trigrams_separated <- review_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) 

trigram_counts <- trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE)

trigrams_united <- trigrams_filtered %>%
  unite(trigram, word1, word2, word3, sep = " ")

trigrams_united<-trigrams_united[!(trigrams_united$trigram=="NA NA NA" ),]
trigrams_united<-trigrams_united[!(trigrams_united$trigram=="ря ря ря" ),]

trigrams_united %>%
  count(trigram, sort = TRUE)

#==========================================================================================================================#

#Ref: https://medium.com/analytics-vidhya/customer-review-analytics-using-text-mining-cd1e17d6ee4e
#Trying to create cluster but error: cannot allocate vector of size 13.2 Gb
#Try using this to split out banks: 
#sales_s1 <-
#sales %>%
#  filter(Store == 1,
#         Dept == 1 | Dept == 2 | Dept == 3 | Dept == 4) %>%
#  dplyr::select(-IsHoliday)
#
#glimpse(sales_s1)

#Create the DTM & TDM from the corpus
corpus_review=Corpus(VectorSource(review_all$Review))
review_dtm <- DocumentTermMatrix(corpus_review)
review_tdm <- TermDocumentMatrix(corpus_review)

#Using the TDM to identify frequent terms
#The TDM can also used to identify frequent terms and in subsequent visualization related to the review text.
review_m <- as.matrix(review_tdm)# Convert TDM to matrix
review_term_freq <- rowSums(review_m)# Sum rows and frequency data frame
review_term_freq <- sort(review_term_freq, decreasing = T)# Sort term_frequency in descending order
review_term_freq[1:10]# View the top 10 most common words
barplot(review_term_freq[1:20], col = "steel blue", las = 2)# Plot a barchart of the 20 most common words

# Error in removeSparseTerms(corpus_review, sparse = 0.9) : 
#inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")) is not TRUE

tidy_reviewshc <- removeSparseTerms(review_dtm, sparse = 0.95)
hc <- hclust(d = dist(tidy_reviewshc, method = "euclidean"), method = "complete")
# Plot a dendrogram
plot(hc)

