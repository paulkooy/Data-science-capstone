#   Create 4-Grams

library(scales)
library(dplyr)
library(knitr)
library(tm)
library(quanteda)
library(lattice)

twitr4gram <- quanteda::tokenize(twitrClean, ngrams = 4, concatenator = "_")
trm4Twitr <- termFreq(PlainTextDocument(twitr4gram))
names <- rownames(trm4Twitr)
rownames(trm4Twitr) <- NULL
trm4Twitr <- cbind(names, trm4Twitr)
trm4Twitr <- as.data.frame(trm4Twitr)
colnames(trm4Twitr) <- c("Four_grams", "Frequency")
trm4Twitr$Frequency <- as.numeric(as.character(trm4Twitr$Frequency))
trm4Twitr$Four_grams <- sub('^.?.?\"', '', trm4Twitr$Four_grams)
trm4Twitr$Four_grams <- sub('\".$', '', trm4Twitr$Four_grams)
trm4Twitr$Four_grams <- gsub('_', ' ', trm4Twitr$Four_grams)
trm4Twitr <- filter(trm4Twitr, Four_grams != "null")
write.csv(trm4Twitr, file = "../data/sample/twitr4grams.csv")
freq4Twitr <- cbind(data.frame(head(arrange(trm4Twitr, desc(Frequency)), n=25)), Dataset = "Twitter")

news4gram <- quanteda::tokenize(newsClean, ngrams = 4, concatenator = "_")
trm4News <- termFreq(PlainTextDocument(news4gram))
names <- rownames(trm4News)
rownames(trm4News) <- NULL
trm4News <- cbind(names, trm4News)
trm4News <- as.data.frame(trm4News)
colnames(trm4News) <- c("Four_grams", "Frequency")
trm4News$Frequency <- as.numeric(as.character(trm4News$Frequency))
trm4News$Four_grams <- sub('^.?.?\"', '', trm4News$Four_grams)
trm4News$Four_grams <- sub('\".$', '', trm4News$Four_grams)
trm4News$Four_grams <- gsub('_', ' ', trm4News$Four_grams)
trm4News <- filter(trm4News, Four_grams != "null")
write.csv(trm4News, file = "../data/sample/news4grams.csv")
freq4News <- cbind(data.frame(head(arrange(trm4News, desc(Frequency)), n=25)), Dataset = "News")

blogs4gram <- quanteda::tokenize(blogsClean, ngrams = 4, concatenator = "_")
trm4Blogs <- termFreq(PlainTextDocument(blogs4gram))
names <- rownames(trm4Blogs)
rownames(trm4Blogs) <- NULL
trm4Blogs <- cbind(names, trm4Blogs)
trm4Blogs <- as.data.frame(trm4Blogs)
colnames(trm4Blogs) <- c("Four_grams", "Frequency")
trm4Blogs$Frequency <- as.numeric(as.character(trm4Blogs$Frequency))
trm4Blogs$Four_grams <- sub('^.?.?\"', '', trm4Blogs$Four_grams)
trm4Blogs$Four_grams <- sub('\".$', '', trm4Blogs$Four_grams)
trm4Blogs$Four_grams <- gsub('_', ' ', trm4Blogs$Four_grams)
trm4Blogs <- filter(trm4Blogs, Four_grams != "null")
write.csv(trm4Blogs, file = "../data/sample/blogs4grams.csv")
freq4Blogs <- cbind(data.frame(head(arrange(trm4Blogs, desc(Frequency)), n=25)), Dataset = "Blogs")

word4Freq <- rbind(freq4Twitr, freq4News, freq4Blogs)
colnames(word4Freq) <- c("Four_grams", "Frequency", "Dataset")

#   Merge 4-grams together 
freq4grams <- bind_rows(data.frame(trm4Twitr), data.frame(trm4News), data.frame(trm4Blogs))
#   Add occurences if same words together
freq4grams %>% group_by(Four_grams) %>% summarise(sum(Frequency)) -> freq4grams
#   Sort words on frequency of occurence
colnames(freq4grams) <- c("Four_grams", "Frequency")
freq4gramsSorted <- arrange(freq4grams, desc(Frequency))
#   In the analysis we established that 40.3% of the most frequent words to cover 90%
#   of any text. We will cut of the list at 50%, which should give us around 93.7%
#   language coverage
cutoffFrequency <- freq4gramsSorted[nrow(freq4gramsSorted)*.5,2]
#   Cut of the low frequency 50% tail 
freq4gramsList <- subset(freq4grams, Frequency >= as.integer(cutoffFrequency))
#   Save for later use
write.csv(freq4gramsList, file = "../data/model/freq4gramsList.csv")
#   Organize the remaining words as an associative network
