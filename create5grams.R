#   Create 5-Grams

library(scales)
library(dplyr)
library(knitr)
library(tm)
library(quanteda)
library(lattice)

twitr5gram <- quanteda::tokenize(twitrClean, ngrams = 5, concatenator = "_")
trm5Twitr <- termFreq(PlainTextDocument(twitr5gram))
names <- rownames(trm5Twitr)
rownames(trm5Twitr) <- NULL
trm5Twitr <- cbind(names, trm5Twitr)
trm5Twitr <- as.data.frame(trm5Twitr)
colnames(trm5Twitr) <- c("Five_grams", "Frequency")
trm5Twitr$Frequency <- as.numeric(as.character(trm5Twitr$Frequency))
trm5Twitr$Five_grams <- sub('^.?.?\"', '', trm5Twitr$Five_grams)
trm5Twitr$Five_grams <- sub('\".$', '', trm5Twitr$Five_grams)
trm5Twitr$Five_grams <- gsub('_', ' ', trm5Twitr$Five_grams)
trm5Twitr <- filter(trm5Twitr, Five_grams != "null")
write.csv(trm5Twitr, file = "../data/sample/twitr5grams.csv")
freq5Twitr <- cbind(data.frame(head(arrange(trm5Twitr, desc(Frequency)), n=25)), Dataset = "Twitter")

news5gram <- quanteda::tokenize(newsClean, ngrams = 5, concatenator = "_")
trm5News <- termFreq(PlainTextDocument(news5gram))
names <- rownames(trm5News)
rownames(trm5News) <- NULL
trm5News <- cbind(names, trm5News)
trm5News <- as.data.frame(trm5News)
colnames(trm5News) <- c("Five_grams", "Frequency")
trm5News$Frequency <- as.numeric(as.character(trm5News$Frequency))
trm5News$Five_grams <- sub('^.?.?\"', '', trm5News$Five_grams)
trm5News$Five_grams <- sub('\".$', '', trm5News$Five_grams)
trm5News$Five_grams <- gsub('_', ' ', trm5News$Five_grams)
trm5News <- filter(trm5News, Five_grams != "null")
write.csv(trm5News, file = "../data/sample/news5grams.csv")
freq5News <- cbind(data.frame(head(arrange(trm5News, desc(Frequency)), n=25)), Dataset = "News")

blogs5gram <- quanteda::tokenize(blogsClean, ngrams = 5, concatenator = "_")
trm5Blogs <- termFreq(PlainTextDocument(blogs5gram))
names <- rownames(trm5Blogs)
rownames(trm5Blogs) <- NULL
trm5Blogs <- cbind(names, trm5Blogs)
trm5Blogs <- as.data.frame(trm5Blogs)
colnames(trm5Blogs) <- c("Five_grams", "Frequency")
trm5Blogs$Frequency <- as.numeric(as.character(trm5Blogs$Frequency))
trm5Blogs$Five_grams <- sub('^.?.?\"', '', trm5Blogs$Five_grams)
trm5Blogs$Five_grams <- sub('\".$', '', trm5Blogs$Five_grams)
trm5Blogs$Five_grams <- gsub('_', ' ', trm5Blogs$Five_grams)
trm5Blogs <- filter(trm5Blogs, Five_grams != "null")
write.csv(trm5Blogs, file = "../data/sample/blogs5grams.csv")
freq5Blogs <- cbind(data.frame(head(arrange(trm5Blogs, desc(Frequency)), n=25)), Dataset = "Blogs")

word5Freq <- rbind(freq5Twitr, freq5News, freq5Blogs)
colnames(word5Freq) <- c("Five_grams", "Frequency", "Dataset")

#   Merge 5-grams together 
freq5grams <- bind_rows(data.frame(trm5Twitr), data.frame(trm5News), data.frame(trm5Blogs))
#   Add occurences if same words together
freq5grams %>% group_by(Five_grams) %>% summarise(sum(Frequency)) -> freq5grams
#   Sort words on frequency of occurence
colnames(freq5grams) <- c("Five_grams", "Frequency")
freq5gramsSorted <- arrange(freq5grams, desc(Frequency))
#   In the analysis we established that 40.3% of the most frequent words to cover 90%
#   of any text. We will cut of the list at 50%, which should give us around 93.7%
#   language coverage
cutoffFrequency <- freq5gramsSorted[nrow(freq5gramsSorted)*.5,2]
#   Cut of the low frequency 50% tail 
freq5gramsList <- subset(freq5grams, Frequency >= as.integer(cutoffFrequency))
#   Save for later use
write.csv(freq5gramsList, file = "../data/model/freq5gramsList.csv")
#   Organize the remaining words as an associative network
