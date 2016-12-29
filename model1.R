#   Define an End Of Sentence Token (EOS)
EOS <- "EOS"
filterList <- NULL

testModel <- function(sampleFile, percentage, ngrams = 5, dataset) {
    #   Text prediction model test function
    #   This function reads a sample text file (sampleFile)
    #   Takes a percentage of the model words and n-grams to predict (percentage)
    #   Takes which n-grams are used in the model
    #   a fourth optional input is a test dataset (dataset). If NULL the samplefile
    #   is read, else the dataset is used as sample.
    #   then it runs the model on the sample
    #
    #   Output generated is:
    #   1) the size of the model object
    #   2) the runtime of the model
    #   3) the percentage correctly predicted words
    #
    #   Return variable is the predicted dataset
    library(NLP)
    library(tm)
    library(dplyr)
    library(scales)
    
    #   Load the sample text or use the provide dataset
    if (is.null(dataset)) {
        url <- paste0("../data/sample/", sampleFile)
        if (file.exists(url)) {
            dataset <- readLines(con = url, skipNul = TRUE)
        } else {
            print("No sample text found.\n")
        }
    }
    
    #   Load the model test result file or create a new one
    if (file.exists("../data/model/modelResults.csv")) {
        modelResults <- read.csv(file = "../data/model/modelResults.csv", row.names = 1, as.is = c(1,7))
    } else {
        modelResults <- data.frame(testingTime = character(), modelSize = integer() , execTime = numeric(), maxNgrams = integer(), nGramsUsed = numeric(), predictScore = numeric(), sampleFile = character())
    }
    
    #   Determine the size of the model
    start <- Sys.time()
    #   Determine the system time used by the model
    size <- object.size(score <- predictText(dataset, ngrams, percentage))
    #   Determine finish time of the model and the delta since the start
    finish <- round(as.numeric(difftime(Sys.time(), start, units = "secs")))
    print(paste("Model memory use:", size, "bytes. Execution time:", finish, "seconds. Maximum N-grams:", ngrams, "Percentage of n-grams used:", percent(percentage/100), "Prediction score:", percent(score), "Sample: ", sampleFile))
    modelResults <- rbind(modelResults, data.frame(testingTime = as.character(start), modelSize = as.integer(size) , execTime = finish, maxNgrams = ngrams, nGramsUsed = percent(percentage/100), predictScore = percent(score), textSample = sampleFile))
    write.csv(modelResults, file = "../data/model/modelResults.csv")
    
    return(dataset)
}

preProcess <- function(dataset) {
    #   Function to preprocess text and make it ready for NLP
    #   It makes significant use of the tm package
    #
    #   Load Bad words list to filter out
    if (is.null(filterList)) {
        url = "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
        filterList <- readLines(url)
    }
    
    #   Remove empty rows
    dataset <- dataset[dataset != "FALSE"]
    #   Convert to lowercase
    dataset <- tolower(dataset)
    #   Put an artificial en of sentence token (EOS) at the end of each line
    for (i in 1:length(dataset)) dataset[i] <- paste(dataset[i], EOS)
    #   Break lines into seperate sentences based on end of sentence characters: dot, exclamation or question mark.
    #   I assume a space behind it to avoid breaking lines at a decimal point in a number.
    dataset <- gsub(". "," EOS ", dataset, fixed = TRUE)
    dataset <- gsub("! "," EOS ", dataset, fixed = TRUE)
    dataset <- gsub("? "," EOS ", dataset, fixed = TRUE)
    #   Remove whitespace, tabs and leading blanks
    dataset <- gsub("\t"," ", dataset, fixed = TRUE)
    dataset <- gsub("^ +","", dataset)
    dataset <- gsub(" +"," ", dataset)
    #   Simple profanity filter
    dataset <- removeWords(dataset, filterList)
    #   Remove numbers
    dataset <- removeNumbers(dataset)
    #   Remove special characters like / \ @ * #
    dataset <- removePunctuation(dataset)
    return(dataset)
}

predictText <- function(dataset, ngrams, percentage) {
    #   Text prediction function
    #   Tokenize the dataset to start to prediction process
    #   The parameter ngrams determine the maximum Ngram used
    #   The parameter percentage determine the part of the n-grams actually used for prediction
    #   For simplicity reasons we pre-process the text to remove anomalies which is
    #   not realistic, but it make the task a little easier
    #
    #   Return value: Percentage of correctly predicted words
    
    #   Load the n-grams to start the prediction algorithm
    #   Chop the n-grams to determine the optimal model input size
    #   Sort the n-grams on frequency and
    #   Cut off the low frequency tail at the specified percentage
    #
    if (file.exists("../data/model/freq2gramsList.csv") && ngrams > 1) {
        freq2grams <- read.csv(file = "../data/model/freq2gramsList.csv", row.names = 1)
        print("2-grams loaded")
        len2grams <- nrow(freq2grams)
        freq2gramsSorted <- arrange(freq2grams, desc(Frequency))
        freq2gramsSorted <- freq2gramsSorted[1:round(nrow(freq2gramsSorted)*percentage/100),]
        print(paste0("Of the ", len2grams, " available 2-grams ", percentage, "% is used (", nrow(freq2gramsSorted), ")"))
        rm(freq2grams)
    }
    if (file.exists("../data/model/freq3gramsList.csv") && ngrams > 2) {
        freq3grams <- read.csv(file = "../data/model/freq3gramsList.csv", row.names = 1)
        print("3-grams loaded")
        len3grams <- nrow(freq3grams)
        freq3gramsSorted <- arrange(freq3grams, desc(Frequency))
        freq3gramsSorted <- freq3gramsSorted[1:round(nrow(freq3gramsSorted)*percentage/100),]
        print(paste0("Of the ", len3grams, " available 3-grams ", percentage, "% is used (", nrow(freq3gramsSorted), ")"))
        rm(freq3grams)
    }
    if (file.exists("../data/model/freq4gramsList.csv") && ngrams > 3) {
        freq4grams <- read.csv(file = "../data/model/freq4gramsList.csv", row.names = 1)
        print("4-grams loaded")
        len4grams <- nrow(freq4grams)
        freq4gramsSorted <- arrange(freq4grams, desc(Frequency))
        freq4gramsSorted <- freq4gramsSorted[1:round(nrow(freq4gramsSorted)*percentage/100),]
        print(paste0("Of the ", len4grams, " available 4-grams ", percentage, "% is used (", nrow(freq4gramsSorted), ")"))
        rm(freq4grams)
    }
    if (file.exists("../data/model/freq5gramsList.csv") && ngrams > 4) {
        freq5grams <- read.csv(file = "../data/model/freq5gramsList.csv", row.names = 1)
        print("5-grams loaded")
        len5grams <- nrow(freq5grams)
        freq5gramsSorted <- arrange(freq5grams, desc(Frequency))
        freq5gramsSorted <- freq5gramsSorted[1:round(nrow(freq5gramsSorted)*percentage/100),]
        print(paste0("Of the ", len5grams, " available 5-grams ", percentage, "% is used (", nrow(freq5gramsSorted), ")"))
        rm(freq5grams)
        gc()
    }
    
    #   Reset score counter    
    score <- 0
    #   Define a word in sentence counter
    j <- 0
    #   Define a number of EOS tokens counter
    nbOfEOS <- 0
    #   Tokenize the input text
    tt <- as.Token_Tokenizer(wordpunct_tokenizer)
    wordList <- tt(dataset <- preProcess(dataset))
    predList <- wordList
    nbWords <- length(wordList)
    
    # print("--------------------------------------------------------------------")
    # print(wordList)
    # print("--------------------------------------------------------------------")
    
    #   Move through the text
    for (i in 1:nbWords) {
        #   Skip any End Of Sentence (EOS) token and start a new sentence
        if (wordList[i] == EOS) {
            #   Reset the word in sentence counter
            j <- 0
            #   Increase the number of EOS counter (to calculate the correct prediction score)
            nbOfEOS <- nbOfEOS + 1
        } else {
            #   The model does not predict words by the first provided character, hence the first word of
            #   a sentence is the most frequent start word (= the), the other words are determined by the
            #   maximun size of the n-grams used, and whether sufficient preceding words a precent in the 
            #   sentence
            
            #   Increase the word in sentence counter
            j <- j + 1
            
            if (i > 4 && j > 4 && ngrams > 4) {
                #   There are 4 or more previous words in the sentence, so interigate both the 2, 3, 4 and 5-grams
                #   First the 5-grams are used to predict
                found <- grep(paste0("^", wordList[i-4], " ", wordList[i-3], " ", wordList[i-2], " ", wordList[i-1], " "), freq5gramsSorted$Five_grams)
                if (length(found) > 0) {
                    predList[i] <- substring(freq5gramsSorted[min(found),1], nchar(wordList[i-4]) + nchar(wordList[i-3]) + nchar(wordList[i-2]) + nchar(wordList[i-1]) + 5)
                    if (wordList[i] == predList[i]) score <- score + 1
                } else {
                    #   First the 4-grams are used to predict
                    found <- grep(paste0("^", wordList[i-3], " ", wordList[i-2], " ", wordList[i-1], " "), freq4gramsSorted$Four_grams)
                    if (length(found) > 0) {
                        predList[i] <- substring(freq4gramsSorted[min(found),1], nchar(wordList[i-3]) + nchar(wordList[i-2]) + nchar(wordList[i-1]) + 4)
                        if (wordList[i] == predList[i]) score <- score + 1
                    } else {
                        #   then the 3-grams are used to predict
                        found <- grep(paste0("^", wordList[i-2], " ", wordList[i-1], " "), freq3gramsSorted$Three_grams)
                        if (length(found) > 0) {
                            predList[i] <- substring(freq3gramsSorted[min(found),1], nchar(wordList[i-2]) + nchar(wordList[i-1]) + 3)
                            if (wordList[i] == predList[i]) score <- score + 1
                        } else {
                            #   If no prediction was made via 3-grams, the prediction will be tried using 2-grams
                            found <- grep(paste0("^", wordList[i-1], " "), freq2gramsSorted$Two_grams)
                            if (length(found) > 0) {
                                predList[i] <- substring(freq2gramsSorted[min(found),1], nchar(wordList[i-1]) + 2)
                                if (wordList[i] == predList[i]) score <- score + 1
                            }    
                            else predList[i] <- NA
                        }
                    }
                }
            } else if (i > 3 && j > 3 && ngrams > 3) {
                #   There are 3 or more previous words in the sentence, so interigate both the 2, 3 and 4-grams
                #   First the 4-grams are used to predict
                found <- grep(paste0("^", wordList[i-3], " ", wordList[i-2], " ", wordList[i-1], " "), freq4gramsSorted$Four_grams)
                if (length(found) > 0) {
                    predList[i] <- substring(freq4gramsSorted[min(found),1], nchar(wordList[i-3]) + nchar(wordList[i-2]) + nchar(wordList[i-1]) + 4)
                    if (wordList[i] == predList[i]) score <- score + 1
                } else {
                    #   then the 3-grams are used to predict
                    found <- grep(paste0("^", wordList[i-2], " ", wordList[i-1], " "), freq3gramsSorted$Three_grams)
                    if (length(found) > 0) {
                        predList[i] <- substring(freq3gramsSorted[min(found),1], nchar(wordList[i-2]) + nchar(wordList[i-1]) + 3)
                        if (wordList[i] == predList[i]) score <- score + 1
                    } else {
                        #   If no prediction was made via 3-grams, the prediction will be tried using 2-grams
                        found <- grep(paste0("^", wordList[i-1], " "), freq2gramsSorted$Two_grams)
                        if (length(found) > 0) {
                            predList[i] <- substring(freq2gramsSorted[min(found),1], nchar(wordList[i-1]) + 2)
                            if (wordList[i] == predList[i]) score <- score + 1
                        }    
                        else predList[i] <- NA
                    }
                }
            } else if (i > 2 && j > 2 && ngrams > 2) {
                #   There are 2 or more previous words in the sentence, so interigate both the 2 and 3-grams
                #   First the 3-grams are used to predict
                found <- grep(paste0("^", wordList[i-2], " ", wordList[i-1], " "), freq3gramsSorted$Three_grams)
                if (length(found) > 0) {
                    predList[i] <- substring(freq3gramsSorted[min(found),1], nchar(wordList[i-2]) + nchar(wordList[i-1]) + 3)
                    if (wordList[i] == predList[i]) score <- score + 1
                } else {
                    #   If no prediction was made via 3-grams, the prediction will be tried using 2-grams
                    found <- grep(paste0("^", wordList[i-1], " "), freq2gramsSorted$Two_grams)
                    if (length(found) > 0) {
                        predList[i] <- substring(freq2gramsSorted[min(found),1], nchar(wordList[i-1]) + 2)
                        if (wordList[i] == predList[i]) score <- score + 1
                    }    
                    else predList[i] <- NA
                }
            } else if (i > 1 && j > 1) {
                #   There's only 1 prevous word in the sentence, so only interigate the 2-grams
                found <- grep(paste0("^", wordList[i-1], " "), freq2gramsSorted$Two_grams)
                if (length(found) > 0) {
                    predList[i] <- substring(freq2gramsSorted[min(found),1], nchar(wordList[i-1]) + 2)
                    if (wordList[i] == predList[i]) score <- score + 1
                } else predList[i] <- NA
            } else {
                #   This is the first word od the sentence so the most likely chance the is that the word is "the"
                predList[i] <- "the"
            }
            
            # Print the incorrectly predicted words for further analysis
            # print(paste("Word ", i, "= ", wordList[i], "and predicted =", predList[i]))
            # if (wordList[i] != predList[i]) print(paste("Word ", i, "= ", wordList[i], "and predicted =", predList[i]))
        }
    }
    score <- score / (nbWords - nbOfEOS)
    return(score)
}