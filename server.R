#   This application provide a user interface for my text prediction model.
#   The application is part of the Coursera Data Specialization Capstone project.
#   
#   This is the server logic for a Shiny web application.
#
#   Author: Paul van der Kooy
#   Date:   December'16
#
#   Load required libraries
library(shiny)
library(dplyr)
library(scales)
library(NLP)
library(tm)

#   Define an End Of Sentence token (EOS)
EOS <- "EOS"
#   Set a maximum number of word predictions
MAXPRED <- 4
#   Set the values for the most likely start of a sentence
STARTSENT <- c("The", "A", "I", "It")
#   Define the word tokenizer
tt <- as.Token_Tokenizer(wordpunct_tokenizer)
#   Load up to 5-grams into memory for prediction and use 50% of the dataset (for best results)
ngrams <- 5
percentage <- 05

#   Load the n-grams to start the prediction algorithm
#   Chop the n-grams to determine the optimal model input size
#   Sort the n-grams on frequency and
#   Cut off the low frequency tail at the specified percentage
if (file.exists("freq5gramsList.csv") && ngrams > 4) {
    freq5grams <- read.csv(file = "freq5gramsList.csv", row.names = 1)
    len5grams <- nrow(freq5grams)
    freq5gramsSorted <- arrange(freq5grams, desc(Frequency))
    freq5gramsSorted <- freq5gramsSorted[1:round(nrow(freq5gramsSorted)*percentage/100),]
    rm(freq5grams)
}
gc()
if (file.exists("freq4gramsList.csv") && ngrams > 3) {
    freq4grams <- read.csv(file = "freq4gramsList.csv", row.names = 1)
    len4grams <- nrow(freq4grams)
    freq4gramsSorted <- arrange(freq4grams, desc(Frequency))
    freq4gramsSorted <- freq4gramsSorted[1:round(nrow(freq4gramsSorted)*percentage/100),]
    rm(freq4grams)
}
gc()
if (file.exists("freq3gramsList.csv") && ngrams > 2) {
    freq3grams <- read.csv(file = "freq3gramsList.csv", row.names = 1)
    len3grams <- nrow(freq3grams)
    freq3gramsSorted <- arrange(freq3grams, desc(Frequency))
    freq3gramsSorted <- freq3gramsSorted[1:round(nrow(freq3gramsSorted)*percentage/100),]
    rm(freq3grams)
}
gc()
if (file.exists("freq2gramsList.csv") && ngrams > 1) {
    freq2grams <- read.csv(file = "freq2gramsList.csv", row.names = 1)
    len2grams <- nrow(freq2grams)
    freq2gramsSorted <- arrange(freq2grams, desc(Frequency))
    freq2gramsSorted <- freq2gramsSorted[1:round(nrow(freq2gramsSorted)*percentage/100),]
    rm(freq2grams)
}
gc()

preProcess2 <- function(dataset) {
    #   Function to preprocess input text and make it ready for NLP

    #   Convert to lowercase
    dataset <- tolower(dataset)
    #   Break lines into seperate sentences based on end of sentence characters: dot, exclamation or question mark.
    dataset <- gsub("."," EOS ", dataset, fixed = TRUE)
    dataset <- gsub("!"," EOS ", dataset, fixed = TRUE)
    dataset <- gsub("?"," EOS ", dataset, fixed = TRUE)
    #   Remove whitespace, tabs and leading blanks
    dataset <- gsub("\t"," ", dataset, fixed = TRUE)
    dataset <- gsub("^ +","", dataset)
    dataset <- gsub(" +"," ", dataset)
    #   Remove special characters like / \ @ * # ' etc.
    dataset <- removePunctuation(dataset)
    return(dataset)
}

predictText2 <- function(dataset) {
    #   Text prediction function
    #   Tokenize the dataset to start to prediction process
    #
    #   Return value: Top MAXPRED predicted words
    #   Define a word in sentence counter
    j <- 0
    #   Tokenize the input text
    wordList <- tt(dataset <- preProcess2(dataset))
    nbWords <- length(wordList)
    #   Initialize parameters
    predList <- c("", "", "", "")
    lastWord <- c("", "", "", "")
    #   Get the last 4 words
    if (nbWords > 0 && wordList[[nbWords]] != EOS) {lastWord[1] <- wordList[[nbWords]]}
    else {lastWord[1] <- ""}
    if (nbWords > 1 && wordList[[nbWords]] != EOS) {lastWord[2] <- wordList[[nbWords-1]]}
    else {lastWord[2] <- ""}
    if (nbWords > 2 && wordList[[nbWords]] != EOS && wordList[[nbWords-1]] != EOS) {lastWord[3] <- wordList[[nbWords-2]]}
    else {lastWord[3] <- ""}
    if (nbWords > 3 && wordList[[nbWords]] != EOS && wordList[[nbWords-1]] != EOS && wordList[[nbWords-2]] != EOS) {lastWord[4] <- wordList[[nbWords-3]]}
    else {lastWord[4] <- ""}
    #   Determine the number of last words in a sentence (= j)
    if      (lastWord[1] == "" || lastWord[1] == EOS) j <- 0
    else if (lastWord[2] == "" || lastWord[2] == EOS) j <- 1
    else if (lastWord[3] == "" || lastWord[3] == EOS) j <- 2
    else if (lastWord[4] == "" || lastWord[4] == EOS) j <- 3
    else j <- max(4, j + 1)
    #   Set counter for the number of predicted words found (= i)
    #   Sorted on most likely word first.
    i <- 0

    #   Find up to MAXPRED predictions for the next word
    if (j > 3 && ngrams > 4) {
        #   There are 4 or more previous words in the sentence
        #   5-grams are available and are used to predict
        found <- grep(paste0("^", lastWord[4], " ", lastWord[3], " ", lastWord[2], " ", lastWord[1], " "), freq5gramsSorted$Five_grams)
        while (!is.na(found[i+1]) && i < MAXPRED) {
            i <- i + 1
            predList[i] <- substring(freq5gramsSorted[found[i],1], nchar(lastWord[4]) + nchar(lastWord[3]) + nchar(lastWord[2]) + nchar(lastWord[1]) + 5)
        }
    } 
    if (j > 2 && ngrams > 3 && i < MAXPRED) {
        #   There are 3 or more previous words in the sentence
        #   4-grams are available and used to predict
        #   The maximum number of next word predictions has not been reached yet
        found <- grep(paste0("^", lastWord[3], " ", lastWord[2], " ", lastWord[1], " "), freq4gramsSorted$Four_grams)
        cnt <- 1
        while (!is.na(found[cnt]) && i < MAXPRED) {
            i <- i + 1
            predList[i] <- substring(freq4gramsSorted[found[cnt],1], nchar(lastWord[3]) + nchar(lastWord[2]) + nchar(lastWord[1]) + 4)
            cnt <- cnt + 1
        }
    }
    if (j > 1 && ngrams > 2 && i < MAXPRED) {
        #   There are 2 or more previous words in the sentence
        #   3-grams are available and used to predict
        #   The maximum number of next word predictions has not been reached yet
        found <- grep(paste0("^", lastWord[2], " ", lastWord[1], " "), freq3gramsSorted$Three_grams)
        cnt <- 1
        while (!is.na(found[cnt]) && i < MAXPRED) {
            i <- i + 1
            predList[i] <- substring(freq3gramsSorted[found[cnt],1], nchar(lastWord[2]) + nchar(lastWord[1]) + 3)
            cnt <- cnt + 1
        }
    }
    if (j > 0 && ngrams > 1 && i < MAXPRED) {
        #   There are 1 or more previous words in the sentence
        #   2-grams are available and used to predict
        #   The maximum number of next word predictions has not been reached yet
        found <- grep(paste0("^", lastWord[1], " "), freq2gramsSorted$Two_grams)
        cnt <- 1
        while (!is.na(found[cnt]) && i < MAXPRED) {
            i <- i + 1
            predList[i] <- substring(freq2gramsSorted[found[cnt],1], nchar(lastWord[1]) + 2)
            cnt <- cnt + 1
        }
    }
    if (j == 0 && i < MAXPRED) {
        #   This is the first word of the sentence so the most likely chance the is that the word is:
        cnt <- 1
        while (i < MAXPRED) {
            i <- i + 1
            predList[i] <- STARTSENT[cnt]
            cnt <- cnt + 1
        }
    }    
    return(predList)
}
    
shinyServer(function(input, output) {
    #   Start the modeling code when the input text changes
    observe({
        input$inputText
        
        #   Predict the most likely 3 next options for the next word
        if (nchar(input$inputText) > 0) {
            predictions <- predictText2(input$inputText)
        } else {
            predictions <- STARTSENT
        }
        
        output$outputText <- renderText({
            input$inputText
        })
        output$prediction1 <- renderText({
            paste("Most likely word =", predictions[1])
        })
        output$prediction2 <- renderText({
            paste("2d choice word =", predictions[2])
        })
        output$prediction3 <- renderText({
            paste("3d choice word =", predictions[3])
        })
        output$prediction4 <- renderText({
            paste("4th choice word =", predictions[4])
        })
    })
})