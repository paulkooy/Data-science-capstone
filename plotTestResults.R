#   Script to plot the model testing results
library(ggplot2)

#   Load the model test results
url <- "../data/model/modelResults.csv"
if (file.exists(url)) {
    modelResults <- read.csv(con = url)
} else {
    print("No model test results found found.\n")
}
#   Script to plot the model testing results
library(ggplot2)

#   Load the model test results
url <- "../data/model/modelTest2.csv"
if (file.exists(url)) {
    modelResults <- read.csv(con = url)
} else {
    print("No model test results found found.\n")
}


#   Plot the result
ggplot(modelResults, aes(predictScore, execTime, color = textSample, shape = factor(maxNgrams), size = factor(nGramsUsed)), geom = c("point")) + 
    +     geom_point(aes(color = textSample)) + 
    +     ggtitle("Prediction score [% correct] versus execution time [seconds]") +
    +     theme_gray() +
    +     theme(text = element_text(size = 15))

ggsave("../data/model/modelResults2.png")
